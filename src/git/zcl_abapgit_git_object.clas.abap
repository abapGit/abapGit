CLASS zcl_abapgit_git_object DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    CLASS-METHODS parse_commits
      IMPORTING
        !it_objects       TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sort_commits
      CHANGING
        !ct_commits TYPE zif_abapgit_definitions=>ty_commit_tt .


  PRIVATE SECTION.
    CLASS-METHODS _get_1st_child_commit
      IMPORTING
         it_commit_sha1s TYPE zif_abapgit_definitions=>ty_sha1_range
      EXPORTING
         et_commit_sha1s TYPE zif_abapgit_definitions=>ty_sha1_range
         es_1st_commit   TYPE zif_abapgit_definitions=>ty_commit
      CHANGING
         ct_commits      TYPE zif_abapgit_definitions=>ty_commit_tt .
    DATA mt_commits TYPE zif_abapgit_definitions=>ty_commit_tt .

ENDCLASS.



CLASS ZCL_ABAPGIT_GIT_OBJECT IMPLEMENTATION.


  METHOD _get_1st_child_commit.

    DATA: lt_1stchild_commits TYPE zif_abapgit_definitions=>ty_commit_tt,
          ls_parent           LIKE LINE OF it_commit_sha1s,
          lt_commit_sha1s     LIKE it_commit_sha1s.

    FIELD-SYMBOLS: <ls_child_commit> TYPE zif_abapgit_definitions=>ty_commit.

    CLEAR: es_1st_commit.

* get all reachable next commits
    lt_commit_sha1s = it_commit_sha1s.
    LOOP AT ct_commits ASSIGNING <ls_child_commit> WHERE parent1 IN lt_commit_sha1s
                                                      OR parent2 IN lt_commit_sha1s.
      INSERT <ls_child_commit> INTO TABLE lt_1stchild_commits.
    ENDLOOP.

* return oldest one
    SORT lt_1stchild_commits BY time ASCENDING.
    READ TABLE lt_1stchild_commits INTO es_1st_commit INDEX 1.

* remove from available commits
    DELETE ct_commits WHERE sha1 = es_1st_commit-sha1.

* set relevant parent commit sha1s
    IF lines( lt_1stchild_commits ) = 1.
      CLEAR et_commit_sha1s.
    ELSE.
      et_commit_sha1s = it_commit_sha1s.
    ENDIF.

    ls_parent-sign   = 'I'.
    ls_parent-option = 'EQ'.
    ls_parent-low    = es_1st_commit-sha1.
    INSERT ls_parent INTO TABLE et_commit_sha1s.

  ENDMETHOD.


  METHOD parse_commits.

    DATA: ls_commit LIKE LINE OF mt_commits,
          lt_body   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          ls_raw    TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <lv_body>   TYPE string.


    LOOP AT it_objects ASSIGNING <ls_object> USING KEY type
        WHERE type = zif_abapgit_definitions=>c_type-commit.
      ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent1 = ls_raw-parent.
      ls_commit-parent2 = ls_raw-parent2.

      SPLIT ls_raw-body AT zif_abapgit_definitions=>c_newline INTO TABLE lt_body.

      READ TABLE lt_body WITH KEY table_line = ' -----END PGP SIGNATURE-----' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE lt_body TO sy-tabix.
        DELETE lt_body TO 2.
      ENDIF.

      READ TABLE lt_body INDEX 1 INTO ls_commit-message.  "#EC CI_SUBRC
      " The second line is always empty. Therefore we omit it.
      LOOP AT lt_body ASSIGNING <lv_body>
                      FROM 3.
        INSERT <lv_body> INTO TABLE ls_commit-body.
      ENDLOOP.

      zcl_abapgit_utils=>extract_author_data(
        EXPORTING
          iv_author = ls_raw-author
        IMPORTING
          ev_author = ls_commit-author
          ev_email  = ls_commit-email
          ev_time   = ls_commit-time ).

      APPEND ls_commit TO rt_commits.

    ENDLOOP.

  ENDMETHOD.


  METHOD sort_commits.

    DATA: lt_sorted_commits TYPE zif_abapgit_definitions=>ty_commit_tt,
          ls_next_commit    TYPE zif_abapgit_definitions=>ty_commit,
          lt_parents        TYPE zif_abapgit_definitions=>ty_sha1_range,
          ls_parent         LIKE LINE OF lt_parents.

    FIELD-SYMBOLS: <ls_initial_commit> TYPE zif_abapgit_definitions=>ty_commit.

* find initial commit
    READ TABLE ct_commits ASSIGNING <ls_initial_commit> WITH KEY parent1 = space.
    IF sy-subrc = 0.

      ls_parent-sign   = 'I'.
      ls_parent-option = 'EQ'.
      ls_parent-low    = <ls_initial_commit>-sha1.
      INSERT ls_parent INTO TABLE lt_parents.

* first commit
      INSERT <ls_initial_commit> INTO TABLE lt_sorted_commits.

* remove from available commits
      DELETE ct_commits WHERE sha1 = <ls_initial_commit>-sha1.

      DO.
        _get_1st_child_commit( EXPORTING it_commit_sha1s = lt_parents
                               IMPORTING et_commit_sha1s = lt_parents
                                         es_1st_commit   = ls_next_commit
                               CHANGING  ct_commits      = ct_commits ).
        IF ls_next_commit IS INITIAL.
          EXIT. "DO
        ENDIF.
        INSERT ls_next_commit INTO TABLE lt_sorted_commits.
      ENDDO.

      ct_commits = lt_sorted_commits.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
