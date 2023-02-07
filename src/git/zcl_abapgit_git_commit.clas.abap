CLASS zcl_abapgit_git_commit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_pull_result,
        commits TYPE zif_abapgit_definitions=>ty_commit_tt,
        commit  TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_pull_result .

    CLASS-METHODS get_by_branch
      IMPORTING
        !iv_branch_name       TYPE string
        !iv_repo_url          TYPE string
        !iv_deepen_level      TYPE i
        !iv_sorted            TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rs_pull_result) TYPE ty_pull_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_by_commit
      IMPORTING
        !iv_commit_hash   TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_repo_url      TYPE string
        !iv_deepen_level  TYPE i
      RETURNING
        VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS parse_commits
      IMPORTING
        !it_objects       TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sort_commits
      CHANGING
        !ct_commits TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS reverse_sort_order
      CHANGING
        !ct_commits TYPE zif_abapgit_definitions=>ty_commit_tt .
    CLASS-METHODS clear_missing_parents CHANGING ct_commits TYPE zif_abapgit_definitions=>ty_commit_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_sha1_range TYPE RANGE OF zif_abapgit_git_definitions=>ty_sha1 .

    CLASS-METHODS get_1st_child_commit
      IMPORTING
        it_commit_sha1s TYPE ty_sha1_range
      EXPORTING
        et_commit_sha1s TYPE ty_sha1_range
        es_1st_commit   TYPE zif_abapgit_definitions=>ty_commit
      CHANGING
        ct_commits      TYPE zif_abapgit_definitions=>ty_commit_tt .

    CLASS-METHODS is_missing
      IMPORTING
        it_commits       TYPE zif_abapgit_definitions=>ty_commit_tt
        iv_sha1          TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS extract_author_data
      IMPORTING
        !iv_author TYPE string
      EXPORTING
        !ev_author TYPE zif_abapgit_definitions=>ty_commit-author
        !ev_email  TYPE zif_abapgit_definitions=>ty_commit-email
        !ev_time   TYPE zif_abapgit_definitions=>ty_commit-time
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_git_commit IMPLEMENTATION.

  METHOD extract_author_data.

    " unix time stamps are in same time zone, so ignore the zone
    FIND REGEX zif_abapgit_definitions=>c_author_regex IN iv_author
      SUBMATCHES
      ev_author
      ev_email
      ev_time.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error author regex value='{ iv_author }'| ).
    ENDIF.

  ENDMETHOD.

  METHOD clear_missing_parents.

    "Part of #4719 to handle cut commit sequences, todo

    FIELD-SYMBOLS: <ls_commit> TYPE zif_abapgit_definitions=>ty_commit.

    LOOP AT ct_commits ASSIGNING <ls_commit>.

      IF is_missing( it_commits = ct_commits
                     iv_sha1  = <ls_commit>-parent1 ) = abap_true.
        CLEAR <ls_commit>-parent1.
      ENDIF.

      IF is_missing( it_commits = ct_commits
                     iv_sha1  = <ls_commit>-parent2 ) = abap_true.
        CLEAR <ls_commit>-parent2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_1st_child_commit.

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


  METHOD get_by_branch.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.

    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { iv_repo_url }| ).

    zcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = iv_repo_url
        iv_branch_name  = iv_branch_name
        iv_deepen_level = iv_deepen_level
      IMPORTING
        ev_branch       = rs_pull_result-commit
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type <> zif_abapgit_definitions=>c_type-commit.

    rs_pull_result-commits = parse_commits( lt_objects ).

    IF iv_sorted = abap_true.
      sort_commits( CHANGING ct_commits = rs_pull_result-commits ).
    ENDIF.

  ENDMETHOD.


  METHOD get_by_commit.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.

    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { iv_repo_url }| ).

    zcl_abapgit_git_transport=>upload_pack_by_commit(
      EXPORTING
        iv_url          = iv_repo_url
        iv_deepen_level = iv_deepen_level
        iv_hash         = iv_commit_hash
      IMPORTING
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type <> zif_abapgit_definitions=>c_type-commit.

    rt_commits = parse_commits( lt_objects ).
    sort_commits( CHANGING ct_commits = rt_commits ).

  ENDMETHOD.


  METHOD is_missing.

    IF iv_sha1 IS NOT INITIAL.

      READ TABLE it_commits
        TRANSPORTING NO FIELDS
        WITH KEY sha1 = iv_sha1.
      rv_result = boolc( sy-subrc <> 0 ).

    ENDIF.

  ENDMETHOD.


  METHOD parse_commits.

    DATA: ls_commit TYPE zif_abapgit_definitions=>ty_commit,
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

      SPLIT ls_raw-body AT cl_abap_char_utilities=>newline INTO TABLE lt_body.

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

      extract_author_data(
        EXPORTING
          iv_author = ls_raw-author
        IMPORTING
          ev_author = ls_commit-author
          ev_email  = ls_commit-email
          ev_time   = ls_commit-time ).

      APPEND ls_commit TO rt_commits.

    ENDLOOP.

  ENDMETHOD.


  METHOD reverse_sort_order.

    DATA: lt_commits           TYPE zif_abapgit_definitions=>ty_commit_tt.
    FIELD-SYMBOLS: <ls_commit> TYPE zif_abapgit_definitions=>ty_commit.

    LOOP AT ct_commits ASSIGNING <ls_commit>.
      INSERT <ls_commit> INTO lt_commits INDEX 1.
    ENDLOOP.
    ct_commits = lt_commits.
    FREE lt_commits.

  ENDMETHOD.


  METHOD sort_commits.

    DATA: lt_sorted_commits TYPE zif_abapgit_definitions=>ty_commit_tt,
          ls_next_commit    TYPE zif_abapgit_definitions=>ty_commit,
          lt_parents        TYPE ty_sha1_range,
          ls_parent         LIKE LINE OF lt_parents.

    FIELD-SYMBOLS: <ls_initial_commit> TYPE zif_abapgit_definitions=>ty_commit.

    " find initial commit
    READ TABLE ct_commits ASSIGNING <ls_initial_commit> WITH KEY parent1 = space.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Cannot find initial commit. Too many commits. Action not possible.| ).
    ENDIF.

    ls_parent-sign   = 'I'.
    ls_parent-option = 'EQ'.
    ls_parent-low    = <ls_initial_commit>-sha1.
    INSERT ls_parent INTO TABLE lt_parents.

    " first commit
    INSERT <ls_initial_commit> INTO TABLE lt_sorted_commits.

    " remove from available commits
    DELETE ct_commits WHERE sha1 = <ls_initial_commit>-sha1.

    DO.
      get_1st_child_commit( EXPORTING it_commit_sha1s = lt_parents
                            IMPORTING et_commit_sha1s = lt_parents
                                      es_1st_commit   = ls_next_commit
                            CHANGING  ct_commits      = ct_commits ).
      IF ls_next_commit IS INITIAL.
        EXIT. "DO
      ENDIF.
      INSERT ls_next_commit INTO TABLE lt_sorted_commits.
    ENDDO.

    ct_commits = lt_sorted_commits.

  ENDMETHOD.
ENDCLASS.
