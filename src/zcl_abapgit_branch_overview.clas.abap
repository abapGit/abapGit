CLASS zcl_abapgit_branch_overview DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_branch_overview .

    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    TYPES:
      ty_commits TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_commit WITH DEFAULT KEY .

    DATA mt_branches TYPE zif_abapgit_definitions=>ty_git_branch_list_tt .
    DATA mt_commits TYPE ty_commits .
    DATA mt_tags TYPE zif_abapgit_definitions=>ty_git_tag_list_tt .

    CLASS-METHODS parse_commits
      IMPORTING
        !it_objects       TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rt_commits) TYPE ty_commits
      RAISING
        zcx_abapgit_exception .
    METHODS parse_annotated_tags
      IMPORTING
        !it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_branch
      RAISING
        zcx_abapgit_exception .
    METHODS determine_merges
      RAISING
        zcx_abapgit_exception .
    METHODS fixes
      RAISING
        zcx_abapgit_exception .
    METHODS get_git_objects
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_tags
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_BRANCH_OVERVIEW IMPLEMENTATION.


  METHOD constructor.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    CLEAR mt_branches.

    lt_objects = get_git_objects( io_repo ).
    mt_commits = parse_commits( lt_objects ).
    parse_annotated_tags( lt_objects ).

    CLEAR lt_objects.

    determine_branch( ).
    determine_merges( ).
    determine_tags( ).
    fixes( ).

    SORT mt_commits BY time ASCENDING.

  ENDMETHOD.


  METHOD determine_branch.

    CONSTANTS: lc_head TYPE string VALUE 'HEAD'.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF mt_branches,
                   <ls_head>   LIKE LINE OF mt_branches,
                   <ls_commit> LIKE LINE OF mt_commits,
                   <ls_create> LIKE LINE OF <ls_commit>-create.


* exchange HEAD, and make sure the branch determination starts with the HEAD branch
    READ TABLE mt_branches ASSIGNING <ls_head> WITH KEY name = lc_head.
    ASSERT sy-subrc = 0.
    LOOP AT mt_branches ASSIGNING <ls_branch>
        WHERE sha1 = <ls_head>-sha1 AND name <> lc_head.
      <ls_head>-name = <ls_branch>-name.
      DELETE mt_branches INDEX sy-tabix.
      EXIT.
    ENDLOOP.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      lv_name = <ls_branch>-name+11.
      READ TABLE mt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_branch>-sha1.
      ASSERT sy-subrc = 0.

      DO.
        IF <ls_commit>-branch IS INITIAL.
          <ls_commit>-branch = lv_name.
        ELSE.
          APPEND INITIAL LINE TO <ls_commit>-create ASSIGNING <ls_create>.
          <ls_create>-name = lv_name.
          <ls_create>-parent = <ls_commit>-branch.
          EXIT.
        ENDIF.

        IF <ls_commit>-parent1 IS INITIAL.
          EXIT.
        ELSE.
          READ TABLE mt_commits ASSIGNING <ls_commit>
              WITH KEY sha1 = <ls_commit>-parent1.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD determine_merges.

    FIELD-SYMBOLS: <ls_merged> LIKE LINE OF mt_commits,
                   <ls_commit> LIKE LINE OF mt_commits.


* important: start with the newest first and propagate branches
    SORT mt_commits BY time DESCENDING.

    LOOP AT mt_commits ASSIGNING <ls_commit> WHERE NOT parent2 IS INITIAL.
      ASSERT NOT <ls_commit>-branch IS INITIAL.

      READ TABLE mt_commits ASSIGNING <ls_merged> WITH KEY sha1 = <ls_commit>-parent2.
      IF sy-subrc = 0.
        <ls_commit>-merge = <ls_merged>-branch.

* orphaned, branch has been deleted after merge
        WHILE <ls_merged>-branch IS INITIAL.
          <ls_merged>-branch = <ls_commit>-branch.
          READ TABLE mt_commits ASSIGNING <ls_merged> WITH KEY sha1 = <ls_merged>-parent1.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD determine_tags.

    DATA: lv_tag TYPE LINE OF zif_abapgit_definitions=>ty_commit-tags.

    FIELD-SYMBOLS: <ls_tag>    LIKE LINE OF mt_tags,
                   <ls_commit> LIKE LINE OF mt_commits.

    LOOP AT mt_tags ASSIGNING <ls_tag>.

      IF <ls_tag>-type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
        READ TABLE mt_commits WITH KEY sha1 = <ls_tag>-sha1
                              ASSIGNING <ls_commit>.      "#EC CI_SUBRC
      ELSEIF <ls_tag>-type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
        READ TABLE mt_commits WITH KEY sha1 = <ls_tag>-object
                              ASSIGNING <ls_commit>.
      ENDIF.

      CHECK sy-subrc = 0.

      lv_tag = zcl_abapgit_tag=>remove_tag_prefix( <ls_tag>-name ).
      INSERT lv_tag INTO TABLE <ls_commit>-tags.

    ENDLOOP.

  ENDMETHOD.


  METHOD fixes.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF mt_commits.


    LOOP AT mt_commits ASSIGNING <ls_commit> WHERE NOT merge IS INITIAL.
* commits from old branches
      IF <ls_commit>-branch = <ls_commit>-merge.
        CLEAR <ls_commit>-merge.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_git_objects.

    DATA: lo_branch_list       TYPE REF TO zcl_abapgit_git_branch_list,
          lo_progress          TYPE REF TO zcl_abapgit_progress,
          lt_branches_and_tags TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
          lt_tags              TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
          ls_tag               LIKE LINE OF mt_tags.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_tags.


    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = 1.

    lo_progress->show(
      iv_current = 1
      iv_text    = |Get git objects { io_repo->get_name( ) }| ) ##NO_TEXT.

* get objects directly from git, mo_repo only contains a shallow clone of only
* the selected branch

    "TODO refactor

    lo_branch_list = zcl_abapgit_git_transport=>branches( io_repo->get_url( ) ).

    mt_branches = lo_branch_list->get_branches_only( ).
    INSERT LINES OF mt_branches INTO TABLE lt_branches_and_tags.

    lt_tags = lo_branch_list->get_tags_only( ).
    INSERT LINES OF lt_tags INTO TABLE lt_branches_and_tags.

    LOOP AT lt_tags ASSIGNING <ls_branch>.

      IF <ls_branch>-name CP '*^{}'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_branch> TO ls_tag.
      INSERT ls_tag INTO TABLE mt_tags.
    ENDLOOP.

    zcl_abapgit_git_transport=>upload_pack(
      EXPORTING
        iv_url         = io_repo->get_url( )
        iv_branch_name = io_repo->get_branch_name( )
        iv_deepen      = abap_false
        it_branches    = lt_branches_and_tags
      IMPORTING
        et_objects     = rt_objects ).

    DELETE rt_objects WHERE type = zif_abapgit_definitions=>gc_type-blob.

  ENDMETHOD.


  METHOD parse_annotated_tags.

    DATA: ls_raw TYPE zcl_abapgit_git_pack=>ty_tag.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <ls_tag>    LIKE LINE OF mt_tags.

    LOOP AT it_objects ASSIGNING <ls_object> USING KEY type
        WHERE type = zif_abapgit_definitions=>gc_type-tag.

      ls_raw = zcl_abapgit_git_pack=>decode_tag( <ls_object>-data ).

      READ TABLE mt_tags ASSIGNING <ls_tag>
                         WITH KEY sha1 = <ls_object>-sha1.
      ASSERT sy-subrc = 0.

      <ls_tag>-name         = |refs/tags/{ ls_raw-tag }|.
      <ls_tag>-sha1         = <ls_object>-sha1.
      <ls_tag>-object       = ls_raw-object.
      <ls_tag>-type         = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
      <ls_tag>-display_name = ls_raw-tag.
      <ls_tag>-tagger_name  = ls_raw-tagger_name.
      <ls_tag>-tagger_email = ls_raw-tagger_email.
      <ls_tag>-message      = ls_raw-message.
      <ls_tag>-body         = ls_raw-body.

    ENDLOOP.

  ENDMETHOD.


  METHOD parse_commits.

    DATA: ls_commit LIKE LINE OF mt_commits,
          lt_body   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          ls_raw    TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    LOOP AT it_objects ASSIGNING <ls_object> USING KEY type
        WHERE type = zif_abapgit_definitions=>gc_type-commit.
      ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent1 = ls_raw-parent.
      ls_commit-parent2 = ls_raw-parent2.

      SPLIT ls_raw-body AT zif_abapgit_definitions=>gc_newline INTO TABLE lt_body.

      READ TABLE lt_body WITH KEY table_line = ' -----END PGP SIGNATURE-----' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE lt_body TO sy-tabix.
        DELETE lt_body TO 2.
      ENDIF.

      READ TABLE lt_body INDEX 1 INTO ls_commit-message.  "#EC CI_SUBRC

* unix time stamps are in same time zone, so ignore the zone,
      FIND REGEX zif_abapgit_definitions=>gc_author_regex IN ls_raw-author
        SUBMATCHES
        ls_commit-author
        ls_commit-email
        ls_commit-time ##NO_TEXT.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error author regex' ).
      ENDIF.
      APPEND ls_commit TO rt_commits.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_branch_overview~compress.

    DEFINE _compress.
      IF lines( lt_temp ) >= 10.
        READ TABLE lt_temp ASSIGNING <ls_temp> INDEX 1.
        ASSERT sy-subrc = 0.
        APPEND INITIAL LINE TO rt_commits ASSIGNING <ls_new>.
        <ls_new>-time       = <ls_temp>-time.
        <ls_new>-message    = |Compressed, { lines( lt_temp ) } commits|.
        <ls_new>-branch     = lv_name.
        <ls_new>-compressed = abap_true.
      ELSE.
        APPEND LINES OF lt_temp TO rt_commits.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lv_previous TYPE i,
          lv_index    TYPE i,
          lv_name     TYPE string,
          lt_temp     LIKE it_commits.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF mt_branches,
                   <ls_new>    LIKE LINE OF rt_commits,
                   <ls_temp>   LIKE LINE OF lt_temp,
                   <ls_commit> LIKE LINE OF it_commits.


    LOOP AT mt_branches ASSIGNING <ls_branch>.

      CLEAR lt_temp.
      lv_name = <ls_branch>-name+11.

      LOOP AT it_commits ASSIGNING <ls_commit>
          WHERE branch = lv_name.
        lv_index = sy-tabix.

        IF NOT <ls_commit>-merge IS INITIAL
            OR NOT <ls_commit>-create IS INITIAL.
* always show these vertices
          lv_previous = -1.
        ENDIF.

        IF lv_previous + 1 <> sy-tabix.
          _compress.
          CLEAR lt_temp.
        ENDIF.

        lv_previous = lv_index.

        APPEND <ls_commit> TO lt_temp.

      ENDLOOP.

      _compress.

    ENDLOOP.

    SORT rt_commits BY time ASCENDING.

  ENDMETHOD.


  METHOD zif_abapgit_branch_overview~get_branches.
    rt_branches = mt_branches.
  ENDMETHOD.


  METHOD zif_abapgit_branch_overview~get_commits.
    rt_commits = mt_commits.
  ENDMETHOD.


  METHOD zif_abapgit_branch_overview~get_tags.

    rt_tags = mt_tags.

  ENDMETHOD.
ENDCLASS.
