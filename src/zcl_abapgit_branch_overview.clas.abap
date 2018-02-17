CLASS zcl_abapgit_branch_overview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: run
      IMPORTING io_repo           TYPE REF TO zcl_abapgit_repo_online
      RETURNING VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS: compress
      IMPORTING it_commits        TYPE zif_abapgit_definitions=>ty_commit_tt
      RETURNING VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS: get_branches
      RETURNING VALUE(rt_branches) TYPE zif_abapgit_definitions=>ty_git_branch_list_tt.
  PRIVATE SECTION.

    CLASS-METHODS:
      parse_commits
        IMPORTING it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
        RAISING   zcx_abapgit_exception,
      determine_branch
        RAISING zcx_abapgit_exception,
      determine_merges
        RAISING zcx_abapgit_exception,
      fixes
        RAISING zcx_abapgit_exception,
      get_git_objects
        IMPORTING io_repo           TYPE REF TO zcl_abapgit_repo_online
        RETURNING VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
        RAISING   zcx_abapgit_exception,
      determine_tags
        RAISING zcx_abapgit_exception.

    CLASS-DATA:
      gt_branches TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
      gt_commits  TYPE TABLE OF zif_abapgit_definitions=>ty_commit,
      gt_tags     TYPE zif_abapgit_definitions=>ty_git_branch_list_tt.
ENDCLASS.



CLASS ZCL_ABAPGIT_BRANCH_OVERVIEW IMPLEMENTATION.


  METHOD compress.

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

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF gt_branches,
                   <ls_new>    LIKE LINE OF rt_commits,
                   <ls_temp>   LIKE LINE OF lt_temp,
                   <ls_commit> LIKE LINE OF it_commits.


    LOOP AT gt_branches ASSIGNING <ls_branch>.

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


  METHOD determine_branch.

    CONSTANTS: lc_head TYPE string VALUE 'HEAD'.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF gt_branches,
                   <ls_head>   LIKE LINE OF gt_branches,
                   <ls_commit> LIKE LINE OF gt_commits,
                   <ls_create> LIKE LINE OF <ls_commit>-create.


* exchange HEAD, and make sure the branch determination starts with the HEAD branch
    READ TABLE gt_branches ASSIGNING <ls_head> WITH KEY name = lc_head.
    ASSERT sy-subrc = 0.
    LOOP AT gt_branches ASSIGNING <ls_branch>
        WHERE sha1 = <ls_head>-sha1 AND name <> lc_head.
      <ls_head>-name = <ls_branch>-name.
      DELETE gt_branches INDEX sy-tabix.
      EXIT.
    ENDLOOP.

    LOOP AT gt_branches ASSIGNING <ls_branch>.
      lv_name = <ls_branch>-name+11.
      READ TABLE gt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_branch>-sha1.
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
          READ TABLE gt_commits ASSIGNING <ls_commit>
              WITH KEY sha1 = <ls_commit>-parent1.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD determine_merges.

    FIELD-SYMBOLS: <ls_merged> LIKE LINE OF gt_commits,
                   <ls_commit> LIKE LINE OF gt_commits.


* important: start with the newest first and propagate branches
    SORT gt_commits BY time DESCENDING.

    LOOP AT gt_commits ASSIGNING <ls_commit> WHERE NOT parent2 IS INITIAL.
      ASSERT NOT <ls_commit>-branch IS INITIAL.

      READ TABLE gt_commits ASSIGNING <ls_merged> WITH KEY sha1 = <ls_commit>-parent2.
      IF sy-subrc = 0.
        <ls_commit>-merge = <ls_merged>-branch.

* orphaned, branch has been deleted after merge
        WHILE <ls_merged>-branch IS INITIAL.
          <ls_merged>-branch = <ls_commit>-branch.
          READ TABLE gt_commits ASSIGNING <ls_merged> WITH KEY sha1 = <ls_merged>-parent1.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD determine_tags.

    DATA: lv_tag TYPE LINE OF zif_abapgit_definitions=>ty_commit-tags.

    FIELD-SYMBOLS: <ls_tag>    TYPE zif_abapgit_definitions=>ty_git_branch,
                   <ls_commit> TYPE zif_abapgit_definitions=>ty_commit.

    LOOP AT gt_tags ASSIGNING <ls_tag>.

      READ TABLE gt_commits WITH KEY sha1 = <ls_tag>-sha1
                            ASSIGNING <ls_commit>.
      CHECK sy-subrc = 0.

      lv_tag = zcl_abapgit_tag=>remove_tag_prefix( <ls_tag>-name ).
      INSERT lv_tag INTO TABLE <ls_commit>-tags.

    ENDLOOP.

  ENDMETHOD.


  METHOD fixes.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF gt_commits.


    LOOP AT gt_commits ASSIGNING <ls_commit> WHERE NOT merge IS INITIAL.
* commits from old branches
      IF <ls_commit>-branch = <ls_commit>-merge.
        CLEAR <ls_commit>-merge.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_branches.
    rt_branches = gt_branches.
  ENDMETHOD.


  METHOD get_git_objects.

    DATA: lo_branch_list TYPE REF TO zcl_abapgit_git_branch_list,
          lo_progress    TYPE REF TO zcl_abapgit_progress.


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

    gt_branches = lo_branch_list->get_branches_only( ).
    gt_tags = lo_branch_list->get_tags_only( ).

    zcl_abapgit_git_transport=>upload_pack(
      EXPORTING
        iv_url         = io_repo->get_url( )
        iv_branch_name = io_repo->get_branch_name( )
        iv_deepen      = abap_false
        it_branches    = gt_branches
      IMPORTING
        et_objects     = rt_objects ).

    DELETE rt_objects WHERE type = zif_abapgit_definitions=>gc_type-blob.

  ENDMETHOD.


  METHOD parse_commits.

    DATA: ls_commit LIKE LINE OF gt_commits,
          lv_trash  TYPE string ##NEEDED,
          ls_raw    TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    LOOP AT it_objects ASSIGNING <ls_object> WHERE type = zif_abapgit_definitions=>gc_type-commit.
      ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent1 = ls_raw-parent.
      ls_commit-parent2 = ls_raw-parent2.

      SPLIT ls_raw-body AT zif_abapgit_definitions=>gc_newline INTO ls_commit-message lv_trash.

* unix time stamps are in same time zone, so ignore the zone,
      FIND REGEX zif_abapgit_definitions=>gc_author_regex IN ls_raw-author
        SUBMATCHES
        ls_commit-author
        ls_commit-email
        ls_commit-time ##NO_TEXT.
      ASSERT sy-subrc = 0.
      APPEND ls_commit TO gt_commits.

    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.


    CLEAR gt_branches.
    CLEAR gt_commits.

    lt_objects = get_git_objects( io_repo ).
    parse_commits( lt_objects ).
    CLEAR lt_objects.

    determine_branch( ).
    determine_merges( ).
    determine_tags( ).
    fixes( ).

    SORT gt_commits BY time ASCENDING.

    rt_commits = gt_commits.

  ENDMETHOD.
ENDCLASS.
