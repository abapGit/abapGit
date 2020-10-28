CLASS zcl_abapgit_branch_overview DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_branch_overview .
    CONSTANTS c_deleted_branch_name_prefix TYPE string VALUE '__DELETED_BRANCH_' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        io_repo TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_branches TYPE zif_abapgit_definitions=>ty_git_branch_list_tt .
    DATA mt_commits TYPE zif_abapgit_definitions=>ty_commit_tt .
    DATA mt_tags TYPE zif_abapgit_definitions=>ty_git_tag_list_tt .

    METHODS compress_internal
      IMPORTING
        !iv_name    TYPE string
      CHANGING
        !ct_temp    TYPE zif_abapgit_definitions=>ty_commit_tt
        !ct_commits TYPE zif_abapgit_definitions=>ty_commit_tt .
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


  METHOD compress_internal.

    FIELD-SYMBOLS: <ls_temp>     LIKE LINE OF ct_temp,
                   <ls_new>      LIKE LINE OF ct_commits,
                   <ls_temp_end> LIKE LINE OF ct_temp.


    IF lines( ct_temp ) >= 10.
      READ TABLE ct_temp ASSIGNING <ls_temp> INDEX 1.
      ASSERT sy-subrc = 0.
      READ TABLE ct_temp ASSIGNING <ls_temp_end> INDEX lines( ct_temp ).
      ASSERT sy-subrc = 0.
      APPEND INITIAL LINE TO ct_commits ASSIGNING <ls_new>.
      <ls_new>-sha1       = <ls_temp_end>-sha1.
      <ls_new>-parent1    = <ls_temp>-parent1.
      <ls_new>-time       = <ls_temp>-time.
      <ls_new>-message    = |Compressed, { lines( ct_temp ) } commits|.
      <ls_new>-branch     = iv_name.
      <ls_new>-compressed = abap_true.
    ELSE.
      APPEND LINES OF ct_temp TO ct_commits.
    ENDIF.
    CLEAR ct_temp.

  ENDMETHOD.


  METHOD constructor.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    lt_objects = get_git_objects( io_repo ).

    mt_commits = zcl_abapgit_git_commit=>parse_commits( lt_objects ).
    zcl_abapgit_git_commit=>sort_commits( CHANGING ct_commits = mt_commits ).

    parse_annotated_tags( lt_objects ).

    CLEAR lt_objects.

    determine_branch( ).
    determine_merges( ).
    determine_tags( ).
    fixes( ).

  ENDMETHOD.


  METHOD determine_branch.

    CONSTANTS: lc_head TYPE string VALUE 'HEAD'.

    TYPES: BEGIN OF ty_branch_with_time,
             time TYPE string,
             name TYPE string,
             sha1 TYPE zif_abapgit_definitions=>ty_sha1,
           END OF ty_branch_with_time.

    DATA: lt_branches_sorted_by_time TYPE SORTED TABLE OF ty_branch_with_time WITH NON-UNIQUE KEY time,
          ls_branches_with_time      TYPE ty_branch_with_time.

    FIELD-SYMBOLS: <ls_branch>                LIKE LINE OF mt_branches,
                   <ls_branch_sorted_by_time> LIKE LINE OF lt_branches_sorted_by_time,
                   <ls_head>                  LIKE LINE OF mt_branches,
                   <ls_commit>                LIKE LINE OF mt_commits,
                   <ls_create>                LIKE LINE OF <ls_commit>-create.


* Exchange HEAD, and make sure the branch determination starts with the HEAD branch
    READ TABLE mt_branches ASSIGNING <ls_head> WITH KEY name = lc_head.
    ASSERT sy-subrc = 0.
    LOOP AT mt_branches ASSIGNING <ls_branch>
        WHERE sha1 = <ls_head>-sha1 AND name <> lc_head.
      <ls_head>-name = <ls_branch>-name.
      DELETE mt_branches INDEX sy-tabix.
      EXIT.
    ENDLOOP.

* Sort Branches by Commit Time
    LOOP AT mt_branches ASSIGNING <ls_branch>.

      READ TABLE mt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_branch>-sha1.
      IF sy-subrc = 0.

        ls_branches_with_time-name = <ls_branch>-name+11.
        ls_branches_with_time-sha1 = <ls_branch>-sha1.

        IF <ls_branch>-is_head = abap_true.
          ls_branches_with_time-time = '0000000000'. "Force HEAD to be the first one
        ELSE.
          ls_branches_with_time-time = <ls_commit>-time.
        ENDIF.

        INSERT ls_branches_with_time INTO TABLE lt_branches_sorted_by_time.
        CLEAR ls_branches_with_time.

      ENDIF.
    ENDLOOP.


    LOOP AT lt_branches_sorted_by_time ASSIGNING <ls_branch_sorted_by_time>.

      READ TABLE mt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_branch_sorted_by_time>-sha1.
      ASSERT sy-subrc = 0.

      DO.
        IF <ls_commit>-branch IS INITIAL.
          <ls_commit>-branch = <ls_branch_sorted_by_time>-name.
        ELSE.
          APPEND INITIAL LINE TO <ls_commit>-create ASSIGNING <ls_create>.
          <ls_create>-name = <ls_branch_sorted_by_time>-name.
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

    DATA: BEGIN OF ls_deleted_branch_info,
            created TYPE abap_bool,
            index   TYPE string,
            name    TYPE string,
          END OF ls_deleted_branch_info.

    FIELD-SYMBOLS: <ls_merged_branch_commit> TYPE zif_abapgit_definitions=>ty_commit,
                   <ls_merged_branch_parent> TYPE zif_abapgit_definitions=>ty_commit,
                   <ls_commit>               TYPE zif_abapgit_definitions=>ty_commit,
                   <ls_create>               TYPE zif_abapgit_definitions=>ty_create.

* we need latest first here: latest -> initial
    zcl_abapgit_git_commit=>reverse_sort_order( CHANGING ct_commits = mt_commits ).

    LOOP AT mt_commits ASSIGNING <ls_commit> WHERE NOT parent2 IS INITIAL.
      ASSERT NOT <ls_commit>-branch IS INITIAL.

      READ TABLE mt_commits ASSIGNING <ls_merged_branch_commit> WITH KEY sha1 = <ls_commit>-parent2.
      IF sy-subrc = 0.
        <ls_commit>-merge = <ls_merged_branch_commit>-branch.

* orphaned, branch has been deleted after merge
        ls_deleted_branch_info-created = abap_false.

        WHILE <ls_merged_branch_commit>-branch IS INITIAL.
          IF ls_deleted_branch_info-created = abap_false.

            ls_deleted_branch_info-created = abap_true.
            ls_deleted_branch_info-index = ls_deleted_branch_info-index + 1.
            ls_deleted_branch_info-name = c_deleted_branch_name_prefix && ls_deleted_branch_info-index && '__'.
            CONDENSE ls_deleted_branch_info-name NO-GAPS.

            <ls_commit>-merge = ls_deleted_branch_info-name.

          ENDIF.
          <ls_merged_branch_commit>-branch = ls_deleted_branch_info-name.

          READ TABLE mt_commits ASSIGNING <ls_merged_branch_parent>
                                WITH KEY sha1 = <ls_merged_branch_commit>-parent1.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            ASSIGN <ls_merged_branch_parent> TO <ls_merged_branch_commit>.
          ENDIF.
        ENDWHILE.

        IF <ls_merged_branch_parent> IS ASSIGNED.
          APPEND INITIAL LINE TO <ls_merged_branch_parent>-create ASSIGNING <ls_create>.
          <ls_create>-name = ls_deleted_branch_info-name.
          <ls_create>-parent = <ls_commit>-branch.
        ENDIF.

      ENDIF.
    ENDLOOP.

    " switch back to initial -> latest
    zcl_abapgit_git_commit=>reverse_sort_order( CHANGING ct_commits = mt_commits ).

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

      lv_tag = zcl_abapgit_git_tag=>remove_tag_prefix( <ls_tag>-name ).
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
          li_progress          TYPE REF TO zif_abapgit_progress,
          lt_branches_and_tags TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
          lt_tags              TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
          ls_tag               LIKE LINE OF mt_tags.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_tags.


    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git objects { io_repo->get_name( ) }| ).

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

    zcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = io_repo->get_url( )
        iv_branch_name  = io_repo->get_selected_branch( )
        iv_deepen_level = 0
        it_branches     = lt_branches_and_tags
      IMPORTING
        et_objects      = rt_objects ).

    DELETE rt_objects WHERE type = zif_abapgit_definitions=>c_type-blob.


  ENDMETHOD.


  METHOD parse_annotated_tags.

    DATA: ls_raw TYPE zcl_abapgit_git_pack=>ty_tag.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <ls_tag>    LIKE LINE OF mt_tags.

    LOOP AT it_objects ASSIGNING <ls_object> USING KEY type
        WHERE type = zif_abapgit_definitions=>c_type-tag.

      ls_raw = zcl_abapgit_git_pack=>decode_tag( <ls_object>-data ).

      READ TABLE mt_tags ASSIGNING <ls_tag>
                         WITH KEY sha1 = <ls_object>-sha1.
      ASSERT sy-subrc = 0.

      <ls_tag>-name         = zif_abapgit_definitions=>c_git_branch-tags_prefix && ls_raw-tag.
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


  METHOD zif_abapgit_branch_overview~compress.

    DATA: lv_previous TYPE i,
          lv_index    TYPE i,
          lv_name     TYPE string,
          lt_temp     LIKE it_commits.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF mt_branches,
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
          compress_internal(
            EXPORTING
              iv_name    = lv_name
            CHANGING
              ct_temp    = lt_temp
              ct_commits = rt_commits ).
        ENDIF.

        lv_previous = lv_index.

        APPEND <ls_commit> TO lt_temp.

      ENDLOOP.

      compress_internal(
        EXPORTING
          iv_name    = lv_name
        CHANGING
          ct_temp    = lt_temp
          ct_commits = rt_commits ).

    ENDLOOP.

    zcl_abapgit_git_commit=>sort_commits( CHANGING ct_commits = rt_commits ).

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
