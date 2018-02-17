CLASS zcl_abapgit_merge DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        IMPORTING io_repo         TYPE REF TO zcl_abapgit_repo_online
                  iv_source       TYPE string
                  iv_target       TYPE string
        RETURNING VALUE(rs_merge) TYPE zif_abapgit_definitions=>ty_merge
        RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA: gs_merge   TYPE zif_abapgit_definitions=>ty_merge,
                gt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    TYPES: ty_ancestor_tt TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_ancestor WITH DEFAULT KEY.

    CLASS-METHODS:
      all_files
        RETURNING VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_expanded_tt,
      calculate_result
        RAISING zcx_abapgit_exception,
      find_ancestors
        IMPORTING iv_commit           TYPE zif_abapgit_definitions=>ty_sha1
        RETURNING VALUE(rt_ancestors) TYPE ty_ancestor_tt
        RAISING   zcx_abapgit_exception,
      find_first_common
        IMPORTING it_list1         TYPE ty_ancestor_tt
                  it_list2         TYPE ty_ancestor_tt
        RETURNING VALUE(rs_common) TYPE zif_abapgit_definitions=>ty_ancestor
        RAISING   zcx_abapgit_exception,
      fetch_git
        IMPORTING iv_source TYPE string
                  iv_target TYPE string
        RAISING   zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_MERGE IMPLEMENTATION.


  METHOD all_files.

    APPEND LINES OF gs_merge-stree TO rt_files.
    APPEND LINES OF gs_merge-ttree TO rt_files.
    APPEND LINES OF gs_merge-ctree TO rt_files.
    SORT rt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_files COMPARING path name.

  ENDMETHOD.


  METHOD calculate_result.

    DEFINE _from_source.
      READ TABLE gt_objects ASSIGNING <ls_object>
        WITH KEY type = zif_abapgit_definitions=>gc_type-blob
        sha1 = <ls_source>-sha1.
      ASSERT sy-subrc = 0.

      gs_merge-stage->add( iv_path     = <ls_file>-path
                           iv_filename = <ls_file>-name
                           iv_data     = <ls_object>-data ).
    END-OF-DEFINITION.

    DATA: lt_files        TYPE zif_abapgit_definitions=>ty_expanded_tt,
          lv_found_source TYPE abap_bool,
          lv_found_target TYPE abap_bool,
          lv_found_common TYPE abap_bool.

    FIELD-SYMBOLS: <ls_source> LIKE LINE OF lt_files,
                   <ls_target> LIKE LINE OF lt_files,
                   <ls_common> LIKE LINE OF lt_files,
                   <ls_file>   LIKE LINE OF lt_files,
                   <ls_result> LIKE LINE OF gs_merge-result,
                   <ls_object> LIKE LINE OF gt_objects.


    lt_files = all_files( ).

    CREATE OBJECT gs_merge-stage
      EXPORTING
        iv_branch_name  = gs_merge-target-name
        iv_branch_sha1  = gs_merge-target-sha1
        iv_merge_source = gs_merge-source-sha1.

    LOOP AT lt_files ASSIGNING <ls_file>.

      UNASSIGN <ls_source>.
      UNASSIGN <ls_target>.
      UNASSIGN <ls_common>.

      READ TABLE gs_merge-stree ASSIGNING <ls_source>
        WITH KEY path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE gs_merge-ttree ASSIGNING <ls_target>
        WITH KEY path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE gs_merge-ctree ASSIGNING <ls_common>
        WITH KEY path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC

      lv_found_source = boolc( <ls_source> IS ASSIGNED ).
      lv_found_target = boolc( <ls_target> IS ASSIGNED ).
      lv_found_common = boolc( <ls_common> IS ASSIGNED ).

      IF lv_found_source = abap_false
          AND lv_found_target = abap_false.
* deleted in source and target, skip
        CONTINUE.
      ELSEIF lv_found_source = abap_false
          AND lv_found_common = abap_true
          AND <ls_target>-sha1 = <ls_common>-sha1.
* deleted in source, skip
        gs_merge-stage->rm( iv_path     = <ls_file>-path
                            iv_filename = <ls_file>-name ).
        CONTINUE.
      ELSEIF lv_found_target = abap_false
          AND lv_found_common = abap_true
          AND <ls_source>-sha1 = <ls_common>-sha1.
* deleted in target, skip
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO gs_merge-result ASSIGNING <ls_result>.
      <ls_result>-path = <ls_file>-path.
      <ls_result>-name = <ls_file>-name.

      IF lv_found_target = abap_false.
* added in source
        _from_source.
        <ls_result>-sha1 = <ls_source>-sha1.
        CONTINUE.
      ELSEIF lv_found_source = abap_false.
* added in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 = <ls_source>-sha1.
* added in source and target
        <ls_result>-sha1 = <ls_source>-sha1.
      ENDIF.

      IF lv_found_source = abap_false
          OR lv_found_target = abap_false
          OR lv_found_common = abap_false.
        CLEAR gs_merge-result.
        gs_merge-conflict = |{ <ls_file>-name
          } merge conflict, not found anywhere|.
        RETURN.
      ENDIF.

      IF <ls_target>-sha1 = <ls_source>-sha1.
* target and source match
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_target>-sha1 = <ls_common>-sha1.
* changed in source
        _from_source.
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_source>-sha1 = <ls_common>-sha1.
* changed in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSE.
* changed in source and target, conflict
        CLEAR gs_merge-result.
        gs_merge-conflict = |{ <ls_file>-name
          } merge conflict, changed in source and target branch|.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fetch_git.

    DATA: lo_branch_list TYPE REF TO zcl_abapgit_git_branch_list,
          lt_upload      TYPE zif_abapgit_definitions=>ty_git_branch_list_tt.


    lo_branch_list  = zcl_abapgit_git_transport=>branches( gs_merge-repo->get_url( ) ).
    gs_merge-source = lo_branch_list->find_by_name(
      zcl_abapgit_git_branch_list=>complete_heads_branch_name( iv_source ) ).
    gs_merge-target = lo_branch_list->find_by_name(
      zcl_abapgit_git_branch_list=>complete_heads_branch_name( iv_target ) ).

    APPEND gs_merge-source TO lt_upload.
    APPEND gs_merge-target TO lt_upload.

    zcl_abapgit_git_transport=>upload_pack(
      EXPORTING
        iv_url         = gs_merge-repo->get_url( )
        iv_branch_name = gs_merge-repo->get_branch_name( )
        iv_deepen      = abap_false
        it_branches    = lt_upload
      IMPORTING
        et_objects     = gt_objects ).

  ENDMETHOD.


  METHOD find_ancestors.

    DEFINE _visit.
      IF NOT &1 IS INITIAL.
        READ TABLE lt_visit FROM &1 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND &1 TO lt_visit.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    DATA: ls_commit TYPE zcl_abapgit_git_pack=>ty_commit,
          lt_visit  TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_sha1,
          lv_commit LIKE LINE OF lt_visit.

    FIELD-SYMBOLS: <ls_ancestor> LIKE LINE OF rt_ancestors,
                   <ls_object>   LIKE LINE OF gt_objects.


    APPEND iv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE gt_objects ASSIGNING <ls_object>
        WITH KEY type = zif_abapgit_definitions=>gc_type-commit sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      ls_commit = zcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      _visit ls_commit-parent.
      _visit ls_commit-parent2.

      APPEND INITIAL LINE TO rt_ancestors ASSIGNING <ls_ancestor>.
      <ls_ancestor>-commit = lv_commit.
      <ls_ancestor>-tree = ls_commit-tree.
      <ls_ancestor>-body = ls_commit-body.
      FIND REGEX zif_abapgit_definitions=>gc_author_regex IN ls_commit-author
        SUBMATCHES <ls_ancestor>-time ##NO_TEXT.
      ASSERT sy-subrc = 0.
    ENDLOOP.

    SORT rt_ancestors BY time DESCENDING.

  ENDMETHOD.


  METHOD find_first_common.

    FIELD-SYMBOLS: <ls_list1> LIKE LINE OF it_list1,
                   <ls_list2> LIKE LINE OF it_list2.


    LOOP AT it_list1 ASSIGNING <ls_list1>.
      LOOP AT it_list2 ASSIGNING <ls_list2>.
        IF <ls_list1>-tree = <ls_list2>-tree.
          rs_common = <ls_list1>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    zcx_abapgit_exception=>raise( 'error finding common ancestor' ).

  ENDMETHOD.


  METHOD run.

    DATA: lt_asource TYPE ty_ancestor_tt,
          lt_atarget TYPE ty_ancestor_tt.


    IF iv_source = iv_target.
      zcx_abapgit_exception=>raise( 'source = target' ).
    ENDIF.

    CLEAR gs_merge.

    gs_merge-repo = io_repo.

    fetch_git( iv_source = iv_source
               iv_target = iv_target ).

    lt_asource = find_ancestors( gs_merge-source-sha1 ).
    lt_atarget = find_ancestors( gs_merge-target-sha1 ).

    gs_merge-common = find_first_common( it_list1 = lt_asource
                                         it_list2 = lt_atarget ).

    gs_merge-stree = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = gt_objects
      iv_branch  = gs_merge-source-sha1 ).
    gs_merge-ttree = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = gt_objects
      iv_branch  = gs_merge-target-sha1 ).
    gs_merge-ctree = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = gt_objects
      iv_branch  = gs_merge-common-commit ).

    calculate_result( ).

    rs_merge = gs_merge.

  ENDMETHOD.
ENDCLASS.
