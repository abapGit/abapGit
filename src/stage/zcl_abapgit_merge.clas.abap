CLASS zcl_abapgit_merge DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_merge.

    METHODS constructor
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo_online
        !iv_source_branch TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_ancestor_tt TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_ancestor WITH DEFAULT KEY .
    TYPES:
      ty_visit_tt TYPE STANDARD TABLE OF zif_abapgit_git_definitions=>ty_sha1 WITH DEFAULT KEY .

    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA ms_merge TYPE zif_abapgit_merge=>ty_merge .
    DATA mt_conflicts TYPE zif_abapgit_merge=>ty_merge_conflict_tt .
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt .
    DATA mv_source_branch TYPE string .

    METHODS visit
      IMPORTING
        !iv_parent TYPE zif_abapgit_git_definitions=>ty_sha1
      CHANGING
        !ct_visit  TYPE ty_visit_tt .
    METHODS all_files
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_expanded_tt .
    METHODS calculate_result
      RAISING
        zcx_abapgit_exception .
    METHODS fetch_git
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    METHODS find_ancestors
      IMPORTING
        !iv_commit          TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_ancestors) TYPE ty_ancestor_tt
      RAISING
        zcx_abapgit_exception .
    METHODS find_first_common
      IMPORTING
        !it_list1        TYPE ty_ancestor_tt
        !it_list2        TYPE ty_ancestor_tt
      RETURNING
        VALUE(rs_common) TYPE zif_abapgit_definitions=>ty_ancestor
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_MERGE IMPLEMENTATION.


  METHOD all_files.

    APPEND LINES OF ms_merge-stree TO rt_files.
    APPEND LINES OF ms_merge-ttree TO rt_files.
    APPEND LINES OF ms_merge-ctree TO rt_files.
    SORT rt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_files COMPARING path name.

  ENDMETHOD.


  METHOD calculate_result.

    DATA: lt_files        TYPE zif_abapgit_definitions=>ty_expanded_tt,
          lv_found_source TYPE abap_bool,
          lv_found_target TYPE abap_bool,
          lv_found_common TYPE abap_bool.

    FIELD-SYMBOLS: <ls_source>   LIKE LINE OF lt_files,
                   <ls_target>   LIKE LINE OF lt_files,
                   <ls_common>   LIKE LINE OF lt_files,
                   <ls_file>     LIKE LINE OF lt_files,
                   <ls_result>   LIKE LINE OF ms_merge-result,
                   <ls_object>   LIKE LINE OF mt_objects,
                   <ls_conflict> LIKE LINE OF mt_conflicts.

    lt_files = all_files( ).

    CREATE OBJECT ms_merge-stage
      EXPORTING
        iv_merge_source = ms_merge-source-sha1.

    LOOP AT lt_files ASSIGNING <ls_file>.

      UNASSIGN <ls_source>.
      UNASSIGN <ls_target>.
      UNASSIGN <ls_common>.

      READ TABLE ms_merge-stree ASSIGNING <ls_source>
        WITH KEY path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE ms_merge-ttree ASSIGNING <ls_target>
        WITH KEY path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE ms_merge-ctree ASSIGNING <ls_common>
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
        ms_merge-stage->rm( iv_path     = <ls_file>-path
                            iv_filename = <ls_file>-name ).
        CONTINUE.
      ELSEIF lv_found_target = abap_false
          AND lv_found_common = abap_true
          AND <ls_source>-sha1 = <ls_common>-sha1.
* deleted in target, skip
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO ms_merge-result ASSIGNING <ls_result>.
      <ls_result>-path = <ls_file>-path.
      <ls_result>-name = <ls_file>-name.

      IF lv_found_target = abap_false.
* added in source
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        ASSERT sy-subrc = 0.

        ms_merge-stage->add( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-name
                             iv_data     = <ls_object>-data ).
        <ls_result>-sha1 = <ls_source>-sha1.
        CONTINUE.
      ELSEIF lv_found_source = abap_false.
* added in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 = <ls_source>-sha1.
* added in source and target
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 <> <ls_source>-sha1.

        INSERT INITIAL LINE INTO TABLE mt_conflicts ASSIGNING <ls_conflict>.
        <ls_conflict>-path = <ls_file>-path.
        <ls_conflict>-filename = <ls_file>-name.
        <ls_conflict>-source_sha1 = <ls_source>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        <ls_conflict>-source_data = <ls_object>-data.

        <ls_conflict>-target_sha1 = <ls_target>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
            sha1 = <ls_target>-sha1.
        <ls_conflict>-target_data = <ls_object>-data.

* added in source and target, but different, merge conflict must be resolved
        ms_merge-conflict = |{ <ls_file>-name } merge conflict|.
        CONTINUE.
      ENDIF.

      IF lv_found_source = abap_false
          OR lv_found_target = abap_false
          OR lv_found_common = abap_false.
        ms_merge-conflict = |{ <ls_file>-name } merge conflict, not found anywhere|.
        CONTINUE.
      ENDIF.

      IF <ls_target>-sha1 = <ls_source>-sha1.
* target and source match
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_target>-sha1 = <ls_common>-sha1.
* changed in source
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        ASSERT sy-subrc = 0.

        ms_merge-stage->add( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-name
                             iv_data     = <ls_object>-data ).
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_source>-sha1 = <ls_common>-sha1.
* changed in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSE.
* changed in source and target, conflict
* conflict must be resolved before merge
        INSERT INITIAL LINE INTO TABLE mt_conflicts ASSIGNING <ls_conflict>.
        <ls_conflict>-path = <ls_file>-path.
        <ls_conflict>-filename = <ls_file>-name.
        <ls_conflict>-source_sha1 = <ls_source>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        <ls_conflict>-source_data = <ls_object>-data.

        <ls_conflict>-target_sha1 = <ls_target>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
            sha1 = <ls_target>-sha1.
        <ls_conflict>-target_data = <ls_object>-data.

        ms_merge-conflict = |{ <ls_file>-name } merge conflict, changed in source and target branch|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    IF iv_source_branch = io_repo->get_selected_branch( ).
      zcx_abapgit_exception=>raise( 'source = target' ).
    ENDIF.

    mo_repo = io_repo.
    mv_source_branch = iv_source_branch.

  ENDMETHOD.


  METHOD fetch_git.

    DATA: lo_branch_list TYPE REF TO zcl_abapgit_git_branch_list,
          lt_upload      TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.

    lo_branch_list = zcl_abapgit_git_transport=>branches( ms_merge-repo->get_url( ) ).

    ms_merge-source = lo_branch_list->find_by_name(
      zcl_abapgit_git_branch_list=>complete_heads_branch_name( mv_source_branch ) ).

    ms_merge-target = lo_branch_list->find_by_name(
      zcl_abapgit_git_branch_list=>complete_heads_branch_name( mo_repo->get_selected_branch( ) ) ).

    APPEND ms_merge-source TO lt_upload.
    APPEND ms_merge-target TO lt_upload.

    zcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = ms_merge-repo->get_url( )
        iv_branch_name  = ms_merge-repo->get_selected_branch( )
        iv_deepen_level = 0
        it_branches     = lt_upload
      IMPORTING
        et_objects      = rt_objects ).

  ENDMETHOD.


  METHOD find_ancestors.

    DATA: ls_commit TYPE zcl_abapgit_git_pack=>ty_commit,
          lt_visit  TYPE ty_visit_tt,
          lv_commit LIKE LINE OF lt_visit.

    FIELD-SYMBOLS: <ls_ancestor> LIKE LINE OF rt_ancestors,
                   <ls_object>   LIKE LINE OF mt_objects.


    APPEND iv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE mt_objects ASSIGNING <ls_object>
        WITH KEY type COMPONENTS
          type = zif_abapgit_definitions=>c_type-commit
          sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      ls_commit = zcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      visit( EXPORTING iv_parent = ls_commit-parent
             CHANGING ct_visit   = lt_visit ).
      visit( EXPORTING iv_parent = ls_commit-parent2
             CHANGING ct_visit   = lt_visit ).

      APPEND INITIAL LINE TO rt_ancestors ASSIGNING <ls_ancestor>.
      <ls_ancestor>-commit = lv_commit.
      <ls_ancestor>-tree = ls_commit-tree.
      <ls_ancestor>-body = ls_commit-body.
      <ls_ancestor>-time = ls_commit-author.

      "Strip Author entry of all but the time component
      REPLACE ALL OCCURRENCES OF REGEX '[a-zA-Z<>@.-]*' IN <ls_ancestor>-time WITH ''.
      CONDENSE <ls_ancestor>-time.
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


  METHOD visit.

    IF NOT iv_parent IS INITIAL.
      READ TABLE ct_visit FROM iv_parent TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND iv_parent TO ct_visit.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_merge~get_conflicts.

    rt_conflicts = mt_conflicts.

  ENDMETHOD.


  METHOD zif_abapgit_merge~get_result.

    rs_merge = ms_merge.

  ENDMETHOD.


  METHOD zif_abapgit_merge~get_source_branch.

    rv_source_branch = mv_source_branch.

  ENDMETHOD.


  METHOD zif_abapgit_merge~has_conflicts.

    rv_conflicts_exists = boolc( lines( mt_conflicts ) > 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_merge~resolve_conflict.

    FIELD-SYMBOLS: <ls_conflict> TYPE zif_abapgit_merge=>ty_merge_conflict,
                   <ls_result>   LIKE LINE OF ms_merge-result.

    IF is_conflict-result_sha1 IS NOT INITIAL
        AND is_conflict-result_data IS NOT INITIAL.
      READ TABLE mt_conflicts ASSIGNING <ls_conflict> WITH KEY path = is_conflict-path
                                                               filename = is_conflict-filename.
      IF sy-subrc = 0.
        READ TABLE ms_merge-result ASSIGNING <ls_result> WITH KEY path = is_conflict-path
                                                                  name = is_conflict-filename.
        IF sy-subrc = 0.
          <ls_result>-sha1 = is_conflict-result_sha1.

          ms_merge-stage->add( iv_path     = <ls_conflict>-path
                               iv_filename = <ls_conflict>-filename
                               iv_data     = is_conflict-result_data ).

          DELETE mt_conflicts WHERE path     = is_conflict-path
                                AND filename = is_conflict-filename.
        ENDIF.

        READ TABLE ms_merge-result ASSIGNING <ls_result> WITH KEY sha1 = space.
        IF sy-subrc = 0.
          ms_merge-conflict = |{ <ls_result>-name } merge conflict, changed in source and target branch|.
        ELSE.
          CLEAR ms_merge-conflict.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_merge~run.

    DATA: lt_asource TYPE ty_ancestor_tt,
          lt_atarget TYPE ty_ancestor_tt.

    CLEAR: ms_merge, mt_objects, mt_conflicts.

    ms_merge-repo = mo_repo.
    mt_objects = fetch_git( ).

    lt_asource = find_ancestors( ms_merge-source-sha1 ).
    lt_atarget = find_ancestors( ms_merge-target-sha1 ).

    ms_merge-common = find_first_common( it_list1 = lt_asource
                                         it_list2 = lt_atarget ).

    ms_merge-stree = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-source-sha1 ).
    ms_merge-ttree = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-target-sha1 ).
    ms_merge-ctree = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-common-commit ).

    calculate_result( ).

  ENDMETHOD.
ENDCLASS.
