*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_MERGE
*&---------------------------------------------------------------------*

CLASS lcl_merge DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_ancestor,
             commit TYPE ty_sha1,
             tree   TYPE ty_sha1,
             time   TYPE string,
             body   TYPE string,
           END OF ty_ancestor.

    TYPES: BEGIN OF ty_merge,
             repo   TYPE REF TO lcl_repo_online,
             source TYPE lcl_git_transport=>ty_branch_list,
             target TYPE lcl_git_transport=>ty_branch_list,
             common TYPE ty_ancestor,
             stree  TYPE lcl_git_porcelain=>ty_expanded_tt,
             ttree  TYPE lcl_git_porcelain=>ty_expanded_tt,
             ctree  TYPE lcl_git_porcelain=>ty_expanded_tt,
             result TYPE lcl_git_porcelain=>ty_expanded_tt,
           END OF ty_merge.

    CLASS-METHODS:
      run
        IMPORTING io_repo         TYPE REF TO lcl_repo_online
                  iv_source       TYPE string
                  iv_target       TYPE string
        RETURNING VALUE(rs_merge) TYPE ty_merge
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-DATA: gs_merge   TYPE ty_merge,
                gt_objects TYPE ty_objects_tt.

    TYPES: ty_ancestor_tt TYPE STANDARD TABLE OF ty_ancestor WITH DEFAULT KEY.

    CLASS-METHODS:
      all_files
        RETURNING VALUE(rt_files) TYPE lcl_git_porcelain=>ty_expanded_tt,
      calculate_result
        RAISING lcx_exception,
      find_ancestors
        IMPORTING iv_commit           TYPE ty_sha1
        RETURNING VALUE(rt_ancestors) TYPE ty_ancestor_tt
        RAISING   lcx_exception,
      find_first_common
        IMPORTING it_list1         TYPE ty_ancestor_tt
                  it_list2         TYPE ty_ancestor_tt
        RETURNING VALUE(rs_common) TYPE ty_ancestor
        RAISING   lcx_exception,
      fetch_git
        IMPORTING iv_source TYPE string
                  iv_target TYPE string
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_merge IMPLEMENTATION.

  METHOD run.

    DATA: lt_asource TYPE ty_ancestor_tt,
          lt_atarget TYPE ty_ancestor_tt.


    ASSERT NOT iv_source = iv_target.

    CLEAR gs_merge.

    gs_merge-repo = io_repo.

    fetch_git( iv_source = iv_source
               iv_target = iv_target ).

    lt_asource = find_ancestors( gs_merge-source-sha1 ).
    lt_atarget = find_ancestors( gs_merge-target-sha1 ).

    gs_merge-common = find_first_common( it_list1 = lt_asource
                                         it_list2 = lt_atarget ).

    gs_merge-stree = lcl_git_porcelain=>full_tree(
      it_objects = gt_objects
      iv_branch  = gs_merge-source-sha1 ).
    gs_merge-ttree = lcl_git_porcelain=>full_tree(
      it_objects = gt_objects
      iv_branch  = gs_merge-target-sha1 ).
    gs_merge-ctree = lcl_git_porcelain=>full_tree(
      it_objects = gt_objects
      iv_branch  = gs_merge-common-commit ).

    BREAK-POINT.
    calculate_result( ).

    rs_merge = gs_merge.

  ENDMETHOD.

  METHOD all_files.

    APPEND LINES OF gs_merge-stree TO rt_files.
    APPEND LINES OF gs_merge-ttree TO rt_files.
    APPEND LINES OF gs_merge-ctree TO rt_files.
    SORT rt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_files COMPARING path name.

  ENDMETHOD.

  METHOD calculate_result.

    DATA: lt_files        TYPE lcl_git_porcelain=>ty_expanded_tt,
          lv_found_source TYPE abap_bool,
          lv_found_target TYPE abap_bool,
          lv_found_common TYPE abap_bool.

    FIELD-SYMBOLS: <ls_source> LIKE LINE OF lt_files,
                   <ls_target> LIKE LINE OF lt_files,
                   <ls_common> LIKE LINE OF lt_files,
                   <ls_file>   LIKE LINE OF lt_files,
                   <ls_result> LIKE LINE OF gs_merge-result.


    lt_files = all_files( ).

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
        <ls_result>-sha1 = <ls_source>-sha1.
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
        _raise 'merge conflict'.
      ENDIF.

      IF <ls_target>-sha1 = <ls_source>-sha1.
* target and source match
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_target>-sha1 = <ls_common>-sha1.
* changed in source
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_source>-sha1 = <ls_common>-sha1.
* changed in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSE.
* changed in source and target, conflict
        _raise 'merge conflict'.
      ENDIF.

    ENDLOOP.

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

    _raise 'error finding common ancestor'.

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

    DATA: ls_commit TYPE lcl_git_pack=>ty_commit,
          lt_visit  TYPE STANDARD TABLE OF ty_sha1,
          lv_commit LIKE LINE OF lt_visit.

    FIELD-SYMBOLS: <ls_ancestor> LIKE LINE OF rt_ancestors,
                   <ls_object>   LIKE LINE OF gt_objects.


    APPEND iv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE gt_objects ASSIGNING <ls_object>
        WITH KEY type = gc_type-commit sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      ls_commit = lcl_git_pack=>decode_commit( <ls_object>-data ).

      _visit ls_commit-parent.
      _visit ls_commit-parent2.

      APPEND INITIAL LINE TO rt_ancestors ASSIGNING <ls_ancestor>.
      <ls_ancestor>-commit = lv_commit.
      <ls_ancestor>-tree = ls_commit-tree.
      <ls_ancestor>-body = ls_commit-body.
      FIND REGEX '^[\w\s]+ <(.*)> (\d{10}) .\d{4}$' IN ls_commit-author
        SUBMATCHES <ls_ancestor>-time ##NO_TEXT.
      ASSERT sy-subrc = 0.
    ENDLOOP.

    SORT rt_ancestors BY time ASCENDING.

  ENDMETHOD.

  METHOD fetch_git.

    DEFINE _find.
      lv_name = 'refs/heads/' && &1.
      READ TABLE lt_branches INTO &2 WITH KEY name = lv_name.
      ASSERT sy-subrc = 0.
      APPEND &2 TO lt_upload.
    END-OF-DEFINITION.

    DATA: lv_name     TYPE string,
          lt_branches TYPE lcl_git_transport=>ty_branch_list_tt,
          lt_upload   TYPE lcl_git_transport=>ty_branch_list_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_upload.


    lt_branches = lcl_git_transport=>branches( gs_merge-repo->get_url( ) ).

    _find iv_source gs_merge-source.
    _find iv_target gs_merge-target.

    lcl_git_transport=>upload_pack( EXPORTING io_repo = gs_merge-repo
                                              iv_deepen = abap_false
                                              it_branches = lt_upload
                                    IMPORTING et_objects = gt_objects ).

    DELETE gt_objects WHERE type = gc_type-blob.

  ENDMETHOD.

ENDCLASS.

*********************************

CLASS lcl_gui_page_merge DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo   TYPE REF TO lcl_repo_online
                  iv_source TYPE string
                  iv_target TYPE string
        RAISING   lcx_exception,
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          ms_merge TYPE lcl_merge=>ty_merge.

ENDCLASS.                       "lcl_gui_page_merge DEFINITION

CLASS lcl_gui_page_merge IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.

    ms_merge = lcl_merge=>run(
      io_repo   = io_repo
      iv_source = iv_source
      iv_target = iv_target ).

  ENDMETHOD.

  METHOD lif_gui_page~on_event.
    BREAK-POINT.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DEFINE _show_file.
      READ TABLE &1 ASSIGNING <ls_show>
          WITH KEY path = <ls_file>-path name = <ls_file>-name.
      IF sy-subrc = 0.
        IF <ls_show>-sha1 = ls_result-sha1.
          ro_html->add( |<td>{
            <ls_show>-path }{ <ls_show>-name }</td><td><b>{
            <ls_show>-sha1(7) }</b></td>| ).
        ELSE.
          ro_html->add( |<td>{
            <ls_show>-path }{ <ls_show>-name }</td><td>{
            <ls_show>-sha1(7) }</td>| ).
        ENDIF.
      ELSE.
        ro_html->add( '<td></td><td></td>' ).
      ENDIF.
    END-OF-DEFINITION.

    DATA: lt_files  LIKE ms_merge-stree,
          ls_result LIKE LINE OF ms_merge-result.

    FIELD-SYMBOLS: <ls_show> LIKE LINE OF lt_files,
                   <ls_file> LIKE LINE OF lt_files.


    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'MERGE' ) ).
    ro_html->add( '<div id="toc">' ).
    ro_html->add( render_repo_top( mo_repo ) ).

    _add '<table>'.
    _add '<tr>'.
    _add '<td>Source:</td>'.
    _add '<td>'.
    _add ms_merge-source-name.
    _add '</td></tr>'.
    _add '<tr>'.
    _add '<td>Target:</td>'.
    _add '<td>'.
    _add ms_merge-target-name.
    _add '</td></tr>'.
    _add '<tr>'.
    _add '<td>Ancestor:</td>'.
    _add '<td>'.
    _add ms_merge-common-commit.
    _add '</td></tr>'.
    _add '</table>'.

    _add '<br>'.

    APPEND LINES OF ms_merge-stree TO lt_files.
    APPEND LINES OF ms_merge-ttree TO lt_files.
    APPEND LINES OF ms_merge-ctree TO lt_files.
    SORT lt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path name.

    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td><u>Source</u></td>' ).
    ro_html->add( '<td></td>' ).
    ro_html->add( '<td><u>Target</u></td>' ).
    ro_html->add( '<td></td>' ).
    ro_html->add( '<td><u>Ancestor</u></td>' ).
    ro_html->add( '<td></td>' ).
    ro_html->add( '<td><u>Result</u></td>' ).
    ro_html->add( '<td></td>' ).
    ro_html->add( '</tr>' ).
    LOOP AT lt_files ASSIGNING <ls_file>.
      CLEAR ls_result.
      READ TABLE ms_merge-result INTO ls_result
        WITH KEY path = <ls_file>-path name = <ls_file>-name.

      ro_html->add( '<tr>' ).
      _show_file ms_merge-stree.
      _show_file ms_merge-ttree.
      _show_file ms_merge-ctree.
      _show_file ms_merge-result.
      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</table>' ).

    ro_html->add( '<br>Todo' ).

    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.