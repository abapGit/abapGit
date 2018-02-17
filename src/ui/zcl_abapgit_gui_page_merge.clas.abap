CLASS zcl_abapgit_gui_page_merge DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo   TYPE REF TO zcl_abapgit_repo_online
                  iv_source TYPE string
                  iv_target TYPE string
        RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO zcl_abapgit_repo_online,
          ms_merge TYPE zif_abapgit_definitions=>ty_merge.

    CONSTANTS: BEGIN OF c_actions,
                 merge TYPE string VALUE 'merge' ##NO_TEXT,
               END OF c_actions.

    METHODS:
      build_menu
        RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MERGE IMPLEMENTATION.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Merge' iv_act = c_actions-merge ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    ms_control-page_title = 'MERGE'.
    ms_control-page_menu  = build_menu( ).

    mo_repo = io_repo.

    ms_merge = zcl_abapgit_merge=>run(
      io_repo   = io_repo
      iv_source = iv_source
      iv_target = iv_target ).

  ENDMETHOD.


  METHOD render_content.

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

    ro_html->add( '<div id="toc">' ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_show_branch  = abap_false ) ).

    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Source:</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( ms_merge-source-name ).
    ro_html->add( '</td></tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Target:</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( ms_merge-target-name ).
    ro_html->add( '</td></tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Ancestor:</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( ms_merge-common-commit ).
    ro_html->add( '</td></tr>' ).
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).

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
    ro_html->add( '<br>' ).
    ro_html->add( '<b>' ).
    ro_html->add( ms_merge-conflict ).
    ro_html->add( '</b>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content


  METHOD zif_abapgit_gui_page~on_event.

    CASE iv_action.
      WHEN c_actions-merge.
        IF ms_merge-stage->count( ) = 0.
          zcx_abapgit_exception=>raise( 'nothing to merge' ).
        ENDIF.

        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
          EXPORTING
            io_repo  = mo_repo
            io_stage = ms_merge-stage.
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
