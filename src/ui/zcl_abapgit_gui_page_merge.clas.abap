CLASS zcl_abapgit_gui_page_merge DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    METHODS constructor
      IMPORTING
        io_repo   TYPE REF TO zcl_abapgit_repo_online
        iv_source TYPE string
        iv_target TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS zif_abapgit_gui_event_handler~on_event
         REDEFINITION.
  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA mo_merge TYPE REF TO zcl_abapgit_merge .
    CONSTANTS:
      BEGIN OF c_actions,
        merge         TYPE string VALUE 'merge' ##NO_TEXT,
        res_conflicts TYPE string VALUE 'res_conflicts' ##NO_TEXT,
      END OF c_actions .

    METHODS build_menu
      IMPORTING
        VALUE(iv_with_conflict) TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ro_menu)          TYPE REF TO zcl_abapgit_html_toolbar .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MERGE IMPLEMENTATION.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Merge' iv_act = c_actions-merge iv_cur = abap_false ) ##NO_TEXT.

    IF iv_with_conflict = abap_true.
      ro_menu->add( iv_txt = 'Resolve Conflicts' iv_act = c_actions-res_conflicts ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.

    io_repo->set_branch_name( |refs/heads/{ iv_target }| ).

    CREATE OBJECT mo_merge
      EXPORTING
        io_repo          = io_repo
        iv_source_branch = iv_source.
    mo_merge->run( ).

    ms_control-page_title = 'MERGE'.
    ms_control-page_menu  = build_menu( mo_merge->has_conflicts( ) ).

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

    DATA: ls_merge  TYPE zif_abapgit_definitions=>ty_merge,
          lt_files  LIKE ls_merge-stree,
          ls_result LIKE LINE OF ls_merge-result.

    FIELD-SYMBOLS: <ls_show> LIKE LINE OF lt_files,
                   <ls_file> LIKE LINE OF lt_files.

    ls_merge = mo_merge->get_result( ).

    "If now exists no conflicts anymore, conflicts button should disappear
    ms_control-page_menu = build_menu( mo_merge->has_conflicts( ) ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_show_branch  = abap_false ) ).

    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Source</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( ls_merge-source-name ).
    ro_html->add( '</td></tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Target</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( ls_merge-target-name ).
    ro_html->add( '</td></tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Ancestor</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( ls_merge-common-commit ).
    ro_html->add( '</td></tr>' ).
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).

    APPEND LINES OF ls_merge-stree TO lt_files.
    APPEND LINES OF ls_merge-ttree TO lt_files.
    APPEND LINES OF ls_merge-ctree TO lt_files.
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
      READ TABLE ls_merge-result INTO ls_result
        WITH KEY path = <ls_file>-path name = <ls_file>-name.

      ro_html->add( '<tr>' ).
      _show_file ls_merge-stree.
      _show_file ls_merge-ttree.
      _show_file ls_merge-ctree.
      _show_file ls_merge-result.
      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</table>' ).
    ro_html->add( '<br>' ).
    ro_html->add( '<b>' ).
    ro_html->add( ls_merge-conflict ).
    ro_html->add( '</b>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_actions-merge.
        IF mo_merge->has_conflicts( ) = abap_true.
          zcx_abapgit_exception=>raise( 'conflicts exists' ).
        ENDIF.

        IF mo_merge->get_result( )-stage->count( ) = 0.
          zcx_abapgit_exception=>raise( 'nothing to merge' ).
        ENDIF.

        IF mo_repo->get_local_settings( )-code_inspector_check_variant IS NOT INITIAL.

          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_code_insp
            EXPORTING
              io_repo  = mo_repo
              io_stage = mo_merge->get_result( )-stage.

        ELSE.

          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
            EXPORTING
              io_repo  = mo_repo
              io_stage = mo_merge->get_result( )-stage.

        ENDIF.

        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-res_conflicts.

        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_merge_res
          EXPORTING
            io_repo       = mo_repo
            io_merge_page = me
            io_merge      = mo_merge.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.
        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_prev_page = iv_prev_page
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
