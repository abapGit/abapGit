CLASS zcl_abapgit_gui_page_merge DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
    DATA mi_merge TYPE REF TO zif_abapgit_merge .
    CONSTANTS:
      BEGIN OF c_actions,
        merge         TYPE string VALUE 'merge' ##NO_TEXT,
        res_conflicts TYPE string VALUE 'res_conflicts' ##NO_TEXT,
      END OF c_actions .

    METHODS show_file
      IMPORTING
        !it_expanded TYPE zif_abapgit_definitions=>ty_expanded_tt
        !ii_html     TYPE REF TO zif_abapgit_html
        !is_file     TYPE zif_abapgit_definitions=>ty_expanded
        !is_result   TYPE zif_abapgit_definitions=>ty_expanded .
    METHODS build_menu
      IMPORTING
        VALUE(iv_with_conflict) TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ro_menu)          TYPE REF TO zcl_abapgit_html_toolbar .
ENDCLASS.



CLASS zcl_abapgit_gui_page_merge IMPLEMENTATION.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Merge'
                  iv_act = c_actions-merge
                  iv_cur = abap_false ).

    IF iv_with_conflict = abap_true.
      ro_menu->add( iv_txt = 'Resolve Conflicts'
                    iv_act = c_actions-res_conflicts ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.

    io_repo->select_branch( |{ zif_abapgit_definitions=>c_git_branch-heads_prefix }{ iv_target }| ).

    CREATE OBJECT mi_merge TYPE zcl_abapgit_merge
      EXPORTING
        io_repo          = io_repo
        iv_source_branch = iv_source.
    mi_merge->run( ).

    ms_control-page_title = 'Merge'.
    ms_control-page_menu  = build_menu( mi_merge->has_conflicts( ) ).

  ENDMETHOD.


  METHOD render_content.

    DATA: ls_merge  TYPE zif_abapgit_merge=>ty_merge,
          lt_files  LIKE ls_merge-stree,
          ls_result LIKE LINE OF ls_merge-result.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.

    ls_merge = mi_merge->get_result( ).

    "If now exists no conflicts anymore, conflicts button should disappear
    ms_control-page_menu = build_menu( mi_merge->has_conflicts( ) ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="toc">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_show_branch  = abap_false ) ).

    ri_html->add( '<table>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Source</td>' ).
    ri_html->add( '<td>' ).
    ri_html->add( ls_merge-source-name ).
    ri_html->add( '</td></tr>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Target</td>' ).
    ri_html->add( '<td>' ).
    ri_html->add( ls_merge-target-name ).
    ri_html->add( '</td></tr>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Ancestor</td>' ).
    ri_html->add( '<td>' ).
    ri_html->add( ls_merge-common-commit ).
    ri_html->add( '</td></tr>' ).
    ri_html->add( '</table>' ).

    ri_html->add( '<br>' ).

    APPEND LINES OF ls_merge-stree TO lt_files.
    APPEND LINES OF ls_merge-ttree TO lt_files.
    APPEND LINES OF ls_merge-ctree TO lt_files.
    SORT lt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path name.

    ri_html->add( '<table>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td><u>Source</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '<td><u>Target</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '<td><u>Ancestor</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '<td><u>Result</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '</tr>' ).
    LOOP AT lt_files ASSIGNING <ls_file>.
      CLEAR ls_result.
      READ TABLE ls_merge-result INTO ls_result
        WITH KEY path = <ls_file>-path name = <ls_file>-name.

      ri_html->add( '<tr>' ).
      show_file( it_expanded = ls_merge-stree
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      show_file( it_expanded = ls_merge-ttree
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      show_file( it_expanded = ls_merge-ctree
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      show_file( it_expanded = ls_merge-result
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</table>' ).
    ri_html->add( '<br>' ).
    ri_html->add( '<b>' ).
    ri_html->add( ls_merge-conflict ).
    ri_html->add( '</b>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD show_file.

    FIELD-SYMBOLS: <ls_show> LIKE LINE OF it_expanded.


    READ TABLE it_expanded ASSIGNING <ls_show>
        WITH KEY
        path = is_file-path
        name = is_file-name.
    IF sy-subrc = 0.
      IF <ls_show>-sha1 = is_result-sha1.
        ii_html->add( |<td>{ <ls_show>-path }{ <ls_show>-name }</td><td><b>{ <ls_show>-sha1(7) }</b></td>| ).
      ELSE.
        ii_html->add( |<td>{ <ls_show>-path }{ <ls_show>-name }</td><td>{ <ls_show>-sha1(7) }</td>| ).
      ENDIF.
    ELSE.
      ii_html->add( '<td></td><td></td>' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-merge.
        IF mi_merge->has_conflicts( ) = abap_true.
          zcx_abapgit_exception=>raise( 'conflicts exists' ).
        ENDIF.

        IF mi_merge->get_result( )-stage->count( ) = 0.
          zcx_abapgit_exception=>raise( 'nothing to merge' ).
        ENDIF.

        IF mo_repo->get_local_settings( )-code_inspector_check_variant IS NOT INITIAL.

          CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_code_insp
            EXPORTING
              io_repo  = mo_repo
              io_stage = mi_merge->get_result( )-stage.

        ELSE.

          rs_handled-page = zcl_abapgit_gui_page_commit=>create(
            io_repo  = mo_repo
            io_stage = mi_merge->get_result( )-stage ).

        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-res_conflicts.

        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_merge_res
          EXPORTING
            io_repo       = mo_repo
            io_merge_page = me
            io_merge      = mi_merge.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
