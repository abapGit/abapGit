CLASS zcl_abapgit_gui_page_syntax DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page_codi_base
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        io_repo        TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS:
      constructor
        IMPORTING
          io_repo TYPE REF TO zcl_abapgit_repo
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.

    CONSTANTS c_variant TYPE c LENGTH 30 VALUE 'SYNTAX_CHECK'.

  PRIVATE SECTION.

    METHODS:
      run_syntax_check
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_syntax IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
    run_syntax_check( ).
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_syntax.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Syntax Check'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD run_syntax_check.

    DATA: li_syntax_check TYPE REF TO zif_abapgit_code_inspector.

    li_syntax_check = zcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) ).

    TRY.
        mt_result = li_syntax_check->run( c_variant ).
      CATCH zcx_abapgit_exception.
        " Variant SYNTAX_CHECK does not exist in 702
        mt_result = li_syntax_check->run( 'VERI_' && c_variant ).
    ENDTRY.

    mv_summary = li_syntax_check->get_summary( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-rerun.

        run_syntax_check( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        rs_handled = on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Syntax Check'.

    ls_hotkey_action-description = |Re-Run|.
    ls_hotkey_action-action = c_actions-rerun.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = build_base_menu( ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( io_repo        = mo_repo
                                                              iv_show_commit = abap_false ) ).
    ri_html->add( `</div>` ).

    ri_html->add( '<div class="toc">' ).

    ri_html->add( render_variant(
      iv_variant = c_variant
      iv_summary = mv_summary ) ).

    IF lines( mt_result ) = 0.
      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No syntax errors' ).
      ri_html->add( '</div>' ).
    ELSE.
      render_result( ii_html   = ri_html
                     it_result = mt_result ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
