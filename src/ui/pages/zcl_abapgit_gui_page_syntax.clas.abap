CLASS zcl_abapgit_gui_page_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC
    INHERITING FROM zcl_abapgit_gui_page_codi_base.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          io_repo TYPE REF TO zcl_abapgit_repo
        RAISING
          zcx_abapgit_exception,

      zif_abapgit_gui_event_handler~on_event
        REDEFINITION,

      zif_abapgit_gui_renderable~render
        REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS: c_variant TYPE c LENGTH 30 VALUE 'SYNTAX_CHECK'.

    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      build_menu
        RETURNING
          VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar
        RAISING
          zcx_abapgit_exception,

      run_syntax_check
        RAISING
          zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_gui_page_syntax IMPLEMENTATION.


  METHOD build_menu.

    ro_menu = build_base_menu( ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Syntax Check'.
    mo_repo = io_repo.
    run_syntax_check( ).
  ENDMETHOD.


  METHOD render_content.

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
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    ms_control-page_menu = build_menu( ).
    ri_html = super->zif_abapgit_gui_renderable~render( ).

  ENDMETHOD.
ENDCLASS.
