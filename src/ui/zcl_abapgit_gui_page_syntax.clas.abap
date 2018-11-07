CLASS zcl_abapgit_gui_page_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC
    INHERITING FROM zcl_abapgit_gui_page_codi_base.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO zcl_abapgit_repo.

  PROTECTED SECTION.

    METHODS:
      render_content REDEFINITION.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SYNTAX IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SYNTAX CHECK'.
    mo_repo = io_repo.
  ENDMETHOD.


  METHOD render_content.

    DATA: li_syntax_check TYPE REF TO zif_abapgit_code_inspector.
    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_result.

    li_syntax_check = zcl_abapgit_factory=>get_syntax_check( mo_repo->get_package( ) ).

    mt_result = li_syntax_check->run( ).

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="toc">' ).

    IF lines( mt_result ) = 0.
      ro_html->add( 'No errors' ).
    ENDIF.

    LOOP AT mt_result ASSIGNING <ls_result>.
      render_result( io_html   = ro_html
                     iv_result = <ls_result> ).
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
