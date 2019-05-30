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

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_gui_page_syntax IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SYNTAX CHECK'.
    mo_repo = io_repo.
  ENDMETHOD.


  METHOD render_content.

    DATA: li_syntax_check TYPE REF TO zif_abapgit_code_inspector.

    li_syntax_check = zcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) ).

    mt_result = li_syntax_check->run( 'SYNTAX_CHECK' ).

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="toc">' ).

    IF lines( mt_result ) = 0.
      ro_html->add( '<div class="dummydiv success">' ).
      ro_html->add( zcl_abapgit_html=>icon( 'check' ) ).
      ro_html->add( 'No syntax errors' ).
    ELSE.
      render_result( io_html   = ro_html
                     it_result = mt_result ).
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
