CLASS zcl_abapgit_gui_page_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC
    INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO zcl_abapgit_repo.

  PROTECTED SECTION.
    DATA: mo_repo TYPE REF TO zcl_abapgit_repo.

    METHODS:
      render_content REDEFINITION.

ENDCLASS.



CLASS zcl_abapgit_gui_page_syntax IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SYNTAX CHECK'.
    mo_repo = io_repo.
  ENDMETHOD.  " constructor.


  METHOD render_content.

    DATA: li_syntax_check TYPE REF TO zif_abapgit_code_inspector,
          lt_result       TYPE scit_alvlist,
          ls_result       LIKE LINE OF lt_result.

    li_syntax_check = zcl_abapgit_factory=>get_syntax_check( iv_package = mo_repo->get_package( ) ).

    lt_result = li_syntax_check->run( ).

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="toc">' ).

    IF lines( lt_result ) = 0.
      ro_html->add( 'No errors' ).
    ENDIF.

    LOOP AT lt_result INTO ls_result.
      ro_html->add( |{ ls_result-objtype } { ls_result-objname } { ls_result-kind } { ls_result-text }<br>| ).
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content
ENDCLASS.
