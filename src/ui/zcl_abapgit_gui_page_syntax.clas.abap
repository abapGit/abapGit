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



CLASS ZCL_ABAPGIT_GUI_PAGE_SYNTAX IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SYNTAX CHECK'.
    mo_repo = io_repo.
  ENDMETHOD.  " constructor.


  METHOD render_content.

    DATA: lt_result TYPE scit_alvlist,
          ls_result LIKE LINE OF lt_result.


    lt_result = zcl_abapgit_syntax_check=>run( mo_repo->get_package( ) ).

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
