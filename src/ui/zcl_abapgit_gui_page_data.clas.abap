CLASS zcl_abapgit_gui_page_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DATA IMPLEMENTATION.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( 'hello world' ).

  ENDMETHOD.
ENDCLASS.
