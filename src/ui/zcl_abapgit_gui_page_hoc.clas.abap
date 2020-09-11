CLASS zcl_abapgit_gui_page_hoc DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_page
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        ii_child_component TYPE REF TO zif_abapgit_gui_renderable
        iv_page_title TYPE string
        io_page_menu TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
      RETURNING
        VALUE(ri_page_wrap) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.
  PRIVATE SECTION.
    DATA mi_child TYPE REF TO zif_abapgit_gui_renderable.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_HOC IMPLEMENTATION.


  METHOD create.

    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.

    CREATE OBJECT lo_page.
    lo_page->ms_control-page_title = iv_page_title.
    lo_page->ms_control-page_menu  = io_page_menu.
    lo_page->mi_child = ii_child_component.

    ri_page_wrap = lo_page.

  ENDMETHOD.


  METHOD render_content.

    IF mi_child IS BOUND.
      ri_html = mi_child->render( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
