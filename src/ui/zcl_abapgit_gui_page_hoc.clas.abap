CLASS zcl_abapgit_gui_page_hoc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ii_child_component TYPE REF TO zif_abapgit_gui_renderable
        !iv_page_title      TYPE string
        !io_page_menu       TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
      RETURNING
        VALUE(ri_page_wrap) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS get_child
      RETURNING
        VALUE(ri_child) TYPE REF TO zif_abapgit_gui_renderable .
  PROTECTED SECTION.
    METHODS render_content REDEFINITION.
  PRIVATE SECTION.

    DATA mi_child TYPE REF TO zif_abapgit_gui_renderable .
ENDCLASS.



CLASS zcl_abapgit_gui_page_hoc IMPLEMENTATION.


  METHOD create.

    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.

    CREATE OBJECT lo_page.
    lo_page->ms_control-page_title = iv_page_title.
    lo_page->ms_control-page_menu  = io_page_menu.
    lo_page->mi_child = ii_child_component.

    ri_page_wrap = lo_page.

  ENDMETHOD.


  METHOD get_child.
    ri_child = mi_child.
  ENDMETHOD.


  METHOD render_content.

    IF mi_child IS BOUND.
      ri_html = mi_child->render( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
