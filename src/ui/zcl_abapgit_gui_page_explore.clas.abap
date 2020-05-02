CLASS zcl_abapgit_gui_page_explore DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    CONSTANTS c_explore_url TYPE string
      VALUE 'https://dotabap.github.io/explore.html'.

    METHODS constructor
      RAISING zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_EXPLORE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-redirect_url = c_explore_url.
  ENDMETHOD.


  METHOD render_content.
    ASSERT 1 = 1. " Dummy
  ENDMETHOD.
ENDCLASS.
