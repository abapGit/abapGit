CLASS zcl_abapgit_gui_page_explore DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    CONSTANTS c_explore_url TYPE string
      VALUE 'https://dotabap.github.io/explore.html'.

    METHODS constructor.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_EXPLORE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-redirect_url = c_explore_url.
  ENDMETHOD.  "constructor


  METHOD render_content.
    ASSERT 1 = 1. " Dummy
  ENDMETHOD. "render_content.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
