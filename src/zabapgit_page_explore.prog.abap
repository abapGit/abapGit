*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_EXPLORE
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_explore DEFINITION FINAL INHERITING FROM lcl_gui_page.
  PUBLIC SECTION.

    CONSTANTS c_explore_url TYPE string
      VALUE 'http://larshp.github.io/abapGit/explore.html'.

    METHODS constructor.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_explore IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-redirect_url = c_explore_url.
  ENDMETHOD.  "constructor

  METHOD render_content.
    ASSERT 1 = 1. " Dummy
  ENDMETHOD. "render_content.

ENDCLASS.                       "lcl_gui_page_explore IMPLEMENTATION
