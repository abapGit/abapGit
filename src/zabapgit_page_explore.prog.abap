*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_EXPLORE
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_explore DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_explore IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.
    ro_html->add( redirect( 'http://larshp.github.io/abapGit/explore.html' ) ).

  ENDMETHOD.

ENDCLASS.                       "lcl_gui_page_explore IMPLEMENTATION