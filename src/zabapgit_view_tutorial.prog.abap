*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_TUTORIAL
*&---------------------------------------------------------------------*

CLASS lcl_gui_view_tutorial DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.                       "lcl_gui_view_tutorial

CLASS lcl_gui_view_tutorial IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( '<b>Tutorial</b><br>' ).
    ro_html->add( 'To add a repo as favorite'
                 && ' click <img src="img/star-grey"> icon at repo toolbar.<br>' ).
    ro_html->add( 'To choose a repo press <img src="img/burger"> at the favorite bar.<br>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "lif_gui_page~render

ENDCLASS.                       "lcl_gui_view_tutorial