*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_BRANCH_OVERVIEW
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_branch_overview DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo TYPE REF TO lcl_repo_online.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_branch_overview IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BRANCH_OVERVIEW' ) ).

    ro_html->add( 'todo, see https://github.com/larshp/abapGit/issues/272' ).
    ro_html->add( '<br>' ).
    ro_html->add( '<svg width="100" height="100">' ).
    ro_html->add( '<circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />' ).
    ro_html->add( '</svg>' ).
    ro_html->add( '<br>' ).
    ro_html->add( '<canvas id="myCanvas" width="200" height="100" style="border:1px solid #000000;">' ).
    ro_html->add( '</canvas>' ).
    ro_html->add( '<script>' ).
    ro_html->add( 'var c = document.getElementById("myCanvas");' ).
    ro_html->add( 'var ctx = c.getContext("2d");' ).
    ro_html->add( 'ctx.moveTo(0,0);' ).
    ro_html->add( 'ctx.lineTo(200,100);' ).
    ro_html->add( 'ctx.stroke();' ).
    ro_html->add( '</script>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.