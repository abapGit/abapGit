*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_MERGE
*&---------------------------------------------------------------------*


CLASS lcl_gui_page_merge DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo   TYPE REF TO lcl_repo_online
                  iv_source TYPE string
                  iv_target TYPE string,
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo   TYPE REF TO lcl_repo_online,
          mv_source TYPE string,
          mv_target TYPE string.

ENDCLASS.                       "lcl_gui_page_merge DEFINITION

CLASS lcl_gui_page_merge IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo   = io_repo.
    mv_source = iv_source.
    mv_target = iv_target.
  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    BREAK-POINT.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'MERGE' ) ).
    _add '<div id="toc">'.
    ro_html->add( 'todo' ).
    _add '</div>'.
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.