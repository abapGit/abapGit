CLASS zcl_abapgit_gui_page_hoc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ii_child_component TYPE REF TO zif_abapgit_gui_renderable
        !iv_page_title      TYPE string OPTIONAL
        !io_page_menu       TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
        !ii_page_menu_provider TYPE REF TO zif_abapgit_gui_menu_provider OPTIONAL
        !ii_page_title_provider TYPE REF TO zif_abapgit_gui_page_title OPTIONAL
        !iv_extra_css_url       TYPE string OPTIONAL
        !iv_extra_js_url        TYPE string OPTIONAL
        !iv_show_as_modal   TYPE abap_bool DEFAULT abap_false
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

    CLASS-METHODS detect_modal
      IMPORTING
        ii_child_component TYPE REF TO zif_abapgit_gui_renderable
      RETURNING
        VALUE(rv_is_modal) TYPE abap_bool.

    CLASS-METHODS detect_menu_provider
      IMPORTING
        ii_child_component TYPE REF TO zif_abapgit_gui_renderable
      RETURNING
        VALUE(ri_ref) TYPE REF TO zif_abapgit_gui_menu_provider.

    CLASS-METHODS detect_title_provider
      IMPORTING
        ii_child_component TYPE REF TO zif_abapgit_gui_renderable
      RETURNING
        VALUE(ri_ref) TYPE REF TO zif_abapgit_gui_page_title.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_HOC IMPLEMENTATION.


  METHOD create.

    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.

    CREATE OBJECT lo_page.

    lo_page->ms_control-show_as_modal = detect_modal( ii_child_component ).
    IF iv_show_as_modal = abap_true. " TODO, or maybe if supplied ? Should param override implicit detection ?
      lo_page->ms_control-show_as_modal = abap_true.
    ENDIF.

    lo_page->ms_control-page_menu_provider = detect_menu_provider( ii_child_component ).
    IF ii_page_title_provider IS BOUND.
      lo_page->ms_control-page_menu_provider = ii_page_menu_provider.
    ENDIF.

    lo_page->ms_control-page_title_provider = detect_title_provider( ii_child_component ).
    IF ii_page_title_provider IS BOUND.
      lo_page->ms_control-page_title_provider = ii_page_title_provider.
    ENDIF.

    lo_page->ms_control-page_title          = iv_page_title.
    lo_page->ms_control-page_menu           = io_page_menu.
    lo_page->ms_control-extra_css_url       = iv_extra_css_url.
    lo_page->ms_control-extra_js_url        = iv_extra_js_url.
    lo_page->mi_child                       = ii_child_component.

    ri_page_wrap = lo_page.

  ENDMETHOD.


  METHOD detect_menu_provider.
    TRY.
        ri_ref ?= ii_child_component.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.


  METHOD detect_modal.

    DATA li_modal TYPE REF TO zif_abapgit_gui_modal.

    TRY.
        li_modal ?= ii_child_component.
        rv_is_modal = li_modal->is_modal( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.


  METHOD detect_title_provider.
    TRY.
        ri_ref ?= ii_child_component.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
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
