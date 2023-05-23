CLASS zcl_abapgit_gui_page_hoc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ii_child_component     TYPE REF TO zif_abapgit_gui_renderable
        !iv_page_title          TYPE string OPTIONAL
        !iv_page_layout         TYPE string DEFAULT zcl_abapgit_gui_page=>c_page_layout-centered
        !io_page_menu           TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
        !ii_page_menu_provider  TYPE REF TO zif_abapgit_gui_menu_provider OPTIONAL
        !ii_page_title_provider TYPE REF TO zif_abapgit_gui_page_title OPTIONAL
        !iv_extra_css_url       TYPE string OPTIONAL
        !iv_extra_js_url        TYPE string OPTIONAL
        !iv_show_as_modal       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_page_wrap)     TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.
    METHODS get_child
      RETURNING
        VALUE(ri_child) TYPE REF TO zif_abapgit_gui_renderable.
    METHODS constructor
      IMPORTING
        !ii_child_component TYPE REF TO zif_abapgit_gui_renderable
        !is_control         TYPE zcl_abapgit_gui_page=>ty_control
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION.
  PRIVATE SECTION.

    DATA mi_child TYPE REF TO zif_abapgit_gui_renderable .

    METHODS detect_modal
      RETURNING
        VALUE(rv_is_modal) TYPE abap_bool.

    METHODS detect_menu_provider
      RETURNING
        VALUE(ri_ref) TYPE REF TO zif_abapgit_gui_menu_provider.

    METHODS detect_title_provider
      RETURNING
        VALUE(ri_ref) TYPE REF TO zif_abapgit_gui_page_title.

ENDCLASS.



CLASS zcl_abapgit_gui_page_hoc IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    mi_child = ii_child_component.
    ms_control = is_control.

    IF ms_control-show_as_modal = abap_false.
      ms_control-show_as_modal = detect_modal( ).
    ENDIF.

    IF ms_control-page_menu_provider IS NOT BOUND.
      ms_control-page_menu_provider = detect_menu_provider( ).
    ENDIF.

    IF ms_control-page_title_provider IS NOT BOUND.
      ms_control-page_title_provider = detect_title_provider( ).
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.
    DATA ls_control TYPE zcl_abapgit_gui_page=>ty_control.

    ls_control-page_title          = iv_page_title.
    ls_control-page_layout         = iv_page_layout.
    ls_control-page_menu           = io_page_menu.
    ls_control-page_menu_provider  = ii_page_menu_provider.
    ls_control-page_title_provider = ii_page_title_provider.
    ls_control-extra_css_url       = iv_extra_css_url.
    ls_control-extra_js_url        = iv_extra_js_url.
    ls_control-show_as_modal       = iv_show_as_modal.

    CREATE OBJECT lo_page
      EXPORTING
        ii_child_component = ii_child_component
        is_control         = ls_control.

    ri_page_wrap = lo_page.

  ENDMETHOD.


  METHOD detect_menu_provider.
    TRY.
        ri_ref ?= mi_child.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.


  METHOD detect_modal.

    DATA li_modal TYPE REF TO zif_abapgit_gui_modal.

    TRY.
        li_modal ?= mi_child.
        rv_is_modal = li_modal->is_modal( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.


  METHOD detect_title_provider.
    TRY.
        ri_ref ?= mi_child.
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
