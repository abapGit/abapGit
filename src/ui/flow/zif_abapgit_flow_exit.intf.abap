INTERFACE zif_abapgit_flow_exit
  PUBLIC .


  METHODS toolbar_extras
    IMPORTING
      !io_toolbar TYPE REF TO zcl_abapgit_html_toolbar
      !is_feature TYPE zif_abapgit_gui_page_flow=>ty_feature
      !iv_index   TYPE i .

  TYPES: BEGIN OF ty_event_result,
           handled TYPE zif_abapgit_gui_event_handler=>ty_handling_result,
           refresh TYPE abap_bool,
         END OF ty_event_result.

  METHODS on_event
    IMPORTING
      ii_event         TYPE REF TO zif_abapgit_gui_event
      it_features      TYPE zif_abapgit_gui_page_flow=>ty_features
    RETURNING
      VALUE(rs_result) TYPE ty_event_result
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
