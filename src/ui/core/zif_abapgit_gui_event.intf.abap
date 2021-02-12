INTERFACE zif_abapgit_gui_event
  PUBLIC .

  DATA mv_action   TYPE string READ-ONLY.
  DATA mv_getdata  TYPE string READ-ONLY.
  DATA mt_postdata TYPE zif_abapgit_html_viewer=>ty_post_data READ-ONLY.
  DATA mi_gui_services TYPE REF TO zif_abapgit_gui_services READ-ONLY.
  DATA mv_current_page_name TYPE string.

  METHODS query
    RETURNING
      VALUE(ro_string_map) TYPE REF TO zcl_abapgit_string_map
    RAISING
      zcx_abapgit_exception.

  METHODS form_data
    RETURNING
      VALUE(ro_string_map) TYPE REF TO zcl_abapgit_string_map
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
