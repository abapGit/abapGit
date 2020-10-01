INTERFACE zif_abapgit_gui_event
  PUBLIC .

  DATA mv_action   TYPE string READ-ONLY.
  DATA mv_getdata  TYPE string READ-ONLY.
  DATA mt_postdata TYPE cnht_post_data_tab READ-ONLY.
  DATA mi_gui_services TYPE REF TO zif_abapgit_gui_services READ-ONLY.

  METHODS query
    IMPORTING
      iv_upper_cased TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(ro_string_map) TYPE REF TO zcl_abapgit_string_map
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
