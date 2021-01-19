INTERFACE zif_abapgit_xml_output
  PUBLIC .


  METHODS add
    IMPORTING
      !iv_name TYPE clike
      !ig_data TYPE any
    RAISING
      zcx_abapgit_exception .
  METHODS set_raw
    IMPORTING
      !ii_raw TYPE REF TO if_ixml_element .
  METHODS add_xml
    IMPORTING
      !iv_name TYPE clike
      !ii_xml  TYPE REF TO if_ixml_element .
  METHODS render
    IMPORTING
      !iv_normalize TYPE abap_bool DEFAULT abap_true
      !is_metadata  TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
    RETURNING
      VALUE(rv_xml) TYPE string .
  METHODS i18n_params
    IMPORTING
      !is_i18n_params       TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL
    RETURNING
      VALUE(rs_i18n_params) TYPE zif_abapgit_definitions=>ty_i18n_params .
ENDINTERFACE.
