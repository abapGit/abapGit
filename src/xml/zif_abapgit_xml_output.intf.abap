INTERFACE zif_abapgit_xml_output
  PUBLIC .
  TYPES:
    BEGIN OF ty_i18n_params,
      serialize_master_lang_only TYPE abap_bool,
    END OF ty_i18n_params.

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
      !iv_normalize TYPE sap_bool DEFAULT abap_true
      !is_metadata  TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
    RETURNING
      VALUE(rv_xml) TYPE string .
  METHODS i18n_params
    IMPORTING
      iv_serialize_master_lang_only TYPE ty_i18n_params-serialize_master_lang_only OPTIONAL
    RETURNING
      VALUE(rs_params)              TYPE ty_i18n_params.
ENDINTERFACE.
