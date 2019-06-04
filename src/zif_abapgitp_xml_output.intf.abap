INTERFACE zif_abapgitp_xml_output
  PUBLIC .


*    Proxy for lcl_xml_output in ZABAPGIT
  METHODS add
    IMPORTING
      !iv_name TYPE clike
      !ig_data TYPE any
    RAISING
      zcx_abapgitp_object .
  METHODS set_raw
    IMPORTING
      !ii_raw TYPE REF TO if_ixml_element .
  METHODS render
    IMPORTING
      !iv_normalize TYPE sap_bool DEFAULT abap_true
      !is_metadata  TYPE zif_abapgit_definitions=>ty_metadata
    RETURNING
      VALUE(rv_xml) TYPE string .
  METHODS get_wrapped_xml
    RETURNING
      VALUE(ro_object) TYPE REF TO object .
ENDINTERFACE.
