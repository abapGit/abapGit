INTERFACE zif_abapgitp_xml_input
  PUBLIC .


*    Proxy for lcl_xml_input in ZABAPGIT
  METHODS read
    IMPORTING
      !iv_name TYPE clike
    CHANGING
      !cg_data TYPE any
    RAISING
      zcx_abapgitp_object .
  METHODS get_raw
    RETURNING
      VALUE(ri_raw) TYPE REF TO if_ixml_node .
  METHODS get_wrapped_xml
    RETURNING
      VALUE(ro_object) TYPE REF TO object .
ENDINTERFACE.
