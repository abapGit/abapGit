INTERFACE zif_abapgitp_plugin
  PUBLIC .


  TYPES:
    BEGIN OF ty_metadata,
      class        TYPE string,
      version      TYPE string,
      late_deser   TYPE abap_bool,
      delete_tadir TYPE abap_bool,
      ddic         TYPE abap_bool,
    END OF ty_metadata .

  METHODS serialize
    IMPORTING
      !io_xml TYPE REF TO zif_abapgitp_xml_output
    RAISING
      zcx_abapgitp_object .
  METHODS deserialize
    IMPORTING
      !iv_package TYPE devclass
      !io_xml     TYPE REF TO zif_abapgitp_xml_input
    RAISING
      zcx_abapgitp_object .
  METHODS delete
    RAISING
      zcx_abapgitp_object .
  METHODS exists
    RETURNING
      VALUE(rv_bool) TYPE abap_bool
    RAISING
      zcx_abapgitp_object .
  METHODS jump .
  METHODS get_metadata
    RETURNING
      VALUE(rs_metadata) TYPE ty_metadata .
ENDINTERFACE.
