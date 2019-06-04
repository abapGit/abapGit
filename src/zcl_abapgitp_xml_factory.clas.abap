CLASS zcl_abapgitp_xml_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS wrap_xml_input
      IMPORTING
        !io_xml             TYPE REF TO object
      RETURNING
        VALUE(ro_xml_proxy) TYPE REF TO zif_abapgitp_xml_input .
    CLASS-METHODS wrap_xml_output
      IMPORTING
        !io_xml             TYPE REF TO object
      RETURNING
        VALUE(ro_xml_proxy) TYPE REF TO zif_abapgitp_xml_output .
    CLASS-METHODS create_xml_input
      IMPORTING
        !iv_xml             TYPE string OPTIONAL
      RETURNING
        VALUE(ro_xml_proxy) TYPE REF TO zif_abapgitp_xml_input
      RAISING
        zcx_abapgitp_object .
    CLASS-METHODS create_xml_output
      RETURNING
        VALUE(ro_xml_proxy) TYPE REF TO zif_abapgitp_xml_output
      RAISING
        zcx_abapgitp_object .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGITP_XML_FACTORY IMPLEMENTATION.


  METHOD create_xml_input.
    DATA lo_wrapped_xml TYPE REF TO object.

*    IF iv_xml IS SUPPLIED.
    CREATE OBJECT lo_wrapped_xml TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = iv_xml.
*    ELSE.
*      CREATE OBJECT lo_wrapped_xml TYPE zcl_abapgit_xml_input.
*    ENDIF.

    ro_xml_proxy = wrap_xml_input( lo_wrapped_xml ).

  ENDMETHOD.


  METHOD create_xml_output.
    DATA lo_wrapped_xml TYPE REF TO object.
    CREATE OBJECT lo_wrapped_xml TYPE zcl_abapgit_xml_output.

    ro_xml_proxy = wrap_xml_output( lo_wrapped_xml ).

  ENDMETHOD.


  METHOD wrap_xml_input.
    CREATE OBJECT ro_xml_proxy TYPE lcl_xml_input
      EXPORTING
        io_xml = io_xml.
  ENDMETHOD.


  METHOD wrap_xml_output.
    CREATE OBJECT ro_xml_proxy TYPE lcl_xml_output
      EXPORTING
        io_xml = io_xml.
  ENDMETHOD.
ENDCLASS.
