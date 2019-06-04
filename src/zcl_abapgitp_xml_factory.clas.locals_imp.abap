CLASS lcl_xml_output DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgitp_xml_output.

    METHODS constructor
      IMPORTING
        io_xml TYPE REF TO object.

  PRIVATE SECTION.
    DATA mo_wrapped_xml TYPE REF TO object.
ENDCLASS.

CLASS lcl_xml_output IMPLEMENTATION.

  METHOD constructor.
    mo_wrapped_xml = io_xml.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_output~add.
    CALL METHOD mo_wrapped_xml->('ADD')
      EXPORTING
        iv_name = iv_name
        ig_data = ig_data.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_output~set_raw.
    CALL METHOD mo_wrapped_xml->('SET_RAW')
      EXPORTING
        ii_raw = ii_raw.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_output~render.
    CALL METHOD mo_wrapped_xml->('RENDER')
      EXPORTING
        iv_normalize = iv_normalize
        is_metadata  = is_metadata
      RECEIVING
        rv_xml       = rv_xml.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_output~get_wrapped_xml.
    ro_object = mo_wrapped_xml.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_xml_input DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgitp_xml_input.

    METHODS constructor
      IMPORTING
        io_xml TYPE REF TO object.
  PRIVATE SECTION.
    DATA mo_wrapped_xml TYPE REF TO object.
ENDCLASS.

CLASS lcl_xml_input IMPLEMENTATION.

  METHOD constructor.
    mo_wrapped_xml = io_xml.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_input~read.
    CALL METHOD mo_wrapped_xml->('READ')
      EXPORTING
        iv_name = iv_name
      CHANGING
        cg_data = cg_data.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_input~get_raw.
    CALL METHOD mo_wrapped_xml->('GET_RAW')
      RECEIVING
        ri_raw = ri_raw.
  ENDMETHOD.

  METHOD zif_abapgitp_xml_input~get_wrapped_xml.
    ro_object = mo_wrapped_xml.
  ENDMETHOD.

ENDCLASS.
