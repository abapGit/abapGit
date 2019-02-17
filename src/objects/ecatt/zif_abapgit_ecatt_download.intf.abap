INTERFACE zif_abapgit_ecatt_download
  PUBLIC .

  METHODS:
    get_xml_stream
      RETURNING
        VALUE(rv_xml_stream) TYPE xstring.

ENDINTERFACE.
