INTERFACE zif_abapgit_ecatt_upload
  PUBLIC .
  METHODS:
    set_stream_for_upload
      IMPORTING
        iv_xml TYPE xstring.

ENDINTERFACE.
