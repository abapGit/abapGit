INTERFACE zif_abapgit_web_response PUBLIC.

  METHODS set_content_type
    IMPORTING
      iv_type TYPE string.

  METHODS set_cdata
    IMPORTING
      iv_data TYPE string.

  METHODS set_xdata
    IMPORTING
      iv_data TYPE xstring.

ENDINTERFACE.