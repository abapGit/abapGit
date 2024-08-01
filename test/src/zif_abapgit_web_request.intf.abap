INTERFACE zif_abapgit_web_request PUBLIC.

  METHODS get_header_field
    IMPORTING
      iv_name TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.

  METHODS get_method
    RETURNING
      VALUE(rv_method) TYPE string.

  METHODS get_cdata
    RETURNING
      VALUE(rv_data) TYPE string.

ENDINTERFACE.