INTERFACE zif_abapgit_user_record PUBLIC.

  METHODS get_name
    IMPORTING
      iv_username    TYPE sy-uname
    RETURNING
      VALUE(rv_name) TYPE string.

  METHODS get_email
    IMPORTING
      iv_username     TYPE sy-uname
    RETURNING
      VALUE(rv_email) TYPE string.

  METHODS get_title
    IMPORTING
      iv_username     TYPE sy-uname
    RETURNING
      VALUE(rv_title) TYPE string.

ENDINTERFACE.
