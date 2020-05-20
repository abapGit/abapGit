INTERFACE zif_abapgit_frontend_services PUBLIC.
  METHODS file_upload
    IMPORTING
      !iv_path       TYPE string
    RETURNING
      VALUE(rv_xstr) TYPE xstring
    RAISING
      zcx_abapgit_exception .
  METHODS file_download
    IMPORTING
      !iv_path TYPE string
      !iv_xstr TYPE xstring
    RAISING
      zcx_abapgit_exception .
  METHODS show_file_save_dialog
    IMPORTING
      !iv_title            TYPE string
      !iv_extension        TYPE string
      !iv_default_filename TYPE string
    RETURNING
      VALUE(rv_path)       TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS show_file_open_dialog
    IMPORTING
      !iv_title            TYPE string
      !iv_extension        TYPE string
      !iv_default_filename TYPE string
    RETURNING
      VALUE(rv_path)       TYPE string
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
