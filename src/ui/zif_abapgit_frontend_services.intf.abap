INTERFACE zif_abapgit_frontend_services PUBLIC.

  TYPES ty_char1 TYPE c LENGTH 1.

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

  METHODS clipboard_export
    IMPORTING
      iv_no_auth_check TYPE abap_bool DEFAULT abap_false
      VALUE(it_data)   TYPE STANDARD TABLE
    RAISING
      zcx_abapgit_exception.

  METHODS execute
    IMPORTING
      document          TYPE string OPTIONAL
      application       TYPE string OPTIONAL
      parameter         TYPE string OPTIONAL
      default_directory TYPE string OPTIONAL
      maximized         TYPE string OPTIONAL
      minimized         TYPE string OPTIONAL
      synchronous       TYPE string OPTIONAL
      operation         TYPE string DEFAULT 'OPEN'
    RAISING
      zcx_abapgit_exception.

  METHODS get_system_directory
    CHANGING
      system_directory TYPE string
    RAISING
      zcx_abapgit_exception.

  METHODS directory_browse
    IMPORTING
      window_title    TYPE string OPTIONAL
      initial_folder  TYPE string OPTIONAL
    CHANGING
      selected_folder TYPE string
    RAISING
      zcx_abapgit_exception.

  METHODS get_file_separator
    CHANGING
      file_separator TYPE ty_char1
    RAISING
      zcx_abapgit_exception.

  METHODS get_gui_version
    CHANGING
      version_table TYPE filetable
      rc            TYPE i
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
