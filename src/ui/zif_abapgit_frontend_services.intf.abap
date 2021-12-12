INTERFACE zif_abapgit_frontend_services PUBLIC.

  TYPES ty_char1 TYPE c LENGTH 1.

  METHODS file_upload
    IMPORTING
      !iv_path       TYPE string
    RETURNING
      VALUE(rv_xstr) TYPE xstring
    RAISING
      zcx_abapgit_exception.

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
      zcx_abapgit_exception.

  METHODS show_file_open_dialog
    IMPORTING
      !iv_title            TYPE string
      !iv_extension        TYPE string
      !iv_default_filename TYPE string
    RETURNING
      VALUE(rv_path)       TYPE string
    RAISING
      zcx_abapgit_exception.

  METHODS clipboard_export
    IMPORTING
      iv_no_auth_check TYPE abap_bool DEFAULT abap_false
      VALUE(it_data)   TYPE STANDARD TABLE
    RAISING
      zcx_abapgit_exception.

  METHODS execute
    IMPORTING
      !iv_document          TYPE string OPTIONAL
      !iv_application       TYPE string OPTIONAL
      !iv_parameter         TYPE string OPTIONAL
      !iv_default_directory TYPE string OPTIONAL
      !iv_maximized         TYPE string OPTIONAL
      !iv_minimized         TYPE string OPTIONAL
      !iv_synchronous       TYPE string OPTIONAL
      !iv_operation         TYPE string DEFAULT 'OPEN'
    RAISING
      zcx_abapgit_exception.

  METHODS get_system_directory
    CHANGING
      !cv_system_directory TYPE string
    RAISING
      zcx_abapgit_exception.

  METHODS directory_browse
    IMPORTING
      iv_window_title    TYPE string OPTIONAL
      iv_initial_folder  TYPE string OPTIONAL
    CHANGING
      cv_selected_folder TYPE string
    RAISING
      zcx_abapgit_exception.

  METHODS get_file_separator
    CHANGING
      cv_file_separator TYPE ty_char1
    RAISING
      zcx_abapgit_exception.

  METHODS get_gui_version
    CHANGING
      ct_version_table TYPE filetable
      cv_rc            TYPE i
    RAISING
      zcx_abapgit_exception.

  METHODS directory_exist
    IMPORTING
      iv_directory     TYPE string
    RETURNING
      VALUE(rv_exists) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS directory_create
    IMPORTING
      iv_directory TYPE string
    CHANGING
      cv_rc        TYPE i
    RAISING
      zcx_abapgit_exception.

  METHODS gui_is_available
    RETURNING
      VALUE(rv_gui_is_available) TYPE abap_bool.

  METHODS is_sapgui_for_java
    RETURNING
      VALUE(rv_result) TYPE abap_bool.

  METHODS is_sapgui_for_windows
    RETURNING
      VALUE(rv_result) TYPE abap_bool.

  METHODS is_webgui
    RETURNING
      VALUE(rv_is_webgui) TYPE abap_bool.

ENDINTERFACE.
