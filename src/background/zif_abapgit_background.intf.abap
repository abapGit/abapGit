INTERFACE zif_abapgit_background
  PUBLIC .


  CLASS-METHODS get_description
    RETURNING
      VALUE(rv_description) TYPE string .
  CLASS-METHODS get_settings
    CHANGING
      VALUE(ct_settings) TYPE zcl_abapgit_persist_background=>ty_settings_tt .
  METHODS run
    IMPORTING
      !io_repo     TYPE REF TO zcl_abapgit_repo_online
      !it_settings TYPE zcl_abapgit_persist_background=>ty_settings_tt OPTIONAL
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
