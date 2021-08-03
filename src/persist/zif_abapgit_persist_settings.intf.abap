INTERFACE zif_abapgit_persist_settings PUBLIC.

  METHODS modify
    IMPORTING
      !io_settings TYPE REF TO zcl_abapgit_settings
    RAISING
      zcx_abapgit_exception .
  METHODS read
    RETURNING
      VALUE(ro_settings) TYPE REF TO zcl_abapgit_settings .

ENDINTERFACE.
