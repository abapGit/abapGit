INTERFACE zif_abapgit_sap_namespace
  PUBLIC .

  METHODS namespace_exists
    IMPORTING
      iv_namespace TYPE trnspace-namespace
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

  METHODS namespace_is_editable
    IMPORTING
      iv_namespace TYPE trnspace-namespace
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

ENDINTERFACE.
