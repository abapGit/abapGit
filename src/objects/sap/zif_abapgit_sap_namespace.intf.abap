INTERFACE zif_abapgit_sap_namespace
  PUBLIC .

  METHODS exists
    IMPORTING
      iv_namespace  TYPE trnspace-namespace
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

  METHODS is_editable
    IMPORTING
      iv_namespace  TYPE trnspace-namespace
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

  METHODS split_by_name
    IMPORTING
      iv_obj_with_namespace    TYPE tadir-obj_name
    EXPORTING
      ev_namespace             TYPE trnspace-namespace
      ev_obj_without_namespace TYPE tadir-obj_name
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
