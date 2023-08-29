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
      iv_obj_with_namespace   TYPE tadir-obj_name
    RETURNING
      VALUE(rs_obj_namespace) TYPE zif_abapgit_definitions=>ty_obj_namespace
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
