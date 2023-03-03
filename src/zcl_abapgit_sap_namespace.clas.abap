CLASS zcl_abapgit_sap_namespace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_sap_namespace.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_SAP_NAMESPACE IMPLEMENTATION.

  METHOD zif_abapgit_sap_namespace~namespace_exists.
    DATA lv_editflag TYPE trnspace-editflag.
    SELECT SINGLE editflag FROM trnspace INTO lv_editflag WHERE namespace = iv_namespace.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD zif_abapgit_sap_namespace~namespace_is_editable.
    DATA lv_editflag TYPE trnspace-editflag.
    SELECT SINGLE editflag FROM trnspace INTO lv_editflag WHERE namespace = iv_namespace.
    rv_yes = boolc( sy-subrc = 0 AND lv_editflag = 'X' ).
  ENDMETHOD.

ENDCLASS.
