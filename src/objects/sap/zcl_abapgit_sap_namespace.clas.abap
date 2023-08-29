CLASS zcl_abapgit_sap_namespace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_sap_namespace.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_sap_namespace IMPLEMENTATION.


  METHOD zif_abapgit_sap_namespace~exists.
    DATA lv_editflag TYPE trnspace-editflag.
    SELECT SINGLE editflag FROM trnspace INTO lv_editflag WHERE namespace = iv_namespace.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_abapgit_sap_namespace~is_editable.
    DATA lv_editflag TYPE trnspace-editflag.
    SELECT SINGLE editflag FROM trnspace INTO lv_editflag WHERE namespace = iv_namespace.
    rv_yes = boolc( sy-subrc = 0 AND lv_editflag = 'X' ).
  ENDMETHOD.


  METHOD zif_abapgit_sap_namespace~split_by_name.
    DATA lv_regex TYPE string.
    DATA lv_object TYPE string.
    DATA lv_length TYPE i.
    DATA lr_ex TYPE REF TO cx_root.

    lv_regex =  '^\/[^\/]{1,8}\/'.

    TRY.
        FIND REGEX lv_regex IN iv_obj_with_namespace MATCH LENGTH lv_length.
      CATCH cx_root INTO lr_ex.
        zcx_abapgit_exception=>raise( lr_ex->get_text( ) ).
    ENDTRY.

    IF sy-subrc = 0 AND lv_length > 1.
      rs_obj_namespace-namespace = iv_obj_with_namespace(lv_length).
      rs_obj_namespace-obj_without_namespace = iv_obj_with_namespace+lv_length.
    ELSE.
      IF iv_obj_with_namespace(1) = '/'.
        zcx_abapgit_exception=>raise( |The object { iv_obj_with_namespace } has an invalid namespace| ).
      ENDIF.
      rs_obj_namespace-obj_without_namespace = iv_obj_with_namespace.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
