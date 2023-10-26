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


  METHOD zif_abapgit_sap_namespace~exists.
    DATA lv_editflag TYPE trnspace-editflag.
    DATA lo_obj TYPE REF TO object.
    DATA lo_nsp TYPE REF TO object.
    FIELD-SYMBOLS <lg_obj> TYPE any.
    TRY.
        SELECT SINGLE editflag FROM ('TRNSPACE') INTO lv_editflag WHERE namespace = iv_namespace.
        rv_yes = boolc( sy-subrc = 0 ).
      CATCH cx_sy_dynamic_osql_error.
        ASSIGN ('XCO_CP_SYSTEM=>NAMESPACE') TO <lg_obj>.
        lo_obj = <lg_obj>.
        CALL METHOD lo_obj->('IF_XCO_CP_NAMESPACE_FACTORY~FOR')
          EXPORTING
            iv_value     = iv_namespace
          RECEIVING
            ro_namespace = lo_nsp.
        CALL METHOD lo_nsp->('IF_XCO_CP_NAMESPACE~EXISTS')
          RECEIVING
            rv_exists = rv_yes.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_sap_namespace~is_editable.
    DATA lv_editflag TYPE trnspace-editflag.
    DATA lo_obj TYPE REF TO object.
    DATA lo_nsp TYPE REF TO object.
    FIELD-SYMBOLS <lg_obj> TYPE any.
    TRY.
        SELECT SINGLE editflag FROM ('TRNSPACE') INTO lv_editflag WHERE namespace = iv_namespace.
        rv_yes = boolc( sy-subrc = 0 AND lv_editflag = 'X' ).
      CATCH cx_sy_dynamic_osql_error.
        ASSIGN ('XCO_CP_SYSTEM=>NAMESPACE') TO <lg_obj>.
        lo_obj = <lg_obj>.
        CALL METHOD lo_obj->('IF_XCO_CP_NAMESPACE_FACTORY~FOR')
          EXPORTING
            iv_value     = iv_namespace
          RECEIVING
            ro_namespace = lo_nsp.
        CALL METHOD lo_nsp->('IF_XCO_CP_NAMESPACE~IS_CHANGEABLE')
          RECEIVING
            rv_exists = rv_yes.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_sap_namespace~split_by_name.
* use this method instead of function module RS_NAME_SPLIT_NAMESPACE
    DATA lv_regex  TYPE string.
    DATA lv_length TYPE i.
    DATA lr_ex     TYPE REF TO cx_root.

    lv_regex = '^\/[^\/]{1,8}\/'.

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

    IF iv_allow_slash_in_name = abap_false AND rs_obj_namespace-obj_without_namespace CA '/'.
      zcx_abapgit_exception=>raise(
       |Object without namespace { rs_obj_namespace-obj_without_namespace } contains a '/'| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
