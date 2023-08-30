CLASS ltcl_check_split_by_name DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    METHODS check_with_namespace FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_without_namespace FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_exc_starts_with_slash FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_exc_slash_in_name_w_ns FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_exc_slash_in_name_wo_ns FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_check_split_by_name IMPLEMENTATION.

  METHOD check_with_namespace.

    DATA lv_obj_with_namespace TYPE tadir-obj_name.
    DATA ls_obj_with_namespace TYPE zif_abapgit_definitions=>ty_obj_namespace.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.

    lv_obj_with_namespace = '/BLA12345/TEST/123'.

    TRY.
        ls_obj_with_namespace = zcl_abapgit_factory=>get_sap_namespace(  )->split_by_name( lv_obj_with_namespace ).

      CATCH zcx_abapgit_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text(  ) ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
       act = ls_obj_with_namespace-namespace
       exp = '/BLA12345/' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_obj_with_namespace-obj_without_namespace
      exp = 'TEST/123' ).


  ENDMETHOD.
  METHOD check_without_namespace.

    DATA lv_obj_with_namespace TYPE tadir-obj_name.
    DATA ls_obj_with_namespace TYPE zif_abapgit_definitions=>ty_obj_namespace.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.

    lv_obj_with_namespace = 'ZCL_ABAPGIT_SAP_NAMESP'.

    TRY.
        ls_obj_with_namespace = zcl_abapgit_factory=>get_sap_namespace(  )->split_by_name( lv_obj_with_namespace ).
      CATCH zcx_abapgit_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text(  ) ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
       act = ls_obj_with_namespace-namespace
       exp = '' ).

    cl_abap_unit_assert=>assert_equals(
       act = ls_obj_with_namespace-obj_without_namespace
       exp = 'ZCL_ABAPGIT_SAP_NAMESP' ).

  ENDMETHOD.

  METHOD check_exc_starts_with_slash.

    DATA lv_obj_with_namespace TYPE tadir-obj_name.
    lv_obj_with_namespace = '/TEST12345/BLA'.

    TRY.
        zcl_abapgit_factory=>get_sap_namespace(  )->split_by_name( lv_obj_with_namespace ).

      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( 'No Exception raised' ).
  ENDMETHOD.

  METHOD check_exc_slash_in_name_w_ns.
    DATA lv_obj_with_namespace TYPE tadir-obj_name.
    lv_obj_with_namespace = '/TEST/TEST/TEST'.

    TRY.
        zcl_abapgit_factory=>get_sap_namespace(  )->split_by_name(
          iv_obj_with_namespace = lv_obj_with_namespace
          iv_allow_slash_in_name  = abap_false ).

      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( 'No Exception raised' ).
  ENDMETHOD.

  METHOD check_exc_slash_in_name_wo_ns.
    DATA lv_obj_with_namespace TYPE tadir-obj_name.
    lv_obj_with_namespace = 'TEST/TEST'.

    TRY.
        zcl_abapgit_factory=>get_sap_namespace(  )->split_by_name(
          iv_obj_with_namespace = lv_obj_with_namespace
          iv_allow_slash_in_name  = abap_false ).

      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( 'No Exception raised' ).
  ENDMETHOD.

ENDCLASS.
