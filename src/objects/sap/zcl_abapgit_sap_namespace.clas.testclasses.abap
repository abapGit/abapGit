CLASS ltcl_check_split_by_name DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM FINAL.
  PRIVATE SECTION.
    METHODS check_with_namespace FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_without_namespace FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_exception FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_check_split_by_name IMPLEMENTATION.

  METHOD check_with_namespace.

    DATA lv_obj_with_namespace    TYPE tadir-obj_name.
    DATA lv_namespace             TYPE trnspace-namespace.
    DATA lv_obj_without_namespace TYPE tadir-obj_name.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.
    DATA li_namespace TYPE REF TO zif_abapgit_sap_namespace.

    lv_obj_with_namespace = '/BLA12345/TEST/123'.

    TRY.
        li_namespace = zcl_abapgit_factory=>get_sap_namespace( ).
        li_namespace->split_by_name(
          EXPORTING
            iv_obj_with_namespace    =  lv_obj_with_namespace
          IMPORTING
            ev_namespace             = lv_namespace
            ev_obj_without_namespace = lv_obj_without_namespace ).

      CATCH zcx_abapgit_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text(  ) ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
       act = lv_namespace
       exp = '/BLA12345/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_without_namespace
      exp = 'TEST/123' ).


  ENDMETHOD.
  METHOD check_without_namespace.

    DATA lv_obj_with_namespace    TYPE tadir-obj_name.
    DATA lv_namespace             TYPE trnspace-namespace.
    DATA lv_obj_without_namespace TYPE tadir-obj_name.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.
    DATA li_namespace TYPE REF TO zif_abapgit_sap_namespace.

    lv_obj_with_namespace = 'ZCL_ABAPGIT_SAP_NAMESP'.

    TRY.
        li_namespace = zcl_abapgit_factory=>get_sap_namespace( ).
        li_namespace->split_by_name(
          EXPORTING
            iv_obj_with_namespace    =  lv_obj_with_namespace
          IMPORTING
            ev_namespace             = lv_namespace
            ev_obj_without_namespace = lv_obj_without_namespace ).

      CATCH zcx_abapgit_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text(  ) ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
       act = lv_namespace
       exp = '' ).

    cl_abap_unit_assert=>assert_equals(
       act = lv_obj_without_namespace
       exp = 'ZCL_ABAPGIT_SAP_NAMESP' ).

  ENDMETHOD.

  METHOD check_exception.

    DATA lv_obj_with_namespace    TYPE tadir-obj_name.
    DATA lv_namespace             TYPE trnspace-namespace.
    DATA lv_obj_without_namespace TYPE tadir-obj_name.
    DATA li_namespace TYPE REF TO zif_abapgit_sap_namespace.

    lv_obj_with_namespace = '/TEST12345/BLA'.

    TRY.
        li_namespace = zcl_abapgit_factory=>get_sap_namespace( ).
        li_namespace->split_by_name(
          EXPORTING
            iv_obj_with_namespace    =  lv_obj_with_namespace
          IMPORTING
            ev_namespace             = lv_namespace
            ev_obj_without_namespace = lv_obj_without_namespace ).

      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( 'No Exception raised' ).
  ENDMETHOD.

ENDCLASS.
