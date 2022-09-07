CLASS ltcl_tests DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_activation DEFINITION LOCAL FRIENDS ltcl_tests.

CLASS ltcl_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_objects_activation.

    METHODS:
      setup,
      is_active FOR TESTING RAISING zcx_abapgit_exception,
      is_ddic_type FOR TESTING,
      get_ddic_type FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD is_active.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    " DDIC, exists
    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'T000'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_active( ls_item )
      exp = abap_true ).

    " DDIC, does not exist
    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'TABL_ABAPGIT'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_active( ls_item )
      exp = abap_true ).

    " non-DDIC, exists
    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPMSYST'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_active( ls_item )
      exp = abap_true ).

    ls_item-obj_type = 'SFSW'.
    ls_item-obj_name = 'SRIS_SWITCH_SOURCE_SEARCH'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_active( ls_item )
      exp = abap_true ).

    " non-DDIC, does not exist
    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'FUGR_ABAPGIT'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_active( ls_item )
      exp = abap_true ).

  ENDMETHOD.

  METHOD is_ddic_type.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_ddic_type( 'TABL' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_ddic_type( 'PROG' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD get_ddic_type.

    DATA:
      lv_type TYPE ddobjtyp,
      lv_name TYPE ddobjname,
      lv_id   TYPE ddobjectid.

    mo_cut->get_ddic_type(
      EXPORTING
        iv_obj_type = 'TABL'
        iv_obj_name = 'T005'
      IMPORTING
        ev_type     = lv_type
        ev_name     = lv_name
        ev_id       = lv_id ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'TABL' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'T005' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_id
      exp = '' ).

    " index id at +10
    mo_cut->get_ddic_type(
      EXPORTING
        iv_obj_type = 'XINX'
        iv_obj_name = 'T005      Z00'
      IMPORTING
        ev_type     = lv_type
        ev_name     = lv_name
        ev_id       = lv_id ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'XINX' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'T005' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_id
      exp = 'Z00' ).

    " index id at +30
    mo_cut->get_ddic_type(
      EXPORTING
        iv_obj_type = 'XINX'
        iv_obj_name = 'ZLONG_TABLE_NAME              Z99'
      IMPORTING
        ev_type     = lv_type
        ev_name     = lv_name
        ev_id       = lv_id ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'XINX' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'ZLONG_TABLE_NAME' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_id
      exp = 'Z99' ).

  ENDMETHOD.

ENDCLASS.
