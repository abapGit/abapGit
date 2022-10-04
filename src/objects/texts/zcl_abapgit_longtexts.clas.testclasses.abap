CLASS ltcl_longtexts DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_longtexts.

    METHODS:
      setup,
      escape_name FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_longtexts DEFINITION LOCAL FRIENDS ltcl_longtexts.

CLASS ltcl_longtexts IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD escape_name.

    DATA lv_act TYPE dokil-object.

    " no sub-objects
    lv_act = mo_cut->escape_name(
      iv_longtext_id = 'CL'
      iv_object_name = 'ZTEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = 'ZTEST' ).

    lv_act = mo_cut->escape_name(
      iv_longtext_id = 'CL'
      iv_object_name = 'ZCL_TEST_TEXT' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = 'ZCL#_TEST#_TEXT' ).

    " with sub-objects
    lv_act = mo_cut->escape_name(
      iv_longtext_id = 'CA'
      iv_object_name = 'ZTEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = 'ZTEST                         %' ).

    lv_act = mo_cut->escape_name(
      iv_longtext_id = 'CA'
      iv_object_name = 'ZCL_TEST_TEXT' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = 'ZCL#_TEST#_TEXT                 %' ).

  ENDMETHOD.

ENDCLASS.
