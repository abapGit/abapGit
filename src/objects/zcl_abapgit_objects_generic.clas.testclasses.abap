CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_generic DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      serialize FOR TESTING RAISING zcx_abapgit_exception,
      get_primary_table FOR TESTING RAISING zcx_abapgit_exception,
      get_key_fields FOR TESTING RAISING zcx_abapgit_exception,
      get_where_clause FOR TESTING RAISING zcx_abapgit_exception,
      distribute_name_to_components FOR TESTING RAISING zcx_abapgit_exception,
      split_value_to_keys FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_cut  TYPE REF TO zcl_abapgit_objects_generic,
          li_xml  TYPE REF TO zif_abapgit_xml_output,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ASFC'.
    ls_item-obj_name = 'SAP_AS_TEST_001'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item = ls_item.

    CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.

    lo_cut->serialize( li_xml ).
* checks that it does not dump

  ENDMETHOD.

  METHOD get_primary_table.

    DATA: lo_cut  TYPE REF TO zcl_abapgit_objects_generic,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ACGR'.
    ls_item-obj_name = 'SAP_BC_BASIS_ADMIN'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    cl_abap_unit_assert=>assert_equals(
      exp = 'AGR_DEFINE'
      act = lo_cut->get_primary_table( ) ).

  ENDMETHOD.

  METHOD get_key_fields.

    DATA: lo_cut        TYPE REF TO zcl_abapgit_objects_generic,
          ls_item       TYPE zif_abapgit_definitions=>ty_item,
          ls_keys       TYPE dfies,
          lt_key_fields TYPE ddfields.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ACGR'.
    ls_item-obj_name = 'SAP_BC_BASIS_ADMIN'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    lt_key_fields = lo_cut->get_key_fields( 'AGR_HIER' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_key_fields ) ).

    READ TABLE lt_key_fields INTO ls_keys INDEX 3.

    cl_abap_unit_assert=>assert_equals(
      exp = 'OBJECT_ID'
      act = ls_keys-fieldname ).

  ENDMETHOD.

  METHOD get_where_clause.

    DATA: lo_cut  TYPE REF TO zcl_abapgit_objects_generic,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'NSPC'.
    ls_item-obj_name = '/BIC/'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    cl_abap_unit_assert=>assert_equals(
      exp = `NAMESPACE = '/BIC/'`
      act = lo_cut->get_where_clause( 'TRNSPACET' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = `NAMESPACE = '/BIC/' AND SPRAS = 'E'`
      act = lo_cut->get_where_clause( 'TRNSPACETT' ) ).

  ENDMETHOD.

  METHOD distribute_name_to_components.

    DATA: lo_cut           TYPE REF TO zcl_abapgit_objects_generic,
          ls_item          TYPE zif_abapgit_definitions=>ty_item,
          lt_objkey        TYPE zcl_abapgit_objects_generic=>ty_t_objkey,
          ls_objkey        LIKE LINE OF lt_objkey,
          lv_non_value_pos TYPE numc3,
          lt_key_fields    TYPE ddfields.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'WDCC'.
    ls_item-obj_name = 'WDR_CHIP_TEST'.
    ls_item-obj_name+32 = '09'.
    ls_item-obj_name+34 = 'TEST'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    lt_key_fields = lo_cut->get_key_fields( 'WDY_CONFIG_DATA' ).

    lv_non_value_pos = '001'.
    ls_objkey-num    = '001'.
    ls_objkey-value  = ls_item-obj_name.

    lo_cut->distribute_name_to_components(
      EXPORTING
        it_key_component = lt_key_fields
      CHANGING
        ct_objkey        = lt_objkey
        cs_objkey        = ls_objkey
        cv_non_value_pos = lv_non_value_pos ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_objkey ) ).

    READ TABLE lt_objkey INTO ls_objkey INDEX 2.

    cl_abap_unit_assert=>assert_equals(
      exp = '002'
      act = ls_objkey-num ).

    cl_abap_unit_assert=>assert_equals(
      exp = '09'
      act = ls_objkey-value ).

    READ TABLE lt_objkey INTO ls_objkey INDEX 3.

    cl_abap_unit_assert=>assert_equals(
      exp = '003'
      act = ls_objkey-num ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'TEST'
      act = ls_objkey-value ).

  ENDMETHOD.

  METHOD split_value_to_keys.

    DATA: lo_cut           TYPE REF TO zcl_abapgit_objects_generic,
          ls_item          TYPE zif_abapgit_definitions=>ty_item,
          lt_objkey        TYPE zcl_abapgit_objects_generic=>ty_t_objkey,
          ls_objkey        LIKE LINE OF lt_objkey,
          lv_non_value_pos TYPE numc3,
          lt_key_fields    TYPE ddfields.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ASFC'.
    ls_item-obj_name = 'SAP_AS_TEST_002'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    lt_key_fields = lo_cut->get_key_fields( 'AIND_STR4' ).

    ls_objkey-num    = '001'.
    ls_objkey-value  = ls_item-obj_name.
    APPEND ls_objkey TO lt_objkey.

    lv_non_value_pos = '002'.
    ls_objkey-num    = '002'.
    ls_objkey-value  = 'S0099'.

    lo_cut->split_value_to_keys(
      EXPORTING
        it_key_component = lt_key_fields
      CHANGING
        ct_objkey        = lt_objkey
        cs_objkey        = ls_objkey
        cv_non_value_pos = lv_non_value_pos ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_objkey ) ).

    READ TABLE lt_objkey INTO ls_objkey INDEX 3.

    cl_abap_unit_assert=>assert_equals(
      exp = '003'
      act = ls_objkey-num ).

    cl_abap_unit_assert=>assert_equals(
      exp = '0099'
      act = ls_objkey-value ).

  ENDMETHOD.

ENDCLASS.
