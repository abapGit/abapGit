CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_gui_page_data DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS concatenated_key_to_where1 FOR TESTING RAISING cx_static_check.
    METHODS concatenated_key_to_where2 FOR TESTING RAISING cx_static_check.
    METHODS concatenated_key_to_where3 FOR TESTING RAISING cx_static_check.
    METHODS concatenated_key_to_where4 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD concatenated_key_to_where1.

    DATA lv_where TYPE string.

    lv_where = zcl_abapgit_gui_page_data=>concatenated_key_to_where(
      iv_table  = 'T100'
      iv_tabkey = 'EABC55555555555555555001' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_where
      exp = |sprsl = 'E' AND arbgb = 'ABC55555555555555555' AND msgnr = '001'| ).

  ENDMETHOD.

  METHOD concatenated_key_to_where2.

    DATA lv_where TYPE string.

    lv_where = zcl_abapgit_gui_page_data=>concatenated_key_to_where(
      iv_table  = 'T100'
      iv_tabkey = 'ESHORT' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_where
      exp = |sprsl = 'E' AND arbgb = 'SHORT' AND msgnr = ''| ).

  ENDMETHOD.

  METHOD concatenated_key_to_where3.

    DATA lv_where TYPE string.

    lv_where = zcl_abapgit_gui_page_data=>concatenated_key_to_where(
      iv_table  = 'T100'
      iv_tabkey = 'ESHORT               001' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_where
      exp = |sprsl = 'E' AND arbgb = 'SHORT' AND msgnr = '001'| ).

  ENDMETHOD.

  METHOD concatenated_key_to_where4.

    DATA lv_where TYPE string.

    lv_where = zcl_abapgit_gui_page_data=>concatenated_key_to_where(
      iv_table  = 'T100'
      iv_tabkey = 'ESHORT               0' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_where
      exp = |sprsl = 'E' AND arbgb = 'SHORT' AND msgnr = '0'| ).

  ENDMETHOD.

ENDCLASS.
