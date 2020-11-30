
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_xml_pretty.

    METHODS:
      setup,
      pretty1 FOR TESTING RAISING cx_static_check,
      pretty2 FOR TESTING RAISING cx_static_check,
      pretty3 FOR TESTING RAISING cx_static_check,
      malformatted FOR TESTING RAISING cx_static_check,
      dont_ignore_error FOR TESTING RAISING cx_static_check,
      unpretty FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD pretty1.

    DATA lv_result TYPE string.

    lv_result = mo_cut->print( '<foo></foo>' ).
    lv_result = lv_result+1.

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = |<?xml version="1.0" encoding="utf-16"?>\n<foo/>\n| ).

  ENDMETHOD.

  METHOD pretty2.

    DATA lv_result TYPE string.

    lv_result = mo_cut->print( '<foo>2</foo>' ).
    lv_result = lv_result+1.

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = |<?xml version="1.0" encoding="utf-16"?>\n<foo>2</foo>\n| ).

  ENDMETHOD.

  METHOD pretty3.

    DATA lv_result TYPE string.

    lv_result = mo_cut->print( '<foo><bar>2</bar></foo>' ).
    lv_result = lv_result+1.

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = |<?xml version="1.0" encoding="utf-16"?>\n<foo>\n <bar>2</bar>\n</foo>\n| ).

  ENDMETHOD.

  METHOD malformatted.

    DATA lv_result TYPE string.

    lv_result = mo_cut->print( 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'abc' ).

  ENDMETHOD.

  METHOD dont_ignore_error.

    TRY.
        mo_cut->print(
          iv_xml           = 'abc'
          iv_ignore_errors = abap_false ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD unpretty.

    DATA lv_result TYPE string.

    lv_result = mo_cut->print(
      iv_xml      = |<foo>\n <bar>2</bar>\n</foo>|
      iv_unpretty = abap_true ).

    lv_result = lv_result+1.

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = |<?xml version="1.0" encoding="utf-16"?><foo><bar>2</bar></foo>| ).

  ENDMETHOD.

ENDCLASS.
