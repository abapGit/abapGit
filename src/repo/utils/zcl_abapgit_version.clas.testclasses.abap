CLASS ltcl_version DEFINITION DEFERRED.
CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS ltcl_version.

CLASS ltcl_version DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      compare            FOR TESTING,
      normalize          FOR TESTING.

ENDCLASS.

CLASS ltcl_version IMPLEMENTATION.

  METHOD compare.

    DATA lv_result TYPE i.

    " Case 1: version A > version B
    lv_result = zcl_abapgit_version=>compare( iv_a = '1.28.10'
                                              iv_b = '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A > B' ).

    CLEAR: lv_result.

    " Case 2: version A < version B
    lv_result = zcl_abapgit_version=>compare( iv_a = '1.28.10'
                                              iv_b = '2.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = -1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A < B' ).

    CLEAR: lv_result.

    " Case 3: version A = version B
    lv_result = zcl_abapgit_version=>compare( iv_a = '1.28.10'
                                              iv_b = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A = B' ).

  ENDMETHOD.

  METHOD normalize.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( '1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( 'v1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( 'b1.28.10' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( 'x.y.z' )
      exp = '' ).

  ENDMETHOD.

ENDCLASS.
