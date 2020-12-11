CLASS ltcl_version DEFINITION DEFERRED.
CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS ltcl_version.

CLASS ltcl_version DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      version_to_numeric FOR TESTING,
      compare            FOR TESTING,
      normalize          FOR TESTING.

ENDCLASS.

CLASS ltcl_version IMPLEMENTATION.

  METHOD version_to_numeric.

    DATA: lv_version_exp TYPE i VALUE 1023010,
          lv_version_act TYPE i.

    lv_version_act = zcl_abapgit_version=>version_to_numeric( '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = lv_version_exp
                                        act = lv_version_act
                                        msg = ' Error during conversion of version to numeric value' ).

  ENDMETHOD.

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
