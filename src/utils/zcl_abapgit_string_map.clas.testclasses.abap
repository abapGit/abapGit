CLASS ltcl_sm_test DEFINITION
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS simple FOR TESTING RAISING zcx_abapgit_exception.
    METHODS freeze FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_sm_test IMPLEMENTATION.

  METHOD simple.

    DATA lo_cut TYPE REF TO zcl_abapgit_string_map.

    lo_cut = zcl_abapgit_string_map=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( )
      exp = abap_true ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = '1' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->has( 'A' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'A' )
      exp = '1' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->has( 'B' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'B' )
      exp = '' ).

    lo_cut->delete( 'A' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->has( 'A' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'A' )
      exp = '' ).

  ENDMETHOD.

  METHOD freeze.

    DATA lo_cut TYPE REF TO zcl_abapgit_string_map.

    lo_cut = zcl_abapgit_string_map=>create( ).
    lo_cut->set(
      iv_key = 'A'
      iv_val = '1' )->freeze( ).

    TRY.
        lo_cut->set(
          iv_key = 'B'
          iv_val = '2' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_cut->delete( 'A' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_cut->clear( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
