CLASS ltcl_sm_test DEFINITION
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_struc,
        a TYPE string,
        b TYPE abap_bool,
        c TYPE i,
      END OF ty_struc.

    METHODS simple FOR TESTING RAISING zcx_abapgit_exception.
    METHODS freeze FOR TESTING RAISING zcx_abapgit_exception.
    METHODS strict FOR TESTING RAISING zcx_abapgit_exception.
    METHODS case_insensitive FOR TESTING RAISING zcx_abapgit_exception.

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

  METHOD strict.

    DATA ls_struc_act TYPE ty_struc.
    DATA ls_struc_exp TYPE ty_struc.
    DATA lo_x TYPE REF TO cx_root.
    DATA lo_cut TYPE REF TO zcl_abapgit_string_map.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    lo_cut = zcl_abapgit_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).
    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    TRY.
        lo_cut->to_abap( CHANGING cs_container = ls_struc_act ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lo_x.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Component Z not found in target'
          act = lo_x->get_text( ) ).
    ENDTRY.

    lo_map = lo_cut->strict( abap_false ).
    lo_map->to_abap( CHANGING cs_container = ls_struc_act ).

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  ENDMETHOD.

  METHOD case_insensitive.

    DATA lo_cut TYPE REF TO zcl_abapgit_string_map.
    lo_cut = zcl_abapgit_string_map=>create( iv_case_insensitive = abap_true ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'a' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'b' ) ).

  ENDMETHOD.


ENDCLASS.
