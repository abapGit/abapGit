CLASS ltcl_event DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS query FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_event IMPLEMENTATION.

  METHOD query.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA lo_x TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'
        iv_getdata = 'not_a_param'.

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'
        iv_getdata = 'a=b&b=c'.

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    lo_map = li_cut->query( iv_upper_cased = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

    TRY.
        lo_map->set(
          iv_key = 'x'
          iv_val = 'y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lo_x.
        cl_abap_unit_assert=>assert_char_cp(
          act = lo_x->get_text( )
          exp = '*immutable*' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
