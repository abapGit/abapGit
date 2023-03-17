CLASS ltcl_timer DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS:
      check_result
        IMPORTING
          iv_result TYPE string
          iv_regex  TYPE string,

      run_timer FOR TESTING,
      run_timer_with_count FOR TESTING,
      run_timer_with_text FOR TESTING.

ENDCLASS.

CLASS ltcl_timer IMPLEMENTATION.

  METHOD check_result.

    FIND REGEX iv_regex IN iv_result.

    cl_abap_unit_assert=>assert_subrc(
      act = sy-subrc
      msg = 'Did not return right measurement' ).

  ENDMETHOD.

  METHOD run_timer.

    DATA lo_timer TYPE REF TO zcl_abapgit_timer.

    lo_timer = zcl_abapgit_timer=>create( )->start( ).

    WAIT UP TO 1 SECONDS.

    check_result(
      iv_result = lo_timer->end( )
      iv_regex  = '1.0[0-9] seconds' ).

  ENDMETHOD.

  METHOD run_timer_with_count.

    DATA lo_timer TYPE REF TO zcl_abapgit_timer.

    lo_timer = zcl_abapgit_timer=>create( iv_count = 1 )->start( ).

    WAIT UP TO 1 SECONDS.

    check_result(
      iv_result = lo_timer->end( )
      iv_regex  = '1 object, 1.0[0-9] seconds' ).

    lo_timer = zcl_abapgit_timer=>create( iv_count = 1234 )->start( ).

    WAIT UP TO 1 SECONDS.

    check_result(
      iv_result = lo_timer->end( )
      iv_regex  = '1234 objects, 1.0[0-9] seconds' ).

  ENDMETHOD.

  METHOD run_timer_with_text.

    CONSTANTS lc_total TYPE string VALUE 'Total:'.

    DATA lo_timer TYPE REF TO zcl_abapgit_timer.

    lo_timer = zcl_abapgit_timer=>create( lc_total )->start( ).

    WAIT UP TO 1 SECONDS.

    check_result(
      iv_result = lo_timer->end( )
      iv_regex  = |{ lc_total } 1.0[0-9] seconds| ).

  ENDMETHOD.
ENDCLASS.
