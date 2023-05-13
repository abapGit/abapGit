CLASS ltcl_test_strbuf DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

    METHODS join_space FOR TESTING.

ENDCLASS.

CLASS ltcl_test_strbuf IMPLEMENTATION.

  METHOD join_space.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_string_buffer=>new( )->add( 'a' )->add( 'b' )->join_w_space_and_flush( )
      exp = 'a b' ).

  ENDMETHOD.

ENDCLASS.
