class ltcl_test_strbuf definition for testing
  risk level harmless
  duration short.

  public section.

    methods join_space for testing.

endclass.

class ltcl_test_strbuf implementation.

  method join_space.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_string_buffer=>new( )->add( 'a' )->add( 'b' )->join_w_space_and_flush( )
      exp = 'a b' ).

  endmethod.

endclass.
