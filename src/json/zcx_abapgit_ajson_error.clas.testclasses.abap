class ltcl_error definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    methods raise for testing.
    methods raise_w_location for testing.

endclass.

class ltcl_error implementation.

  method raise.

    data lx type ref to zcx_abapgit_ajson_error.
    data lv_msg type string.

    lv_msg = repeat( val = 'a'
                     occ = 50 ) && repeat( val = 'b'
                                           occ = 50 ) && '123'.

    try.
      zcx_abapgit_ajson_error=>raise( lv_msg ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = lv_msg
        act = lx->get_text( ) ).
    endtry.

  endmethod.

  method raise_w_location.

    data lx type ref to zcx_abapgit_ajson_error.

    try.
      zcx_abapgit_ajson_error=>raise( iv_msg = 'a'
                                      iv_location = 'b' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_ajson_error into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'a @b'
        act = lx->get_text( ) ).
    endtry.

  endmethod.

endclass.
