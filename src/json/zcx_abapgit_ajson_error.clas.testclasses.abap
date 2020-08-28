CLASS ltcl_error DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS raise FOR TESTING.
    METHODS raise_w_location FOR TESTING.

ENDCLASS.

CLASS ltcl_error IMPLEMENTATION.

  METHOD raise.

    DATA lo_x TYPE REF TO zcx_abapgit_ajson_error.
    DATA lv_msg TYPE string.

    lv_msg = repeat(
      val = 'a'
      occ = 50 ) && repeat(
      val = 'b'
      occ = 50 ) && '123'.

    TRY.
        zcx_abapgit_ajson_error=>raise_json( lv_msg ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_x.
        cl_abap_unit_assert=>assert_equals(
          exp = lv_msg
          act = lo_x->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD raise_w_location.

    DATA lo_x TYPE REF TO zcx_abapgit_ajson_error.

    TRY.
        zcx_abapgit_ajson_error=>raise_json(
          iv_msg = 'a'
          iv_location = 'b' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_x.
        cl_abap_unit_assert=>assert_equals(
          exp = 'a @b'
          act = lo_x->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
