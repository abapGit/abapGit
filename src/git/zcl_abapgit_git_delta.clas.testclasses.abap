CLASS ltcl_git_delta_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      test_decode_deltas_empty FOR TESTING RAISING cx_static_check,
      test_decode_deltas_no_delta FOR TESTING RAISING cx_static_check,
      test_decode_deltas_success FOR TESTING RAISING cx_static_check,
      test_decode_deltas_missing_base FOR TESTING RAISING cx_static_check,
      test_delta_header_single_byte FOR TESTING RAISING cx_static_check,
      test_delta_header_multi_byte FOR TESTING RAISING cx_static_check.

    METHODS create_test_objects
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        cx_static_check.

ENDCLASS.

CLASS ltcl_git_delta_test IMPLEMENTATION.

  METHOD create_test_objects.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.

    " Create a base blob object
    ls_object-sha1 = '1234567890123456789012345678901234567890'.
    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = zcl_abapgit_convert=>string_to_xstring_utf8( `Hello World` ).
    ls_object-index = 1.
    APPEND ls_object TO rt_objects.

    " Create a delta object that references the base blob
    CLEAR ls_object.
    ls_object-sha1 = '1234567890123456789012345678901234567890'. " same as base for reference
    ls_object-type = zif_abapgit_git_definitions=>c_type-ref_d.
    " Create simple delta data: base size (11), result size (13), copy 11 bytes from offset 0, insert "!!"
    ls_object-data = '0B0D90' && zcl_abapgit_convert=>string_to_xstring_utf8( `!!` ).
    ls_object-index = 2.
    APPEND ls_object TO rt_objects.
  ENDMETHOD.

  METHOD test_decode_deltas_empty.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    zcl_abapgit_git_delta=>decode_deltas(
      EXPORTING iv_show_progress = abap_false
      CHANGING ct_objects = lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_objects )
      exp = 0
      msg = 'Empty object table should remain empty' ).
  ENDMETHOD.

  METHOD test_decode_deltas_no_delta.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.

    " Create a non-delta object
    ls_object-sha1 = '1234567890123456789012345678901234567890'.
    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = zcl_abapgit_convert=>string_to_xstring_utf8( `Hello World` ).
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

    zcl_abapgit_git_delta=>decode_deltas(
      EXPORTING iv_show_progress = abap_false
      CHANGING ct_objects = lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_objects )
      exp = 1
      msg = 'Non-delta objects should remain unchanged' ).

    READ TABLE lt_objects INTO ls_object INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_object-type
      exp = zif_abapgit_git_definitions=>c_type-blob
      msg = 'Object type should remain blob' ).
  ENDMETHOD.

  METHOD test_decode_deltas_success.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_original_count TYPE i.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.

    lt_objects = create_test_objects( ).

    " Store original count
    lv_original_count = lines( lt_objects ).

    zcl_abapgit_git_delta=>decode_deltas(
      EXPORTING iv_show_progress = abap_false
      CHANGING ct_objects = lt_objects ).

    " Should have same number of objects after delta processing
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_objects )
      exp = lv_original_count
      msg = 'Should have same number of objects after delta processing' ).

    " Check that no delta objects remain
    LOOP AT lt_objects INTO ls_object WHERE type = zif_abapgit_git_definitions=>c_type-ref_d.
      cl_abap_unit_assert=>fail( 'No delta objects should remain after processing' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_decode_deltas_missing_base.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    " Create a delta object without its base
    ls_object-sha1 = 'missing_base_sha1_234567890123456789012'.
    ls_object-type = zif_abapgit_git_definitions=>c_type-ref_d.
    ls_object-data = '0B0D90' && zcl_abapgit_convert=>string_to_xstring_utf8( `!!` ).
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

    TRY.
        zcl_abapgit_git_delta=>decode_deltas(
          EXPORTING iv_show_progress = abap_false
          CHANGING ct_objects = lt_objects ).
        cl_abap_unit_assert=>fail( 'Should raise exception for missing base' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( )
          exp = '*Base not found*'
          msg = 'Should indicate base not found' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_delta_header_single_byte.
    " Test requires access to private method delta_header
    " Since we cannot test private methods directly, we'll test through decode_deltas
    " with a specific delta that exercises the single-byte header path
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_base TYPE zif_abapgit_definitions=>ty_object.
    DATA ls_delta TYPE zif_abapgit_definitions=>ty_object.

    " Create base object
    ls_base-sha1 = 'base_sha1_234567890123456789012345678'.
    ls_base-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_base-data = zcl_abapgit_convert=>string_to_xstring_utf8( `Test` ).
    ls_base-index = 1.
    APPEND ls_base TO lt_objects.

    " Create delta with single-byte headers
    ls_delta-sha1 = 'base_sha1_234567890123456789012345678'.
    ls_delta-type = zif_abapgit_git_definitions=>c_type-ref_d.
    " Delta: base_size=4, result_size=4, copy 4 bytes from offset 0
    ls_delta-data = '0404900004'.
    ls_delta-index = 2.
    APPEND ls_delta TO lt_objects.

    " This should not raise an exception if header parsing works
    zcl_abapgit_git_delta=>decode_deltas(
      EXPORTING iv_show_progress = abap_false
      CHANGING ct_objects = lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_objects )
      exp = 2
      msg = 'Delta processing should complete successfully' ).
  ENDMETHOD.

  METHOD test_delta_header_multi_byte.
    " Test multi-byte header through decode_deltas
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_base TYPE zif_abapgit_definitions=>ty_object.
    DATA ls_delta TYPE zif_abapgit_definitions=>ty_object.
    DATA lv_large_text TYPE string.

    " Create base object (large enough to require multi-byte encoding)
    ls_base-sha1 = 'base_sha1_234567890123456789012345678'.
    ls_base-type = zif_abapgit_git_definitions=>c_type-blob.
    " Create a string that's exactly 128 bytes to test multi-byte header
    DO 128 TIMES.
      lv_large_text = lv_large_text && 'X'.
    ENDDO.
    ls_base-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_large_text ).
    ls_base-index = 1.
    APPEND ls_base TO lt_objects.

    " Create delta with multi-byte headers
    ls_delta-sha1 = 'base_sha1_234567890123456789012345678'.
    ls_delta-type = zif_abapgit_git_definitions=>c_type-ref_d.
    " Delta: base_size=128 (0x8001), result_size=128 (0x8001), copy all
    ls_delta-data = '80018001978000'.
    ls_delta-index = 2.
    APPEND ls_delta TO lt_objects.

    " This should not raise an exception if multi-byte header parsing works
    zcl_abapgit_git_delta=>decode_deltas(
      EXPORTING iv_show_progress = abap_false
      CHANGING ct_objects = lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_objects )
      exp = 2
      msg = 'Multi-byte delta processing should complete successfully' ).
  ENDMETHOD.

ENDCLASS.
