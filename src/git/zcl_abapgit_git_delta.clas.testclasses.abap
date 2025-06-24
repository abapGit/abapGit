CLASS ltcl_git_delta_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      test_decode_deltas_empty FOR TESTING RAISING cx_static_check,
      test_decode_deltas_no_delta FOR TESTING RAISING cx_static_check,
      test_decode_deltas_success FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_git_delta_test IMPLEMENTATION.

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
    DATA lt_objects        TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_original_count TYPE i.
    DATA ls_object         TYPE zif_abapgit_definitions=>ty_object.

    " Create a base blob object
    ls_object-sha1 = '1234567890123456789012345678901234567890'.
    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = zcl_abapgit_convert=>string_to_xstring_utf8( `Hello World` ).
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

    " Create a delta object that references the base blob
    CLEAR ls_object.
    ls_object-sha1 = '1234567890123456789012345678901234567890'. " same as base for reference
    ls_object-type = zif_abapgit_git_definitions=>c_type-ref_d.
    ls_object-data = '0B0D900B'.
    ls_object-index = 2.
    APPEND ls_object TO lt_objects.

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
    LOOP AT lt_objects INTO ls_object.
      IF ls_object-type = zif_abapgit_git_definitions=>c_type-ref_d.
        cl_abap_unit_assert=>fail( 'No delta objects should remain after processing' ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
