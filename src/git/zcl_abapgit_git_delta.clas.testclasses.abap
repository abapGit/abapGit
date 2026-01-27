CLASS ltcl_git_delta_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      test_decode_deltas_empty FOR TESTING RAISING cx_static_check,
      test_decode_deltas_no_delta FOR TESTING RAISING cx_static_check,
      test_decode_deltas_success FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_stream_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      test_offset_only_1byte FOR TESTING RAISING cx_static_check,
      test_offset_only_2bytes FOR TESTING RAISING cx_static_check,
      test_offset_only_4bytes FOR TESTING RAISING cx_static_check,
      test_length_only_1byte FOR TESTING RAISING cx_static_check,
      test_length_only_2bytes FOR TESTING RAISING cx_static_check,
      test_length_only_3bytes FOR TESTING RAISING cx_static_check,
      test_length_zero_default FOR TESTING RAISING cx_static_check,
      test_offset_and_length FOR TESTING RAISING cx_static_check,
      test_all_bits_set FOR TESTING RAISING cx_static_check,
      test_no_bits_set FOR TESTING RAISING cx_static_check.

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

CLASS ltcl_stream_test IMPLEMENTATION.

  METHOD test_offset_only_1byte.
    " Instruction 01 = offset byte 1 present
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = 'FF'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '01'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 255
      msg = 'Offset should be 255' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 65536
      msg = 'Length should default to 65536' ).
  ENDMETHOD.

  METHOD test_offset_only_2bytes.
    " Instruction 03 = offset bytes 1 and 2 present
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '0102'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '03'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    " offset = 1 + 2*256 = 513
    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 513
      msg = 'Offset should be 513' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 65536
      msg = 'Length should default to 65536' ).
  ENDMETHOD.

  METHOD test_offset_only_4bytes.
    " Instruction 0F = all 4 offset bytes present
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '01020304'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '0F'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    " offset = 1 + 2*256 + 3*65536 + 4*16777216 = 67305985
    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 67305985
      msg = 'Offset should be 67305985' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 65536
      msg = 'Length should default to 65536' ).
  ENDMETHOD.

  METHOD test_length_only_1byte.
    " Instruction 10 = length byte 1 present
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '0A'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '10'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 0
      msg = 'Offset should be 0' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 10
      msg = 'Length should be 10' ).
  ENDMETHOD.

  METHOD test_length_only_2bytes.
    " Instruction 30 = length bytes 1 and 2 present
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '0102'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '30'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    " length = 1 + 2*256 = 513
    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 0
      msg = 'Offset should be 0' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 513
      msg = 'Length should be 513' ).
  ENDMETHOD.

  METHOD test_length_only_3bytes.
    " Instruction 70 = all 3 length bytes present
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '010203'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '70'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    " length = 1 + 2*256 + 3*65536 = 197121
    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 0
      msg = 'Offset should be 0' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 197121
      msg = 'Length should be 197121' ).
  ENDMETHOD.

  METHOD test_length_zero_default.
    " Instruction 00 = no bytes present, length defaults to 65536
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '00'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '00'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 0
      msg = 'Offset should be 0' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 65536
      msg = 'Length should default to 65536' ).
  ENDMETHOD.

  METHOD test_offset_and_length.
    " Instruction 11 = offset byte 1 + length byte 1
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '0A14'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '11'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 10
      msg = 'Offset should be 10' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 20
      msg = 'Length should be 20' ).
  ENDMETHOD.

  METHOD test_all_bits_set.
    " Instruction 7F = all 7 bits set (4 offset bytes + 3 length bytes)
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '01020304050607'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '7F'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    " offset = 1 + 2*256 + 3*65536 + 4*16777216 = 67305985
    " length = 5 + 6*256 + 7*65536 = 460293
    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 67305985
      msg = 'Offset should be 67305985' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 460293
      msg = 'Length should be 460293' ).
  ENDMETHOD.

  METHOD test_no_bits_set.
    " Instruction 80 = copy instruction with no offset/length bits set
    DATA lo_stream TYPE REF TO lcl_stream.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    CREATE OBJECT lo_stream EXPORTING iv_data = '00'.

    lo_stream->eat_offset_and_length(
      EXPORTING iv_instruction = '80'
      IMPORTING ev_offset = lv_offset
                ev_length = lv_length ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_offset
      exp = 0
      msg = 'Offset should be 0' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 65536
      msg = 'Length should default to 65536' ).
  ENDMETHOD.

ENDCLASS.
