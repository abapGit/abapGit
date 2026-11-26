CLASS ltcl_progress_double DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_progress.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS ltcl_progress_double IMPLEMENTATION.

  METHOD zif_abapgit_progress~set_total.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_progress~show.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_progress~off.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_tree DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test01 FOR TESTING RAISING zcx_abapgit_exception,
      test02 FOR TESTING RAISING zcx_abapgit_exception,
      test03 FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_tree IMPLEMENTATION.

  METHOD test01.

    DATA: lt_nodes  TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    CLEAR ls_node.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.
    APPEND ls_node TO lt_nodes.

    lv_data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
    lt_result = zcl_abapgit_git_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.

  METHOD test02.

    DATA: lt_nodes  TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    CLEAR ls_node.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.
    APPEND ls_node TO lt_nodes.

    CLEAR ls_node.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name = 'something.md'.
    ls_node-sha1 = '1236cb3c4b7f0b3600b64f744cde614a283a88dc'.
    APPEND ls_node TO lt_nodes.

    lv_data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
    lt_result = zcl_abapgit_git_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.

  METHOD test03.

    DATA: lt_nodes  TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    CLEAR ls_node.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = '0000003c4b7f0b3600b64f744cde614a28000000'.
    APPEND ls_node TO lt_nodes.

    lv_data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
    lt_result = zcl_abapgit_git_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_type_and_length DEFINITION DEFERRED.
CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS ltcl_type_and_length.

CLASS ltcl_type_and_length DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_length   TYPE i
          iv_type     TYPE zif_abapgit_git_definitions=>ty_type DEFAULT zif_abapgit_git_definitions=>c_type-commit
          iv_expected TYPE xstring
        RAISING
          zcx_abapgit_exception,
      type_and_length_0 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_1 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_10 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_15 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_16 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_17 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_100 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_128 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_2047 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_2048 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_90000 FOR TESTING RAISING zcx_abapgit_exception,
      type_and_length_1000000 FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_type_and_length IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE xstring.

    lv_result = zcl_abapgit_git_pack=>type_and_length(
      iv_type   = iv_type
      iv_length = iv_length ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = iv_expected ).

  ENDMETHOD.

  METHOD type_and_length_100.

    test( iv_length   = 100
          iv_expected = '9406' ).

  ENDMETHOD.

  METHOD type_and_length_2047.

    test( iv_length   = 2047
          iv_expected = '9F7F' ).

  ENDMETHOD.

  METHOD type_and_length_2048.

    test( iv_length   = 2048
          iv_expected = '908001' ).

  ENDMETHOD.

  METHOD type_and_length_90000.

    test( iv_length   = 90000
          iv_type     = zif_abapgit_git_definitions=>c_type-blob
          iv_expected = 'B0F92B' ).

  ENDMETHOD.

  METHOD type_and_length_10.

    test( iv_length   = 10
          iv_expected = '1A' ).

  ENDMETHOD.

  METHOD type_and_length_1000000.

    test( iv_length   = 1000000
          iv_expected = '90A4E803' ).

  ENDMETHOD.

  METHOD type_and_length_0.

    test( iv_length   = 0
          iv_expected = '10' ).

  ENDMETHOD.

  METHOD type_and_length_128.

    test( iv_length   = 128
          iv_expected = '9008' ).

  ENDMETHOD.

  METHOD type_and_length_1.

    test( iv_length   = 1
          iv_expected = '11' ).

  ENDMETHOD.

  METHOD type_and_length_15.

    test( iv_length   = 15
          iv_expected = '1F' ).

  ENDMETHOD.

  METHOD type_and_length_16.

    test( iv_length   = 16
          iv_expected = '9001' ).

  ENDMETHOD.

  METHOD type_and_length_17.

    test( iv_length   = 17
          iv_expected = '9101' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_pack DEFINITION DEFERRED.
CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS ltcl_pack.

CLASS ltcl_pack DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    CONSTANTS: c_sha TYPE zif_abapgit_git_definitions=>ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    METHODS setup.

    METHODS:
      commit FOR TESTING
        RAISING zcx_abapgit_exception,
      commit_newline FOR TESTING
        RAISING zcx_abapgit_exception,
      signed_commit FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_short FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_long FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_multiple FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_foreign_zlib FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_wrong_length FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_false_hit FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_corrupt_checksum FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_corrupt_adjacent FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_corrupt_short FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_any_guess FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_multi_block FOR TESTING
        RAISING zcx_abapgit_exception,
      zlib_end_not_found FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_checksum_overlaps_eob FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_two_tails_same_data FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_foreign_zlib_two FOR TESTING
        RAISING zcx_abapgit_exception,
      next_object_fits_trailer FOR TESTING,
      next_object_fits_object FOR TESTING,
      next_object_fits_ref_delta FOR TESTING,
      sort_tree1 FOR TESTING,
      sort_tree2 FOR TESTING.

    METHODS:
      object_blob
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_object
        RAISING   zcx_abapgit_exception,
      stored_deflate
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rv_stream) TYPE xstring.

ENDCLASS.


CLASS ltcl_pack IMPLEMENTATION.

  METHOD setup.
    DATA lo_progress_double TYPE REF TO ltcl_progress_double.
    CREATE OBJECT lo_progress_double TYPE ltcl_progress_double.

    zcl_abapgit_progress=>set_instance( lo_progress_double ).
  ENDMETHOD.

  METHOD sort_tree1.

    DATA: lt_tree TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF lt_tree.


    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    <ls_tree>-name  = 'b.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    <ls_tree>-name  = 'a.txt'.
    <ls_tree>-sha1  = '0123'.

    lt_tree = zcl_abapgit_git_pack=>sort_tree( lt_tree ).

    READ TABLE lt_tree INDEX 1 ASSIGNING <ls_tree>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_tree>-name
      exp = 'a.txt' ).

  ENDMETHOD.

  METHOD sort_tree2.

    DATA: lt_tree TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF lt_tree.


    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    <ls_tree>-name  = 'foo.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_git_definitions=>c_chmod-dir.
    <ls_tree>-name  = 'foo'.
    <ls_tree>-sha1  = '0123'.

    lt_tree = zcl_abapgit_git_pack=>sort_tree( lt_tree ).

    READ TABLE lt_tree INDEX 1 ASSIGNING <ls_tree>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_tree>-name
      exp = 'foo.txt' ).

  ENDMETHOD.

  METHOD pack_multiple.

    CONSTANTS: lc_data TYPE x LENGTH 15 VALUE '123456789ABCDEF545794254754554',
               lc_sha  TYPE zif_abapgit_git_definitions=>ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_nodes   TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
          ls_node    LIKE LINE OF lt_nodes,
          ls_commit  TYPE zcl_abapgit_git_pack=>ty_commit,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring.


* blob
    lv_data = lc_data.
    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1_blob( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = lv_data.
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

* commit
    CLEAR ls_commit.
    ls_commit-tree      = lc_sha.
    ls_commit-parent    = lc_sha.
    ls_commit-author    = 'John Foobar'.
    ls_commit-committer = 'John Foobar'.
    ls_commit-body      = 'body'.
    lv_data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1_commit( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-commit.
    ls_object-data = lv_data.
    ls_object-index = 2.
    APPEND ls_object TO lt_objects.

* tree
    CLEAR ls_node.
    ls_node-chmod     = '12456'.
    ls_node-name      = 'foobar.abap'.
    ls_node-sha1      = lc_sha.
    APPEND ls_node TO lt_nodes.
    lv_data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1_tree( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-tree.
    ls_object-data = lv_data.
    ls_object-index = 3.
    APPEND ls_object TO lt_objects.


    CLEAR lv_data.
    lv_data = zcl_abapgit_git_pack=>encode( lt_objects ).
    lt_result = zcl_abapgit_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.

  METHOD object_blob.

    rs_object-sha1 = zcl_abapgit_hash=>sha1_blob( iv_data ).
    rs_object-type = zif_abapgit_git_definitions=>c_type-blob.
    rs_object-data = iv_data.
    rs_object-index = 1.

  ENDMETHOD.

  METHOD stored_deflate.

    " DEFLATE stream of uncompressed blocks (RFC 1951, BTYPE 00). Valid for every
    " inflater, but never what the kernel produces when compressing.
    DATA: lv_offset TYPE i,
          lv_chunk  TYPE i,
          lv_final  TYPE x LENGTH 1,
          lv_len    TYPE x LENGTH 2,
          lv_nlen   TYPE x LENGTH 2.

    DO.
      lv_chunk = xstrlen( iv_data ) - lv_offset.
      IF lv_chunk > 65535.
        lv_chunk = 65535.
        lv_final = '00'.
      ELSE.
        lv_final = '01'.
      ENDIF.
      lv_len  = lv_chunk.
      lv_nlen = 65535 - lv_chunk.
      CONCATENATE rv_stream lv_final lv_len+1(1) lv_len(1) lv_nlen+1(1) lv_nlen(1)
        iv_data+lv_offset(lv_chunk) INTO rv_stream IN BYTE MODE.
      lv_offset = lv_offset + lv_chunk.
      IF lv_final = '01'.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD pack_foreign_zlib.

    " Zlib output that differs from the kernel's: the stream end is found by the search
    CONSTANTS: lc_pack_header TYPE x LENGTH 12 VALUE '5041434B0000000200000001',
               lc_zlib        TYPE x LENGTH 2 VALUE '789C'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring,
          lv_pack    TYPE xstring,
          lv_header  TYPE xstring,
          lv_adler32 TYPE zif_abapgit_git_definitions=>ty_adler32,
          lv_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_stream  TYPE xstring,
          lv_zlib    TYPE xstring,
          lv_trailer TYPE xstring.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'Compressed by another zlib, abapGit' ).
    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    lv_header = zcl_abapgit_git_pack=>type_and_length(
      iv_type   = zif_abapgit_git_definitions=>c_type-blob
      iv_length = xstrlen( lv_data ) ).
    lv_adler32 = zcl_abapgit_hash=>adler32( lv_data ).
    lv_stream = stored_deflate( lv_data ).
    CONCATENATE lc_pack_header lv_header lc_zlib lv_stream lv_adler32
      INTO lv_pack IN BYTE MODE.
    lv_sha1 = to_upper( zcl_abapgit_hash=>sha1_raw( lv_pack ) ).
    lv_trailer = lv_sha1.
    CONCATENATE lv_pack lv_trailer INTO lv_pack IN BYTE MODE.

    CONCATENATE lv_stream lv_adler32 INTO lv_zlib IN BYTE MODE.
    cl_abap_unit_assert=>assert_equals(
      exp = xstrlen( lv_stream )
      act = zcl_abapgit_git_pack=>find_zlib_end(
              iv_data         = lv_zlib
              iv_decompressed = lv_data ) ).

    lt_result = zcl_abapgit_git_pack=>decode( lv_pack ).

    cl_abap_unit_assert=>assert_equals(
      exp = lt_objects
      act = lt_result ).

  ENDMETHOD.

  METHOD pack_foreign_zlib_two.

    " Two objects in foreign zlib: the end of the first one is checked against the next object
    CONSTANTS: lc_pack_header TYPE x LENGTH 12 VALUE '5041434B0000000200000002',
               lc_zlib        TYPE x LENGTH 2 VALUE '789C'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring,
          lv_pack    TYPE xstring,
          lv_header  TYPE xstring,
          lv_adler32 TYPE zif_abapgit_git_definitions=>ty_adler32,
          lv_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_stream  TYPE xstring,
          lv_trailer TYPE xstring.

    lv_pack = lc_pack_header.

    DO 2 TIMES.
      lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( |Object { sy-index } in another zlib, abapGit| ).
      ls_object = object_blob( lv_data ).
      ls_object-index = sy-index.
      APPEND ls_object TO lt_objects.

      lv_header = zcl_abapgit_git_pack=>type_and_length(
        iv_type   = zif_abapgit_git_definitions=>c_type-blob
        iv_length = xstrlen( lv_data ) ).
      lv_adler32 = zcl_abapgit_hash=>adler32( lv_data ).
      lv_stream = stored_deflate( lv_data ).
      CONCATENATE lv_pack lv_header lc_zlib lv_stream lv_adler32 INTO lv_pack IN BYTE MODE.
    ENDDO.

    lv_sha1 = to_upper( zcl_abapgit_hash=>sha1_raw( lv_pack ) ).
    lv_trailer = lv_sha1.
    CONCATENATE lv_pack lv_trailer INTO lv_pack IN BYTE MODE.

    lt_result = zcl_abapgit_git_pack=>decode( lv_pack ).

    cl_abap_unit_assert=>assert_equals(
      exp = lt_objects
      act = lt_result ).

  ENDMETHOD.

  METHOD next_object_fits_trailer.

    " Stream end at offset 0, then 4 Adler-32 bytes and the 20-byte pack SHA-1
    CONSTANTS lc_data TYPE x LENGTH 24 VALUE '010203041111111111111111111111111111111111111111'.

    DATA lv_data TYPE xstring.

    lv_data = lc_data.

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 4 ) ).
    " One byte off: 21 or 19 bytes left, and no object header there
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 3 ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 5 ) ).

  ENDMETHOD.

  METHOD next_object_fits_object.

    " Adler-32 bytes, then a blob header with a two-byte size (B5 01) and the zlib header
    CONSTANTS lc_data TYPE x LENGTH 10 VALUE '01020304B501789C0102'.

    DATA lv_data TYPE xstring.

    lv_data = lc_data.

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 4 ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 3 ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 5 ) ).

  ENDMETHOD.

  METHOD next_object_fits_ref_delta.

    " A ref delta header (7F) is followed by the 20-byte base SHA-1 before the zlib header
    CONSTANTS: lc_head TYPE x LENGTH 5 VALUE '010203047F',
               lc_base TYPE x LENGTH 20 VALUE '7801789C7801789C7801789C7801789C7801789C',
               lc_tail TYPE x LENGTH 4 VALUE '789C0102'.

    DATA lv_data TYPE xstring.

    CONCATENATE lc_head lc_base lc_tail INTO lv_data IN BYTE MODE.

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 4 ) ).

    " Without the base SHA-1 the zlib header is not where it must be
    CONCATENATE lc_head lc_tail INTO lv_data IN BYTE MODE.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_git_pack=>next_object_fits( iv_data   = lv_data
                                                    iv_offset = 4 ) ).

  ENDMETHOD.

  METHOD pack_wrong_length.

    " The object header announces one byte more than the stream contains
    CONSTANTS: lc_pack_header TYPE x LENGTH 12 VALUE '5041434B0000000200000001',
               lc_zlib        TYPE x LENGTH 2 VALUE '789C'.

    DATA: lv_data    TYPE xstring,
          lv_pack    TYPE xstring,
          lv_header  TYPE xstring,
          lv_stream  TYPE xstring,
          lv_adler32 TYPE zif_abapgit_git_definitions=>ty_adler32,
          lv_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_trailer TYPE xstring.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'Length does not match the header' ).

    lv_header = zcl_abapgit_git_pack=>type_and_length(
      iv_type   = zif_abapgit_git_definitions=>c_type-blob
      iv_length = xstrlen( lv_data ) + 1 ).
    lv_adler32 = zcl_abapgit_hash=>adler32( lv_data ).
    lv_stream = stored_deflate( lv_data ).
    CONCATENATE lc_pack_header lv_header lc_zlib lv_stream lv_adler32
      INTO lv_pack IN BYTE MODE.
    lv_sha1 = to_upper( zcl_abapgit_hash=>sha1_raw( lv_pack ) ).
    lv_trailer = lv_sha1.
    CONCATENATE lv_pack lv_trailer INTO lv_pack IN BYTE MODE.

    TRY.
        zcl_abapgit_git_pack=>decode( lv_pack ).
        cl_abap_unit_assert=>fail( 'Length mismatch not detected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD zlib_end_corrupt_checksum.

    " The checksum at the real end is corrupt, the right one follows later.
    " The end is found from the stream alone, not where the right checksum is.
    CONSTANTS: lc_corrupt TYPE x LENGTH 4 VALUE '00000000',
               lc_filler  TYPE x LENGTH 12 VALUE 'FFFFFFFFFFFFFFFFFFFFFFFF'.

    DATA: lv_data    TYPE xstring,
          lv_stream  TYPE xstring,
          lv_input   TYPE xstring,
          lv_adler32 TYPE zif_abapgit_git_definitions=>ty_adler32.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'Corrupt checksum at the end of the stream' ).
    lv_adler32 = zcl_abapgit_hash=>adler32( lv_data ).
    lv_stream = stored_deflate( lv_data ).

    CONCATENATE lv_stream lc_corrupt lc_filler lv_adler32
      INTO lv_input IN BYTE MODE.

    cl_abap_unit_assert=>assert_equals(
      exp = xstrlen( lv_stream )
      act = zcl_abapgit_git_pack=>find_zlib_end(
              iv_data         = lv_input
              iv_decompressed = lv_data ) ).

  ENDMETHOD.

  METHOD zlib_end_corrupt_adjacent.

    " The right checksum follows directly after the corrupt one at the real end
    CONSTANTS lc_corrupt TYPE x LENGTH 4 VALUE '00000000'.

    DATA: lv_data    TYPE xstring,
          lv_stream  TYPE xstring,
          lv_input   TYPE xstring,
          lv_adler32 TYPE zif_abapgit_git_definitions=>ty_adler32.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'Corrupt checksum, right one directly behind' ).
    lv_adler32 = zcl_abapgit_hash=>adler32( lv_data ).
    lv_stream = stored_deflate( lv_data ).

    CONCATENATE lv_stream lc_corrupt lv_adler32 INTO lv_input IN BYTE MODE.

    cl_abap_unit_assert=>assert_equals(
      exp = xstrlen( lv_stream )
      act = zcl_abapgit_git_pack=>find_zlib_end(
              iv_data         = lv_input
              iv_decompressed = lv_data ) ).

  ENDMETHOD.

  METHOD zlib_end_corrupt_short.

    " Empty blob: fixed-Huffman final block with only the end-of-block code, two bytes.
    " Its checksum is 00000001; a corrupt one is followed by the right one.
    CONSTANTS lc_input TYPE x LENGTH 10 VALUE '0300FFFFFFFF00000001'.

    DATA: lv_input TYPE xstring,
          lv_empty TYPE xstring.

    lv_input = lc_input.

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = zcl_abapgit_git_pack=>find_zlib_end(
              iv_data         = lv_input
              iv_decompressed = lv_empty ) ).

  ENDMETHOD.

  METHOD pack_checksum_overlaps_eob.

    " 600-byte blob compressed by zlib level 1. Its Adler-32 is 00000000, and the stream
    " ends with the byte 00 (end-of-block code and padding only). Without that last byte the
    " kernel already returns all 600 bytes, so the end must not be found one byte early.
    CONSTANTS: lc_pack1 TYPE x LENGTH 46
                 VALUE '5041434B0000000200000001B82578010B63A00310543276094D2BE790D4B2F689CDAB17503472A683AD082B1811',
               lc_pack2 TYPE x LENGTH 34
                 VALUE '4C3AB2FCFE8F5CB0020000000000BA069217AC353F145F416109445EAF542A863D6A'.

    DATA: lv_pack   TYPE xstring,
          lt_result TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_result LIKE LINE OF lt_result.

    CONCATENATE lc_pack1 lc_pack2 INTO lv_pack IN BYTE MODE.
    lt_result = zcl_abapgit_git_pack=>decode( lv_pack ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lt_result ) ).
    READ TABLE lt_result INTO ls_result INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      exp = 600
      act = xstrlen( ls_result-data ) ).

  ENDMETHOD.

  METHOD pack_two_tails_same_data.

    " Crafted dynamic-Huffman stream: literal a has the code 0, length 4 the code 111 and
    " distance 2 the code 1. The data ends with 64 literals a (eight 00 bytes) before the
    " end-of-block code. Cut in front of those bytes, the zero tail gives 64 literals a and
    " the one tail 16 copies of length 4: both give all 131 bytes. The end is at 48.
    CONSTANTS: lc_pack1 TYPE x LENGTH 44
                 VALUE '5041434B0000000200000001B308789C15C10109000000C3A0ACBE7F8871DDB66DDBB66DDBB66DDBB66DDBB6',
               lc_pack2 TYPE x LENGTH 44
                 VALUE '6DDBB66DDBB66DDBB66D1B000000000000000001E66031E53BED7DF7E955067AB0355CBBF10E5EE7644361A7'.

    DATA: lv_pack   TYPE xstring,
          lt_result TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_result LIKE LINE OF lt_result.

    CONCATENATE lc_pack1 lc_pack2 INTO lv_pack IN BYTE MODE.
    lt_result = zcl_abapgit_git_pack=>decode( lv_pack ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lt_result ) ).
    READ TABLE lt_result INTO ls_result INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      exp = '18608636db760a3ee9a39ecb2138558f7879110c'
      act = ls_result-sha1 ).

  ENDMETHOD.

  METHOD zlib_end_any_guess.

    " Wherever the search starts, before or behind the end, it finds the end exactly.
    " Once for a stored block, once for the kernel's compressed output.
    CONSTANTS lc_next TYPE x LENGTH 8 VALUE '789C0123456789AB'.

    DATA: lv_text   TYPE string,
          lv_data   TYPE xstring,
          lv_stream TYPE xstring,
          lv_length TYPE i,
          lv_input  TYPE xstring,
          lv_guess  TYPE i,
          lv_round  TYPE i.

    DO 40 TIMES.
      lv_text = |{ lv_text }line { sy-index } of a text that compresses well\n|.
    ENDDO.
    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_text ).

    DO 2 TIMES.
      lv_round = sy-index.
      IF lv_round = 1.
        lv_stream = stored_deflate( lv_data ).
      ELSE.
        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in       = lv_data
          IMPORTING
            gzip_out     = lv_stream
            gzip_out_len = lv_length ).
        lv_stream = lv_stream(lv_length).
      ENDIF.
      CONCATENATE lv_stream lc_next lc_next INTO lv_input IN BYTE MODE.

      lv_guess = 0.
      WHILE lv_guess <= xstrlen( lv_input ) + 1.
        cl_abap_unit_assert=>assert_equals(
          exp = xstrlen( lv_stream )
          act = zcl_abapgit_git_pack=>find_zlib_end(
                  iv_data         = lv_input
                  iv_decompressed = lv_data
                  iv_guess        = lv_guess )
          msg = |round { lv_round }, guess { lv_guess }| ).
        lv_guess = lv_guess + 11.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.

  METHOD zlib_end_multi_block.

    " Streams of several blocks, as zlib writes them when it is flushed:
    " 1. a sync flush (empty stored block) between two blocks with data
    " 2. a sync flush behind the data, then an empty final block. All of the data is
    "    decompressed several bytes before the stream ends.
    CONSTANTS: lc_between TYPE x LENGTH 28
                 VALUE '4ACB2C2A2E5148CAC94FCED65100000000FFFF2B4E4DCECF4B81F000',
               lc_behind  TYPE x LENGTH 29
                 VALUE '4ACB2C2A2E5148CAC94FCED651284E4DCECF4B81F000000000FFFF0300',
               lc_next    TYPE x LENGTH 8 VALUE '789C0123456789AB'.

    DATA: lv_data   TYPE xstring,
          lv_stream TYPE xstring,
          lv_input  TYPE xstring,
          lv_guess  TYPE i,
          lv_round  TYPE i.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'first block, second block' ).

    DO 2 TIMES.
      lv_round = sy-index.
      IF lv_round = 1.
        lv_stream = lc_between.
      ELSE.
        lv_stream = lc_behind.
      ENDIF.
      CONCATENATE lv_stream lc_next INTO lv_input IN BYTE MODE.

      lv_guess = 0.
      WHILE lv_guess <= xstrlen( lv_input ) + 1.
        cl_abap_unit_assert=>assert_equals(
          exp = xstrlen( lv_stream )
          act = zcl_abapgit_git_pack=>find_zlib_end(
                  iv_data         = lv_input
                  iv_decompressed = lv_data
                  iv_guess        = lv_guess )
          msg = |round { lv_round }, guess { lv_guess }| ).
        lv_guess = lv_guess + 1.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.

  METHOD zlib_end_not_found.

    " The data ends before the stream does: a stored block without its last byte.
    " No end is reported, wherever the search starts, so that decode takes its fallback.
    DATA: lv_data   TYPE xstring,
          lv_stream TYPE xstring,
          lv_input  TYPE xstring,
          lv_length TYPE i,
          lv_guess  TYPE i.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'The data ends before the stream does' ).
    lv_stream = stored_deflate( lv_data ).
    lv_length = xstrlen( lv_stream ) - 1.
    lv_input = lv_stream(lv_length).

    lv_guess = 0.
    WHILE lv_guess <= lv_length + 1.
      cl_abap_unit_assert=>assert_equals(
        exp = -1
        act = zcl_abapgit_git_pack=>find_zlib_end(
                iv_data         = lv_input
                iv_decompressed = lv_data
                iv_guess        = lv_guess )
        msg = |guess { lv_guess }| ).
      lv_guess = lv_guess + 1.
    ENDWHILE.

    " One byte, or none, cannot hold a stream
    lv_input = lv_stream(1).
    cl_abap_unit_assert=>assert_equals(
      exp = -1
      act = zcl_abapgit_git_pack=>find_zlib_end(
              iv_data         = lv_input
              iv_decompressed = lv_data ) ).

    CLEAR lv_input.
    cl_abap_unit_assert=>assert_equals(
      exp = -1
      act = zcl_abapgit_git_pack=>find_zlib_end(
              iv_data         = lv_input
              iv_decompressed = lv_data ) ).

  ENDMETHOD.

  METHOD zlib_end_false_hit.

    " 31744 zero bytes followed by 7F7F0100 have the Adler-32 checksum 7F7F0100,
    " so the checksum also occurs inside the stream, before its end.
    " The search does not look for the checksum; this data must not mislead it either.
    CONSTANTS: lc_tail TYPE x LENGTH 4 VALUE '7F7F0100',
               lc_next TYPE x LENGTH 2 VALUE 'FFFF'.

    DATA: lv_zeros   TYPE xstring,
          lv_data    TYPE xstring,
          lv_stream  TYPE xstring,
          lv_input   TYPE xstring,
          lv_adler32 TYPE zif_abapgit_git_definitions=>ty_adler32,
          lv_offset  TYPE i,
          lv_end     TYPE i.

    lv_zeros = '00'.
    DO 15 TIMES. " 2^15 = 32768 bytes
      CONCATENATE lv_zeros lv_zeros INTO lv_zeros IN BYTE MODE.
    ENDDO.
    lv_zeros = lv_zeros(31744).
    CONCATENATE lv_zeros lc_tail INTO lv_data IN BYTE MODE.

    lv_adler32 = zcl_abapgit_hash=>adler32( lv_data ).
    cl_abap_unit_assert=>assert_equals(
      exp = lc_tail
      act = lv_adler32 ).

    lv_stream = stored_deflate( lv_data ).
    CONCATENATE lv_stream lv_adler32 lc_next INTO lv_input IN BYTE MODE.

    FIND FIRST OCCURRENCE OF lv_adler32 IN lv_input IN BYTE MODE MATCH OFFSET lv_offset.
    cl_abap_unit_assert=>assert_equals(
      exp = xstrlen( lv_stream ) - 4
      act = lv_offset ).

    lv_end = zcl_abapgit_git_pack=>find_zlib_end(
      iv_data         = lv_input
      iv_decompressed = lv_data ).

    cl_abap_unit_assert=>assert_equals(
      exp = xstrlen( lv_stream )
      act = lv_end ).

  ENDMETHOD.

  METHOD pack_short.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring.


    lv_data = lc_data.

    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = zcl_abapgit_git_pack=>encode( lt_objects ).
    lt_result = zcl_abapgit_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.

  METHOD pack_long.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lv_xstring TYPE xstring,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring.


    lv_xstring = lc_data.
    DO 20 TIMES.
      CONCATENATE lv_xstring lv_data INTO lv_data IN BYTE MODE.
    ENDDO.

    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = zcl_abapgit_git_pack=>encode( lt_objects ).
    lt_result = zcl_abapgit_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.

  METHOD commit.

    DATA: ls_commit TYPE zcl_abapgit_git_pack=>ty_commit,
          ls_result TYPE zcl_abapgit_git_pack=>ty_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = c_sha.
    ls_commit-parent    = c_sha.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.

    lv_data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    ls_result = zcl_abapgit_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.

  METHOD commit_newline.

    DATA: ls_commit TYPE zcl_abapgit_git_pack=>ty_commit,
          ls_result TYPE zcl_abapgit_git_pack=>ty_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = c_sha.
    ls_commit-parent    = c_sha.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'
                        && cl_abap_char_utilities=>newline
                        && cl_abap_char_utilities=>newline.

    lv_data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    ls_result = zcl_abapgit_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.

  METHOD signed_commit.

    DATA: ls_commit TYPE zcl_abapgit_git_pack=>ty_commit,
          ls_result TYPE zcl_abapgit_git_pack=>ty_commit,
          lv_data   TYPE xstring.

    ls_commit-tree      = c_sha.
    ls_commit-parent    = c_sha.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.
    ls_commit-gpgsig    = '-----END PGP SIGNATURE-----'
                          && |{ cl_abap_char_utilities=>newline } { cl_abap_char_utilities=>newline }|.

    lv_data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    ls_result = zcl_abapgit_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_git_pack_decode_commit DEFINITION FOR TESTING
    RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    METHODS:
      decode1 FOR TESTING
        RAISING zcx_abapgit_exception,
      decode2 FOR TESTING
        RAISING zcx_abapgit_exception,
      decode3 FOR TESTING
        RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: ms_raw TYPE zcl_abapgit_git_pack=>ty_commit,
          mv_str TYPE string.

    METHODS:
      setup,
      decode
        RAISING zcx_abapgit_exception,
      add
        IMPORTING iv_string TYPE string.

ENDCLASS.

CLASS ltcl_git_pack_decode_commit IMPLEMENTATION.

  METHOD setup.
    CLEAR ms_raw.
    CLEAR mv_str.
  ENDMETHOD.

  METHOD add.

    CONCATENATE mv_str iv_string cl_abap_char_utilities=>newline INTO mv_str.

  ENDMETHOD.

  METHOD decode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( mv_str ).

    ms_raw = zcl_abapgit_git_pack=>decode_commit( lv_xstr ).

  ENDMETHOD.

  METHOD decode1.

    add( 'tree tree' ).
    add( 'parent parent1' ).
    add( 'parent parent2' ).
    add( 'author author' ).
    add( 'committer committer' ).
    add( '' ).
    add( 'comment' ).

    decode( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-tree
      exp = 'tree' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-author
      exp = 'author' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-committer
      exp = 'committer' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = 'parent1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent2
      exp = 'parent2' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ms_raw-body
      exp = 'comment+' ).

  ENDMETHOD.

  METHOD decode2.

    add( 'tree tree' ).
    add( 'author author' ).
    add( 'committer committer' ).
    add( '' ).
    add( 'comment' ).

    decode( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-tree
      exp = 'tree' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-author
      exp = 'author' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-committer
      exp = 'committer' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = '' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ms_raw-body
      exp = 'comment+' ).

  ENDMETHOD.

  METHOD decode3.

    add( 'tree tree' ).
    add( 'parent parent1' ).
    add( 'author author' ).
    add( 'committer committer' ).
    add( '' ).
    add( 'comment' ).

    decode( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-tree
      exp = 'tree' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-author
      exp = 'author' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-committer
      exp = 'committer' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = 'parent1' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ms_raw-body
      exp = 'comment+' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_tag DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      decode_tag FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_tag IMPLEMENTATION.

  METHOD decode_tag.

    DATA: lv_test_data TYPE string,
          lv_xstring   TYPE xstring,
          ls_tag       TYPE zcl_abapgit_git_pack=>ty_tag.

    lv_test_data = |object 175f9a21b15a9012c97a3dd15aea6d74d4204b6b\n|
                && |type commit\n|
                && |tag tag_1\n|
                && |tagger Christian Guenter <christianguenter@googlemail.com> 1526718052 +0000\n|
                && |\n|
                && |This is an annotated tag\n|.

    lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( lv_test_data ).

    ls_tag = zcl_abapgit_git_pack=>decode_tag( lv_xstring ).

    cl_abap_unit_assert=>assert_equals(
      exp = |175f9a21b15a9012c97a3dd15aea6d74d4204b6b|
      act = ls_tag-object ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'commit'
      act = ls_tag-type ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'tag_1'
      act = ls_tag-tag ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'Christian Guenter'
      act = ls_tag-tagger_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'christianguenter@googlemail.com'
      act = ls_tag-tagger_email ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'This is an annotated tag'
      act = ls_tag-message ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_get_length DEFINITION DEFERRED.
CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS ltcl_get_length.

CLASS ltcl_get_length DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_data     TYPE xstring
          iv_expected TYPE i,
      test_data_advances
        IMPORTING
          iv_data              TYPE xstring
          iv_expected_length   TYPE i
          iv_expected_data_len TYPE i,
      length_0 FOR TESTING RAISING zcx_abapgit_exception,
      length_1 FOR TESTING RAISING zcx_abapgit_exception,
      length_10 FOR TESTING RAISING zcx_abapgit_exception,
      length_15 FOR TESTING RAISING zcx_abapgit_exception,
      length_16 FOR TESTING RAISING zcx_abapgit_exception,
      length_17 FOR TESTING RAISING zcx_abapgit_exception,
      length_31 FOR TESTING RAISING zcx_abapgit_exception,
      length_100 FOR TESTING RAISING zcx_abapgit_exception,
      length_127 FOR TESTING RAISING zcx_abapgit_exception,
      length_128 FOR TESTING RAISING zcx_abapgit_exception,
      length_255 FOR TESTING RAISING zcx_abapgit_exception,
      length_2047 FOR TESTING RAISING zcx_abapgit_exception,
      length_2048 FOR TESTING RAISING zcx_abapgit_exception,
      length_22783 FOR TESTING RAISING zcx_abapgit_exception,
      length_90000 FOR TESTING RAISING zcx_abapgit_exception,
      length_1000000 FOR TESTING RAISING zcx_abapgit_exception,
      length_200000000 FOR TESTING RAISING zcx_abapgit_exception,
      data_advances_1_byte FOR TESTING RAISING zcx_abapgit_exception,
      data_advances_2_bytes FOR TESTING RAISING zcx_abapgit_exception,
      data_advances_3_bytes FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_get_length IMPLEMENTATION.

  METHOD test.

    DATA lv_length TYPE i.
    DATA lv_data TYPE xstring.

    lv_data = iv_data.

    zcl_abapgit_git_pack=>get_length(
      IMPORTING
        ev_length = lv_length
      CHANGING
        cv_data   = lv_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = iv_expected ).

  ENDMETHOD.

  METHOD test_data_advances.

    DATA lv_length TYPE i.
    DATA lv_data TYPE xstring.

    lv_data = iv_data.

    zcl_abapgit_git_pack=>get_length(
      IMPORTING
        ev_length = lv_length
      CHANGING
        cv_data   = lv_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = iv_expected_length
      msg = |Length mismatch| ).

    cl_abap_unit_assert=>assert_equals(
      act = xstrlen( lv_data )
      exp = iv_expected_data_len
      msg = |Data pointer did not advance correctly| ).

  ENDMETHOD.

  METHOD length_0.

    test(
      iv_data     = '00'
      iv_expected = 0 ).

  ENDMETHOD.

  METHOD length_1.

    test(
      iv_data     = '01'
      iv_expected = 1 ).

  ENDMETHOD.

  METHOD length_10.

    test(
      iv_data     = '0A'
      iv_expected = 10 ).

  ENDMETHOD.

  METHOD length_15.
* four least significant bits set
    test(
      iv_data     = '0F'
      iv_expected = 15 ).

  ENDMETHOD.

  METHOD length_16.
* first value requiring two bytes (MSB set + continuation)
    test(
      iv_data     = '8001'
      iv_expected = 16 ).

  ENDMETHOD.

  METHOD length_17.

    test(
      iv_data     = '8101'
      iv_expected = 17 ).

  ENDMETHOD.

  METHOD length_31.

    test(
      iv_data     = '8F01'
      iv_expected = 31 ).

  ENDMETHOD.

  METHOD length_100.
* 100 = 4 (from first byte bits 0-3) + 6*16 (from second byte bits 0-6)
* first byte: 1000 0100 = 84, second byte: 0000 0110 = 06
    test(
      iv_data     = '8406'
      iv_expected = 100 ).

  ENDMETHOD.

  METHOD length_127.
* 127 = 15 + 7*16 = 15 + 112
* first byte: 1000 1111 = 8F, second byte: 0000 0111 = 07
    test(
      iv_data     = '8F07'
      iv_expected = 127 ).

  ENDMETHOD.

  METHOD length_128.
* 128 = 0 + 8*16
* first byte: 1000 0000 = 80, second byte: 0000 1000 = 08
    test(
      iv_data     = '8008'
      iv_expected = 128 ).

  ENDMETHOD.

  METHOD length_255.
* 255 = 15 + 15*16
* first byte: 1000 1111 = 8F, second byte: 0000 1111 = 0F
    test(
      iv_data     = '8F0F'
      iv_expected = 255 ).

  ENDMETHOD.

  METHOD length_2047.
* 2047 = 15 + 127*16
* first byte: 1000 1111 = 8F, second byte: 0111 1111 = 7F
    test(
      iv_data     = '8F7F'
      iv_expected = 2047 ).

  ENDMETHOD.

  METHOD length_2048.
* 2048 requires three bytes
* first byte: 1000 0000 = 80, second byte: 1000 0000 = 80, third byte: 0000 0001 = 01
    test(
      iv_data     = '808001'
      iv_expected = 2048 ).

  ENDMETHOD.

  METHOD length_22783.

    test(
      iv_data     = '8F8F0B'
      iv_expected = 22783 ).

  ENDMETHOD.

  METHOD length_90000.
* Large value test - 90000 requires three bytes
* Encoded as: B0F92B (from type_and_length tests, but stripping the type bits)
* For get_length, the type bits are in the same byte, so we need the raw encoding
* 90000 = 0 (bits 0-3) + ... complex calculation
* Using reverse of type_and_length: 90000 in binary
* 90000 = 0x15F90 = 0001 0101 1111 1001 0000
* Encoding: first 4 bits = 0, then 7 bits each
* first byte: MSB=1, bits 0-3 of length = 0 -> 80
* second byte: MSB=1, bits 4-10 -> F9 (with MSB) -> F9
* third byte: MSB=0, bits 11-17 -> 2B
    test(
      iv_data     = '80F92B'
      iv_expected = 90000 ).

  ENDMETHOD.

  METHOD length_1000000.
* Very large value test - 1000000 requires four bytes
* 1000000 = 0xF4240
* Encoding similar to above
    test(
      iv_data     = '80A4E803'
      iv_expected = 1000000 ).

  ENDMETHOD.

  METHOD data_advances_1_byte.
* Test that cv_data advances by 1 byte for single-byte length
    test_data_advances(
      iv_data              = '0FDEADBEEF'
      iv_expected_length   = 15
      iv_expected_data_len = 4 ).

  ENDMETHOD.

  METHOD data_advances_2_bytes.
* Test that cv_data advances by 2 bytes for two-byte length
    test_data_advances(
      iv_data              = '8F01DEADBEEF'
      iv_expected_length   = 31
      iv_expected_data_len = 4 ).

  ENDMETHOD.

  METHOD data_advances_3_bytes.
* Test that cv_data advances by 3 bytes for three-byte length
    test_data_advances(
      iv_data              = '8F8F0BDEADBEEF'
      iv_expected_length   = 22783
      iv_expected_data_len = 4 ).

  ENDMETHOD.

  METHOD length_200000000.
* Large value requiring 5 bytes - tests arithmetic with large factor values
* 200000000 = 0 + 32*16 + 120*2048 + 122*262144 + 5*33554432
* Encoding: 80 A0 F8 FA 05
    test(
      iv_data     = '80A0F8FA05'
      iv_expected = 200000000 ).

  ENDMETHOD.

ENDCLASS.
