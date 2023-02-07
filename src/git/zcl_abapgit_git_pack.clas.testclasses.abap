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
    ls_node-chmod = zif_abapgit_definitions=>c_chmod-file.
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
    ls_node-chmod = zif_abapgit_definitions=>c_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.
    APPEND ls_node TO lt_nodes.

    CLEAR ls_node.
    ls_node-chmod = zif_abapgit_definitions=>c_chmod-file.
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
    ls_node-chmod = zif_abapgit_definitions=>c_chmod-file.
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
          iv_type     TYPE zif_abapgit_git_definitions=>ty_type DEFAULT zif_abapgit_definitions=>c_type-commit
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
          iv_type     = zif_abapgit_definitions=>c_type-blob
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
      sort_tree1 FOR TESTING,
      sort_tree2 FOR TESTING.

    METHODS:
      object_blob
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_object
        RAISING   zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_pack IMPLEMENTATION.

  METHOD sort_tree1.

    DATA: lt_tree TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF lt_tree.


    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>c_chmod-file.
    <ls_tree>-name  = 'b.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>c_chmod-file.
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
    <ls_tree>-chmod = zif_abapgit_definitions=>c_chmod-file.
    <ls_tree>-name  = 'foo.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>c_chmod-dir.
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
    ls_object-type = zif_abapgit_definitions=>c_type-blob.
    ls_object-data = lv_data.
    ls_object-index = 1.
    ls_object-adler32 = zcl_abapgit_hash=>adler32( lv_data ).
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
    ls_object-type = zif_abapgit_definitions=>c_type-commit.
    ls_object-data = lv_data.
    ls_object-index = 2.
    ls_object-adler32 = zcl_abapgit_hash=>adler32( lv_data ).
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
    ls_object-type = zif_abapgit_definitions=>c_type-tree.
    ls_object-data = lv_data.
    ls_object-index = 3.
    ls_object-adler32 = zcl_abapgit_hash=>adler32( lv_data ).
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
    rs_object-type = zif_abapgit_definitions=>c_type-blob.
    rs_object-data = iv_data.
    rs_object-index = 1.
    rs_object-adler32 = zcl_abapgit_hash=>adler32( iv_data ).

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
      length_0 FOR TESTING RAISING zcx_abapgit_exception,
      length_1 FOR TESTING RAISING zcx_abapgit_exception,
      length_15 FOR TESTING RAISING zcx_abapgit_exception,
      length_31 FOR TESTING RAISING zcx_abapgit_exception,
      length_22783 FOR TESTING RAISING zcx_abapgit_exception.

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

  METHOD length_15.
* four least significant bits set
    test(
      iv_data     = '0F'
      iv_expected = 15 ).

  ENDMETHOD.

  METHOD length_31.

    test(
      iv_data     = '8F01'
      iv_expected = 31 ).

  ENDMETHOD.

  METHOD length_22783.

    test(
      iv_data     = '8F8F0B'
      iv_expected = 22783 ).

  ENDMETHOD.

ENDCLASS.
