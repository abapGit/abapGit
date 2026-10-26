
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      adler32 FOR TESTING,
      sha1 FOR TESTING RAISING zcx_abapgit_exception,
      sha1_raw_valid FOR TESTING RAISING zcx_abapgit_exception,
      sha1_raw_empty FOR TESTING RAISING zcx_abapgit_exception,
      sha256_raw_valid FOR TESTING RAISING zcx_abapgit_exception,
      sha256_raw_empty FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD adler32.

    DATA: lv_adler TYPE zif_abapgit_git_definitions=>ty_adler32.

    lv_adler = zcl_abapgit_hash=>adler32( '1122334455667788' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_adler
      exp = '08000265' ).

  ENDMETHOD.

  METHOD sha1.

    DATA: lv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    lv_sha1 = zcl_abapgit_hash=>sha1(
      iv_type = zif_abapgit_git_definitions=>c_type-commit
      iv_data = '112211221122' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = 'af2261a340c5188baf86a64a581d22012303023c' ).

  ENDMETHOD.


  METHOD sha1_raw_valid.

    DATA: lv_sha1  TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_input TYPE xstring.

    lv_input = 'C5188BAF86A64A581D2201'.
    lv_sha1 = zcl_abapgit_hash=>sha1_raw( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = '0ec2eba75071f87988ced3237cae5ec7c5efd795' ).

  ENDMETHOD.

  METHOD sha1_raw_empty.

    DATA: lv_sha1  TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_input TYPE xstring.

* a value like 'LOREM_IPSUM' will get an empty xstring,
    lv_input = ''.
    lv_sha1 = zcl_abapgit_hash=>sha1_raw( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = 'da39a3ee5e6b4b0d3255bfef95601890afd80709' ).

  ENDMETHOD.


  METHOD sha256_raw_valid.

    DATA lv_hash TYPE string.

    lv_hash = zcl_abapgit_hash=>sha256_raw( '616263' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_hash
      exp = 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad' ).

  ENDMETHOD.


  METHOD sha256_raw_empty.

    DATA: lv_hash  TYPE string,
          lv_input TYPE xstring.

    lv_input = ''.
    lv_hash = zcl_abapgit_hash=>sha256_raw( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_hash
      exp = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855' ).

  ENDMETHOD.

ENDCLASS.
