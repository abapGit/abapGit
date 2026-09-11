CLASS ltcl_obj_name_length DEFINITION DEFERRED.
CLASS zcl_abapgit_object_sicf DEFINITION LOCAL FRIENDS ltcl_obj_name_length.

CLASS ltcl_obj_name_length DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

* The item part of an SICF filename consists of the ICF node name (15 characters,
* padded with blanks) followed by a hash of the URL (25 characters).
* Dots are not allowed in filenames and are therefore escaped as '%2e' which makes
* the name part longer when serializing.
* When deserializing, the escaping has already been reverted before the object name
* is mapped. Here, the node name is assumed to have been cut off after escaping the
* dots, ie. the length is reduced by 2 characters for each dot.
* Test data taken from https://github.com/abapGit-tests/SICF_name_with_dot

  PRIVATE SECTION.

    METHODS name_without_dot FOR TESTING.
    METHODS name_with_leading_dot FOR TESTING.
    METHODS name_with_dot FOR TESTING.
    METHODS name_with_multiple_dots FOR TESTING.
    METHODS name_of_legacy_filename FOR TESTING.
    METHODS dot_in_suffix FOR TESTING.
    METHODS escaped_dot_in_suffix FOR TESTING.

    " Length of the node name in the escaped object name (serialize)
    METHODS assert_length_esc
      IMPORTING
        iv_obj_name TYPE string
        iv_exp      TYPE i.

    " Length of the node name in the unescaped filename (deserialize)
    METHODS assert_length
      IMPORTING
        iv_filename TYPE string
        iv_exp      TYPE i.

ENDCLASS.

CLASS ltcl_obj_name_length IMPLEMENTATION.

  METHOD assert_length_esc.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp
      act = zcl_abapgit_object_sicf=>get_length_of_obj_name_esc( iv_obj_name )
      msg = |Wrong length of node name in escaped object name { iv_obj_name }| ).

  ENDMETHOD.

  METHOD assert_length.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp
      act = zcl_abapgit_object_sicf=>get_length_of_obj_name( iv_filename )
      msg = |Wrong length of node name in filename { iv_filename }| ).

  ENDMETHOD.

  METHOD name_without_dot.

    " ZABAPGIT_TEST
    assert_length_esc(
      iv_obj_name = 'zabapgit_test  eec64e283d0bfe28fb497dd3b'
      iv_exp      = 15 ).

    assert_length(
      iv_filename = 'zabapgit_test  eec64e283d0bfe28fb497dd3b'
      iv_exp      = 15 ).

  ENDMETHOD.

  METHOD name_with_leading_dot.

    " .TEST
    assert_length_esc(
      iv_obj_name = '%2etest          cbd1bab4f9f09ea99331d97eb'
      iv_exp      = 17 ).

    " One dot -> 2 characters less
    assert_length(
      iv_filename = '.test          cbd1bab4f9f09ea99331d97eb'
      iv_exp      = 13 ).

  ENDMETHOD.

  METHOD name_with_dot.

    " HELLO.WORLD
    assert_length_esc(
      iv_obj_name = 'hello%2eworld    5d821cca40d056a07c9bcf2e8'
      iv_exp      = 17 ).

    " One dot -> 2 characters less
    assert_length(
      iv_filename = 'hello.world    5d821cca40d056a07c9bcf2e8'
      iv_exp      = 13 ).

  ENDMETHOD.

  METHOD name_with_multiple_dots.

    " TEST..TEST_-.-_
    assert_length_esc(
      iv_obj_name = 'test%2e%2etest_-%2e-_0f868fe2fff52d5b4d580322b'
      iv_exp      = 21 ).

    " Three dots -> 6 characters less
    assert_length(
      iv_filename = 'test..test_-.-_0f868fe2fff52d5b4d580322b'
      iv_exp      = 9 ).

  ENDMETHOD.

  METHOD name_of_legacy_filename.

    " Older versions of abapGit cut off the node name after escaping the dots, so the
    " name of HELLO.WORLD ended up as 'hello%2eworld  ' instead of 'hello%2eworld    '
    " which is the length assumed when deserializing
    assert_length(
      iv_filename = 'hello.world  5d821cca40d056a07c9bcf2e8'
      iv_exp      = 13 ).

  ENDMETHOD.

  METHOD dot_in_suffix.

    assert_length(
      iv_filename = 'hello.world    ab.cd                '
      iv_exp      = 13 ).

  ENDMETHOD.

  METHOD escaped_dot_in_suffix.

    assert_length_esc(
      iv_obj_name = 'hello%2eworld    ab%2ecd            '
      iv_exp      = 17 ).

  ENDMETHOD.

ENDCLASS.
