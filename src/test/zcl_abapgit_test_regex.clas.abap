CLASS zcl_abapgit_test_regex DEFINITION
  PUBLIC
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.
    CLASS-METHODS: check_definitions_author_regex
      IMPORTING iv_author                  TYPE string OPTIONAL
      RETURNING VALUE(rv_assertion_failed) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_TEST_REGEX IMPLEMENTATION.


  METHOD check_definitions_author_regex.

    IF    iv_author IS SUPPLIED
      AND iv_author IS NOT INITIAL.

      cl_abap_unit_assert=>assert_text_matches(
        EXPORTING
          pattern = zif_abapgit_definitions=>c_author_regex
          text    = iv_author
          RECEIVING
          assertion_failed = rv_assertion_failed ).

    ELSE.

      " TRUE
      cl_abap_unit_assert=>assert_text_matches(
        EXPORTING
          pattern = zif_abapgit_definitions=>c_author_regex
          text    = 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000'
        RECEIVING
          assertion_failed = rv_assertion_failed ).

      " FALSE: [bot&]
      cl_abap_unit_assert=>assert_text_matches(
        EXPORTING
          pattern = zif_abapgit_definitions=>c_author_regex
          text    = 'pull[bot&] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000'
          quit    = if_aunit_constants=>no
        RECEIVING
          assertion_failed = rv_assertion_failed ).

      " FALSE: +00001
      cl_abap_unit_assert=>assert_text_matches(
        EXPORTING
          pattern = zif_abapgit_definitions=>c_author_regex
          text    = 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +00001'
          quit    = if_aunit_constants=>no
        RECEIVING
          assertion_failed = rv_assertion_failed ).

      " FALSE: 15732169881
      cl_abap_unit_assert=>assert_text_matches(
        EXPORTING
          pattern = zif_abapgit_definitions=>c_author_regex
          text    = 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 15732169881 +0000'
          quit    = if_aunit_constants=>no
        RECEIVING
          assertion_failed = rv_assertion_failed ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
