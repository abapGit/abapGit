CLASS zcl_abapgit_test_regex DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS check_author_regex FOR TESTING.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_test_regex IMPLEMENTATION.


  METHOD check_author_regex.

    DATA: ls_commit TYPE zif_abapgit_definitions=>ty_commit.

    cl_abap_unit_assert=>assert_text_matches(
      EXPORTING
        pattern = zif_abapgit_definitions=>c_author_regex
        text    = 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).

    cl_abap_unit_assert=>assert_text_matches(
      EXPORTING
        pattern = zif_abapgit_definitions=>c_author_regex
        text    = 'Volker Jägle <github@beimir.net> 1573216988 +0000' ).

    " special characters are supported in author name
    cl_abap_unit_assert=>assert_text_matches(
      EXPORTING
        pattern = zif_abapgit_definitions=>c_author_regex
        text    = 'pull[bot&%#$] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000'
        quit    = if_aunit_constants=>no ).

*    " +00001 too long
*    cl_abap_unit_assert=>assert_true(
*      cl_abap_unit_assert=>assert_text_matches(
*        EXPORTING
*          pattern = zif_abapgit_definitions=>c_author_regex
*          text    = 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +00001'
*          quit    = if_aunit_constants=>no
*          level   = if_aunit_constants=>tolerable ) ).
*
*    " datetime is too long
*    cl_abap_unit_assert=>assert_true(
*      cl_abap_unit_assert=>assert_text_matches(
*        EXPORTING
*          pattern = zif_abapgit_definitions=>c_author_regex
*          text    = 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 15732169881 +0000'
*          quit    = if_aunit_constants=>no ) ).
*
*    " FALSE: no author
*    cl_abap_unit_assert=>assert_true(
*      cl_abap_unit_assert=>assert_text_matches(
*        EXPORTING
*          pattern = zif_abapgit_definitions=>c_author_regex
*          text    = '<39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000'
*          quit    = if_aunit_constants=>no ) ).

    " TRUE: special characters from other languages also valid
    cl_abap_unit_assert=>assert_text_matches(
      EXPORTING
        pattern = zif_abapgit_definitions=>c_author_regex
        text    = 'äÖüßÐÑÒסעף <+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).

    " TRUE: brackets don't confuse regex
    cl_abap_unit_assert=>assert_text_matches(
      EXPORTING
        pattern = zif_abapgit_definitions=>c_author_regex
        text    = '<pull[bot]> <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).

  ENDMETHOD.
ENDCLASS.
