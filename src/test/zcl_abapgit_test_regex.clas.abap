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

    cl_abap_unit_assert=>assert_true( zcl_abapgit_utils=>is_commit_author(
     'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ) ).

    " language-specific characters are supported in author name
    cl_abap_unit_assert=>assert_true( zcl_abapgit_utils=>is_commit_author(
     'Volker Jägle äÖüßÐÑÒסעף <github@beimir.net> 1573216988 +0000' ) ).

    " special characters are supported in author name
    cl_abap_unit_assert=>assert_true( zcl_abapgit_utils=>is_commit_author(
     'pull[bot&%#$] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ) ).

    " +00001 too long
    cl_abap_unit_assert=>assert_false( zcl_abapgit_utils=>is_commit_author(
     'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +00001' ) ).

    " datetime is too long
    cl_abap_unit_assert=>assert_false( zcl_abapgit_utils=>is_commit_author(
     'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 15732169881 +0000' ) ).

    " no author
    cl_abap_unit_assert=>assert_false( zcl_abapgit_utils=>is_commit_author(
     '<39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ) ).

    " no email address
    cl_abap_unit_assert=>assert_false( zcl_abapgit_utils=>is_commit_author(
     'pull[bot] 1573216988 +0000' ) ).

    " no datetime
    cl_abap_unit_assert=>assert_false( zcl_abapgit_utils=>is_commit_author(
     'pull[bot] <39814207+pull[bot]@users.noreply.github.com> +0000' ) ).

    " missing +0000
    cl_abap_unit_assert=>assert_false( zcl_abapgit_utils=>is_commit_author(
     'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988' ) ).

    " brackets don't confuse regex
    cl_abap_unit_assert=>assert_true( zcl_abapgit_utils=>is_commit_author(
     '<pull[bot]> <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ) ).

  ENDMETHOD.
ENDCLASS.
