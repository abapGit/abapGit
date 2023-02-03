*"* use this source file for your ABAP unit test classes
CLASS ltcl_find_remote_dot_abapgit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS: c_dummy_repo_key TYPE zif_abapgit_persistence=>ty_value VALUE '000000001'.
    METHODS:
      positive FOR TESTING RAISING cx_static_check,
      new_repo_needs_no_dot_abapgit FOR TESTING RAISING cx_static_check,

      given_any_repo,
      when_find_remote_dot_abapgit,
      then_dot_abapgit_is_bound,
      then_no_exception_is_raised,
      given_dot_abapgit_file
        RAISING zcx_abapgit_exception,
      given_no_dot_abapgit_file,
      then_dot_abapgit_is_not_bound,
      given_repo_has_files
        IMPORTING
          iv_number_of_files TYPE i.

    DATA:
      mo_repo        TYPE REF TO zcl_abapgit_repo,
      mx_error       TYPE REF TO zcx_abapgit_exception,
      mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.

ENDCLASS.

CLASS ltcl_find_remote_dot_abapgit IMPLEMENTATION.

  METHOD positive.

    given_any_repo( ).
    given_dot_abapgit_file( ).

    when_find_remote_dot_abapgit( ).

    then_dot_abapgit_is_bound( ).
    then_no_exception_is_raised( ).

  ENDMETHOD.


  METHOD new_repo_needs_no_dot_abapgit.

    given_any_repo( ).
    given_repo_has_files( 3 ). " a few random files
    given_no_dot_abapgit_file( ).

    when_find_remote_dot_abapgit( ).

    then_dot_abapgit_is_not_bound( ).
    then_no_exception_is_raised( ).

  ENDMETHOD.


  METHOD given_any_repo.

    DATA: ls_data TYPE zif_abapgit_persistence=>ty_repo.

    ls_data-key = c_dummy_repo_key.

    " online/offline doesn't matter...
    CREATE OBJECT mo_repo TYPE zcl_abapgit_repo_offline
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD given_dot_abapgit_file.

    DATA:
      lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt,
      ls_file  LIKE LINE OF lt_files.

    ls_file-path = zif_abapgit_definitions=>c_root_dir.
    ls_file-filename = zif_abapgit_definitions=>c_dot_abapgit.
    ls_file-data = zcl_abapgit_convert=>string_to_xstring(
                         |<?xml version="1.0" encoding="utf-8"?>|
                      && |<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">|
                      && | <asx:values>|
                      && |  <DATA>|
                      && |   <MASTER_LANGUAGE>E</MASTER_LANGUAGE>|
                      && |   <STARTING_FOLDER>/src/</STARTING_FOLDER>|
                      && |   <FOLDER_LOGIC>PREFIX</FOLDER_LOGIC>|
                      && |   <IGNORE>|
                      && |    <item>/.gitignore</item>|
                      && |    <item>/LICENSE</item>|
                      && |    <item>/README.md</item>|
                      && |    <item>/package.json</item>|
                      && |    <item>/.travis.yml</item>|
                      && |   </IGNORE>|
                      && |  </DATA>|
                      && | </asx:values>|
                      && |</asx:abap>| ).
    INSERT ls_file INTO TABLE lt_files.

    mo_repo->set_files_remote( lt_files ).

  ENDMETHOD.


  METHOD given_no_dot_abapgit_file.

  ENDMETHOD.


  METHOD when_find_remote_dot_abapgit.

    TRY.
        mo_dot_abapgit = mo_repo->find_remote_dot_abapgit( ).
      CATCH zcx_abapgit_exception INTO mx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD then_dot_abapgit_is_bound.
    cl_abap_unit_assert=>assert_bound( mo_dot_abapgit ).
  ENDMETHOD.


  METHOD then_no_exception_is_raised.
    cl_abap_unit_assert=>assert_not_bound( mx_error ).
  ENDMETHOD.


  METHOD then_dot_abapgit_is_not_bound.
    cl_abap_unit_assert=>assert_not_bound( mo_dot_abapgit ).
  ENDMETHOD.


  METHOD given_repo_has_files.

    DATA: lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt,
          ls_file  LIKE LINE OF lt_files.

    ls_file-path = zif_abapgit_definitions=>c_root_dir.

    DO iv_number_of_files TIMES.
      ls_file-filename = |File_{ sy-index }|.
      INSERT ls_file INTO TABLE lt_files.
    ENDDO.

    mo_repo->set_files_remote( lt_files ).

  ENDMETHOD.

ENDCLASS.
