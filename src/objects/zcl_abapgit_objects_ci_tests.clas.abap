CLASS zcl_abapgit_objects_ci_tests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL DANGEROUS .

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        IMPORTING
          !iv_object TYPE tadir-object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_CI_TESTS IMPLEMENTATION.


  METHOD run.

    DATA:
      lt_repos         TYPE zif_abapgit_exit=>ty_repos,
      lr_repos         TYPE RANGE OF string,
      ls_repos         LIKE LINE OF lr_repos,
      ld_options       TYPE REF TO data,
      ld_result        TYPE REF TO data,
      li_repo_provider TYPE REF TO object,
      li_ci_controller TYPE REF TO object,
      lx_error         TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS:
      <ls_options>     TYPE any,
      <lv_option>      TYPE any,
      <ls_result>      TYPE any,
      <lt_repo_result> TYPE ANY TABLE,
      <ls_repo_result> TYPE any,
      <lv_result>      TYPE string.


    "Objects will be created and deleted, do not run in customer system!
    "These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)
    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_run_critical_tests( ) = abap_false.
      cl_abap_unit_assert=>fail(
        msg   = 'Cancelled. You can enable these tests at the Settings page'
        level = if_aunit_constants=>tolerable ).
    ENDIF.

    " Get list of repos via exit
    lt_repos = zcl_abapgit_exit=>get_instance( )->get_ci_tests( 'TABL' ).

    ls_repos-sign = 'I'.
    ls_repos-option = 'EQ'.
    LOOP AT lt_repos INTO ls_repos-low.
      APPEND ls_repos TO lr_repos.
    ENDLOOP.

    IF lines( lr_repos ) = 0.
      RETURN.
    ENDIF.

    TRY.
        " Prepare the CI test controller
        CREATE OBJECT li_repo_provider TYPE ('ZCL_ABAPGIT_CI_TEST_REPOS')
          EXPORTING
            it_repo_name_range = lr_repos.

        CREATE DATA ld_options TYPE ('ZIF_ABAPGIT_CI_DEFINITIONS=>TY_OPTIONS').
        ASSIGN ld_options->* TO <ls_options>.

        ASSIGN COMPONENT 'EXEC_REPOSITORY_CHECKS' OF STRUCTURE <ls_options> TO <lv_option>.
        ASSERT sy-subrc = 0.
        <lv_option> = abap_true.

        ASSIGN COMPONENT 'REPO_CHECK_OPTIONS-CHECK_LOCAL' OF STRUCTURE <ls_options> TO <lv_option>.
        ASSERT sy-subrc = 0.
        <lv_option> = abap_true.

        CREATE OBJECT li_ci_controller TYPE ('ZCL_ABAPGIT_CI_CONTROLLER')
          EXPORTING
            ii_repo_provider = li_repo_provider "<<< how to dynamically cast to ZIF_ABAPGIT_CI_CONTROLLER ???
            is_options       = <ls_options>.

        " Run the CI tests
        CALL METHOD li_ci_controller->('RUN')
          RECEIVING
            rs_result = <ls_result>.

        " Check results for individual repos
        ASSIGN COMPONENT 'REPO_RESULT_LIST' OF STRUCTURE <ls_result> TO <lt_repo_result>.
        ASSERT sy-subrc = 0.
        LOOP AT <lt_repo_result> ASSIGNING <ls_repo_result>.
          ASSIGN COMPONENT 'CREATE_PACKAGE' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error creating package').
          ASSIGN COMPONENT 'CLONE' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error cloning repo').
          ASSIGN COMPONENT 'PULL' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error pulling repo').
          ASSIGN COMPONENT 'SYNTAX_CHECK' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error during syntax check').
          ASSIGN COMPONENT 'OBJECT_CHECK' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error during object check').
          ASSIGN COMPONENT 'PURGE' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error purging repo').
          ASSIGN COMPONENT 'CHECK_LEFTOVERS' OF STRUCTURE <ls_repo_result> TO <lv_result>.
          ASSERT sy-subrc = 0.
          cl_abap_unit_assert=>assert_equals(
            exp = 'OK'
            act = <lv_result>
            msg = 'Error checking for leftovers').
        ENDLOOP.

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>fail( msg = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
