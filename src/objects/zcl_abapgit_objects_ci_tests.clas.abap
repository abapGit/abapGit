CLASS zcl_abapgit_objects_ci_tests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL CRITICAL .

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
      lt_repos    TYPE zif_abapgit_exit=>ty_repos,
      lo_ci_repos TYPE REF TO object,
      ld_options  TYPE REF TO data,
      ld_results  TYPE REF TO data,
      lv_msg      TYPE string,
      lv_check    TYPE string,
      lx_error    TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS:
      <ls_options>     TYPE any,
      <lv_option>      TYPE any,
      <lt_repo_result> TYPE ANY TABLE,
      <ls_repo_result> TYPE any,
      <lv_result>      TYPE any,
      <lv_repo>        TYPE any.

    " Add the default test repo from https://github.com/abapGit-tests
    INSERT VALUE #(
      name      = iv_object
      clone_url = |https://github.com/abapGit-tests/{ iv_object }| )
      INTO TABLE lt_repos.

    " Get list of repos via exit
    zcl_abapgit_exit=>get_instance( )->get_ci_tests(
      EXPORTING
        iv_object = iv_object
      CHANGING
        ct_repos  = lt_repos ).

    IF lines( lt_repos ) = 0.
      RETURN.
    ENDIF.

    "Objects will be created and deleted, do not run in customer system!
    "These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)
    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_run_critical_tests( ) = abap_false.
      cl_abap_unit_assert=>fail(
        msg   = 'Cancelled. You can enable these tests at the Settings page'
        level = if_aunit_constants=>tolerable ).
    ENDIF.

    TRY.
        " Prepare CI repo test input
        CREATE DATA ld_options TYPE ('ZIF_ABAPGIT_CI_DEFINITIONS=>TY_REPO_CHECK_OPTIONS').
        ASSIGN ld_options->* TO <ls_options>.

        ASSIGN COMPONENT 'CHECK_LOCAL' OF STRUCTURE <ls_options> TO <lv_option>.
        ASSERT sy-subrc = 0.
        <lv_option> = abap_true.

        CREATE DATA ld_results TYPE ('ZIF_ABAPGIT_CI_DEFINITIONS=>TY_RESULT-REPO_RESULT_LIST').
        ASSIGN ld_results->* TO <lt_repo_result>.

        " Run CI repo tests
        CREATE OBJECT lo_ci_repos TYPE ('ZCL_ABAPGIT_CI_REPOS').

        CALL METHOD lo_ci_repos->('PROCESS_REPOS')
          EXPORTING
            it_repos       = lt_repos
            is_options     = <ls_options>
          RECEIVING
            rt_result_list = <lt_repo_result>.

        " Check results for individual repos
        LOOP AT <lt_repo_result> ASSIGNING <ls_repo_result>.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_repo_result> TO <lv_repo>.
          ASSERT sy-subrc = 0.

          CLEAR lv_msg.
          DO 7 TIMES.
            CASE sy-index.
              WHEN 1.
                lv_check = 'CREATE_PACKAGE'.
              WHEN 2.
                lv_check = 'CLONE'.
              WHEN 3.
                lv_check = 'PULL'.
              WHEN 4.
                lv_check = 'SYNTAX_CHECK'.
              WHEN 5.
                lv_check = 'OBJECT_CHECK'.
              WHEN 6.
                lv_check = 'PURGE'.
              WHEN 7.
                lv_check = 'CHECK_LEFTOVERS'.
            ENDCASE.
            ASSIGN COMPONENT lv_check OF STRUCTURE <ls_repo_result> TO <lv_result>.
            ASSERT sy-subrc = 0.
            IF <lv_result> <> 'OK'.
              IF lv_msg IS INITIAL.
                lv_msg = |{ lv_check }:{ <lv_result> }|.
              ELSE.
                lv_msg = |{ lv_msg } { lv_check }:{ <lv_result> }|.
              ENDIF.
            ENDIF.
          ENDDO.

          cl_abap_unit_assert=>assert_equals(
            exp = ''
            act = lv_msg
            msg = |{ <lv_repo> } { lv_msg }| ).

        ENDLOOP.

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>fail( msg = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
