CLASS zcl_abapgit_objects_ci_tests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL CRITICAL .

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_object TYPE string
        !iv_url    TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_objects_ci_tests IMPLEMENTATION.


  METHOD run.

    DATA:
      ls_repo     TYPE zif_abapgit_exit=>ty_ci_repo,
      lt_repos    TYPE zif_abapgit_exit=>ty_ci_repos,
      li_exit     TYPE REF TO zif_abapgit_exit,
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
    ls_repo-name = iv_object.
    IF iv_url IS NOT INITIAL.
      ls_repo-clone_url = iv_url.
    ELSE.
      ls_repo-clone_url = |https://github.com/abapGit-tests/{ iv_object }|.
    ENDIF.
    APPEND ls_repo TO lt_repos.

    " Get list of repos via exit
    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->get_ci_tests(
      EXPORTING
        iv_object    = |{ iv_object }|
      CHANGING
        ct_ci_repos  = lt_repos ).

    IF lines( lt_repos ) = 0.
      RETURN.
    ENDIF.

    " Objects will be created and deleted, do not run in customer system!
    " These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)
    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_run_critical_tests( ) = abap_false.
      RETURN.
    ENDIF.

    " Check if abapGit-CI is installed
    SELECT SINGLE clsname FROM seoclass INTO lv_check WHERE clsname = 'ZCL_ABAPGIT_CI_REPOS'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        " Prepare input for CI repo test
        CREATE DATA ld_options TYPE ('ZIF_ABAPGIT_CI_DEFINITIONS=>TY_REPO_CHECK_OPTIONS').
        ASSIGN ld_options->* TO <ls_options>.

        ASSIGN COMPONENT 'CREATE_PACKAGE' OF STRUCTURE <ls_options> TO <lv_option>.
        ASSERT sy-subrc = 0.
        <lv_option> = abap_true.

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
            cl_abap_unit_assert=>assert_equals(
              exp = 'OK'
              act = <lv_result>
              msg = |{ <lv_repo> }: { lv_check } = { <lv_result> }| ).
          ENDDO.
        ENDLOOP.

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>fail( msg = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
