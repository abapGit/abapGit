CLASS ltcl_code_inspector DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_code_inspector.

    METHODS:
      constructor
        IMPORTING
          iv_package TYPE devclass.

  PRIVATE SECTION.
    DATA:
      mv_package TYPE devclass.

ENDCLASS.

CLASS ltcl_code_inspector IMPLEMENTATION.

  METHOD constructor.

    mv_package = iv_package.

  ENDMETHOD.

  METHOD zif_abapgit_code_inspector~run.

    DATA: ls_list LIKE LINE OF rt_list.

    IF mv_package = '$DUMMY'.

      ls_list-kind = 'E'.
      INSERT ls_list INTO TABLE rt_list.

    ENDIF.


  ENDMETHOD.

  METHOD zif_abapgit_code_inspector~get_inspection. "##needed

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_run_code_inspection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_repo_online   TYPE REF TO zcl_abapgit_repo_online,
      mv_error_text    TYPE string,
      mt_act_list      TYPE scit_alvlist,
      mv_check_variant TYPE string.

    METHODS:
      exception_when_no_check_var FOR TESTING RAISING cx_static_check,
      exception_when_error FOR TESTING RAISING cx_static_check,
      no_exception_when_no_error FOR TESTING RAISING cx_static_check,
      push_not_possible_if_ci_req FOR TESTING RAISING cx_static_check,

      given_online_repo
        IMPORTING
          iv_package TYPE devclass
        RAISING
          zcx_abapgit_exception,

      given_check_variant
        IMPORTING
          iv_check_variant TYPE string,

      given_mock_code_inspector
        IMPORTING
          iv_package       TYPE devclass
          iv_check_variant TYPE sci_chkv,

      given_block_commit
        IMPORTING
          iv_block_commit TYPE abap_bool,

      when_push,

      then_exception_text_is
        IMPORTING
          iv_exp_error_text TYPE csequence,

      when_run_code_inspector,

      then_ci_detected_an_error,

      then_no_exception_is_raised.

ENDCLASS.

CLASS zcl_abapgit_repo_online DEFINITION LOCAL FRIENDS ltcl_run_code_inspection.

CLASS ltcl_run_code_inspection IMPLEMENTATION.

  METHOD exception_when_no_check_var.

    given_online_repo( iv_package = '$DUMMY' ).
    given_check_variant( || ).
    when_run_code_inspector( ).
    then_exception_text_is( |Please supply check variant| ).

  ENDMETHOD.

  METHOD exception_when_error.


    given_online_repo( '$DUMMY' ).
    given_check_variant( |variant| ).
    given_mock_code_inspector( iv_package       = '$DUMMY'
                               iv_check_variant = |variant| ).

    when_run_code_inspector( ).
    then_ci_detected_an_error( ).

  ENDMETHOD.

  METHOD no_exception_when_no_error.

    given_online_repo( '$PACKAGE' ).
    given_check_variant( |variant| ).
    given_mock_code_inspector( iv_package       = '$PACKAGE'
                               iv_check_variant = |variant| ).
    when_run_code_inspector( ).
    then_no_exception_is_raised( ).

  ENDMETHOD.


  METHOD given_online_repo.

    DATA: ls_data TYPE zif_abapgit_persistence=>ty_repo.

    ls_data-key     = 'dummmy'.
    ls_data-package = iv_package.

    CREATE OBJECT mo_repo_online
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD given_check_variant.
    mv_check_variant = iv_check_variant.
    mo_repo_online->ms_data-local_settings-code_inspector_check_variant = iv_check_variant.
  ENDMETHOD.


  METHOD given_mock_code_inspector.

    DATA: lo_mock_code_inspector TYPE REF TO ltcl_code_inspector.

    CREATE OBJECT lo_mock_code_inspector
      EXPORTING
        iv_package = iv_package.

    zcl_abapgit_injector=>set_code_inspector( iv_package            = iv_package
                                              iv_check_variant_name = iv_check_variant
                                              ii_code_inspector     = lo_mock_code_inspector ).


  ENDMETHOD.

  METHOD push_not_possible_if_ci_req.

    given_online_repo( '$PACKAGE' ).
    given_check_variant( |variant| ).
    given_block_commit( abap_true ).
    when_push( ).
    then_exception_text_is( |A successful code inspection is required| ).

  ENDMETHOD.


  METHOD given_block_commit.

    mo_repo_online->ms_data-local_settings-block_commit = abap_true.

  ENDMETHOD.


  METHOD when_push.

    DATA: ls_comment TYPE zif_abapgit_definitions=>ty_comment,
          lo_stage   TYPE REF TO  zcl_abapgit_stage,
          lx_error   TYPE REF TO  zcx_abapgit_exception.

    CREATE OBJECT lo_stage.

    TRY.
        mo_repo_online->push( is_comment = ls_comment
                              io_stage   = lo_stage ).

      CATCH zcx_abapgit_exception INTO lx_error.
        mv_error_text = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD then_exception_text_is.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp_error_text
      act = mv_error_text ).

  ENDMETHOD.


  METHOD when_run_code_inspector.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        mt_act_list = mo_repo_online->run_code_inspector( |{ mv_check_variant }| ).
      CATCH zcx_abapgit_exception INTO lx_error.
        mv_error_text = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD then_ci_detected_an_error.

    READ TABLE mt_act_list TRANSPORTING NO FIELDS
                           WITH KEY kind = 'E'.
    cl_abap_unit_assert=>assert_subrc( exp = 0 ).

  ENDMETHOD.


  METHOD then_no_exception_is_raised.

    cl_abap_unit_assert=>assert_initial( mv_error_text ).

  ENDMETHOD.

ENDCLASS.
