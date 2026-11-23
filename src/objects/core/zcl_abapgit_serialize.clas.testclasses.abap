CLASS ltcl_determine_max_processes DEFINITION DEFERRED.
CLASS ltcl_determine_server_group DEFINITION DEFERRED.
CLASS ltcl_determine_batch_size DEFINITION DEFERRED.
CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS ltcl_determine_max_processes ltcl_determine_server_group
                                                     ltcl_determine_batch_size.

CLASS ltd_settings DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_persist_settings.

    METHODS:
      set_parallel_proc_disabled
        IMPORTING
          iv_parallel_proc_disabled TYPE abap_bool,
      set_experimental_features
        IMPORTING
          iv_features TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_parallel_proc_disabled TYPE zif_abapgit_persist_user=>ty_s_user_settings-parallel_proc_disabled,
      mv_experimental_features  TYPE string.

ENDCLASS.


CLASS ltd_settings IMPLEMENTATION.

  METHOD zif_abapgit_persist_settings~modify.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
    ro_settings->set_parallel_proc_disabled( mv_parallel_proc_disabled ).
    ro_settings->set_experimental_features( mv_experimental_features ).
  ENDMETHOD.

  METHOD set_parallel_proc_disabled.
    mv_parallel_proc_disabled = iv_parallel_proc_disabled.
  ENDMETHOD.

  METHOD set_experimental_features.
    mv_experimental_features = iv_features.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_function_module DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_function_module.

ENDCLASS.


CLASS ltd_function_module IMPLEMENTATION.

  METHOD zif_abapgit_function_module~function_exists.
    rv_exists = abap_true.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_environment DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_environment.

    METHODS:
      set_server_group
        IMPORTING iv_group TYPE rzlli_apcl,

      set_is_merged
        IMPORTING iv_is_merged TYPE abap_bool,

      set_available_sessions
        IMPORTING iv_available_sessions TYPE i,

      set_free_work_processes
        IMPORTING iv_free_work_processes TYPE i.

  PRIVATE SECTION.
    DATA:
      mv_group               TYPE rzlli_apcl,
      mv_is_merged           TYPE abap_bool,
      mv_available_sessions  TYPE i VALUE 6, " system default
      mv_free_work_processes TYPE i.

ENDCLASS.


CLASS ltd_environment IMPLEMENTATION.

  METHOD zif_abapgit_environment~compare_with_inactive.
  ENDMETHOD.

  METHOD zif_abapgit_environment~get_basis_release.
  ENDMETHOD.

  METHOD zif_abapgit_environment~get_available_user_sessions.
    rv_sessions = mv_available_sessions.
  ENDMETHOD.

  METHOD zif_abapgit_environment~get_system_language_filter.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_merged.
    rv_result = mv_is_merged.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_restart_required.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_cloud_platform.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_object_allowed.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_variant_maintenance.
  ENDMETHOD.

  METHOD zif_abapgit_environment~init_parallel_processing.
    rv_free_work_processes = mv_free_work_processes.
  ENDMETHOD.

  METHOD zif_abapgit_environment~check_parallel_processing.
    rv_checked = boolc( iv_group = mv_group ).
  ENDMETHOD.

  METHOD set_server_group.
    mv_group = iv_group.
  ENDMETHOD.

  METHOD set_is_merged.
    mv_is_merged = iv_is_merged.
  ENDMETHOD.

  METHOD set_available_sessions.
    mv_available_sessions = iv_available_sessions.
  ENDMETHOD.


  METHOD set_free_work_processes.
    mv_free_work_processes = iv_free_work_processes.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_exit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_exit.

    METHODS:
      set_server_group
        IMPORTING iv_group TYPE rzlli_apcl,

      set_max_parallel_processes
        IMPORTING
          iv_max_parallel_processes TYPE i.

  PRIVATE SECTION.
    DATA:
      mv_group                  TYPE rzlli_apcl,
      mv_max_parallel_processes TYPE i.

ENDCLASS.


CLASS ltd_exit IMPLEMENTATION.

  METHOD zif_abapgit_exit~adjust_commit_message.
  ENDMETHOD.

  METHOD zif_abapgit_exit~adjust_display_commit_url.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_committer_info.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_password_popup_username.
  ENDMETHOD.

  METHOD zif_abapgit_exit~adjust_display_filename.
  ENDMETHOD.

  METHOD zif_abapgit_exit~allow_sap_objects.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_local_host.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_max_parallel_processes.
    IF mv_max_parallel_processes IS NOT INITIAL.
      cv_max_processes = mv_max_parallel_processes.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_proxy_authentication.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_proxy_port.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_proxy_url.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_rfc_server_group.
    IF mv_group IS NOT INITIAL.
      cv_group = mv_group.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_supported_data_objects.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_supported_object_types.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_tadir.
  ENDMETHOD.

  METHOD zif_abapgit_exit~create_http_client.
  ENDMETHOD.

  METHOD zif_abapgit_exit~custom_serialize_abap_clif.
  ENDMETHOD.

  METHOD zif_abapgit_exit~deserialize_postprocess.
  ENDMETHOD.

  METHOD zif_abapgit_exit~determine_transport_request.
  ENDMETHOD.

  METHOD zif_abapgit_exit~enable_adjust_commit_message.
  ENDMETHOD.

  METHOD zif_abapgit_exit~enhance_any_toolbar.
  ENDMETHOD.

  METHOD zif_abapgit_exit~enhance_repo_toolbar.
  ENDMETHOD.

  METHOD zif_abapgit_exit~get_ci_tests.
  ENDMETHOD.

  METHOD zif_abapgit_exit~get_ssl_id.
  ENDMETHOD.

  METHOD zif_abapgit_exit~http_client.
  ENDMETHOD.

  METHOD zif_abapgit_exit~on_event.
  ENDMETHOD.

  METHOD zif_abapgit_exit~post_pull.
  ENDMETHOD.

  METHOD zif_abapgit_exit~pre_calculate_repo_status.
  ENDMETHOD.

  METHOD zif_abapgit_exit~serialize_postprocess.
  ENDMETHOD.

  METHOD zif_abapgit_exit~validate_before_push.
  ENDMETHOD.

  METHOD zif_abapgit_exit~validate_after_push.
  ENDMETHOD.

  METHOD zif_abapgit_exit~validate_before_pull.
  ENDMETHOD.

  METHOD zif_abapgit_exit~wall_message_list.
  ENDMETHOD.

  METHOD zif_abapgit_exit~wall_message_repo.
  ENDMETHOD.

  METHOD set_server_group.
    mv_group = iv_group.
  ENDMETHOD.

  METHOD set_max_parallel_processes.
    mv_max_parallel_processes = iv_max_parallel_processes.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_determine_server_group DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut                TYPE REF TO zcl_abapgit_serialize,
      mo_environment_double TYPE REF TO ltd_environment,
      mo_exit               TYPE REF TO ltd_exit,
      mv_act_group          TYPE rzlli_apcl.

    METHODS:
      setup,

      default_server_group FOR TESTING RAISING zcx_abapgit_exception,
      legacy_server_group FOR TESTING RAISING zcx_abapgit_exception,
      exit_server_group FOR TESTING RAISING zcx_abapgit_exception,
      exit_not_exist_server_group FOR TESTING RAISING zcx_abapgit_exception,

      teardown,

      given_db_server_group
        IMPORTING
          iv_group TYPE rzlli_apcl,

      given_exit_chg_server_group
        IMPORTING
          iv_group TYPE rzlli_apcl,

      when_determine_server_group
        RAISING
          zcx_abapgit_exception,

      then_we_shd_have_server_group
        IMPORTING
          iv_exp_group TYPE rzlli_apcl.

ENDCLASS.

CLASS ltcl_determine_server_group IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_environment_double.
    zcl_abapgit_injector=>set_environment( mo_environment_double ).

    CREATE OBJECT mo_exit.
    zcl_abapgit_injector=>set_exit( mo_exit ).

    TRY.
        CREATE OBJECT mo_cut.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.

  ENDMETHOD.

  METHOD teardown.
    CLEAR: mo_cut->mv_group.
  ENDMETHOD.

  METHOD default_server_group.
    when_determine_server_group( ).
    then_we_shd_have_server_group( '' ).
  ENDMETHOD.

  METHOD legacy_server_group.
    given_db_server_group( 'parallel_generators' ).
    when_determine_server_group( ).
    then_we_shd_have_server_group( 'parallel_generators' ).
  ENDMETHOD.

  METHOD exit_server_group.
    given_db_server_group( 'my_group' ).
    given_exit_chg_server_group( 'my_group' ).
    when_determine_server_group( ).
    then_we_shd_have_server_group( 'my_group' ).
  ENDMETHOD.

  METHOD exit_not_exist_server_group.
    given_exit_chg_server_group( 'my_servers' ).
    when_determine_server_group( ).
    then_we_shd_have_server_group( '' ).
  ENDMETHOD.

  METHOD given_db_server_group.
    mo_environment_double->set_server_group( iv_group ).
  ENDMETHOD.

  METHOD given_exit_chg_server_group.
    mo_exit->set_server_group( iv_group ).
  ENDMETHOD.

  METHOD when_determine_server_group.
    mv_act_group = mo_cut->determine_rfc_server_group( ).
  ENDMETHOD.

  METHOD then_we_shd_have_server_group.
    cl_abap_unit_assert=>assert_equals(
      act = mv_act_group
      exp = iv_exp_group ).
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_determine_max_processes DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut                    TYPE REF TO zcl_abapgit_serialize,
      mv_act_processes          TYPE i,
      mo_settings_double        TYPE REF TO ltd_settings,
      mo_environment_double     TYPE REF TO ltd_environment,
      mo_function_module_double TYPE REF TO ltd_function_module,
      mo_exit                   TYPE REF TO ltd_exit.

    METHODS:
      setup,

      determine_max_processes_free FOR TESTING RAISING zcx_abapgit_exception,
      det_max_processes_not_free FOR TESTING RAISING zcx_abapgit_exception,
      det_max_proc_none_available FOR TESTING RAISING zcx_abapgit_exception,
      det_max_proc_amdahls_law FOR TESTING RAISING zcx_abapgit_exception,
      determine_max_processes_no_pp FOR TESTING RAISING zcx_abapgit_exception,
      determine_max_processes_merged FOR TESTING RAISING zcx_abapgit_exception,
      determine_max_proc_exit_lower FOR TESTING RAISING zcx_abapgit_exception,
      determine_max_proc_exit_higher FOR TESTING RAISING zcx_abapgit_exception,
      determine_max_processes_capped FOR TESTING RAISING zcx_abapgit_exception,
      force FOR TESTING RAISING zcx_abapgit_exception,

      teardown,

      given_parallel_proc_disabled
        IMPORTING
          iv_parallel_proc_disabled TYPE abap_bool,

      given_is_merged
        IMPORTING
          iv_is_merged TYPE abap_bool,

      given_available_sessions
        IMPORTING
          iv_available_sessions TYPE i,

      given_free_work_processes
        IMPORTING
          iv_free_work_processes TYPE i,

      when_determine_max_processes
        IMPORTING
          iv_force_sequential TYPE abap_bool OPTIONAL
        RAISING
          zcx_abapgit_exception,

      then_we_shd_have_n_processes
        IMPORTING
          iv_exp_processes TYPE i,

      given_exit_chg_max_processes
        IMPORTING
          iv_max_processes TYPE i.

ENDCLASS.

CLASS ltcl_determine_max_processes IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_settings_double.
    zcl_abapgit_persist_injector=>set_settings( mo_settings_double ).

    CREATE OBJECT mo_environment_double.
    zcl_abapgit_injector=>set_environment( mo_environment_double ).

    CREATE OBJECT mo_function_module_double.
    zcl_abapgit_injector=>set_function_module( mo_function_module_double ).

    CREATE OBJECT mo_exit.
    zcl_abapgit_injector=>set_exit( mo_exit ).

    TRY.
        CREATE OBJECT mo_cut.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.

  ENDMETHOD.

  METHOD teardown.

    CLEAR: mo_cut->gv_max_processes.

  ENDMETHOD.

  METHOD determine_max_processes_free.

    given_parallel_proc_disabled( abap_false ).
    given_is_merged( abap_false ).
    given_free_work_processes( 10 ).
    given_available_sessions( 10 ).

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 9 ).

  ENDMETHOD.

  METHOD det_max_processes_not_free.

    given_parallel_proc_disabled( abap_false ).
    given_is_merged( abap_false ).
    given_free_work_processes( 0 ).

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 1 ).

  ENDMETHOD.

  METHOD det_max_proc_none_available.

    given_parallel_proc_disabled( abap_false ).
    given_is_merged( abap_false ).
    given_free_work_processes( 10 ).
    given_available_sessions( 0 ).

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 1 ).

  ENDMETHOD.

  METHOD det_max_proc_amdahls_law.

    given_parallel_proc_disabled( abap_false ).
    given_is_merged( abap_false ).
    given_free_work_processes( 50 ).
    given_available_sessions( 50 ).

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 32 ).

  ENDMETHOD.

  METHOD determine_max_processes_no_pp.

    given_parallel_proc_disabled( abap_true ).
    given_is_merged( abap_false ).

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 1 ).

  ENDMETHOD.

  METHOD determine_max_processes_merged.

    given_parallel_proc_disabled( abap_false ).
    given_is_merged( abap_true ).

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 1 ).

  ENDMETHOD.

  METHOD determine_max_proc_exit_lower.

    given_free_work_processes( 26 ).
    given_available_sessions( 11 ).

    given_exit_chg_max_processes( 7 ).
    when_determine_max_processes( ).
    then_we_shd_have_n_processes( 7 ).

  ENDMETHOD.

  METHOD determine_max_proc_exit_higher.

    given_free_work_processes( 20 ).
    given_available_sessions( 15 ).

    given_exit_chg_max_processes( 30 ).
    when_determine_max_processes( ).
    then_we_shd_have_n_processes( 30 ).

  ENDMETHOD.

  METHOD determine_max_processes_capped.

    given_parallel_proc_disabled( abap_false ).
    given_is_merged( abap_false ).
    given_free_work_processes( 50 ). " big system
    given_available_sessions( 10 ). " but user session is capped

    when_determine_max_processes( ).

    then_we_shd_have_n_processes( 10 ).

  ENDMETHOD.

  METHOD force.

    when_determine_max_processes( abap_true ).

    then_we_shd_have_n_processes( 1 ).

  ENDMETHOD.

  METHOD given_parallel_proc_disabled.

    mo_settings_double->set_parallel_proc_disabled( iv_parallel_proc_disabled ).

  ENDMETHOD.

  METHOD given_is_merged.

    mo_environment_double->set_is_merged( iv_is_merged ).

  ENDMETHOD.

  METHOD given_available_sessions.

    mo_environment_double->set_available_sessions( iv_available_sessions ).

  ENDMETHOD.

  METHOD given_free_work_processes.

    mo_environment_double->set_free_work_processes( iv_free_work_processes ).

  ENDMETHOD.

  METHOD when_determine_max_processes.

    mv_act_processes = mo_cut->determine_max_processes(
                           iv_force_sequential = iv_force_sequential
                           iv_package          = 'ZDUMMY' ).

  ENDMETHOD.

  METHOD then_we_shd_have_n_processes.

    cl_abap_unit_assert=>assert_equals(
      act = mv_act_processes
      exp = iv_exp_processes ).

  ENDMETHOD.

  METHOD given_exit_chg_max_processes.

    mo_exit->set_max_parallel_processes( iv_max_processes ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_serialize DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_dot               TYPE REF TO zcl_abapgit_dot_abapgit,
      mo_settings_double   TYPE REF TO ltd_settings,
      mi_previous_settings TYPE REF TO zif_abapgit_persist_settings,
      mo_cut               TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      teardown,
      test FOR TESTING RAISING zcx_abapgit_exception,
      unsupported FOR TESTING RAISING zcx_abapgit_exception,
      ignored FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_serialize IMPLEMENTATION.

  METHOD setup.

    " Per-object mode, whatever the settings in the database say about SERIALIZE_BATCH
    mi_previous_settings = zcl_abapgit_persist_factory=>get_settings( ).
    CREATE OBJECT mo_settings_double.
    zcl_abapgit_persist_injector=>set_settings( mo_settings_double ).

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    TRY.
        CREATE OBJECT mo_cut
          EXPORTING
            io_dot_abapgit = mo_dot.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_persist_injector=>set_settings( mi_previous_settings ).
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_sequential TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_parallel   TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'PROG'.
    <ls_tadir>-obj_name = 'RSABAPPROGRAM'.
    <ls_tadir>-devclass = 'PACKAGE'.
    <ls_tadir>-path     = 'foobar'.
    <ls_tadir>-masterlang = sy-langu.

    lt_sequential = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_true ).

    lt_parallel = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_sequential
      exp = lt_parallel ).

  ENDMETHOD.

  METHOD unsupported.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_msg   TYPE zif_abapgit_log=>ty_log_out,
          lt_msg   TYPE zif_abapgit_log=>ty_log_outs,
          li_log1  TYPE REF TO zif_abapgit_log,
          li_log2  TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'ABCD'.
    <ls_tadir>-obj_name = 'OBJECT'.

    CREATE OBJECT li_log1 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log1
      iv_force_sequential = abap_true ).

    CREATE OBJECT li_log2 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log2
      iv_force_sequential = abap_false ).

    lt_msg = li_log1->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object type ABCD not supported*' ).

    lt_msg = li_log2->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object type ABCD not supported*' ).

  ENDMETHOD.

  METHOD ignored.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_msg   TYPE zif_abapgit_log=>ty_log_out,
          lt_msg   TYPE zif_abapgit_log=>ty_log_outs,
          li_log1  TYPE REF TO zif_abapgit_log,
          li_log2  TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    mo_dot->add_ignore(
      iv_path     = '/src/'
      iv_filename = 'zcl_test_ignore.clas.*' ).

    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'CLAS'.
    <ls_tadir>-obj_name = 'ZCL_TEST'.
    <ls_tadir>-devclass = '$ZTEST'.

    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'CLAS'.
    <ls_tadir>-obj_name = 'ZCL_TEST_IGNORE'.
    <ls_tadir>-devclass = '$ZTEST'.

    CREATE OBJECT li_log1 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      iv_package          = '$ZTEST'
      it_tadir            = lt_tadir
      ii_log              = li_log1
      iv_force_sequential = abap_true ).

    CREATE OBJECT li_log2 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      iv_package          = '$ZTEST'
      it_tadir            = lt_tadir
      ii_log              = li_log2
      iv_force_sequential = abap_false ).

    lt_msg = li_log1->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object CLAS ZCL_TEST_IGNORE ignored*' ).

    lt_msg = li_log2->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object CLAS ZCL_TEST_IGNORE ignored*' ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_i18n DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_english TYPE sy-langu VALUE 'E',
      c_german  TYPE sy-langu VALUE 'D'.

    DATA:
      mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
      mo_cut         TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      test FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_i18n IMPLEMENTATION.

  METHOD setup.
    DATA ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

    " Main language: English, Translations: German
    ls_data-master_language = c_english.
    " ls_data-i18n_languages needs to be initial to get classic I18N data

    TRY.
        CREATE OBJECT mo_dot_abapgit
          EXPORTING
            is_data = ls_data.

        CREATE OBJECT mo_cut
          EXPORTING
            io_dot_abapgit = mo_dot_abapgit.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_result     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lv_xml        TYPE string,
          lo_input      TYPE REF TO zcl_abapgit_xml_input,
          ls_dd02v      TYPE dd02v,
          lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS: <ls_tadir>      LIKE LINE OF lt_tadir,
                   <ls_result>     LIKE LINE OF lt_result,
                   <ls_i18n_langs> LIKE LINE OF lt_i18n_langs.

    " Assumption: Table T100 has at least English and German description
    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'TABL'.
    <ls_tadir>-obj_name = 'T100'.
    <ls_tadir>-devclass = 'PACKAGE'.
    <ls_tadir>-path     = 'foobar'.

    lt_result = mo_cut->serialize( lt_tadir ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1 ).

    READ TABLE lt_result ASSIGNING <ls_result> INDEX 1.
    ASSERT sy-subrc = 0.

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_result>-file-data ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.

    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'DD02V'
                                          CHANGING  cg_data = ls_dd02v ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd02v-ddlanguage
      exp = c_english ).

    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'I18N_LANGS'
                                          CHANGING  cg_data = lt_i18n_langs ).

    cl_abap_unit_assert=>assert_not_initial( lt_i18n_langs ).

    READ TABLE lt_i18n_langs ASSIGNING <ls_i18n_langs> WITH KEY table_line = c_german.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_determine_batch_size DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      small_repo FOR TESTING,
      several_tasks_per_process FOR TESTING,
      capped FOR TESTING,
      no_processes FOR TESTING,
      assert_batch_size
        IMPORTING
          iv_count     TYPE i
          iv_processes TYPE i
          iv_exp       TYPE i.

ENDCLASS.


CLASS ltcl_determine_batch_size IMPLEMENTATION.

  METHOD setup.
    TRY.
        CREATE OBJECT mo_cut.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
  ENDMETHOD.

  METHOD small_repo.
    assert_batch_size( iv_count     = 30
                       iv_processes = 9
                       iv_exp       = 1 ).
  ENDMETHOD.

  METHOD several_tasks_per_process.
    assert_batch_size( iv_count     = 100
                       iv_processes = 3
                       iv_exp       = 9 ).
  ENDMETHOD.

  METHOD capped.
    assert_batch_size( iv_count     = 1000
                       iv_processes = 9
                       iv_exp       = 20 ).
  ENDMETHOD.

  METHOD no_processes.
    assert_batch_size( iv_count     = 5
                       iv_processes = 0
                       iv_exp       = 2 ).
  ENDMETHOD.

  METHOD assert_batch_size.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->determine_batch_size( iv_count     = iv_count
                                          iv_processes = iv_processes )
      exp = iv_exp ).
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_serialize_batch DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_settings_double   TYPE REF TO ltd_settings,
      mi_previous_settings TYPE REF TO zif_abapgit_persist_settings,
      mo_cut               TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      teardown,
      test FOR TESTING RAISING zcx_abapgit_exception,
      not_existing FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_serialize_batch IMPLEMENTATION.

  METHOD setup.

    mi_previous_settings = zcl_abapgit_persist_factory=>get_settings( ).
    CREATE OBJECT mo_settings_double.
    mo_settings_double->set_experimental_features( zcl_abapgit_serialize=>c_feature_batch ).
    zcl_abapgit_persist_injector=>set_settings( mo_settings_double ).

    TRY.
        CREATE OBJECT mo_cut
          EXPORTING
            io_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.

  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_persist_injector=>set_settings( mi_previous_settings ).
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_sequential TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_parallel   TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object     = 'PROG'.
    <ls_tadir>-obj_name   = 'RSABAPPROGRAM'.
    <ls_tadir>-devclass   = 'PACKAGE'.
    <ls_tadir>-path       = 'foobar'.
    <ls_tadir>-masterlang = sy-langu.

    lt_sequential = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_true ).

    lt_parallel = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_not_initial( lt_parallel ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_sequential
      exp = lt_parallel ).

  ENDMETHOD.

  METHOD not_existing.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt,
          li_log   TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    " In batch mode, serialize( ) checks existence itself.
    " A TADIR row can outlive its object, e.g. until the transport is released
    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object     = 'PROG'.
    <ls_tadir>-obj_name   = 'ZABAPGIT_NOT_EXISTING_OBJECT'.
    <ls_tadir>-devclass   = 'PACKAGE'.
    <ls_tadir>-path       = 'foobar'.
    <ls_tadir>-masterlang = sy-langu.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.
    lt_files = mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log
      iv_force_sequential = abap_true ).

    cl_abap_unit_assert=>assert_initial( lt_files ).
    cl_abap_unit_assert=>assert_equals(
      act = li_log->count( )
      exp = 0 ).

    CREATE OBJECT li_log TYPE zcl_abapgit_log.
    lt_files = mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_initial( lt_files ).
    cl_abap_unit_assert=>assert_equals(
      act = li_log->count( )
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltd_batch_serializer DEFINITION INHERITING FROM zcl_abapgit_serialize FINAL FOR TESTING.

* Batch mode without RFC: run_batch only queues a task, and each wait delivers the oldest
* queued task's callback. Files are faked, one per object

  PUBLIC SECTION.
    TYPES ty_dispatches TYPE STANDARD TABLE OF ty_parallel_objects WITH DEFAULT KEY.

    DATA mt_dispatched TYPE ty_dispatches READ-ONLY.
    DATA mv_batch_size TYPE i VALUE 2.
    " Tasks with several objects fail
    DATA mv_fail_multi TYPE abap_bool.
    " All tasks fail
    DATA mv_fail_all TYPE abap_bool.
    " No task ever finishes
    DATA mv_stall TYPE abap_bool.
    " This many waits deliver nothing before tasks finish again
    DATA mv_stalled_waits TYPE i.

  PROTECTED SECTION.
    METHODS determine_max_processes REDEFINITION.
    METHODS determine_batch_size REDEFINITION.
    METHODS filter_unsupported_objects REDEFINITION.
    METHODS filter_ignored_objects REDEFINITION.
    METHODS run_batch REDEFINITION.
    METHODS wait_for_callback REDEFINITION.

  PRIVATE SECTION.
    DATA mt_pending TYPE STANDARD TABLE OF ty_char32 WITH DEFAULT KEY.

ENDCLASS.


CLASS ltd_batch_serializer IMPLEMENTATION.

  METHOD determine_max_processes.
    rv_processes = 2.
  ENDMETHOD.

  METHOD determine_batch_size.
    rv_size = mv_batch_size.
  ENDMETHOD.

  METHOD filter_unsupported_objects.
    RETURN.
  ENDMETHOD.

  METHOD filter_ignored_objects.
    RETURN.
  ENDMETHOD.

  METHOD run_batch.

    DATA ls_task TYPE ty_task.

    ASSERT mv_free > 0.

    APPEND it_objects TO mt_dispatched.

    ls_task-task    = iv_task.
    ls_task-objects = it_objects.
    INSERT ls_task INTO TABLE mt_tasks.
    APPEND iv_task TO mt_pending.

    mv_free = mv_free - 1.

  ENDMETHOD.

  METHOD wait_for_callback.

    DATA: lv_task    TYPE ty_char32,
          lv_failed  TYPE abap_bool,
          ls_task    TYPE ty_task,
          ls_file    TYPE zif_abapgit_git_definitions=>ty_file,
          lt_results TYPE ty_parallel_results.

    FIELD-SYMBOLS: <ls_object> TYPE ty_parallel_object,
                   <ls_result> TYPE ty_parallel_result.


    IF mv_stall = abap_true.
      RETURN.
    ENDIF.
    IF mv_stalled_waits > 0.
      mv_stalled_waits = mv_stalled_waits - 1.
      RETURN.
    ENDIF.

    READ TABLE mt_pending INTO lv_task INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DELETE mt_pending INDEX 1.

    READ TABLE mt_tasks INTO ls_task WITH TABLE KEY task = lv_task.
    ASSERT sy-subrc = 0.

    IF mv_fail_all = abap_true OR ( mv_fail_multi = abap_true AND lines( ls_task-objects ) > 1 ).
      lv_failed = abap_true.
    ELSE.
      LOOP AT ls_task-objects ASSIGNING <ls_object>.
        APPEND INITIAL LINE TO lt_results ASSIGNING <ls_result>.
        <ls_result>-path = <ls_object>-tadir-path.
        <ls_result>-file_item-item-obj_type = <ls_object>-tadir-object.
        <ls_result>-file_item-item-obj_name = <ls_object>-tadir-obj_name.
        ls_file-filename = <ls_object>-tadir-obj_name.
        APPEND ls_file TO <ls_result>-file_item-files.
      ENDLOOP.
    ENDIF.

    handle_batch_result(
      iv_task    = lv_task
      iv_failed  = lv_failed
      iv_message = 'Task failed'
      it_results = lt_results ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_serialize_batch_tasks DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_settings_double   TYPE REF TO ltd_settings,
      mi_previous_settings TYPE REF TO zif_abapgit_persist_settings,
      mo_cut               TYPE REF TO ltd_batch_serializer,
      mi_log               TYPE REF TO zif_abapgit_log.

    METHODS:
      setup,
      teardown,
      given_programs
        IMPORTING
          iv_count        TYPE i
        RETURNING
          VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt,
      two_objects_one_task FOR TESTING RAISING zcx_abapgit_exception,
      failed_tasks_retried_singly FOR TESTING RAISING zcx_abapgit_exception,
      failed_retries_logged FOR TESTING RAISING zcx_abapgit_exception,
      stalled_tasks_raise FOR TESTING,
      slow_batch_waited_for FOR TESTING RAISING zcx_abapgit_exception,
      slow_single_object_raises FOR TESTING.

ENDCLASS.


CLASS ltcl_serialize_batch_tasks IMPLEMENTATION.

  METHOD setup.

    mi_previous_settings = zcl_abapgit_persist_factory=>get_settings( ).
    CREATE OBJECT mo_settings_double.
    mo_settings_double->set_experimental_features( zcl_abapgit_serialize=>c_feature_batch ).
    zcl_abapgit_persist_injector=>set_settings( mo_settings_double ).

    TRY.
        CREATE OBJECT mo_cut.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.

  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_persist_injector=>set_settings( mi_previous_settings ).
  ENDMETHOD.

  METHOD given_programs.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    DO iv_count TIMES.
      APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-object     = 'PROG'.
      <ls_tadir>-obj_name   = |ZABAPGIT_BATCH_{ sy-index }|.
      <ls_tadir>-devclass   = 'PACKAGE'.
      <ls_tadir>-path       = '/src/'.
      <ls_tadir>-masterlang = sy-langu.
    ENDDO.

  ENDMETHOD.

  METHOD two_objects_one_task.

    DATA: lt_files   TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_objects TYPE zcl_abapgit_serialize=>ty_parallel_objects.

    lt_files = mo_cut->serialize(
      it_tadir = given_programs( 2 )
      ii_log   = mi_log ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mo_cut->mt_dispatched )
      exp = 1 ).
    READ TABLE mo_cut->mt_dispatched INTO lt_objects INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_objects )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 0 ).

  ENDMETHOD.

  METHOD failed_tasks_retried_singly.

    DATA: lt_files   TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_objects TYPE zcl_abapgit_serialize=>ty_parallel_objects.

    " Two tasks of two objects on two processes: the first task fails while the main loop waits
    " for a free process, the second only in the final wait, after the main loop has ended
    mo_cut->mv_fail_multi = abap_true.

    lt_files = mo_cut->serialize(
      it_tadir = given_programs( 4 )
      ii_log   = mi_log ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mo_cut->mt_dispatched )
      exp = 6 ).
    LOOP AT mo_cut->mt_dispatched INTO lt_objects FROM 3.
      cl_abap_unit_assert=>assert_equals(
        act = lines( lt_objects )
        exp = 1 ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 0 ).

  ENDMETHOD.

  METHOD failed_retries_logged.

    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt.

    mo_cut->mv_fail_all = abap_true.

    lt_files = mo_cut->serialize(
      it_tadir = given_programs( 2 )
      ii_log   = mi_log ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mo_cut->mt_dispatched )
      exp = 3 ).
    cl_abap_unit_assert=>assert_initial( lt_files ).
    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 2 ).

  ENDMETHOD.

  METHOD stalled_tasks_raise.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception,
          lv_found TYPE abap_bool.

    mo_cut->mv_stall = abap_true.

    TRY.
        mo_cut->serialize(
          it_tadir = given_programs( 4 )
          ii_log   = mi_log ).
        cl_abap_unit_assert=>fail( 'Incomplete serialization must raise' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        lv_found = boolc( lx_error->get_text( ) CS '4 objects outstanding' ).
        cl_abap_unit_assert=>assert_equals(
          act = lv_found
          exp = abap_true ).
    ENDTRY.

  ENDMETHOD.

  METHOD slow_batch_waited_for.

    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt.

    " Tasks of two objects get two waits; per-object mode would wait as long
    mo_cut->mv_stalled_waits = 1.

    lt_files = mo_cut->serialize(
      it_tadir = given_programs( 4 )
      ii_log   = mi_log ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 4 ).

  ENDMETHOD.

  METHOD slow_single_object_raises.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception,
          lv_found TYPE abap_bool.

    " Tasks of one object get one wait, as in per-object mode
    mo_cut->mv_batch_size    = 1.
    mo_cut->mv_stalled_waits = 1.

    TRY.
        mo_cut->serialize(
          it_tadir = given_programs( 2 )
          ii_log   = mi_log ).
        cl_abap_unit_assert=>fail( 'Incomplete serialization must raise' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        lv_found = boolc( lx_error->get_text( ) CS '2 objects outstanding' ).
        cl_abap_unit_assert=>assert_equals(
          act = lv_found
          exp = abap_true ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
