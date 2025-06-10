CLASS ltcl_determine_max_processes DEFINITION DEFERRED.
CLASS ltcl_determine_server_group DEFINITION DEFERRED.
CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS ltcl_determine_max_processes ltcl_determine_server_group.

CLASS ltd_settings DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_persist_settings.

    METHODS:
      set_parallel_proc_disabled
        IMPORTING
          iv_parallel_proc_disabled TYPE abap_bool.

  PRIVATE SECTION.
    DATA:
      mv_parallel_proc_disabled TYPE zif_abapgit_persist_user=>ty_s_user_settings-parallel_proc_disabled.

ENDCLASS.


CLASS ltd_settings IMPLEMENTATION.

  METHOD zif_abapgit_persist_settings~modify.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
    ro_settings->set_parallel_proc_disabled( mv_parallel_proc_disabled ).
  ENDMETHOD.

  METHOD set_parallel_proc_disabled.
    mv_parallel_proc_disabled = iv_parallel_proc_disabled.
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

  METHOD zif_abapgit_exit~adjust_display_commit_url.
  ENDMETHOD.

  METHOD zif_abapgit_exit~change_committer_info.
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

  METHOD zif_abapgit_exit~pre_calculate_repo_status.
  ENDMETHOD.

  METHOD zif_abapgit_exit~serialize_postprocess.
  ENDMETHOD.

  METHOD zif_abapgit_exit~validate_before_push.
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
      mo_dot TYPE REF TO zcl_abapgit_dot_abapgit,
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      test FOR TESTING RAISING zcx_abapgit_exception,
      unsupported FOR TESTING RAISING zcx_abapgit_exception,
      ignored FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_serialize IMPLEMENTATION.

  METHOD setup.

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    TRY.
        CREATE OBJECT mo_cut
          EXPORTING
            io_dot_abapgit = mo_dot.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
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
