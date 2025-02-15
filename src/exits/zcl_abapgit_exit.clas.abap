CLASS zcl_abapgit_exit DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_abapgit_injector.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_exit.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_exit) TYPE REF TO zif_abapgit_exit.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_global_exit TYPE REF TO zif_abapgit_exit.
    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_exit.

    CLASS-METHODS is_running_in_test_context
      RETURNING
        VALUE(rv_running_in_test_context) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_exit IMPLEMENTATION.


  METHOD get_instance.

    DATA lv_class_name TYPE string.

    IF gi_global_exit IS NOT INITIAL.
      ri_exit = gi_global_exit.
      RETURN.
    ENDIF.

    lv_class_name = 'ZCL_ABAPGIT_USER_EXIT'.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of exit handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    " Prevent non-mocked exit calls in unit tests
    IF is_running_in_test_context( ) = abap_false.
      TRY.
          CREATE OBJECT gi_exit TYPE (lv_class_name).
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT gi_global_exit TYPE zcl_abapgit_exit. " this class

    ri_exit = gi_global_exit.

  ENDMETHOD.


  METHOD is_running_in_test_context.

    IF sy-sysid = 'ABC'.
      " always run on open-abap
      rv_running_in_test_context = abap_true.
      RETURN.
    ENDIF.

    " Check if the local test class can be accessed by RTTI. If so the current process is running in a unit test.
    " Note this approach only works for the developer version. The standalone version will always report not running in
    " test context which should be fine as there are no unit tests delivered in it.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = |\\PROGRAM={ sy-repid }\\CLASS=LTCL_TEST|
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    rv_running_in_test_context = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_commit_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->adjust_display_commit_url(
            EXPORTING
              iv_repo_url    = iv_repo_url
              iv_repo_name   = iv_repo_name
              iv_repo_key    = iv_repo_key
              iv_commit_hash = iv_commit_hash
            CHANGING
              cv_display_url = cv_display_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_filename.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_filename = gi_exit->adjust_display_filename(
            is_repo_meta = is_repo_meta
            iv_filename  = iv_filename ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_filename IS INITIAL.
      rv_filename = iv_filename.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_allowed = gi_exit->allow_sap_objects( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_max_parallel_processes.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_max_parallel_processes(
            EXPORTING
              iv_package       = iv_package
            CHANGING
              cv_max_processes = cv_max_processes ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_authentication(
            EXPORTING
              iv_repo_url             = iv_repo_url
            CHANGING
              cv_proxy_authentication = cv_proxy_authentication ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_port(
            EXPORTING
              iv_repo_url   = iv_repo_url
            CHANGING
              cv_proxy_port = cv_proxy_port ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_url(
            EXPORTING
              iv_repo_url  = iv_repo_url
            CHANGING
              cv_proxy_url = cv_proxy_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_rfc_server_group.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_rfc_server_group( CHANGING cv_group = cv_group ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_data_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_data_objects( CHANGING ct_objects = ct_objects ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_object_types.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_object_types( CHANGING ct_types = ct_types ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_tadir(
            EXPORTING
              iv_package            = iv_package
              ii_log                = ii_log
              is_dot_abapgit        = is_dot_abapgit
              iv_ignore_subpackages = iv_ignore_subpackages
              iv_only_local_objects = iv_only_local_objects
            CHANGING
              ct_tadir              = ct_tadir ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          ri_client = gi_exit->create_http_client( iv_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    " This exit might be called twice per object
    " 1st call: it_source = initial
    "    Can be used for serializing complete source
    "    If source is returned, there will be no second call
    " 2nd call: it_source = code as serialized by abapGit
    "    Can be used for post-processing of source
    IF gi_exit IS NOT INITIAL.
      TRY.
          rt_source = gi_exit->custom_serialize_abap_clif(
            is_class_key = is_class_key
            it_source    = it_source ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rt_source IS INITIAL.
      rt_source = it_source.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->deserialize_postprocess( is_step = is_step
                                            ii_log  = ii_log ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~determine_transport_request.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->determine_transport_request(
            EXPORTING
              io_repo              = io_repo
              iv_transport_type    = iv_transport_type
            CHANGING
              cv_transport_request = cv_transport_request ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~enhance_any_toolbar.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->enhance_any_toolbar( io_menu ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~enhance_repo_toolbar.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->enhance_repo_toolbar(
            io_menu = io_menu
            iv_key  = iv_key
            iv_act  = iv_act ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->get_ci_tests(
            EXPORTING
              iv_object   = iv_object
            CHANGING
              ct_ci_repos = ct_ci_repos ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_ssl_id = gi_exit->get_ssl_id( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->http_client(
            iv_url    = iv_url
            ii_client = ii_client ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~on_event.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rs_handled = gi_exit->on_event( ii_event ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~pre_calculate_repo_status.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->pre_calculate_repo_status(
            EXPORTING
              is_repo_meta = is_repo_meta
            CHANGING
              ct_local     = ct_local
              ct_remote    = ct_remote ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~serialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->serialize_postprocess(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_files   = ct_files ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~validate_before_push.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->validate_before_push(
            is_comment = is_comment
            io_stage   = io_stage
            io_repo    = io_repo ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_list.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_list( ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_repo.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_repo(
            is_repo_meta = is_repo_meta
            ii_html      = ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
