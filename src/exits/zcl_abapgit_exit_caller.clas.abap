CLASS zcl_abapgit_exit_caller DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_exit.

    CLASS-METHODS init.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_exit_super TYPE seoclsname VALUE 'ZCL_ABAPGIT_EXIT_SUPER'.

    TYPES:
      ty_instance TYPE REF TO zif_abapgit_exit.

    CLASS-DATA:
      gv_init  TYPE abap_bool,
      gi_exit  TYPE ty_instance,
      gt_exits TYPE STANDARD TABLE OF ty_instance WITH DEFAULT KEY.

ENDCLASS.



CLASS zcl_abapgit_exit_caller IMPLEMENTATION.


  METHOD init.

    DATA lv_clsname TYPE seoclsname.

    IF gv_init = abap_true.
      RETURN.
    ENDIF.

    " Get all classes that inherit from abapGit default exit implementation
    SELECT clsname FROM seometarel INTO lv_clsname
      WHERE refclsname = c_exit_super AND reltype = '2'.

      TRY.
          CREATE OBJECT gi_exit TYPE (lv_clsname).
          INSERT gi_exit INTO TABLE gt_exits.
        CATCH cx_root.
          CONTINUE. "ignore
      ENDTRY.
    ENDSELECT.

    gv_init = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_commit_url.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
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
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_filename.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          rv_filename = gi_exit->adjust_display_filename( iv_filename ).

          IF rv_filename IS NOT INITIAL.
            RETURN. ">>>
          ENDIF.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    IF rv_filename IS INITIAL.
      rv_filename = iv_filename.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    init( ).

    " First call wins
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          rv_allowed = gi_exit->allow_sap_objects( ).

          IF rv_allowed IS NOT INITIAL.
            RETURN. ">>>
          ENDIF.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_proxy_authentication(
            EXPORTING
              iv_repo_url             = iv_repo_url
            CHANGING
              cv_proxy_authentication = cv_proxy_authentication ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_proxy_port(
            EXPORTING
              iv_repo_url   = iv_repo_url
            CHANGING
              cv_proxy_port = cv_proxy_port ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_proxy_url(
            EXPORTING
              iv_repo_url  = iv_repo_url
            CHANGING
              cv_proxy_url = cv_proxy_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_data_objects.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_supported_data_objects( CHANGING ct_objects = ct_objects ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_object_types.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_supported_object_types( CHANGING ct_types = ct_types ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->change_tadir(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_tadir   = ct_tadir ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    init( ).

    " First call wins
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          ri_client = gi_exit->create_http_client( iv_url ).

          IF ri_client IS NOT INITIAL.
            RETURN. ">>>
          ENDIF.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    init( ).

    " First call wins

    " This exit might be called twice per object
    " 1st call: it_source = initial
    "    Can be used for serializing complete source
    "    If source is returned, there will be no second call
    " 2nd call: it_source = code as serialized by abapGit
    "    Can be used for post-processing of source
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          rt_source = gi_exit->custom_serialize_abap_clif(
            is_class_key = is_class_key
            it_source    = it_source ).

          IF rt_source IS NOT INITIAL.
            RETURN. ">>>
          ENDIF.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    IF rt_source IS INITIAL.
      rt_source = it_source.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->deserialize_postprocess( is_step = is_step
                                            ii_log  = ii_log ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~determine_transport_request.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->determine_transport_request(
            EXPORTING
              io_repo              = io_repo
              iv_transport_type    = iv_transport_type
            CHANGING
              cv_transport_request = cv_transport_request ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~enhance_repo_toolbar.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->enhance_repo_toolbar(
            io_menu = io_menu
            iv_key  = iv_key
            iv_act  = iv_act ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->get_ci_tests(
            EXPORTING
              iv_object   = iv_object
            CHANGING
              ct_ci_repos = ct_ci_repos ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

    init( ).

    " First call wins
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          rv_ssl_id = gi_exit->get_ssl_id( ).

          IF rv_ssl_id IS NOT INITIAL.
            RETURN. ">>>
          ENDIF.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->http_client(
            iv_url    = iv_url
            ii_client = ii_client ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~on_event.

    init( ).

    " First call wins
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          rs_handled = gi_exit->on_event( ii_event ).

          IF rs_handled IS NOT INITIAL.
            RETURN. ">>>
          ENDIF.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~pre_calculate_repo_status.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->pre_calculate_repo_status(
            EXPORTING
              is_repo_meta = is_repo_meta
            CHANGING
              ct_local     = ct_local
              ct_remote    = ct_remote ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~serialize_postprocess.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->serialize_postprocess(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_files   = ct_files ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~validate_before_push.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->validate_before_push(
            is_comment = is_comment
            io_stage   = io_stage
            io_repo    = io_repo ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_list.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->wall_message_list( ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_repo.

    init( ).

    " Multiple calls possible
    LOOP AT gt_exits INTO gi_exit.
      TRY.
          gi_exit->wall_message_repo(
            is_repo_meta = is_repo_meta
            ii_html      = ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
