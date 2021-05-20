CLASS lcl_exit_deinterface_proxy DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_exit.

    CLASS-METHODS get_instance IMPORTING iv_classname     TYPE seoclsname
                               RETURNING VALUE(ri_result) TYPE REF TO zif_abapgit_exit.




  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_obj TYPE REF TO object.
    DATA lx_error TYPE REF TO cx_static_check.

ENDCLASS.

CLASS lcl_exit_deinterface_proxy IMPLEMENTATION.


  METHOD get_instance.

    DATA lo_proxy TYPE REF TO lcl_exit_deinterface_proxy.

    CREATE OBJECT lo_proxy.
    TRY.
        CREATE OBJECT lo_proxy->mo_obj TYPE (iv_classname).
      CATCH cx_sy_create_object_error ##NO_HANDLER.
    ENDTRY.

    ri_result = lo_proxy.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_commit_url.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('ADJUST_DISPLAY_COMMIT_URL')
          EXPORTING
            iv_repo_url    = iv_repo_url
            iv_repo_name   = iv_repo_name
            iv_repo_key    = iv_repo_key
            iv_commit_hash = iv_commit_hash
          CHANGING
            cv_display_url = cv_display_url.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.

      CATCH cx_static_check INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('ALLOW_SAP_OBJECTS')
          RECEIVING
            rv_allowed = rv_allowed.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CHANGE_LOCAL_HOST')
          CHANGING
            ct_hosts = ct_hosts.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CHANGE_PROXY_AUTHENTICATION')
          EXPORTING
            iv_repo_url             = iv_repo_url
          CHANGING
            cv_proxy_authentication = cv_proxy_authentication.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CHANGE_PROXY_PORT')
          EXPORTING
            iv_repo_url   = iv_repo_url
          CHANGING
            cv_proxy_port = cv_proxy_port.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CHANGE_PROXY_URL')
          EXPORTING
            iv_repo_url  = iv_repo_url
          CHANGING
            cv_proxy_url = cv_proxy_url.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CHANGE_TADIR')
          EXPORTING
            iv_package = iv_package
            ii_log     = ii_log
          CHANGING
            ct_tadir   = ct_tadir.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CREATE_HTTP_CLIENT')
          EXPORTING
            iv_url    = iv_url
          RECEIVING
            ri_client = ri_client.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.

      CATCH cx_static_check INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('CUSTOM_SERIALIZE_ABAP_CLIF')
          EXPORTING
            is_class_key = is_class_key
          RECEIVING
            rt_source    = rt_source.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.

      CATCH cx_static_check INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('DESERIALIZE_POSTPROCESS')
          EXPORTING
            is_step = is_step
            ii_log  = ii_log.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('GET_CI_TESTS')
          EXPORTING
            iv_object   = iv_object
          CHANGING
            ct_ci_repos = ct_ci_repos.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('GET_SSL_ID')
          RECEIVING
            rv_ssl_id = rv_ssl_id.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('HTTP_CLIENT')
          EXPORTING
            iv_url    = iv_url
            ii_client = ii_client.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~on_event.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('ON_EVENT')
          EXPORTING
            ii_event   = ii_event
          RECEIVING
            rs_handled = rs_handled.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.

      CATCH cx_static_check INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~pre_calculate_repo_status.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('PRE_CALCULATE_REPO_STATUS')
          EXPORTING
            is_repo_meta = is_repo_meta
          CHANGING
            ct_local     = ct_local
            ct_remote    = ct_remote.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.

      CATCH cx_static_check INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_list.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('WALL_MESSAGE_LIST')
          EXPORTING
            ii_html = ii_html.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_repo.

    CHECK mo_obj IS BOUND.

    TRY.
        CALL METHOD mo_obj->('WALL_MESSAGE_REPO')
          EXPORTING
            is_repo_meta = is_repo_meta
            ii_html      = ii_html.

      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
