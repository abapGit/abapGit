CLASS zcl_abapgit_exit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ri_exit) TYPE REF TO zif_abapgit_exit.

    INTERFACES: zif_abapgit_exit.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_exit .
ENDCLASS.



CLASS zcl_abapgit_exit IMPLEMENTATION.


  METHOD get_instance.

    IF gi_exit IS INITIAL.
      TRY.
          CREATE OBJECT gi_exit TYPE ('ZCL_ABAPGIT_USER_EXIT').
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT ri_exit TYPE zcl_abapgit_exit.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    TRY.
        rv_allowed = gi_exit->allow_sap_objects( ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    TRY.
        gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    TRY.
        gi_exit->change_proxy_authentication(
          EXPORTING
            iv_repo_url             = iv_repo_url
          CHANGING
            cv_proxy_authentication = cv_proxy_authentication ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    TRY.
        gi_exit->change_proxy_port(
          EXPORTING
            iv_repo_url   = iv_repo_url
          CHANGING
            cv_proxy_port = cv_proxy_port ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    TRY.
        gi_exit->change_proxy_url(
          EXPORTING
            iv_repo_url  = iv_repo_url
          CHANGING
            cv_proxy_url = cv_proxy_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

    TRY.
        gi_exit->change_tadir(
          EXPORTING
            iv_package = iv_package
            ii_log     = ii_log
          CHANGING
            ct_tadir   = ct_tadir ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    TRY.
        ri_client = gi_exit->create_http_client( iv_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.
    TRY.
        rt_source = gi_exit->custom_serialize_abap_clif( is_class_key ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.

    TRY.
        gi_exit->deserialize_postprocess( is_step = is_step
                                          ii_log  = ii_log ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.

    TRY.
        gi_exit->get_ci_tests(
          EXPORTING
            iv_object   = iv_object
          CHANGING
            ct_ci_repos = ct_ci_repos ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

    TRY.
        rv_ssl_id = gi_exit->get_ssl_id( ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    TRY.
        gi_exit->http_client(
          iv_url    = iv_url
          ii_client = ii_client ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_commit_url.

    TRY.
        gi_exit->adjust_display_commit_url(
          EXPORTING
            iv_repo_url           = iv_repo_url
            iv_repo_name          = iv_repo_name
            iv_repo_key           = iv_repo_key
            iv_commit_hash        = iv_commit_hash
          CHANGING
            cv_display_url        = cv_display_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
