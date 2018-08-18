CLASS zcl_abapgit_exit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ri_exit) TYPE REF TO zif_abapgit_exit.

    INTERFACES: zif_abapgit_exit.

  PRIVATE SECTION.

    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_exit .
ENDCLASS.



CLASS zcl_abapgit_exit IMPLEMENTATION.


  METHOD get_instance.

    IF gi_exit IS INITIAL.
      TRY.
          CREATE OBJECT gi_exit TYPE ('ZCL_ABAPGIT_USER_EXIT').
        CATCH cx_sy_create_object_error.
      ENDTRY.
    ENDIF.

    CREATE OBJECT ri_exit TYPE zcl_abapgit_exit.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    TRY.
        rv_allowed = gi_exit->allow_sap_objects( ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    TRY.
        gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    TRY.
        gi_exit->change_proxy_authentication(
          EXPORTING
            iv_repo_url            = iv_repo_url
          CHANGING
            cv_proxy_authentication = cv_proxy_authentication ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    TRY.
        gi_exit->change_proxy_port(
          EXPORTING
            iv_repo_url  = iv_repo_url
          CHANGING
            cv_proxy_port = cv_proxy_port ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    TRY.
        gi_exit->change_proxy_url(
          EXPORTING
            iv_repo_url = iv_repo_url
          CHANGING
            cv_proxy_url = cv_proxy_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    TRY.
        ri_client = gi_exit->create_http_client( iv_url ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    TRY.
        gi_exit->http_client( ii_client ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_exit~change_tadir.

    TRY.
        gi_exit->change_tadir(
          EXPORTING
            iv_package = iv_package
            io_log     = io_log
          CHANGING
            ct_tadir   = ct_tadir ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
