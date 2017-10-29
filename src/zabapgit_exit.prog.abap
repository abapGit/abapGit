*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_USER_EXITS
*&---------------------------------------------------------------------*

INTERFACE lif_exit.

  METHODS:
    change_local_host
      CHANGING ct_hosts TYPE zif_abapgit_definitions=>ty_icm_sinfo2_tt,
    allow_sap_objects
      RETURNING VALUE(rv_allowed) TYPE abap_bool,
    change_proxy_url
      IMPORTING iv_repo_url TYPE csequence
      CHANGING  c_proxy_url TYPE string,
    change_proxy_port
      IMPORTING iv_repo_url  TYPE csequence
      CHANGING  c_proxy_port TYPE string,
    change_proxy_authentication
      IMPORTING iv_repo_url            TYPE csequence
      CHANGING  c_proxy_authentication TYPE abap_bool.


ENDINTERFACE.

* add class LCL_USER_EXIT implementing LIF_EXIT in following include,
* place the include in a different package than ZABAPGIT
INCLUDE zabapgit_user_exit IF FOUND.

*******************

CLASS lcl_exit DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ri_exit) TYPE REF TO lif_exit.

    INTERFACES: lif_exit.

ENDCLASS.

CLASS lcl_exit IMPLEMENTATION.

  METHOD get_instance.

    TRY.
        CREATE OBJECT ri_exit TYPE ('LCL_USER_EXIT').
      CATCH cx_sy_create_object_error.
        CREATE OBJECT ri_exit TYPE lcl_exit.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_exit~change_local_host.
* default behavior
    RETURN.
  ENDMETHOD.

  METHOD lif_exit~allow_sap_objects.
    rv_allowed = abap_false.
  ENDMETHOD.


  METHOD lif_exit~change_proxy_url.
* default behavior change nothing
    RETURN.
  ENDMETHOD.

  METHOD lif_exit~change_proxy_port.
* default behavior change nothing
    RETURN.
  ENDMETHOD.

  METHOD lif_exit~change_proxy_authentication.
* default behavior change nothing
    RETURN.
  ENDMETHOD.

ENDCLASS.
