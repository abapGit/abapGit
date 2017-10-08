*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_USER_EXITS
*&---------------------------------------------------------------------*

INTERFACE lif_exit.

  METHODS:
    change_local_host
      CHANGING ct_hosts TYPE ZIF_ABAPGIT_DEFINITIONS=>ty_icm_sinfo2_tt,
    allow_sap_objects
      RETURNING VALUE(rv_allowed) TYPE abap_bool.

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

ENDCLASS.
