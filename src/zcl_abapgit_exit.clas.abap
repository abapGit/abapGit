CLASS zcl_abapgit_exit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ri_exit) TYPE REF TO zif_abapgit_exit.

    INTERFACES: zif_abapgit_exit.

ENDCLASS.



CLASS ZCL_ABAPGIT_EXIT IMPLEMENTATION.


  METHOD get_instance.

    TRY.
        CREATE OBJECT ri_exit TYPE ('ZCL_ABAPGIT_USER_EXIT').
      CATCH cx_sy_create_object_error.
        CREATE OBJECT ri_exit TYPE zcl_abapgit_exit.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.
    rv_allowed = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.
* default behavior
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.
* default behavior change nothing
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.
* default behavior change nothing
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.
* default behavior change nothing
    RETURN.
  ENDMETHOD.
ENDCLASS.
