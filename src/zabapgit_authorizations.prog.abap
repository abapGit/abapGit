*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_AUTHORIZATIONS
*&---------------------------------------------------------------------*

INTERFACE lif_auth.
  TYPES: ty_authorization TYPE string.

  CONSTANTS: BEGIN OF gc_authorization,
               uninstall TYPE ty_authorization VALUE 'UNINSTALL',
             END OF gc_authorization.

  METHODS:
    is_allowed
      IMPORTING iv_authorization  TYPE ty_authorization
                iv_param          TYPE string OPTIONAL
      RETURNING VALUE(rv_allowed) TYPE abap_bool.
ENDINTERFACE.

* implement class LCL_AUTH_EXIT implementing LIF_AUTH in following include,
* place the include in a different package than ZABAPGIT
INCLUDE zabapgit_authorizations_exit IF FOUND.

*******************

CLASS lcl_auth DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      is_allowed
        IMPORTING iv_authorization  TYPE lif_auth=>ty_authorization
                  iv_param          TYPE string OPTIONAL
        RETURNING VALUE(rv_allowed) TYPE abap_bool.

ENDCLASS.

CLASS lcl_auth IMPLEMENTATION.

  METHOD is_allowed.

    DATA: li_auth TYPE REF TO lif_auth.

    TRY.
        CREATE OBJECT li_auth TYPE ('LCL_AUTH_EXIT').
        rv_allowed = li_auth->is_allowed( iv_authorization = iv_authorization
                                          iv_param         = iv_param ).
      CATCH cx_sy_create_object_error.
        rv_allowed = abap_true.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
