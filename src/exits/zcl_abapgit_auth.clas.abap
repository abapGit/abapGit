CLASS zcl_abapgit_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_allowed
      IMPORTING
        !iv_authorization TYPE zif_abapgit_auth=>ty_authorization
        !iv_param         TYPE string OPTIONAL
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_auth IMPLEMENTATION.


  METHOD is_allowed.

    DATA: li_auth TYPE REF TO zif_abapgit_auth.

    TRY.
        CREATE OBJECT li_auth TYPE ('ZCL_ABAPGIT_AUTH_EXIT').
        rv_allowed = li_auth->is_allowed( iv_authorization = iv_authorization
                                          iv_param         = iv_param ).
      CATCH cx_sy_create_object_error.
        rv_allowed = abap_true.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
