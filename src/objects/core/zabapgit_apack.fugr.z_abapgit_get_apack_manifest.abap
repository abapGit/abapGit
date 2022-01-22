FUNCTION z_abapgit_get_apack_manifest.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CLSNAME) TYPE  SEOCLSNAME
*"  EXPORTING
*"     VALUE(EV_PROVIDER_SERIALIZED) TYPE  XSTRING
*"----------------------------------------------------------------------
  DATA: lo_provider TYPE REF TO zcl_abapgit_apack_provider_prx.

  lo_provider = zcl_abapgit_apack_provider_prx=>get_provider( iv_clsname ).

  IF lo_provider IS BOUND.
    CALL TRANSFORMATION id SOURCE root = lo_provider RESULT XML ev_provider_serialized.
  ENDIF.

ENDFUNCTION.
