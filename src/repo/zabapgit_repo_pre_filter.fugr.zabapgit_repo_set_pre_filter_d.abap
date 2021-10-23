FUNCTION zabapgit_repo_set_pre_filter_d .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  RAISING
*"      ZCX_ABAPGIT_EXCEPTION
*"----------------------------------------------------------------------

  CALL SELECTION-SCREEN 1001 STARTING AT 5 5 ENDING AT 100 8.
  IF sy-subrc = 0.
    zcl_abapgit_repo_pre_filter=>get_instance( )->set_filter_values( it_r_trkorr = s_trkorr[]  ).
  ELSE.
    zcx_abapgit_exception=>raise( 'Pre-Filter Canceled' ).
  ENDIF.

ENDFUNCTION.
