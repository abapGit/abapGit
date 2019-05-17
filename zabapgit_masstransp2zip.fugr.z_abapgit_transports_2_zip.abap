FUNCTION z_abapgit_transports_2_zip.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

* Call transport selection popup
  CALL SELECTION-SCREEN 100 STARTING AT 5 5.
  IF sy-subrc = 0.
    PERFORM f_main.
  ENDIF.

ENDFUNCTION.
