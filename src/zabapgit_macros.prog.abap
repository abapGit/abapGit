*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_MACROS
*&---------------------------------------------------------------------*

* Macros

DEFINE _object_check_timestamp.
  IF sy-subrc = 0 AND &1 IS NOT INITIAL AND &2 IS NOT INITIAL.
    cl_abap_tstmp=>systemtstmp_syst2utc(
      EXPORTING syst_date = &1
                syst_time = &2
      IMPORTING utc_tstmp = lv_ts ).
    IF lv_ts < iv_timestamp.
      rv_changed = abap_false. " Unchanged
    ELSE.
      rv_changed = abap_true.
      RETURN.
    ENDIF.
  ELSE. " Not found? => changed
    rv_changed = abap_true.
    RETURN.
  ENDIF.
END-OF-DEFINITION.
