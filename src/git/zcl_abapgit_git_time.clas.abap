CLASS zcl_abapgit_git_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_unixtime TYPE c LENGTH 16 .

    CLASS-METHODS get_unix
      RETURNING
        VALUE(rv_time) TYPE ty_unixtime
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_utc
      IMPORTING
        !iv_unix TYPE ty_unixtime
      EXPORTING
        !ev_date TYPE sy-datum
        !ev_time TYPE sy-uzeit .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_git_time IMPLEMENTATION.


  METHOD get_unix.
* returns seconds since unix epoch, including timezone indicator

    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.
    DATA lv_time TYPE timestamp.
    DATA lv_seconds TYPE i.

    GET TIME STAMP FIELD lv_time.

    lv_seconds = cl_abap_tstmp=>subtract(
      tstmp1 = lv_time
      tstmp2 = lc_epoch ).

    rv_time = lv_seconds.
    CONDENSE rv_time.
    rv_time+11 = '+000000'.

  ENDMETHOD.


  METHOD get_utc.

    CONSTANTS lc_epoch TYPE d VALUE '19700101'.

    DATA: lv_i       TYPE i,
          lv_utcdiff TYPE t,
          lv_utcsign TYPE c LENGTH 1.


    lv_i = iv_unix(10).
    lv_utcsign = iv_unix+11.
    lv_utcdiff = iv_unix+12.

    " GMT + time-zone
    CASE lv_utcsign.
      WHEN '+'.
        lv_i = lv_i + lv_utcdiff.
      WHEN '-'.
        lv_i = lv_i - lv_utcdiff.
    ENDCASE.

    ev_time = lv_i MOD 86400.
    lv_i = lv_i - ev_time.
    lv_i = lv_i / 86400.
    ev_date = lv_i + lc_epoch.

  ENDMETHOD.
ENDCLASS.
