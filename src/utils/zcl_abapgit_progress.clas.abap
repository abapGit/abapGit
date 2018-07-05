CLASS zcl_abapgit_progress DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS show
      IMPORTING
        VALUE(iv_current) TYPE i
        !iv_text          TYPE csequence .
    METHODS constructor
      IMPORTING
        !iv_total TYPE i .
  PROTECTED SECTION.

    DATA mv_total TYPE i .

    METHODS calc_pct
      IMPORTING
        !iv_current   TYPE i
      RETURNING
        VALUE(rv_pct) TYPE i .
private section.

  data MV_CV_TIME_NEXT type SY-UZEIT .
  data MV_CV_DATUM_NEXT type SY-DATUM .
ENDCLASS.



CLASS ZCL_ABAPGIT_PROGRESS IMPLEMENTATION.


  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mv_total = iv_total.

  ENDMETHOD.


  METHOD show.

    DATA: lv_pct  TYPE i.
    DATA: lv_time TYPE t.

    CONSTANTS: c_wait_secs TYPE i VALUE 2.

    lv_pct = calc_pct( iv_current ).

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time  >= mv_cv_time_next  AND sy-datum = mv_cv_datum_next  OR
       sy-datum >  mv_cv_datum_next.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.
      mv_cv_time_next = lv_time + c_wait_secs.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
