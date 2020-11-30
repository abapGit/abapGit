CLASS zcl_abapgit_progress DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_progress .

    CLASS-METHODS set_instance
      IMPORTING
        !ii_progress TYPE REF TO zif_abapgit_progress .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_total          TYPE i
      RETURNING
        VALUE(ri_progress) TYPE REF TO zif_abapgit_progress .
  PROTECTED SECTION.

    DATA mv_total TYPE i .
    CLASS-DATA gi_progress TYPE REF TO zif_abapgit_progress .

    METHODS calc_pct
      IMPORTING
        !iv_current   TYPE i
      RETURNING
        VALUE(rv_pct) TYPE i .
  PRIVATE SECTION.

    DATA mv_cv_time_next TYPE sy-uzeit .
    DATA mv_cv_datum_next TYPE sy-datum .
ENDCLASS.



CLASS zcl_abapgit_progress IMPLEMENTATION.


  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ELSEIF rv_pct = 0.
      rv_pct = 1.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

* max one progress indicator at a time is supported

    IF gi_progress IS INITIAL.
      CREATE OBJECT gi_progress TYPE zcl_abapgit_progress.
    ENDIF.

    gi_progress->set_total( iv_total ).

    ri_progress = gi_progress.

  ENDMETHOD.


  METHOD set_instance.

    gi_progress = ii_progress.

  ENDMETHOD.


  METHOD zif_abapgit_progress~off.

    " Clear the status bar
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  ENDMETHOD.


  METHOD zif_abapgit_progress~set_total.

    mv_total = iv_total.

    CLEAR mv_cv_time_next.
    CLEAR mv_cv_datum_next.

  ENDMETHOD.


  METHOD zif_abapgit_progress~show.

    DATA: lv_pct  TYPE i,
          lv_time TYPE t.

    CONSTANTS: lc_wait_secs TYPE i VALUE 2.

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time >= mv_cv_time_next
        AND sy-datum = mv_cv_datum_next
        OR sy-datum > mv_cv_datum_next.

      lv_pct = calc_pct( iv_current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.
      mv_cv_time_next = lv_time + lc_wait_secs.

    ENDIF.
    IF sy-datum > mv_cv_datum_next.
      mv_cv_datum_next = sy-datum.
    ENDIF.
    IF mv_cv_time_next < lv_time.
      mv_cv_datum_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
