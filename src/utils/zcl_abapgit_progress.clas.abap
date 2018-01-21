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
  PRIVATE SECTION.
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

    DATA: lv_pct TYPE i.

    lv_pct = calc_pct( iv_current ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_text.

  ENDMETHOD.
ENDCLASS.
