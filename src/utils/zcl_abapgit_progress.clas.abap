CLASS zcl_abapgit_progress DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      show
        IMPORTING
          !iv_key           TYPE string
          VALUE(iv_current) TYPE i
          !iv_total         TYPE i
          !iv_text          TYPE csequence .

  PRIVATE SECTION.
    CLASS-METHODS:
      calc_pct
        IMPORTING
          !iv_current   TYPE i
          !iv_total     TYPE i
        RETURNING
          VALUE(rv_pct) TYPE i .
ENDCLASS.



CLASS zcl_abapgit_progress IMPLEMENTATION.


  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / iv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ENDIF.

  ENDMETHOD.


  METHOD show.

    DATA: lv_pct  TYPE i,
          lv_text TYPE string.

    lv_pct = calc_pct( iv_current = iv_current
                       iv_total   = iv_total ).
    CONCATENATE iv_key '-' iv_text INTO lv_text SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = lv_text.

  ENDMETHOD.

ENDCLASS.
