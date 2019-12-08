CLASS zcl_abapgit_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_unixtime TYPE c LENGTH 16.

    CLASS-METHODS get_unix
      IMPORTING iv_date        TYPE sydatum DEFAULT sy-datum
                iv_time        TYPE syuzeit DEFAULT sy-uzeit
      RETURNING VALUE(rv_time) TYPE ty_unixtime
      RAISING   zcx_abapgit_exception.
    CLASS-METHODS get_utc
      IMPORTING iv_unix TYPE ty_unixtime
      EXPORTING ev_date TYPE sydatum
                ev_time TYPE syuzeit.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_epoch TYPE d VALUE '19700101'.
ENDCLASS.



CLASS zcl_abapgit_time IMPLEMENTATION.


  METHOD get_unix.

    DATA: lv_i       TYPE i,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.


    lv_i = iv_date - c_epoch.
    lv_i = lv_i * 86400.
    lv_i = lv_i + iv_time.

    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = lv_tz.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = lv_tz
        if_local_date    = iv_date
        if_local_time    = iv_time
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Timezone error' ).
    ENDIF.

    CASE lv_utcsign.
      WHEN '+'.
        lv_i = lv_i - lv_utcdiff.
      WHEN '-'.
        lv_i = lv_i + lv_utcdiff.
    ENDCASE.

    rv_time = lv_i.
    CONDENSE rv_time.
    rv_time+11 = lv_utcsign.
    rv_time+12 = lv_utcdiff.

  ENDMETHOD.


  METHOD get_utc.

    DATA: lv_i       TYPE i,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.


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
    ev_date = lv_i + c_epoch.

  ENDMETHOD.
ENDCLASS.
