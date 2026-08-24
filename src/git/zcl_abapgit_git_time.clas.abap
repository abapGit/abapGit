CLASS zcl_abapgit_git_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_unixtime TYPE zif_abapgit_git_definitions=>ty_unixtime .

    CLASS-METHODS get_unix
      RETURNING
        VALUE(rv_time) TYPE ty_unixtime
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS get_unix_days_ago
      IMPORTING
        !iv_days       TYPE i
      RETURNING
        VALUE(rv_time) TYPE i
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS get_unix_from_local
      IMPORTING
        !iv_date       TYPE d
        !iv_time       TYPE t
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

    CLASS-METHODS get_system_timezone
      RETURNING
        VALUE(rv_timezone) TYPE timezone .
ENDCLASS.



CLASS zcl_abapgit_git_time IMPLEMENTATION.


  METHOD get_system_timezone.
* the time zone of the application server, ie the time zone of SY-DATUM and SY-UZEIT
* and of most date and time fields on the database, like E070-AS4DATE and E070-AS4TIME
* note SY-ZONLO is the personal time zone of the user, it can be empty,
* eg in background jobs, so it cannot be used here

    DATA lv_fm TYPE string.

    lv_fm = 'GET_SYSTEM_TIMEZONE'.

    TRY.
        CALL METHOD ('CL_ABAP_TSTMP')=>get_system_timezone
          RECEIVING
            system_timezone = rv_timezone.
      CATCH cx_sy_dyn_call_illegal_method.
        CALL FUNCTION lv_fm
          IMPORTING
            timezone            = rv_timezone
          EXCEPTIONS
            customizing_missing = 1
            OTHERS              = 2 ##FM_SUBRC_OK.
    ENDTRY.

  ENDMETHOD.


  METHOD get_unix_days_ago.
* https://www.epochconverter.com
    CONSTANTS lc_seconds_per_day TYPE i VALUE 86400.
    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.
    DATA lv_time TYPE timestamp.

    GET TIME STAMP FIELD lv_time.

    rv_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_time
      tstmp2 = lc_epoch ).

    IF iv_days <= 0 OR iv_days > 24855.
      zcx_abapgit_exception=>raise( |Invalid iv_days: { iv_days }| ).
    ENDIF.

    rv_time = rv_time - iv_days * lc_seconds_per_day.
  ENDMETHOD.


  METHOD get_unix.
* returns seconds since Unix epoch, including timezone indicator

    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.
    DATA lv_time TYPE timestamp.
    DATA lv_seconds TYPE i.

    GET TIME STAMP FIELD lv_time.

    lv_seconds = cl_abap_tstmp=>subtract(
      tstmp1 = lv_time
      tstmp2 = lc_epoch ).

    rv_time = lv_seconds.
    CONDENSE rv_time.
    rv_time+11 = '+0000'.

  ENDMETHOD.


  METHOD get_unix_from_local.
* returns seconds since Unix epoch, including timezone indicator
* the input is expected to be in the time zone of the application server,
* like SY-DATUM and SY-UZEIT, it is converted to UTC, so the timezone
* indicator is always '+0000', the same layout as GET_UNIX returns

    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.

    DATA lv_seconds   TYPE i.
    DATA lv_timestamp TYPE timestamp.
    DATA lv_timezone  TYPE timezone.

    IF iv_date IS INITIAL.
      zcx_abapgit_exception=>raise( 'Cannot determine unix time, date is initial' ).
    ENDIF.

    lv_timezone = get_system_timezone( ).

    CONVERT DATE iv_date TIME iv_time
      INTO TIME STAMP lv_timestamp TIME ZONE lv_timezone.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Unknown time zone "{ lv_timezone }"| ).
    ENDIF.

    IF lv_timestamp < lc_epoch.
      zcx_abapgit_exception=>raise( |Date { iv_date } is before the unix epoch| ).
    ENDIF.

    lv_seconds = cl_abap_tstmp=>subtract(
      tstmp1 = lv_timestamp
      tstmp2 = lc_epoch ).

    rv_time = |{ lv_seconds } +0000|.

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
