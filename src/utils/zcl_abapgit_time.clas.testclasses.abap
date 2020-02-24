CLASS ltcl_time_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_unix FOR TESTING RAISING cx_static_check.
    METHODS get_utc FOR TESTING.
ENDCLASS.


CLASS ltcl_time_test IMPLEMENTATION.

  METHOD get_unix.

    DATA: lv_unix    TYPE zcl_abapgit_time=>ty_unixtime,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.

    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = lv_tz.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = lv_tz
        if_local_date    = sy-datum
        if_local_time    = sy-uzeit
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.

    lv_unix = '1574605521'.
    lv_unix+11 = lv_utcsign.
    lv_unix+12 = lv_utcdiff.

    cl_abap_unit_assert=>assert_equals(
        act                  = zcl_abapgit_time=>get_unix( iv_date = '20191124'
                                                           iv_time = '152521' )
        exp                  = lv_unix ). " User-specific test!

  ENDMETHOD.


  METHOD get_utc.

    DATA: lv_date    TYPE sydatum,
          lv_time    TYPE syuzeit,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.

    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = lv_tz.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = lv_tz
        if_local_date    = sy-datum
        if_local_time    = sy-uzeit
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.

    zcl_abapgit_time=>get_utc( EXPORTING iv_unix = '1574605521'
                               IMPORTING ev_date = lv_date
                                         ev_time = lv_time ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_date
        exp = '20191124' ).

    CASE lv_utcsign.
      WHEN '+'.
        lv_time = lv_time + lv_utcdiff.
      WHEN '-'.
        lv_time = lv_time - lv_utcdiff.
    ENDCASE.

    cl_abap_unit_assert=>assert_equals(
        act = lv_time
        exp = '152521' ). " This is GMT +1. Please provide the expected value by your timezone!

  ENDMETHOD.

ENDCLASS.
