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

    CONSTANTS: lc_unix TYPE zcl_abapgit_time=>ty_unixtime VALUE '1574605521',
               lc_date TYPE sydatum VALUE '20191124',
               lc_time TYPE syuzeit VALUE '152521'.

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
        if_local_date    = lc_date
        if_local_time    = lc_time
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.

    lv_unix    = lc_unix.
    lv_unix+11 = lv_utcsign.
    lv_unix+12 = lv_utcdiff.

    cl_abap_unit_assert=>assert_equals(
        act                  = zcl_abapgit_time=>get_unix( iv_date = lc_date
                                                           iv_time = lc_time )
        exp                  = lv_unix ). " User-specific test!

  ENDMETHOD.


  METHOD get_utc.

    CONSTANTS: lc_unix TYPE zcl_abapgit_time=>ty_unixtime VALUE '1574605521',
               lc_date TYPE sydatum VALUE '20191124',
               lc_time TYPE syuzeit VALUE '152521'.

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
        if_local_date    = lc_date
        if_local_time    = lc_time
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.

    zcl_abapgit_time=>get_utc( EXPORTING iv_unix = lc_unix
                               IMPORTING ev_date = lv_date
                                         ev_time = lv_time ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_date
        exp = lc_date ).

    CASE lv_utcsign.
      WHEN '+'.
        lv_time = lv_time + lv_utcdiff.
      WHEN '-'.
        lv_time = lv_time - lv_utcdiff.
    ENDCASE.

    cl_abap_unit_assert=>assert_equals(
        act = lv_time
        exp = lc_time ). " This is GMT +1. Please provide the expected value by your timezone!

  ENDMETHOD.

ENDCLASS.
