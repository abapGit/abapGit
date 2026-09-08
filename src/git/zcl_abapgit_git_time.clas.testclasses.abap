CLASS ltcl_time_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_unix FOR TESTING RAISING cx_static_check.
    METHODS get_unix_from_local FOR TESTING RAISING cx_static_check.
    METHODS get_unix_from_local_now FOR TESTING RAISING cx_static_check.
    METHODS get_utc FOR TESTING.
ENDCLASS.


CLASS ltcl_time_test IMPLEMENTATION.

  METHOD get_unix.

    DATA: lv_time TYPE zcl_abapgit_git_time=>ty_unixtime.

    lv_time = zcl_abapgit_git_time=>get_unix( ).

    cl_abap_unit_assert=>assert_not_initial( lv_time ).

  ENDMETHOD.


  METHOD get_unix_from_local.

* round trip, GET_UTC must decode the same point in time again,
* independent of the time zone of the application server

    CONSTANTS lc_utc  TYPE timezone VALUE 'UTC'.
    CONSTANTS lc_date TYPE d VALUE '20240208'.
    CONSTANTS lc_time TYPE t VALUE '123456'.

    DATA lv_unix     TYPE zcl_abapgit_git_time=>ty_unixtime.
    DATA lv_seconds  TYPE zcl_abapgit_git_time=>ty_unixtime.
    DATA lv_date     TYPE sy-datum.
    DATA lv_time     TYPE sy-uzeit.
    DATA lv_act      TYPE timestamp.
    DATA lv_exp      TYPE timestamp.
    DATA lv_timezone TYPE timezone.

    lv_unix = zcl_abapgit_git_time=>get_unix_from_local(
      iv_date = lc_date
      iv_time = lc_time ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_unix+11
      exp = '+0000' ).

* only the seconds are passed, the timezone indicator is asserted above
* and does not shift the point in time
    lv_seconds = lv_unix(10).

    zcl_abapgit_git_time=>get_utc(
      EXPORTING
        iv_unix = lv_seconds
      IMPORTING
        ev_date = lv_date
        ev_time = lv_time ).

    CONVERT DATE lv_date TIME lv_time
      INTO TIME STAMP lv_act TIME ZONE lc_utc.

    lv_timezone = zcl_abapgit_time_date=>get_system_timezone( ).

    CONVERT DATE lc_date TIME lc_time
      INTO TIME STAMP lv_exp TIME ZONE lv_timezone.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.


  METHOD get_unix_from_local_now.

* the current time converted from local time must match GET_UNIX,
* this fails if the wrong time zone is assumed for the input

    DATA lv_unix  TYPE zcl_abapgit_git_time=>ty_unixtime.
    DATA lv_local TYPE zcl_abapgit_git_time=>ty_unixtime.
    DATA lv_diff  TYPE i.

    GET TIME.

    lv_local = zcl_abapgit_git_time=>get_unix_from_local(
      iv_date = sy-datum
      iv_time = sy-uzeit ).
    lv_unix = zcl_abapgit_git_time=>get_unix( ).

    lv_diff = abs( lv_unix(10) - lv_local(10) ).

    IF lv_diff > 5.
      cl_abap_unit_assert=>fail( |Off by { lv_diff } seconds, { lv_unix } vs { lv_local }| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_utc.

    DATA: lv_date TYPE sy-datum,
          lv_time TYPE sy-uzeit.

    zcl_abapgit_git_time=>get_utc(
      EXPORTING
        iv_unix = '1574605521'
      IMPORTING
        ev_date = lv_date
        ev_time = lv_time ).

    cl_abap_unit_assert=>assert_not_initial( lv_date ).
    cl_abap_unit_assert=>assert_not_initial( lv_time ).

  ENDMETHOD.

ENDCLASS.
