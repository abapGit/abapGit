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

    DATA: lv_time TYPE zcl_abapgit_time=>ty_unixtime.

    zcl_abapgit_time=>get_unix(
      EXPORTING
        iv_date = sy-datum
        iv_time = sy-uzeit
      RECEIVING
        rv_time = lv_time ).

    cl_abap_unit_assert=>assert_not_initial( lv_time ).

  ENDMETHOD.


  METHOD get_utc.

    DATA: lv_date TYPE sydatum,
          lv_time TYPE syuzeit.

    zcl_abapgit_time=>get_utc(
      EXPORTING
        iv_unix = '1574605521'
      IMPORTING
        ev_date = lv_date
        ev_time = lv_time ).

    cl_abap_unit_assert=>assert_not_initial( lv_date ).
    cl_abap_unit_assert=>assert_not_initial( lv_time ).

  ENDMETHOD.

ENDCLASS.
