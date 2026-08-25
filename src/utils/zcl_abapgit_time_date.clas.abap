CLASS zcl_abapgit_time_date DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_system_timezone
      RETURNING
        VALUE(rv_timezone) TYPE timezone .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_time_date IMPLEMENTATION.


  METHOD get_system_timezone.
* returns the time zone of the application server,
* CL_ABAP_TSTMP=>GET_SYSTEM_TIMEZONE does not exist in lower releases,
* and the function module is not released for ABAP Cloud, so call both dynamically

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
            OTHERS              = 2.
        ASSERT sy-subrc = 0.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
