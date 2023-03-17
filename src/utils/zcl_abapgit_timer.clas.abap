CLASS zcl_abapgit_timer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_text        TYPE string OPTIONAL
        !iv_count       TYPE i OPTIONAL
          PREFERRED PARAMETER iv_text
      RETURNING
        VALUE(ro_timer) TYPE REF TO zcl_abapgit_timer.

    METHODS constructor
      IMPORTING
        !iv_text  TYPE string OPTIONAL
        !iv_count TYPE i OPTIONAL.

    METHODS start
      RETURNING
        VALUE(ro_timer) TYPE REF TO zcl_abapgit_timer.

    METHODS end
      IMPORTING
        !iv_output_as_status_message TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result)             TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_text TYPE string.
    DATA mv_count TYPE i.
    DATA mv_timer TYPE timestampl.

ENDCLASS.



CLASS zcl_abapgit_timer IMPLEMENTATION.


  METHOD constructor.
    mv_text  = iv_text.
    mv_count = iv_count.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_timer
      EXPORTING
        iv_text  = iv_text
        iv_count = iv_count.
  ENDMETHOD.


  METHOD end.

    DATA:
      lv_timestamp TYPE timestampl,
      lv_runtime   TYPE timestampl,
      lv_sec       TYPE p LENGTH 11 DECIMALS 2.

    IF mv_timer IS INITIAL.
      rv_result = 'Runtime measurement has not been started'.
    ELSE.
      GET TIME STAMP FIELD lv_timestamp.

      TRY.
          lv_runtime = cl_abap_tstmp=>subtract(
            tstmp1 = lv_timestamp
            tstmp2 = mv_timer ).

          lv_sec = lv_runtime. " round to 2 decimal places

          IF mv_count = 1.
            rv_result = |1 object, |.
          ELSEIF mv_count > 1.
            rv_result = |{ mv_count } objects, |.
          ENDIF.

          rv_result = rv_result && |{ lv_sec } seconds|.

        CATCH cx_parameter_invalid.
          rv_result = 'Error getting runtime measurement'.
      ENDTRY.
    ENDIF.

    IF iv_output_as_status_message = abap_true.
      MESSAGE s000(oo) WITH mv_text rv_result.
    ENDIF.

    IF mv_text IS NOT INITIAL.
      rv_result = |{ mv_text } { rv_result }|.
    ENDIF.

  ENDMETHOD.


  METHOD start.
    GET TIME STAMP FIELD mv_timer.
    ro_timer = me.
  ENDMETHOD.
ENDCLASS.
