CLASS zcl_abapgit_aunit_result DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! Converts an AUnit result of type IF_SAUNIT_INTERNAL_RESULT,
    "! accessed dynamically as it does not exist in all releases
    CLASS-METHODS from_aunit
      IMPORTING
        !io_aunit        TYPE REF TO object
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_aunit_runner=>ty_result
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS parse_errors
      IMPORTING
        !it_indices      TYPE ANY TABLE
      RETURNING
        VALUE(rt_errors) TYPE string_table.

    CLASS-METHODS parse_programs
      IMPORTING
        !it_programs       TYPE ANY TABLE
        !it_indices        TYPE ANY TABLE
      RETURNING
        VALUE(rt_programs) TYPE zif_abapgit_aunit_runner=>ty_programs_tt.

    CLASS-METHODS parse_method
      IMPORTING
        !is_method       TYPE any
        !it_indices      TYPE ANY TABLE
        !iv_program_ndx  TYPE sy-tabix
        !iv_class_ndx    TYPE sy-tabix
        !iv_method_ndx   TYPE sy-tabix
      RETURNING
        VALUE(rs_method) TYPE zif_abapgit_aunit_runner=>ty_method.

ENDCLASS.



CLASS zcl_abapgit_aunit_result IMPLEMENTATION.


  METHOD from_aunit.

    FIELD-SYMBOLS <ls_task_data> TYPE any.
    FIELD-SYMBOLS <lt_indices>   TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_programs>  TYPE ANY TABLE.

    IF io_aunit IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN io_aunit->('F_TASK_DATA') TO <ls_task_data>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Unexpected AUnit result format| ).
    ENDIF.
    ASSIGN COMPONENT 'ALERTS_BY_INDICIES' OF STRUCTURE <ls_task_data> TO <lt_indices>.
    ASSIGN COMPONENT 'PROGRAMS' OF STRUCTURE <ls_task_data> TO <lt_programs>.

    rs_result-errors = parse_errors( <lt_indices> ).

    rs_result-programs = parse_programs(
      it_programs = <lt_programs>
      it_indices  = <lt_indices> ).

  ENDMETHOD.


  METHOD parse_errors.

    DATA lv_text  TYPE string.
    DATA lv_param TYPE string.

    FIELD-SYMBOLS <ls_alert_by_index> TYPE any.
    FIELD-SYMBOLS <lt_alerts>         TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert>          TYPE any.
    FIELD-SYMBOLS <lt_params>         TYPE string_table.
    FIELD-SYMBOLS <lt_text_info>      TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_text_info>      TYPE any.

    LOOP AT it_indices ASSIGNING <ls_alert_by_index>.
      ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_alert_by_index> TO <lt_alerts>.
      LOOP AT <lt_alerts> ASSIGNING <ls_alert> WHERE ('KIND = ''F'' OR KIND = ''S'' OR KIND = ''E'' OR KIND = ''W''').
        CLEAR lv_text.
        ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
        LOOP AT <lt_params> INTO lv_param.
          lv_text = lv_text && lv_param.
        ENDLOOP.

        ASSIGN COMPONENT 'TEXT_INFOS' OF STRUCTURE <ls_alert> TO <lt_text_info>.
        LOOP AT <lt_text_info> ASSIGNING <ls_text_info>.
          ASSIGN COMPONENT 'PARAMS' OF STRUCTURE <ls_text_info> TO <lt_params>.
          LOOP AT <lt_params> INTO lv_param.
            lv_text = lv_text && lv_param.
          ENDLOOP.
        ENDLOOP.

        IF lv_text NP '*SAUNIT_NO_TEST_CLASS*'.
          APPEND lv_text TO rt_errors.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse_method.

    DATA lv_param   TYPE string.
    DATA lv_runtime TYPE timestampl.

    FIELD-SYMBOLS <ls_alert_by_index> TYPE any.
    FIELD-SYMBOLS <lt_alerts>         TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert>          TYPE any.
    FIELD-SYMBOLS <lt_params>         TYPE string_table.
    FIELD-SYMBOLS <lv_any>            TYPE any.
    FIELD-SYMBOLS <lv_start>          TYPE timestampl.
    FIELD-SYMBOLS <lv_end>            TYPE timestampl.

    ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE is_method TO <lv_any>.
    IF sy-subrc = 0.
      rs_method-name = <lv_any>.
    ENDIF.

    READ TABLE it_indices WITH KEY
      ('PROGRAM_NDX') = iv_program_ndx
      ('CLASS_NDX')   = iv_class_ndx
      ('METHOD_NDX')  = iv_method_ndx
      ASSIGNING <ls_alert_by_index>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_alert_by_index> TO <lt_alerts>.
      LOOP AT <lt_alerts> ASSIGNING <ls_alert>.
        ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
        LOOP AT <lt_params> INTO lv_param.
          rs_method-failure = lv_param.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    ASSIGN COMPONENT 'INFO-START_ON' OF STRUCTURE is_method TO <lv_start>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'INFO-END_ON' OF STRUCTURE is_method TO <lv_end>.
      IF sy-subrc = 0.
        TRY.
            lv_runtime = cl_abap_tstmp=>subtract(
              tstmp1 = <lv_end>
              tstmp2 = <lv_start> ) * 1000.
            rs_method-runtime_ms  = lv_runtime.
            rs_method-has_runtime = abap_true.
          CATCH cx_parameter_invalid ##NO_HANDLER. "ignore
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD parse_programs.

    DATA ls_program     TYPE zif_abapgit_aunit_runner=>ty_program.
    DATA ls_class       TYPE zif_abapgit_aunit_runner=>ty_class.
    DATA ls_method      TYPE zif_abapgit_aunit_runner=>ty_method.
    DATA lv_program_ndx TYPE sy-tabix.
    DATA lv_class_ndx   TYPE sy-tabix.
    DATA lv_method_ndx  TYPE sy-tabix.

    FIELD-SYMBOLS <ls_program> TYPE any.
    FIELD-SYMBOLS <lt_classes> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_class>   TYPE any.
    FIELD-SYMBOLS <lt_methods> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_method>  TYPE any.
    FIELD-SYMBOLS <lv_any>     TYPE any.

    LOOP AT it_programs ASSIGNING <ls_program>.
      lv_program_ndx = sy-tabix.
      CLEAR ls_program.

      ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_program> TO <lv_any>.
      IF sy-subrc = 0.
        ls_program-name = <lv_any>.
      ENDIF.

      " KEY field does not exist in 750
      ASSIGN COMPONENT 'INFO-KEY-OBJ_TYPE' OF STRUCTURE <ls_program> TO <lv_any>.
      IF sy-subrc = 0.
        ls_program-obj_type = <lv_any>.
        ASSIGN COMPONENT 'INFO-KEY-OBJ_NAME' OF STRUCTURE <ls_program> TO <lv_any>.
        IF sy-subrc = 0.
          ls_program-obj_name = <lv_any>.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'CLASSES' OF STRUCTURE <ls_program> TO <lt_classes>.
      LOOP AT <lt_classes> ASSIGNING <ls_class>.
        lv_class_ndx = sy-tabix.
        CLEAR ls_class.

        ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_class> TO <lv_any>.
        IF sy-subrc = 0.
          ls_class-name = <lv_any>.
        ENDIF.

        ASSIGN COMPONENT 'METHODS' OF STRUCTURE <ls_class> TO <lt_methods>.
        LOOP AT <lt_methods> ASSIGNING <ls_method>.
          lv_method_ndx = sy-tabix.
          ls_method = parse_method(
            is_method      = <ls_method>
            it_indices     = it_indices
            iv_program_ndx = lv_program_ndx
            iv_class_ndx   = lv_class_ndx
            iv_method_ndx  = lv_method_ndx ).
          APPEND ls_method TO ls_class-methods.
        ENDLOOP.

        APPEND ls_class TO ls_program-classes.
      ENDLOOP.

      APPEND ls_program TO rt_programs.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
