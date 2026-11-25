CLASS zcl_abapgit_unit_test_runner DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_method,
        name        TYPE string,
        failure     TYPE string,
        has_runtime TYPE abap_bool,
        runtime_ms  TYPE i,
      END OF ty_method,
      ty_methods_tt TYPE STANDARD TABLE OF ty_method WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_class,
        name    TYPE string,
        methods TYPE ty_methods_tt,
      END OF ty_class,
      ty_classes_tt TYPE STANDARD TABLE OF ty_class WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_program,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        name     TYPE string,
        classes  TYPE ty_classes_tt,
      END OF ty_program,
      ty_programs_tt TYPE STANDARD TABLE OF ty_program WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_result,
        errors   TYPE string_table,
        programs TYPE ty_programs_tt,
        summary  TYPE string,
      END OF ty_result.

    "! Raises an exception if the AUnit runner is not available in the system,
    "! either SAPLSAUCV_GUI_RUNNER does not exist in old releases or
    "! passport=>get is private in newer releases NW >= 756
    CLASS-METHODS new
      IMPORTING
        !ii_repo           TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_unit_test_runner
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

    "! Runs the unit tests for all objects in the repository
    METHODS run
      RETURNING
        VALUE(rs_result) TYPE ty_result
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_key,
        obj_name TYPE tadir-obj_name,
        obj_type TYPE tadir-object,
      END OF ty_key,
      ty_keys_tt TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

    DATA mi_repo TYPE REF TO zif_abapgit_repo.

    METHODS build_keys
      RETURNING
        VALUE(rt_keys) TYPE ty_keys_tt
      RAISING
        zcx_abapgit_exception.

    METHODS create_runner
      RETURNING
        VALUE(ro_runner) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    "! Returns an object of type IF_SAUNIT_INTERNAL_RESULT
    METHODS run_aunit
      IMPORTING
        !it_keys        TYPE ty_keys_tt
      RETURNING
        VALUE(ro_aunit) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    METHODS parse_errors
      IMPORTING
        !it_indices      TYPE ANY TABLE
      RETURNING
        VALUE(rt_errors) TYPE string_table.

    METHODS parse_programs
      IMPORTING
        !it_programs       TYPE ANY TABLE
        !it_indices        TYPE ANY TABLE
      RETURNING
        VALUE(rt_programs) TYPE ty_programs_tt.

    METHODS parse_method
      IMPORTING
        !is_method       TYPE any
        !it_indices      TYPE ANY TABLE
        !iv_program_ndx  TYPE sy-tabix
        !iv_class_ndx    TYPE sy-tabix
        !iv_method_ndx   TYPE sy-tabix
      RETURNING
        VALUE(rs_method) TYPE ty_method.

ENDCLASS.



CLASS zcl_abapgit_unit_test_runner IMPLEMENTATION.


  METHOD build_keys.

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_tadir LIKE LINE OF lt_tadir.
    DATA ls_key   LIKE LINE OF rt_keys.

    lt_tadir = mi_repo->get_tadir_objects( ).

    LOOP AT lt_tadir INTO ls_tadir.
      CLEAR ls_key.
      ls_key-obj_type = ls_tadir-object.
      ls_key-obj_name = ls_tadir-obj_name.
      APPEND ls_key TO rt_keys.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mi_repo = ii_repo.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD create_runner.

    DATA lo_passport TYPE REF TO object.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
          RECEIVING
            result = lo_passport.

        CALL METHOD ('CL_AUCV_TEST_RUNNER_STANDARD')=>create
          EXPORTING
            i_passport = lo_passport
          RECEIVING
            result     = ro_runner.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD new.

    CREATE OBJECT ro_instance EXPORTING ii_repo = ii_repo.

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

    DATA ls_program     TYPE ty_program.
    DATA ls_class       TYPE ty_class.
    DATA ls_method      TYPE ty_method.
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


  METHOD run.

    DATA lo_aunit TYPE REF TO object.
    DATA lo_timer TYPE REF TO zcl_abapgit_timer.
    DATA lt_keys  TYPE ty_keys_tt.

    FIELD-SYMBOLS <ls_task_data> TYPE any.
    FIELD-SYMBOLS <lt_indices>   TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_programs>  TYPE ANY TABLE.

    lt_keys = build_keys( ).

    lo_timer = zcl_abapgit_timer=>create( iv_count = lines( lt_keys ) )->start( ).

    lo_aunit = run_aunit( lt_keys ).

    rs_result-summary = lo_timer->end( ).

    ASSIGN lo_aunit->('F_TASK_DATA') TO <ls_task_data>.
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


  METHOD run_aunit.

    DATA lo_runner TYPE REF TO object.
    DATA li_result TYPE REF TO data.

    FIELD-SYMBOLS <li_result> TYPE any.

    lo_runner = create_runner( ).

    CREATE DATA li_result TYPE REF TO ('IF_SAUNIT_INTERNAL_RESULT').
    ASSIGN li_result->* TO <li_result>.

    CALL METHOD lo_runner->('RUN_FOR_PROGRAM_KEYS')
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = it_keys
      IMPORTING
        e_aunit_result               = <li_result>.

    ro_aunit = <li_result>.

  ENDMETHOD.
ENDCLASS.
