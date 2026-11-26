CLASS zcl_abapgit_aunit_modern DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_aunit_runner.

    "! Runs the tests via CL_AUNIT_FACTORY, with the listener of CL_SAUNIT_GUI_SERVICE
    "! collecting the results. All calls are dynamic, as the AUnit API does not exist
    "! in all releases. Raises an exception if the API is not available in the system
    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_enum_risk_level_critical TYPE string VALUE 'IF_AUNIT_ATTRIBUTE_ENUMS=>C_RISK_LEVEL-CRITICAL'.
    CONSTANTS c_enum_duration_long TYPE string VALUE 'IF_AUNIT_ATTRIBUTE_ENUMS=>C_DURATION-LONG'.

    DATA mi_repo TYPE REF TO zif_abapgit_repo.

    METHODS create_listener
      RETURNING
        VALUE(ro_listener) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    "! Returns a data reference of type REF TO IF_AUNIT_LISTENER
    METHODS cast_listener
      IMPORTING
        !io_listener       TYPE REF TO object
      RETURNING
        VALUE(rr_listener) TYPE REF TO data
      RAISING
        zcx_abapgit_exception.

    METHODS create_task
      IMPORTING
        !io_listener   TYPE REF TO object
      RETURNING
        VALUE(ro_task) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    METHODS restrict
      IMPORTING
        !io_task         TYPE REF TO object
      RETURNING
        VALUE(rt_errors) TYPE string_table.

    METHODS add_objects
      IMPORTING
        !io_task         TYPE REF TO object
        !it_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_errors) TYPE string_table.

    METHODS contains_test_cases
      IMPORTING
        !io_task       TYPE REF TO object
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.

    METHODS run_task
      IMPORTING
        !io_task     TYPE REF TO object
        !io_listener TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    "! Returns an object of type IF_SAUNIT_INTERNAL_RESULT
    METHODS get_result
      IMPORTING
        !io_listener    TYPE REF TO object
      RETURNING
        VALUE(ro_aunit) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_aunit_modern IMPLEMENTATION.


  METHOD add_objects.

    DATA ls_tadir   LIKE LINE OF it_tadir.
    DATA lv_program TYPE progname.
    DATA lv_error   TYPE string.
    DATA lx_error   TYPE REF TO cx_root.

    " an object that cannot be added must not prevent the tests of the other objects
    LOOP AT it_tadir INTO ls_tadir.
      lv_program = ls_tadir-obj_name.
      TRY.
          CASE ls_tadir-object.
            WHEN 'CLAS'.
              CALL METHOD io_task->('IF_AUNIT_TASK~ADD_CLASS_POOL')
                EXPORTING
                  class = lv_program.
            WHEN 'PROG'.
              CALL METHOD io_task->('IF_AUNIT_TASK~ADD_PROGRAM')
                EXPORTING
                  program = lv_program.
            WHEN 'FUGR'.
              CALL METHOD io_task->('IF_AUNIT_TASK~ADD_FUNCTION_GROUP')
                EXPORTING
                  fugr_name = lv_program.
          ENDCASE.
        CATCH cx_root INTO lx_error.
          lv_error = |{ ls_tadir-object } { ls_tadir-obj_name }: { lx_error->get_text( ) }|.
          APPEND lv_error TO rt_errors.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD cast_listener.

    DATA lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS <li_listener> TYPE any.

    TRY.
        CREATE DATA rr_listener TYPE REF TO ('IF_AUNIT_LISTENER').
        ASSIGN rr_listener->* TO <li_listener>.
        <li_listener> ?= io_listener.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    mi_repo = ii_repo.

    " check that the API is available in this release
    create_listener( ).

  ENDMETHOD.


  METHOD contains_test_cases.

    TRY.
        CALL METHOD io_task->('IF_AUNIT_TASK~CONTAINS_TEST_CASES')
          RECEIVING
            result = rv_bool.
      CATCH cx_root.
        " not available in all releases, then the task is run anyhow
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD create_listener.

    DATA lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_SAUNIT_GUI_SERVICE')=>create_listener
          RECEIVING
            result = ro_listener.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD create_task.

    DATA lo_factory  TYPE REF TO object.
    DATA lr_listener TYPE REF TO data.
    DATA lx_error    TYPE REF TO cx_root.

    FIELD-SYMBOLS <li_listener> TYPE any.

    lr_listener = cast_listener( io_listener ).
    ASSIGN lr_listener->* TO <li_listener>.

    TRY.
        CREATE OBJECT lo_factory TYPE ('CL_AUNIT_FACTORY').

        CALL METHOD lo_factory->('CREATE_TASK')
          EXPORTING
            listener = <li_listener>
          RECEIVING
            result   = ro_task.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_result.

    DATA lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD io_listener->('IF_SAUNIT_INTERNAL_LISTENER~GET_RESULT_AFTER_END_OF_TASK')
          RECEIVING
            result = ro_aunit.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD restrict.

    DATA lv_error TYPE string.
    DATA lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS <lv_value> TYPE any.

    " the constants have the types of the parameters
    ASSIGN (c_enum_risk_level_critical) TO <lv_value>.
    IF sy-subrc <> 0.
      lv_error = |Restrict risk level: { c_enum_risk_level_critical } not found|.
      APPEND lv_error TO rt_errors.
    ELSE.
      TRY.
          CALL METHOD io_task->('IF_AUNIT_TASK~RESTRICT_RISK_LEVEL')
            EXPORTING
              max_risk_level = <lv_value>.
        CATCH cx_root INTO lx_error.
          lv_error = |Restrict risk level: { lx_error->get_text( ) }|.
          APPEND lv_error TO rt_errors.
      ENDTRY.
    ENDIF.

    ASSIGN (c_enum_duration_long) TO <lv_value>.
    IF sy-subrc <> 0.
      lv_error = |Restrict duration category: { c_enum_duration_long } not found|.
      APPEND lv_error TO rt_errors.
    ELSE.
      TRY.
          CALL METHOD io_task->('IF_AUNIT_TASK~RESTRICT_DURATION_CATEGORY')
            EXPORTING
              max_duration_category = <lv_value>.
        CATCH cx_root INTO lx_error.
          lv_error = |Restrict duration category: { lx_error->get_text( ) }|.
          APPEND lv_error TO rt_errors.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD run_task.

    DATA lr_listener TYPE REF TO data.
    DATA lx_error    TYPE REF TO cx_root.

    FIELD-SYMBOLS <li_listener> TYPE any.

    lr_listener = cast_listener( io_listener ).
    ASSIGN lr_listener->* TO <li_listener>.

    TRY.
        CALL METHOD io_task->('IF_AUNIT_TASK~RUN')
          EXPORTING
            listener = <li_listener>.
      CATCH cx_sy_dyn_call_param_not_found.
        " RUN has no listener parameter in all releases, then the listener from CREATE_TASK is used
        TRY.
            CALL METHOD io_task->('IF_AUNIT_TASK~RUN').
          CATCH cx_root INTO lx_error.
            zcx_abapgit_exception=>raise_with_text( lx_error ).
        ENDTRY.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_aunit_runner~run.

    DATA lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lo_timer    TYPE REF TO zcl_abapgit_timer.
    DATA lo_listener TYPE REF TO object.
    DATA lo_task     TYPE REF TO object.
    DATA lo_aunit    TYPE REF TO object.
    DATA lt_errors   TYPE string_table.
    DATA lt_add      TYPE string_table.
    DATA lv_summary  TYPE string.

    lt_tadir = zcl_abapgit_aunit_objects=>get( mi_repo ).

    lo_timer = zcl_abapgit_timer=>create( iv_count = lines( lt_tadir ) )->start( ).

    lo_listener = create_listener( ).
    lo_task = create_task( lo_listener ).

    lt_errors = restrict( lo_task ).

    lt_add = add_objects(
      io_task  = lo_task
      it_tadir = lt_tadir ).
    APPEND LINES OF lt_add TO lt_errors.

    IF contains_test_cases( lo_task ) = abap_true.
      run_task(
        io_task     = lo_task
        io_listener = lo_listener ).
      lo_aunit = get_result( lo_listener ).
    ENDIF.

    lv_summary = lo_timer->end( ).

    rs_result = zcl_abapgit_aunit_result=>from_aunit( lo_aunit ).
    rs_result-summary = lv_summary.
    APPEND LINES OF lt_errors TO rs_result-errors.

  ENDMETHOD.
ENDCLASS.
