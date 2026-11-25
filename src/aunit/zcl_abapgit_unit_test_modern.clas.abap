CLASS zcl_abapgit_unit_test_modern DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_unit_test_runner.

    "! Runs the tests via CL_AUNIT_FACTORY, with the listener of CL_SAUNIT_GUI_SERVICE
    "! collecting the results. All access is dynamic, as the AUnit API does not exist
    "! in all releases. Raises an exception if the API is not available in the system
    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_type,
        factory     TYPE string VALUE 'CL_AUNIT_FACTORY',
        gui_service TYPE string VALUE 'CL_SAUNIT_GUI_SERVICE',
        task        TYPE string VALUE 'IF_AUNIT_TASK',
        listener    TYPE string VALUE 'IF_SAUNIT_INTERNAL_LISTENER',
      END OF c_type.

    CONSTANTS c_risk_level_critical TYPE string VALUE '33'.
    CONSTANTS c_duration_long TYPE string VALUE '36'.

    DATA mi_repo TYPE REF TO zif_abapgit_repo.

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
        VALUE(rv_bool) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    "! Calls a public method dynamically, parameter names and types are determined via RTTI.
    "! The value is passed to the first mandatory importing parameter, or the first optional
    "! one if there is no mandatory parameter
    "! @parameter io_object | Instance, static method call if not bound
    "! @parameter iv_type | Class or interface defining the method
    "! @parameter iv_method | Method name
    "! @parameter ig_value | Value for the importing parameter
    "! @parameter iv_parameter | Pass the value to this parameter instead, skipped if the method does not have it
    "! @parameter rr_result | Returning value, not bound if the method has none
    METHODS call_method
      IMPORTING
        !io_object       TYPE REF TO object OPTIONAL
        !iv_type         TYPE string
        !iv_method       TYPE abap_methname
        !ig_value        TYPE any OPTIONAL
        !iv_parameter    TYPE abap_parmname OPTIONAL
      RETURNING
        VALUE(rr_result) TYPE REF TO data
      RAISING
        zcx_abapgit_exception.

    METHODS describe_method
      IMPORTING
        !iv_type   TYPE string
        !iv_method TYPE abap_methname
      EXPORTING
        !eo_descr  TYPE REF TO cl_abap_objectdescr
        !es_method TYPE abap_methdescr
      RAISING
        zcx_abapgit_exception.

    METHODS has_method
      IMPORTING
        !iv_type       TYPE string
        !iv_method     TYPE abap_methname
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.

    METHODS create_value
      IMPORTING
        !io_descr       TYPE REF TO cl_abap_objectdescr
        !iv_method      TYPE abap_methname
        !iv_parameter   TYPE abap_parmname
      RETURNING
        VALUE(rr_value) TYPE REF TO data
      RAISING
        zcx_abapgit_exception.

    METHODS to_object
      IMPORTING
        !ir_data         TYPE REF TO data
      RETURNING
        VALUE(ro_object) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_unit_test_modern IMPLEMENTATION.


  METHOD add_objects.

    DATA ls_tadir  LIKE LINE OF it_tadir.
    DATA lv_method TYPE abap_methname.
    DATA lv_error  TYPE string.
    DATA lx_error  TYPE REF TO zcx_abapgit_exception.

    LOOP AT it_tadir INTO ls_tadir.
      CASE ls_tadir-object.
        WHEN 'CLAS'.
          lv_method = 'ADD_CLASS_POOL'.
        WHEN 'PROG'.
          lv_method = 'ADD_PROGRAM'.
        WHEN 'FUGR'.
          lv_method = 'ADD_FUNCTION_GROUP'.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      " an object that cannot be added must not prevent the tests of the other objects
      TRY.
          call_method(
            io_object = io_task
            iv_type   = c_type-task
            iv_method = lv_method
            ig_value  = ls_tadir-obj_name ).
        CATCH zcx_abapgit_exception INTO lx_error.
          lv_error = |{ ls_tadir-object } { ls_tadir-obj_name }: { lx_error->get_text( ) }|.
          APPEND lv_error TO rt_errors.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD call_method.

    DATA lo_descr     TYPE REF TO cl_abap_objectdescr.
    DATA ls_method    TYPE abap_methdescr.
    DATA ls_param     TYPE abap_parmdescr.
    DATA ls_importing TYPE abap_parmdescr.
    DATA ls_bind      TYPE abap_parmbind.
    DATA lt_params    TYPE abap_parmbind_tab.
    DATA lv_name      TYPE string.
    DATA lx_error     TYPE REF TO cx_root.

    FIELD-SYMBOLS <lg_value> TYPE any.

    describe_method(
      EXPORTING
        iv_type   = iv_type
        iv_method = iv_method
      IMPORTING
        eo_descr  = lo_descr
        es_method = ls_method ).

    IF iv_parameter IS NOT INITIAL.
      READ TABLE ls_method-parameters INTO ls_importing
        WITH KEY name = iv_parameter parm_kind = cl_abap_objectdescr=>importing.
    ELSEIF ig_value IS SUPPLIED.
      LOOP AT ls_method-parameters INTO ls_param WHERE parm_kind = cl_abap_objectdescr=>importing.
        IF ls_param-is_optional = abap_false.
          ls_importing = ls_param.
          EXIT.
        ELSEIF ls_importing IS INITIAL.
          ls_importing = ls_param.
        ENDIF.
      ENDLOOP.
      IF ls_importing IS INITIAL.
        zcx_abapgit_exception=>raise( |{ iv_type }=>{ iv_method }: no importing parameter| ).
      ENDIF.
    ENDIF.

    IF ls_importing IS NOT INITIAL.

      CLEAR ls_bind.
      ls_bind-name = ls_importing-name.
      ls_bind-kind = cl_abap_objectdescr=>exporting.
      TRY.
          ls_bind-value = create_value(
            io_descr     = lo_descr
            iv_method    = iv_method
            iv_parameter = ls_importing-name ).
          ASSIGN ls_bind-value->* TO <lg_value>.
          IF ls_importing-type_kind = cl_abap_typedescr=>typekind_oref.
            <lg_value> ?= ig_value.
          ELSE.
            <lg_value> = ig_value.
          ENDIF.
        CATCH cx_sy_create_data_error.
          " generically typed parameter, pass the value as is
          GET REFERENCE OF ig_value INTO ls_bind-value.
        CATCH cx_root INTO lx_error.
          zcx_abapgit_exception=>raise_with_text( lx_error ).
      ENDTRY.
      INSERT ls_bind INTO TABLE lt_params.
    ENDIF.

    READ TABLE ls_method-parameters INTO ls_param WITH KEY parm_kind = cl_abap_objectdescr=>returning.
    IF sy-subrc = 0.
      CLEAR ls_bind.
      ls_bind-name = ls_param-name.
      ls_bind-kind = cl_abap_objectdescr=>receiving.
      ls_bind-value = create_value(
        io_descr     = lo_descr
        iv_method    = iv_method
        iv_parameter = ls_param-name ).
      INSERT ls_bind INTO TABLE lt_params.
      rr_result = ls_bind-value.
    ENDIF.

    TRY.
        IF io_object IS NOT BOUND.
          CALL METHOD (iv_type)=>(iv_method) PARAMETER-TABLE lt_params.
        ELSEIF lo_descr->kind = cl_abap_typedescr=>kind_intf.
          lv_name = |{ iv_type }~{ iv_method }|.
          CALL METHOD io_object->(lv_name) PARAMETER-TABLE lt_params.
        ELSE.
          CALL METHOD io_object->(iv_method) PARAMETER-TABLE lt_params.
        ENDIF.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    mi_repo = ii_repo.

    " check that the needed API exists and is public in this release
    describe_method(
      iv_type   = c_type-gui_service
      iv_method = 'CREATE_LISTENER' ).
    describe_method(
      iv_type   = c_type-factory
      iv_method = 'CREATE_TASK' ).
    describe_method(
      iv_type   = c_type-task
      iv_method = 'RUN' ).
    describe_method(
      iv_type   = c_type-listener
      iv_method = 'GET_RESULT_AFTER_END_OF_TASK' ).

  ENDMETHOD.


  METHOD contains_test_cases.

    DATA lr_bool TYPE REF TO data.

    FIELD-SYMBOLS <lv_bool> TYPE any.

    " not available in all releases, then the task is run anyhow
    IF has_method( iv_type   = c_type-task
                   iv_method = 'CONTAINS_TEST_CASES' ) = abap_false.
      rv_bool = abap_true.
      RETURN.
    ENDIF.

    lr_bool = call_method(
      io_object = io_task
      iv_type   = c_type-task
      iv_method = 'CONTAINS_TEST_CASES' ).
    IF lr_bool IS NOT BOUND.
      rv_bool = abap_true.
      RETURN.
    ENDIF.

    ASSIGN lr_bool->* TO <lv_bool>.
    rv_bool = <lv_bool>.

  ENDMETHOD.


  METHOD create_value.

    DATA lo_type TYPE REF TO cl_abap_datadescr.

    io_descr->get_method_parameter_type(
      EXPORTING
        p_method_name       = iv_method
        p_parameter_name    = iv_parameter
      RECEIVING
        p_descr_ref         = lo_type
      EXCEPTIONS
        parameter_not_found = 1
        method_not_found    = 2
        OTHERS              = 3 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |{ iv_method }: type of parameter { iv_parameter } not found| ).
    ENDIF.

    CREATE DATA rr_value TYPE HANDLE lo_type.

  ENDMETHOD.


  METHOD describe_method.

    DATA lo_descr TYPE REF TO cl_abap_typedescr.

    CLEAR: eo_descr, es_method.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_type
      RECEIVING
        p_descr_ref    = lo_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |{ iv_type } not found, not supported in your NW release| ).
    ENDIF.
    IF lo_descr->kind <> cl_abap_typedescr=>kind_class AND lo_descr->kind <> cl_abap_typedescr=>kind_intf.
      zcx_abapgit_exception=>raise( |{ iv_type } is not a class or interface| ).
    ENDIF.
    eo_descr ?= lo_descr.

    READ TABLE eo_descr->methods INTO es_method WITH KEY name = iv_method.
    IF sy-subrc <> 0 OR es_method-visibility <> cl_abap_objectdescr=>public.
      zcx_abapgit_exception=>raise( |{ iv_type }=>{ iv_method } not available, not supported in your NW release| ).
    ENDIF.

  ENDMETHOD.


  METHOD has_method.

    TRY.
        describe_method(
          iv_type   = iv_type
          iv_method = iv_method ).
        rv_bool = abap_true.
      CATCH zcx_abapgit_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD to_object.

    FIELD-SYMBOLS <lo_object> TYPE any.

    IF ir_data IS BOUND.
      ASSIGN ir_data->* TO <lo_object>.
      ro_object = <lo_object>.
    ENDIF.

    IF ro_object IS NOT BOUND.
      zcx_abapgit_exception=>raise( |AUnit API did not return an instance| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_unit_test_runner~run.

    DATA lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lo_timer    TYPE REF TO zcl_abapgit_timer.
    DATA lo_listener TYPE REF TO object.
    DATA lo_factory  TYPE REF TO object.
    DATA lo_task     TYPE REF TO object.
    DATA lo_aunit    TYPE REF TO object.
    DATA lt_errors   TYPE string_table.
    DATA lv_summary  TYPE string.

    lt_tadir = zcl_abapgit_aunit_objects=>get( mi_repo ).

    lo_timer = zcl_abapgit_timer=>create( iv_count = lines( lt_tadir ) )->start( ).

    lo_listener = to_object( call_method(
      iv_type   = c_type-gui_service
      iv_method = 'CREATE_LISTENER' ) ).

    TRY.
        CREATE OBJECT lo_factory TYPE (c_type-factory).
      CATCH cx_sy_create_object_error.
        zcx_abapgit_exception=>raise( |{ c_type-factory } cannot be instantiated| ).
    ENDTRY.

    lo_task = to_object( call_method(
      io_object = lo_factory
      iv_type   = c_type-factory
      iv_method = 'CREATE_TASK'
      ig_value  = lo_listener ) ).

    call_method(
      io_object = lo_task
      iv_type   = c_type-task
      iv_method = 'RESTRICT_RISK_LEVEL'
      ig_value  = c_risk_level_critical ).

    call_method(
      io_object = lo_task
      iv_type   = c_type-task
      iv_method = 'RESTRICT_DURATION_CATEGORY'
      ig_value  = c_duration_long ).

    lt_errors = add_objects(
      io_task  = lo_task
      it_tadir = lt_tadir ).

    IF contains_test_cases( lo_task ) = abap_true.
      " depending on the release, RUN has a LISTENER parameter, else the listener from CREATE_TASK is used
      call_method(
        io_object    = lo_task
        iv_type      = c_type-task
        iv_method    = 'RUN'
        ig_value     = lo_listener
        iv_parameter = 'LISTENER' ).

      lo_aunit = to_object( call_method(
        io_object = lo_listener
        iv_type   = c_type-listener
        iv_method = 'GET_RESULT_AFTER_END_OF_TASK' ) ).
    ENDIF.

    lv_summary = lo_timer->end( ).

    rs_result = zcl_abapgit_unit_test_result=>from_aunit( lo_aunit ).
    rs_result-summary = lv_summary.
    APPEND LINES OF lt_errors TO rs_result-errors.

  ENDMETHOD.
ENDCLASS.
