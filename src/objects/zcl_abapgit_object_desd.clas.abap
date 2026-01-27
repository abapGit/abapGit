CLASS zcl_abapgit_object_desd DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by            REDEFINITION.
    METHODS zif_abapgit_object~get_deserialize_steps REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS _create_les_handler
      IMPORTING
        ir_desd_name      TYPE REF TO data
      RETURNING
        VALUE(ro_handler) TYPE REF TO object
      RAISING
        cx_sy_ref_creation.

ENDCLASS.



CLASS zcl_abapgit_object_desd IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    DATA lo_handler       TYPE REF TO object.
    DATA lr_getstate_enum TYPE REF TO data.
    DATA lx_error         TYPE REF TO cx_root.
    DATA lr_desd_name     TYPE REF TO data.

    TRY.
        CREATE DATA lr_desd_name TYPE ('DD_LES_NAME').
        lr_desd_name->* = ms_item-obj_name.

        lo_handler = _create_les_handler( ir_desd_name = lr_desd_name ).

        CREATE DATA lr_getstate_enum TYPE ('IF_DD_LES_PERSIST=>EN_GET_STATE').
        IF ms_item-inactive = abap_true.
          lr_getstate_enum->* = CONV #( 2 ).
        ELSE.
          lr_getstate_enum->* = CONV #( 0 ).
        ENDIF.

        CALL METHOD lo_handler->('IF_DD_LES_HANDLER~GET_CHANGED_BY')
          EXPORTING
            iv_state      = lr_getstate_enum->*
          RECEIVING
            rv_changed_by = rv_user.
      CATCH cx_root INTO lx_error ##CATCH_ALL.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    INSERT zif_abapgit_object=>gc_step_id-ddic INTO TABLE rt_steps.
  ENDMETHOD.

  METHOD _create_les_handler.
    DATA lo_handler_fctry         TYPE REF TO object.
    DATA lo_dd_logger             TYPE REF TO object.
    DATA lr_data_of_logger_object TYPE REF TO data.
    DATA lr_logger_type_descr     TYPE REF TO cl_abap_typedescr.
    DATA lr_logger_ref_descr      TYPE REF TO cl_abap_refdescr.

    CALL METHOD ('CL_DD_LOG_FACTORY')=>('CREATE_RESTRICTED_LOGGER')
      EXPORTING
        iv_log_id = -1
      RECEIVING
        ro_logger = lo_dd_logger.
    CREATE OBJECT lo_handler_fctry TYPE ('CL_DD_LES_HANDLER_FACTORY').

    lr_logger_type_descr = cl_abap_typedescr=>describe_by_name( 'CL_DD_LOG_ABS_LOGGER' ).

    lr_logger_ref_descr = cl_abap_refdescr=>get(
      p_referenced_type = lr_logger_type_descr ).
    CREATE DATA lr_data_of_logger_object TYPE HANDLE lr_logger_ref_descr.
    lr_data_of_logger_object->* ?= lo_dd_logger.

    CALL METHOD lo_handler_fctry->('CREATE')
      EXPORTING
        io_logger = lr_data_of_logger_object->*
        iv_name   = ir_desd_name->*
      RECEIVING
        r_result  = ro_handler.
  ENDMETHOD.

ENDCLASS.
