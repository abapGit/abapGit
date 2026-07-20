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
        iv_desd_name      TYPE sobj_name
      RETURNING
        VALUE(ro_handler) TYPE REF TO object.

ENDCLASS.


CLASS zcl_abapgit_object_desd IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    DATA lo_handler       TYPE REF TO object.
    DATA lx_error         TYPE REF TO cx_root.
    FIELD-SYMBOLS <lv_getstate_enum_value> TYPE any.

    TRY.
        lo_handler = _create_les_handler( ms_item-obj_name ).
        IF ms_item-inactive = abap_true.
          ASSIGN ('IF_DD_LES_PERSIST=>S_GET_STATE-NEWEST') TO <lv_getstate_enum_value>.
        ELSE.
          ASSIGN ('IF_DD_LES_PERSIST=>S_GET_STATE-ACTIVE') TO <lv_getstate_enum_value>.
        ENDIF.
        CALL METHOD lo_handler->('IF_DD_LES_HANDLER~GET_CHANGED_BY')
          EXPORTING
            iv_state      = <lv_getstate_enum_value>
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
    DATA lr_desd_name             TYPE REF TO data.
    FIELD-SYMBOLS <lv_logger_object> TYPE any.
    FIELD-SYMBOLS <lv_desd_name> TYPE any.

    CALL METHOD ('CL_DD_LOG_FACTORY')=>('CREATE_RESTRICTED_LOGGER')
      EXPORTING
        iv_log_id = -1
      RECEIVING
        ro_logger = lo_dd_logger.

    lr_logger_type_descr = cl_abap_typedescr=>describe_by_name( 'CL_DD_LOG_ABS_LOGGER' ).
    lr_logger_ref_descr = cl_abap_refdescr=>get( lr_logger_type_descr ).
    CREATE DATA lr_data_of_logger_object TYPE HANDLE lr_logger_ref_descr.
    ASSIGN lr_data_of_logger_object->* TO <lv_logger_object>.
    <lv_logger_object> ?= lo_dd_logger.


    CREATE DATA lr_desd_name TYPE ('DD_LES_NAME').
    ASSIGN lr_desd_name->* TO <lv_desd_name>.
    <lv_desd_name> = iv_desd_name.

    CREATE OBJECT lo_handler_fctry TYPE ('CL_DD_LES_HANDLER_FACTORY').
    CALL METHOD lo_handler_fctry->('CREATE')
      EXPORTING
        io_logger = <lv_logger_object>
        iv_name   = <lv_desd_name>
      RECEIVING
        r_result  = ro_handler.
  ENDMETHOD.

ENDCLASS.
