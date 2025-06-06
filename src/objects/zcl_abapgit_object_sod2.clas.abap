CLASS zcl_abapgit_object_sod2 DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_xml_transformation_name TYPE string VALUE 'SOD2',
               c_data_model_class_name   TYPE string VALUE 'CL_APS_ODA_WBI_SOD2_DATA_MODEL'.

    METHODS create_wb_object_operator
      IMPORTING
        !is_object_type              TYPE wbobjtype
        !iv_object_key               TYPE seu_objkey
        !iv_transport_request        TYPE trkorr OPTIONAL
        !iv_do_commits               TYPE abap_bool DEFAULT abap_true
        !iv_run_in_test_mode         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_wb_object_operator) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    METHODS get_wb_object_operator
      IMPORTING
        !is_object_type              TYPE wbobjtype
        !iv_object_key               TYPE seu_objkey
        !iv_transport_request        TYPE trkorr OPTIONAL
      RETURNING
        VALUE(ro_wb_object_operator) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    METHODS clear_metadata_fields
      CHANGING
        !cs_data TYPE any.

    METHODS clear_content_fields
      CHANGING
        !cs_data TYPE any.

    METHODS clear_field
      IMPORTING
        !iv_fieldname TYPE csequence
      CHANGING
        !cs_metadata  TYPE any.

ENDCLASS.



CLASS zcl_abapgit_object_sod2 IMPLEMENTATION.


  METHOD clear_content_fields.

    FIELD-SYMBOLS <ls_content_data> TYPE any.

    ASSIGN COMPONENT 'CONTENT' OF STRUCTURE cs_data TO <ls_content_data>.

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_USER'
      CHANGING
        cs_metadata  = <ls_content_data> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_TIMESTAMP'
      CHANGING
        cs_metadata  = <ls_content_data> ).

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.


  METHOD clear_metadata_fields.

    FIELD-SYMBOLS <ls_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE cs_data TO <ls_metadata>.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = <ls_metadata> ).

  ENDMETHOD.


  METHOD constructor.

    DATA lo_data_model TYPE REF TO object.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    TRY.
        CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD create_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
            do_commits        = iv_do_commits
            run_in_test_mode  = iv_run_in_test_mode
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('GET_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_data_model  TYPE REF TO if_wb_object_data_model,
          lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        rv_user = lo_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lo_factory     TYPE REF TO object,
          lx_error       TYPE REF TO cx_root.

    ls_object_type-objtype_tr = ms_item-obj_type.
    lv_object_key             = ms_item-obj_name.

    TRY.

        lo_factory = get_wb_object_operator( is_object_type       = ls_object_type
                                             iv_object_key        = lv_object_key
                                             iv_transport_request = iv_transport ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~DELETE').

      CATCH cx_root INTO lx_error.

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_factory               TYPE REF TO object,
          lo_data_model            TYPE REF TO if_wb_object_data_model,
          lv_data_type_name        TYPE string,
          ls_data                  TYPE REF TO data,
          ls_object_type           TYPE wbobjtype,
          lv_object_key            TYPE seu_objkey,
          lo_logger                TYPE REF TO cl_wb_checklist,
          lx_create_error          TYPE REF TO cx_root,
          lx_error                 TYPE REF TO cx_root,
          lt_msgs                  TYPE TABLE OF string,
          lt_error_msgs_create     TYPE swbme_error_tab,
          ls_error_msg_create      LIKE LINE OF lt_error_msgs_create,
          lv_error_msg             TYPE string,
          lv_abap_language_version TYPE c LENGTH 1. " abap_language_version

    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).

    " if_wb_object_data_selection_co=>c_all_data
    CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
      EXPORTING
        p_data_selection = 'AL'
      RECEIVING
        result           = lv_data_type_name.

    CREATE DATA ls_data TYPE (lv_data_type_name).
    ASSIGN ls_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_transformation_name
      CHANGING
        cg_data = <ls_data> ).

    CALL METHOD lo_data_model->('SET_SELECTED_DATA')
      EXPORTING
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
        p_data           = <ls_data>.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        IF zif_abapgit_object~exists( ) = abap_true.

          CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_data_model
              version           = 'A'
              transport_request = iv_transport.

        ELSE.

          TRY.

              CALL METHOD lo_data_model->('GET_ABAP_LANGUAGE_VERSION')
                RECEIVING
                  result = lv_abap_language_version.

              CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data        = lo_data_model
                  version               = 'A'
                  package               = iv_package
                  abap_language_version = lv_abap_language_version
                  transport_request     = iv_transport
                IMPORTING
                  logger                = lo_logger.

            CATCH cx_root INTO lx_create_error.

              " Check for error messages from Workbench API to provide more error infos to user
              lo_logger->get_error_messages( IMPORTING p_error_tab = lt_error_msgs_create ).

              IF lt_error_msgs_create IS NOT INITIAL.

                LOOP AT lt_error_msgs_create INTO ls_error_msg_create.

                  APPEND LINES OF ls_error_msg_create-mtext TO lt_msgs.

                ENDLOOP.

                CONCATENATE LINES OF lt_msgs INTO lv_error_msg SEPARATED BY '; '.
                zcx_abapgit_exception=>raise( iv_text     = lv_error_msg
                                              ix_previous = lx_create_error ).

              ELSE.

                zcx_abapgit_exception=>raise_with_text( lx_create_error ).

              ENDIF.

          ENDTRY.

        ENDIF.

      CATCH cx_root INTO lx_error.

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CHECK_EXISTENCE')
          RECEIVING
            r_result = rv_bool.

      CATCH cx_root INTO lx_error.

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }*| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_data_model     TYPE REF TO if_wb_object_data_model,
          lv_data_type_name TYPE string,
          lo_factory        TYPE REF TO object,
          ls_object_type    TYPE wbobjtype,
          lv_object_key     TYPE seu_objkey,
          lx_error          TYPE REF TO cx_root.

    DATA ls_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        " if_wb_object_data_selection_co=>c_all_data
        CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
          EXPORTING
            p_data_selection = 'AL'
          RECEIVING
            result           = lv_data_type_name.

        CREATE DATA ls_data TYPE (lv_data_type_name).
        ASSIGN ls_data->* TO <ls_data>.

        CALL METHOD lo_data_model->('GET_SELECTED_DATA')
          EXPORTING
            p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            p_data           = <ls_data>.

        clear_metadata_fields( CHANGING cs_data = <ls_data> ).
        clear_content_fields( CHANGING cs_data = <ls_data> ).
        clear_field( EXPORTING iv_fieldname = 'PLUGIN_CONFIG'
                     CHANGING  cs_metadata  = <ls_data> ).

        io_xml->add( iv_name = c_xml_transformation_name
                     ig_data = <ls_data> ).

      CATCH cx_root INTO lx_error.

        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
