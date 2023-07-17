CLASS zcl_abapgit_object_sod2 DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: cv_xml_transformation_name TYPE string VALUE 'SOD2',
               cv_data_model_class_name   TYPE string VALUE 'CL_APS_ODA_WBI_SOD2_DATA_MODEL'.

ENDCLASS.



CLASS zcl_abapgit_object_sod2 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    TRY.

        DATA(lo_factory) = cl_wb_object_operator_factory=>get_object_operator( object_type = VALUE #( objtype_tr = ms_item-obj_type )
                                                                               object_key  = CONV #( ms_item-obj_name ) ).

        lo_factory->delete( ).

      CATCH cx_root INTO DATA(lx_error).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_data_model TYPE REF TO if_wb_object_data_model,
          ls_data       TYPE REF TO data.

    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE OBJECT lo_data_model TYPE (cv_data_model_class_name).

    DATA(lv_type) = lo_data_model->get_datatype_name( p_data_selection = if_wb_object_data_selection_co=>c_all_data ).
    CREATE DATA ls_data TYPE (lv_type).
    ASSIGN ls_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = cv_xml_transformation_name
      CHANGING
        cg_data = <ls_data> ).

    lo_data_model->set_selected_data( p_data_selection = if_wb_object_data_selection_co=>c_all_data
                                      p_data           = <ls_data> ).

    TRY.

        DATA(lo_factory) = cl_wb_object_operator_factory=>get_object_operator( object_type = VALUE #( objtype_tr = ms_item-obj_type )
                                                                               object_key  = CONV #( ms_item-obj_name ) ).

        DATA(lv_transport_request) = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

        IF zif_abapgit_object~exists( ) = abap_true.

*          lo_factory->read(
*            IMPORTING
*              eo_object_data = DATA(lo_existing_obj)
*          ).

*          lo_data_model->set_created_by( p_user_name = lo_existing_obj->get_created_by( ) ).
*
*          lo_existing_obj->get_created_on(
*            IMPORTING
*              p_date = DATA(lt_created_on_date)
*              p_time = DATA(lt_created_on_time)
*          ).
*
*          lo_data_model->set_created_on( p_date = lt_created_on_date
*                                         p_time = lt_created_on_time ).
*
*          lo_data_model->set_changed_by( p_user_name = sy-uname ).
*          lo_data_model->set_changed_on( p_date = sy-datlo
*                                         p_time = sy-timlo ).

          lo_factory->update(
            EXPORTING
              io_object_data    = lo_data_model
              version           = 'A'
              transport_request = lv_transport_request
          ).

        ELSE.

          TRY.

              lo_factory->create(
                EXPORTING
                  io_object_data        = lo_data_model
                  version               = 'A'
                  package               = iv_package
                  abap_language_version = lo_data_model->get_abap_language_version( )
                  transport_request     = lv_transport_request
                IMPORTING
                  logger                = DATA(lo_logger)
              ).

            CATCH cx_wb_object_operation_error INTO DATA(lx_create_error).

              " Check for error messages from Workbench API to provide more error infos to user
              IF lo_logger->has_error_messages( ).

                DATA lt_msgs TYPE TABLE OF string.
                lo_logger->get_error_messages(
                  IMPORTING
                    p_error_tab = DATA(lt_error_msgs_create)
                ).

                LOOP AT lt_error_msgs_create INTO DATA(ls_error_msg_create).
                  LOOP AT ls_error_msg_create-mtext INTO DATA(ls_mtext).
                    APPEND ls_mtext TO lt_msgs.
                  ENDLOOP.
                ENDLOOP.

                CONCATENATE LINES OF lt_msgs INTO DATA(lv_error_msg) SEPARATED BY '; '.
                zcx_abapgit_exception=>raise( iv_text     = lv_error_msg
                                              ix_previous = lx_create_error ).

              ELSE.

                zcx_abapgit_exception=>raise( iv_text     = lx_create_error->get_text( )
                                              ix_previous = lx_create_error->previous ).

              ENDIF.

          ENDTRY.

        ENDIF.

      CATCH cx_root INTO DATA(lx_error).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).

    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    TRY.

        DATA(lo_factory) = cl_wb_object_operator_factory=>get_object_operator( object_type = VALUE #( objtype_tr = ms_item-obj_type )
                                                                               object_key  = CONV #( ms_item-obj_name ) ).

        rv_bool = lo_factory->check_existence( ).

      CATCH cx_root INTO DATA(lx_error).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.

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

    zcl_abapgit_objects=>jump(
      is_item         = ms_item
    ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    TRY.

        DATA(lo_factory) = cl_wb_object_operator_factory=>create_object_operator( object_type = VALUE #( objtype_tr = ms_item-obj_type )
                                                                                  object_key  = CONV #( ms_item-obj_name ) ).

        lo_factory->read(
          IMPORTING
            eo_object_data = DATA(lo_data_model)
        ).

        lo_data_model->set_changed_by( p_user_name = '' ).
        lo_data_model->set_changed_on( p_date = sy-datum ).
        lo_data_model->set_created_by( p_user_name = '' ).
        lo_data_model->set_created_on( p_date = sy-datum ).

        DATA ls_data TYPE REF TO data.
        FIELD-SYMBOLS <ls_data> TYPE any.

        DATA(lv_type) = lo_data_model->get_datatype_name( p_data_selection = if_wb_object_data_selection_co=>c_all_data ).
        CREATE DATA ls_data TYPE (lv_type).
        ASSIGN ls_data->* TO <ls_data>.

        lo_data_model->get_selected_data(
          EXPORTING
            p_data_selection = if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            p_data           = <ls_data>
        ).

        io_xml->add( iv_name = cv_xml_transformation_name
                     ig_data = <ls_data> ).

      CATCH cx_root INTO DATA(lx_error).

        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
