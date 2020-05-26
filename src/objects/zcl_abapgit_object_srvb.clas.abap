CLASS zcl_abapgit_object_srvb DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS:
      clear_fields
        CHANGING
          cs_service_binding TYPE any,

      clear_field
        IMPORTING
          iv_fieldname       TYPE csequence
        CHANGING
          cs_service_binding TYPE any.

    DATA:
      mi_persistence         TYPE REF TO if_wb_object_persist,
      mv_service_binding_key TYPE seu_objkey,
      mr_service_binding     TYPE REF TO data.

ENDCLASS.



CLASS zcl_abapgit_object_srvb IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_service_binding_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_service_binding TYPE ('CL_SRVB_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_SRVB_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        zcx_abapgit_exception=>raise( |SRVB not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.

    DATA:
      li_object_data_model TYPE REF TO if_wb_object_data_model.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = mv_service_binding_key
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data_model ).

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_swb_exception.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_swb_exception.

    TRY.
        mi_persistence->delete( mv_service_binding_key ).

      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      li_object_data_model TYPE REF TO if_wb_object_data_model,
      lx_error             TYPE REF TO cx_swb_exception,
      lv_access_mode       TYPE c LENGTH 8. " if_wb_adt_rest_resource_co=>ty_access_mode

    FIELD-SYMBOLS:
      <ls_service_binding> TYPE any,
      <lv_language>        TYPE data.

    ASSIGN mr_service_binding->* TO <ls_service_binding>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'SRVB'
      CHANGING
        cg_data = <ls_service_binding> ).

    TRY.
        IF zif_abapgit_object~exists( ) = abap_true.
          lv_access_mode = 'MODIFY'. " cl_wb_adt_rest_resource=>co_access_mode_modify
        ELSE.
          lv_access_mode = 'INSERT'. " cl_wb_adt_rest_resource=>co_access_mode_insert.
          tadir_insert( iv_package ).
        ENDIF.

        " We have to set the language explicitly,
        " because otherwise the description isn't stored
        ASSIGN COMPONENT 'METADATA-LANGUAGE' OF STRUCTURE <ls_service_binding>
               TO <lv_language>.
        ASSERT sy-subrc = 0.
        <lv_language> = mv_language.

        CREATE OBJECT li_object_data_model TYPE ('CL_SRVB_OBJECT_DATA').
        li_object_data_model->set_data( <ls_service_binding> ).

        CALL METHOD mi_persistence->('SAVE')
          EXPORTING
            p_object_data = li_object_data_model
            p_access_mode = lv_access_mode. " does not exist in 702

        corr_insert( iv_package ).

      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key           = mv_service_binding_key
            p_version              = 'A'
            p_existence_check_only = abap_true ).

        rv_bool = abap_true.

      CATCH cx_swb_object_does_not_exist cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).

    rs_metadata-ddic         = abap_true.
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |RC={ sy-subrc } from RS_TOOL_ACCESS| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      li_object_data_model TYPE REF TO if_wb_object_data_model,
      lx_error             TYPE REF TO cx_swb_exception.

    FIELD-SYMBOLS:
      <ls_service_binding> TYPE any.

    ASSIGN mr_service_binding->* TO <ls_service_binding>.
    ASSERT sy-subrc = 0.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = mv_service_binding_key
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data_model ).

        li_object_data_model->get_data( IMPORTING p_data = <ls_service_binding> ).

        clear_fields( CHANGING cs_service_binding = <ls_service_binding> ).

      CATCH cx_swb_exception INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'SRVB'
        ig_data = <ls_service_binding> ).

  ENDMETHOD.


  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-LANGUAGE'
      CHANGING
        cs_service_binding = cs_service_binding ).

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_service_binding
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.

ENDCLASS.
