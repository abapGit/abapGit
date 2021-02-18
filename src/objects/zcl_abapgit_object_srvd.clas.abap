CLASS zcl_abapgit_object_srvd DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

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

  PROTECTED SECTION.


  PRIVATE SECTION.

    ALIASES c_abap_version_default
      FOR zif_abapgit_object~c_abap_version_default .
    ALIASES c_abap_version_sap_cp
      FOR zif_abapgit_object~c_abap_version_sap_cp .
    ALIASES gc_step_id
      FOR zif_abapgit_object~gc_step_id .
    ALIASES changed_by
      FOR zif_abapgit_object~changed_by .
    ALIASES delete
      FOR zif_abapgit_object~delete .
    ALIASES deserialize
      FOR zif_abapgit_object~deserialize .
    ALIASES exists
      FOR zif_abapgit_object~exists .
    ALIASES get_comparator
      FOR zif_abapgit_object~get_comparator .
    ALIASES get_deserialize_steps
      FOR zif_abapgit_object~get_deserialize_steps .
    ALIASES is_locked
      FOR zif_abapgit_object~is_locked .
    ALIASES jump
      FOR zif_abapgit_object~jump .
    ALIASES serialize
      FOR zif_abapgit_object~serialize .

    DATA mi_persistence TYPE REF TO if_wb_object_persist .
    DATA mv_service_definition_key TYPE seu_objkey .
    DATA mr_service_definition TYPE REF TO data .
    CONSTANTS mc_source_file TYPE string VALUE 'srvdsrv' ##NO_TEXT.
    CONSTANTS mc_xml_parent_name TYPE string VALUE 'SRVD' ##NO_TEXT.
    DATA mo_object_operator TYPE REF TO if_wb_object_operator .

    METHODS get_object_data
      IMPORTING
        !io_xml               TYPE REF TO zif_abapgit_xml_input
      RETURNING
        VALUE(ro_object_data) TYPE REF TO if_wb_object_data_model
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_xml
      IMPORTING
        !io_xml        TYPE REF TO zif_abapgit_xml_input
      RETURNING
        VALUE(rs_data) TYPE cl_srvd_wb_object_data=>ty_srvd_object_data-metadata
      RAISING
        zcx_abapgit_exception .
    METHODS get_serializer_version
      RETURNING
        VALUE(rv_version) TYPE string .
    METHODS get_transport_req_if_needed
      IMPORTING
        !iv_package                 TYPE devclass
      RETURNING
        VALUE(rv_transport_request) TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS get_wb_object_operator
      RETURNING
        VALUE(ro_object_operator) TYPE REF TO if_wb_object_operator
      RAISING
        zcx_abapgit_exception .
    METHODS merge_object_data
      IMPORTING
        !io_object_data       TYPE REF TO if_wb_object_data_model
        !iv_data_selection    TYPE wb_object_data_selection OPTIONAL
      RETURNING
        VALUE(ro_object_data) TYPE REF TO if_wb_object_data_model
      RAISING
        zcx_abapgit_exception
        cx_wb_object_operation_error .
    METHODS is_ddic
      RETURNING
        VALUE(rv_ddic) TYPE abap_bool .
    METHODS is_delete_tadir
      RETURNING
        VALUE(rv_delete_tadir) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SRVD IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA lo_object_data TYPE REF TO if_wb_object_data_model.
    DATA lo_wb_object_operator TYPE REF TO if_wb_object_operator.
    DATA lx_error TYPE REF TO cx_wb_object_operation_error.
    TRY.
        lo_wb_object_operator = get_wb_object_operator( ).
        lo_wb_object_operator->read(
          EXPORTING
            data_selection = if_wb_object_data_selection_co=>c_properties
          IMPORTING
            eo_object_data = lo_object_data ).

        rv_user = lo_object_data->get_changed_by( ).

      CATCH cx_wb_object_operation_error INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).
    ENDTRY.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA lx_error TYPE REF TO cx_wb_object_operation_error.
    DATA lo_wb_object_operator TYPE REF TO if_wb_object_operator.
    TRY.
        lo_wb_object_operator = get_wb_object_operator( ).
        lo_wb_object_operator->delete( ).
      CATCH cx_wb_object_operation_error INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_wb_object_operation_error,
      lv_transport_request  TYPE trkorr,
      lv_category           TYPE wbadt_resource_category,
      lo_wb_object_operator TYPE REF TO if_wb_object_operator,
      lo_merged_data_all    TYPE REF TO if_wb_object_data_model,
      lo_merged_data_prop   TYPE REF TO if_wb_object_data_model,
      lo_merged_data_cont   TYPE REF TO if_wb_object_data_model.

    TRY.
        lo_object_data = get_object_data( io_xml ).
        lv_transport_request = get_transport_req_if_needed( iv_package ).
        lv_category = cl_blue_wb_utility=>get_resource_category( is_object_type = if_srvd_types=>co_global_objtype ).
        lo_wb_object_operator = get_wb_object_operator( ).

        IF exists( ) = abap_false.
          CASE lv_category.
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              lo_wb_object_operator->create(
                   io_object_data        = lo_object_data
                   data_selection        = if_wb_object_data_selection_co=>c_all_data
                   version               = swbm_version_inactive
                   package               = iv_package
                   transport_request     = lv_transport_request ).
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              lo_wb_object_operator->create(
                  io_object_data        = lo_object_data
                  data_selection        = if_wb_object_data_selection_co=>c_properties
                  version               = swbm_version_inactive
                  package               = iv_package
                  transport_request     = lv_transport_request ).
              lo_wb_object_operator->update(
                  io_object_data        = lo_object_data
                  data_selection        = if_wb_object_data_selection_co=>c_data_content
                  version               = swbm_version_inactive
                  transport_request     = lv_transport_request ).
            WHEN OTHERS.
              zcx_abapgit_exception=>raise( |Category '{ lv_category }' not supported| ).
          ENDCASE.
        ELSE.
          CASE lv_category.
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              lo_merged_data_all = merge_object_data( lo_object_data ).
              lo_wb_object_operator->update(
                  io_object_data    = lo_merged_data_all
                  data_selection    = if_wb_object_data_selection_co=>c_all_data
                  version           = swbm_version_inactive
                  transport_request = lv_transport_request ).
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              lo_merged_data_prop = merge_object_data( lo_object_data ).
              lo_merged_data_cont = merge_object_data( lo_object_data ).
              lo_wb_object_operator->update(
                  io_object_data    = lo_merged_data_prop
                  data_selection    = if_wb_object_data_selection_co=>c_properties
                  version           = swbm_version_inactive
                  transport_request = lv_transport_request ).
              lo_wb_object_operator->update(
                  io_object_data    = lo_merged_data_cont
                  data_selection    = if_wb_object_data_selection_co=>c_data_content
                  version           = swbm_version_inactive
                  transport_request = lv_transport_request ).
            WHEN OTHERS.
              zcx_abapgit_exception=>raise( |Category '{ lv_category }' not supported| ).
          ENDCASE.

        ENDIF.

      CATCH cx_wb_object_operation_error INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lo_object_data TYPE REF TO if_wb_object_data_model.
    DATA lo_wb_object_operator TYPE REF TO if_wb_object_operator.

    TRY.
        lo_wb_object_operator = get_wb_object_operator( ).
        lo_wb_object_operator->read(
          EXPORTING
            data_selection = if_wb_object_data_selection_co=>c_properties
          IMPORTING
            eo_object_data = lo_object_data ).
        rv_bool = boolc( lo_object_data IS NOT INITIAL AND lo_object_data->get_object_key( ) IS NOT INITIAL ).
      CATCH cx_wb_object_operation_error.
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
    rs_metadata              = get_metadata( ).
    rs_metadata-ddic         = is_ddic( ).
    rs_metadata-delete_tadir = is_delete_tadir( ).
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
      ls_data            TYPE cl_srvd_wb_object_data=>ty_srvd_object_data,
      lo_object_operator TYPE REF TO if_wb_object_operator,
      lo_object_data     TYPE REF TO if_wb_object_data_model,
      lx_error           TYPE REF TO cx_wb_object_operation_error,
      lv_object_key      TYPE seu_objkey.

    TRY.
        lv_object_key = ms_item-obj_name.
        lo_object_operator = cl_wb_object_operator_factory=>create_object_operator(
                               object_type = if_srvd_types=>co_global_objtype
                               object_key  = lv_object_key ).

        lo_object_operator->read(
          EXPORTING
            version        = swbm_version_active
            data_selection = if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            eo_object_data = lo_object_data ).

        IF lo_object_data IS BOUND.
          lo_object_data->get_data( IMPORTING p_data = ls_data ).

          CLEAR ls_data-metadata-version.
          CLEAR ls_data-metadata-changed_at.
          CLEAR ls_data-metadata-changed_by.
          CLEAR ls_data-metadata-created_at.
          CLEAR ls_data-metadata-created_by.
          CLEAR ls_data-metadata-responsible.
          CLEAR ls_data-metadata-package_ref.
          CLEAR ls_data-metadata-master_system.
          CLEAR ls_data-metadata-dt_uuid.
          CLEAR ls_data-metadata-abap_language_version.
          CLEAR ls_data-metadata-links.

          mo_files->add_string( iv_ext = mc_source_file
                                iv_string = ls_data-content-source ).

          CLEAR ls_data-content-source.

          io_xml->add( iv_name = mc_xml_parent_name
                       ig_data = ls_data-metadata ).
        ENDIF.

      CATCH cx_wb_object_operation_error INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).
    ENDTRY.

  ENDMETHOD.


  METHOD merge_object_data.
    DATA ls_new TYPE cl_srvd_wb_object_data=>ty_srvd_object_data.
    DATA ls_old TYPE cl_srvd_wb_object_data=>ty_srvd_object_data.
    DATA lo_wb_object_operator TYPE REF TO if_wb_object_operator.
    DATA lo_old_object_data TYPE REF TO if_wb_object_data_model.

    io_object_data->get_data(
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        p_data           = ls_new ).

    lo_wb_object_operator = get_wb_object_operator( ).

    lo_wb_object_operator->read(
      EXPORTING
        data_selection = if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        eo_object_data = lo_old_object_data ).

    lo_old_object_data->get_data(
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        p_data           = ls_old ).

    ls_old-metadata-description = ls_new-metadata-description.
    ls_old-content-source = ls_new-content-source.

    CREATE OBJECT ro_object_data TYPE cl_srvd_wb_object_data.
    ro_object_data->set_data( ls_old ).
  ENDMETHOD.


  METHOD is_delete_tadir.
    rv_delete_tadir = abap_true.
  ENDMETHOD.


  METHOD is_ddic.
    rv_ddic = abap_true.
  ENDMETHOD.


  METHOD get_wb_object_operator.
    DATA lx_error TYPE REF TO cx_wb_object_operation_error.
    DATA lv_object_key TYPE seu_objkey.
    IF mo_object_operator IS INITIAL.
      TRY.
          lv_object_key = ms_item-obj_name.
          ro_object_operator = cl_wb_object_operator_factory=>create_object_operator(
                       object_type = if_srvd_types=>co_global_objtype
                       object_key  = lv_object_key ).
          mo_object_operator = ro_object_operator.
        CATCH cx_wb_object_operation_error INTO lx_error.
          zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                        ix_previous = lx_error->previous ).
      ENDTRY.
    ELSE.
      ro_object_operator = mo_object_operator.
    ENDIF.
  ENDMETHOD.


  METHOD get_transport_req_if_needed.

    DATA: li_sap_package TYPE REF TO zif_abapgit_sap_package.

    li_sap_package = zcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_sap_package->are_changes_recorded_in_tr_req( ) = abap_true.
      rv_transport_request = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.
    ENDIF.

  ENDMETHOD.


  METHOD get_serializer_version.
    rv_version = 'v1.0.0'.
  ENDMETHOD.


  METHOD get_object_data.
    DATA ls_data TYPE cl_srvd_wb_object_data=>ty_srvd_object_data.

    ls_data-metadata = deserialize_xml( io_xml ).
    ls_data-content-source = mo_files->read_string( mc_source_file ).

    CREATE OBJECT ro_object_data TYPE cl_srvd_wb_object_data.
    ro_object_data->set_data( ls_data ).
  ENDMETHOD.


  METHOD deserialize_xml.
    DATA ls_metadata TYPE zif_abapgit_definitions=>ty_metadata.
    ls_metadata = io_xml->get_metadata( ).

    IF ls_metadata-version = get_serializer_version( ).
      io_xml->read(
        EXPORTING
          iv_name = mc_xml_parent_name
        CHANGING
          cg_data = rs_data ).
    ELSE.
      zcx_abapgit_exception=>raise( iv_text = |Unsupported serializer version { ls_metadata-version }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
