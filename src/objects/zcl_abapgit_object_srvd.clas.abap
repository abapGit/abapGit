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


private section.

  aliases C_ABAP_VERSION_DEFAULT
    for ZIF_ABAPGIT_OBJECT~C_ABAP_VERSION_DEFAULT .
  aliases C_ABAP_VERSION_SAP_CP
    for ZIF_ABAPGIT_OBJECT~C_ABAP_VERSION_SAP_CP .
  aliases GC_STEP_ID
    for ZIF_ABAPGIT_OBJECT~GC_STEP_ID .
  aliases CHANGED_BY
    for ZIF_ABAPGIT_OBJECT~CHANGED_BY .
  aliases DELETE
    for ZIF_ABAPGIT_OBJECT~DELETE .
  aliases DESERIALIZE
    for ZIF_ABAPGIT_OBJECT~DESERIALIZE .
  aliases EXISTS
    for ZIF_ABAPGIT_OBJECT~EXISTS .
  aliases GET_COMPARATOR
    for ZIF_ABAPGIT_OBJECT~GET_COMPARATOR .
  aliases GET_DESERIALIZE_STEPS
    for ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS .
  aliases IS_LOCKED
    for ZIF_ABAPGIT_OBJECT~IS_LOCKED .
  aliases JUMP
    for ZIF_ABAPGIT_OBJECT~JUMP .
  aliases SERIALIZE
    for ZIF_ABAPGIT_OBJECT~SERIALIZE .

  data MI_PERSISTENCE type ref to IF_WB_OBJECT_PERSIST .
  data MV_SERVICE_DEFINITION_KEY type SEU_OBJKEY .
  data MR_SERVICE_DEFINITION type ref to DATA .
  constants MC_SOURCE_FILE type STRING value 'srvdsrv' ##NO_TEXT.
  constants MC_XML_PARENT_NAME type STRING value 'SRVD' ##NO_TEXT.
  data MO_OBJECT_OPERATOR type ref to IF_WB_OBJECT_OPERATOR .

  methods CLEAR_FIELDS
    changing
      !CS_SERVICE_DEFINITION type ANY .
  methods CLEAR_FIELD
    importing
      !IV_FIELDNAME type CSEQUENCE
    changing
      !CS_SERVICE_DEFINITION type ANY .
  methods GET_OBJECT_DATA
    importing
      !IO_XML type ref to ZIF_ABAPGIT_XML_INPUT
    returning
      value(RO_OBJECT_DATA) type ref to IF_WB_OBJECT_DATA_MODEL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_XML
    importing
      !IO_XML type ref to ZIF_ABAPGIT_XML_INPUT
    returning
      value(RS_DATA) type CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA-METADATA
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_SERIALIZER_VERSION
    returning
      value(RV_VERSION) type STRING .
  methods GET_TRANSPORT_REQ_IF_NEEDED
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RV_TRANSPORT_REQUEST) type TRKORR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_WB_OBJECT_OPERATOR
    returning
      value(RO_OBJECT_OPERATOR) type ref to IF_WB_OBJECT_OPERATOR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods MERGE_OBJECT_DATA
    importing
      !IO_OBJECT_DATA type ref to IF_WB_OBJECT_DATA_MODEL
      !IV_DATA_SELECTION type WB_OBJECT_DATA_SELECTION optional
    returning
      value(RO_OBJECT_DATA) type ref to IF_WB_OBJECT_DATA_MODEL
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_WB_OBJECT_OPERATION_ERROR .
  methods IS_DDIC
    returning
      value(RV_DDIC) type ABAP_BOOL .
  methods IS_DELETE_TADIR
    returning
      value(RV_DELETE_TADIR) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SRVD IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      li_object_data_model TYPE REF TO if_wb_object_data_model.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = mv_service_definition_key
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data_model ).

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_swb_exception.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    TRY.
        get_wb_object_operator( )->delete( ).
      CATCH cx_wb_object_operation_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lv_transport_request  TYPE trkorr,
      lv_category           TYPE wbadt_resource_category,
      lo_wb_object_operator TYPE REF TO if_wb_object_operator.

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
                  version               = swbm_version_active
                  package               = iv_package
                  transport_request     = lv_transport_request ).
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              lo_wb_object_operator->create(
                  io_object_data        = lo_object_data
                  data_selection        = if_wb_object_data_selection_co=>c_properties
                  version               = swbm_version_active
                  package               = iv_package
                  transport_request     = lv_transport_request ).
              lo_wb_object_operator->update(
                  io_object_data        = lo_object_data
                  data_selection        = if_wb_object_data_selection_co=>c_data_content
                  version               = swbm_version_active
                  transport_request     = lv_transport_request ).
            WHEN OTHERS.
              cx_abapgit_exception=>raise( |Category '{ lv_category }' not supported| ).
          ENDCASE.
        ELSE.
          CASE lv_category.
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              DATA(lo_merged_data_all) = merge_object_data( lo_object_data ).
              lo_wb_object_operator->update(
                  io_object_data    = lo_merged_data_all
                  data_selection    = if_wb_object_data_selection_co=>c_all_data
                  version           = swbm_version_active
                  transport_request = lv_transport_request ).
            WHEN if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              DATA(lo_merged_data_prop) = merge_object_data( lo_object_data ).
              DATA(lo_merged_data_cont) = merge_object_data( lo_object_data ).
              lo_wb_object_operator->update(
                  io_object_data    = lo_merged_data_prop
                  data_selection    = if_wb_object_data_selection_co=>c_properties
                  version           = swbm_version_active
                  transport_request = lv_transport_request ).
              lo_wb_object_operator->update(
                  io_object_data    = lo_merged_data_cont
                  data_selection    = if_wb_object_data_selection_co=>c_data_content
                  version           = swbm_version_active
                  transport_request = lv_transport_request ).
            WHEN OTHERS.
              zcx_abapgit_exception=>raise( |Category '{ lv_category }' not supported| ).
          ENDCASE.

        ENDIF.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise(
            iv_text     = lx_error->get_text( )
            ix_previous = lx_error ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lo_object_data TYPE REF TO if_wb_object_data_model.

    TRY.
        get_wb_object_operator( )->read(
          EXPORTING
            data_selection = if_wb_object_data_selection_co=>c_properties
          IMPORTING
            eo_object_data = lo_object_data
        ).

        IF lo_object_data IS INITIAL OR lo_object_data->get_object_key( ) IS INITIAL.
          rv_bool = abap_false.
        ELSE.
          rv_bool = abap_true.
        ENDIF.
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
    rs_metadata-version      = get_serializer_version( ).
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
      lo_object_data     TYPE REF TO if_wb_object_data_model.

    TRY.
        lo_object_operator = cl_wb_object_operator_factory=>create_object_operator(
                               object_type = if_srvd_types=>co_global_objtype
                               object_key  = CONV #( ms_item-obj_name )
                                ).

        lo_object_operator->read(
          EXPORTING
            version        = swbm_version_active
            data_selection = if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            eo_object_data = lo_object_data ).

        IF lo_object_data IS BOUND.
          lo_object_data->get_data(
            IMPORTING
              p_data = ls_data
          ).

          CLEAR ls_data-metadata-version.
          CLEAR ls_data-metadata-changed_at.
          CLEAR ls_data-metadata-changed_by.
          CLEAR ls_data-metadata-created_at.
          CLEAR ls_data-metadata-created_by.
          CLEAR ls_data-metadata-responsible.
          CLEAR ls_data-metadata-package_ref.
          CLEAR ls_data-metadata-master_system.
          CLEAR ls_data-metadata-dt_uuid.

          mo_files->add_string( iv_ext = mc_source_file iv_string = ls_data-content-source ).

          CLEAR ls_data-content-source.

          io_xml->add( iv_name = mc_xml_parent_name ig_data = ls_data-metadata ).
        ENDIF.

      CATCH cx_wb_object_operation_error INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error->previous ).
    ENDTRY.

  ENDMETHOD.


  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'CONTENT-SOURCE'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_LANGUAGE'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-RESPONSIBLE'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-DT_UUID'
      CHANGING
        cs_service_definition = cs_service_definition ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-PACKAGE_REF-NAME'
      CHANGING
        cs_service_definition = cs_service_definition ).

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_service_definition
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

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
        p_data           = ls_new
    ).

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
        p_data           = ls_old
    ).

    ls_old-metadata-description = ls_new-metadata-description.
    ls_old-content-source = ls_new-content-source.

    ro_object_data = NEW cl_srvd_wb_object_data( ).
    ro_object_data->set_data( ls_old ).
  ENDMETHOD.


  METHOD is_delete_tadir.
    rv_delete_tadir = abap_true.
  ENDMETHOD.


  METHOD is_ddic.
    rv_ddic = abap_true.
  ENDMETHOD.


  METHOD get_wb_object_operator.
    IF mo_object_operator IS INITIAL.
      TRY.
          ro_object_operator = cl_wb_object_operator_factory=>create_object_operator(
                       object_type = if_srvd_types=>co_global_objtype
                       object_key  = CONV #( ms_item-obj_name )
                        ).
          mo_object_operator = ro_object_operator.
        CATCH cx_wb_object_operation_error INTO DATA(lx_error).
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


  method GET_SERIALIZER_VERSION.
  endmethod.


  METHOD get_object_data.
    DATA ls_data TYPE cl_srvd_wb_object_data=>ty_srvd_object_data.

    ls_data-metadata = deserialize_xml( io_xml ).
    ls_data-content-source = mo_files->read_string( mc_source_file ).

    ro_object_data = NEW cl_srvd_wb_object_data( ).
    ro_object_data->set_data( ls_data ).
  ENDMETHOD.


  METHOD deserialize_xml.
    DATA(ls_metadata) = io_xml->get_metadata( ).

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
