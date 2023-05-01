CLASS zcl_abapgit_object_srvb DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.
  PROTECTED SECTION.
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
    METHODS get_wb_object_operator
      RETURNING
        VALUE(ro_object_operator) TYPE REF TO object
      RAISING
        zcx_abapgit_exception .
    METHODS merge_object_data
      IMPORTING
        !io_object_data              TYPE REF TO object
      RETURNING
        VALUE(ro_object_data_merged) TYPE REF TO if_wb_object_data_model
      RAISING
        zcx_abapgit_exception .
    METHODS get_object_data
      IMPORTING
        !io_xml               TYPE REF TO zif_abapgit_xml_input
      RETURNING
        VALUE(ro_object_data) TYPE REF TO if_wb_object_data_model
      RAISING
        zcx_abapgit_exception .
    METHODS is_ai_supported
      RETURNING VALUE(rv_ai_supported) TYPE abap_bool.

    DATA:
      mi_persistence           TYPE REF TO if_wb_object_persist,
      mv_is_inactive_supported TYPE abap_bool,
      mv_service_binding_key   TYPE seu_objkey,
      mr_service_binding       TYPE REF TO data,
      mr_srvb_svrs_config      TYPE REF TO object,
      mo_object_operator       TYPE REF TO object.
ENDCLASS.



CLASS zcl_abapgit_object_srvb IMPLEMENTATION.


  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_service_binding
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.


  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-VERSION'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_service_binding = cs_service_binding ).

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

    clear_field(
      EXPORTING
      iv_fieldname          = 'METADATA-PACKAGE_REF'
      CHANGING
      cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'METADATA-MASTER_SYSTEM'
      CHANGING
      cs_service_binding = cs_service_binding ).


  ENDMETHOD.


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

    mv_is_inactive_supported = is_ai_supported(  ).

  ENDMETHOD.


  METHOD get_object_data.

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


    " We have to set the language explicitly,
    " because otherwise the description isn't stored
    ASSIGN COMPONENT 'METADATA-LANGUAGE' OF STRUCTURE <ls_service_binding>
           TO <lv_language>.
    ASSERT sy-subrc = 0.
    <lv_language> = mv_language.

    CREATE OBJECT ro_object_data TYPE ('CL_SRVB_OBJECT_DATA').
    ro_object_data->set_data( p_data = <ls_service_binding>  ).

  ENDMETHOD.


  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mo_object_operator IS BOUND.
      ro_object_operator = mo_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'SRVB'.
    ls_object_type-subtype_wb = 'SVB'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_service_binding_key
          RECEIVING
            result      = mo_object_operator.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ro_object_operator = mo_object_operator.

  ENDMETHOD.


  METHOD is_ai_supported.
    TRY.
        CREATE OBJECT mr_srvb_svrs_config TYPE ('CL_SRVB_SVRS_CONFIG')
          EXPORTING iv_objtype = 'SRVB'.
      CATCH cx_sy_create_error.
        rv_ai_supported = abap_false.
    ENDTRY.
    CALL METHOD mr_srvb_svrs_config->('HAS_INACTIVE_VERSION')
      RECEIVING
        rv_has_inactive = rv_ai_supported.

  ENDMETHOD.


  METHOD merge_object_data.

    DATA:
      lo_object_data        TYPE REF TO object,
      lo_object_data_old    TYPE REF TO if_wb_object_data_model,
      lr_new                TYPE REF TO data,
      lr_old                TYPE REF TO data,
      lo_wb_object_operator TYPE REF TO object.

    FIELD-SYMBOLS:
      <ls_new>       TYPE any,
      <ls_old>       TYPE any,
      <lv_field_old> TYPE any,
      <lv_field_new> TYPE any.

    CREATE OBJECT lo_object_data TYPE ('CL_SRVB_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_SRVB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_SRVB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_old->* TO <ls_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD lo_object_data->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL'
      IMPORTING
        p_data           = <ls_new>.

    lo_wb_object_operator = get_wb_object_operator( ).

    CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      EXPORTING
        data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        eo_object_data = lo_object_data_old.

    CALL METHOD lo_object_data_old->('GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        p_data           = <ls_old>.

    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    CREATE OBJECT ro_object_data_merged TYPE ('CL_SRVB_OBJECT_DATA').

    CALL METHOD ro_object_data_merged->('SET_DATA')
      EXPORTING
        p_data = <ls_old>.

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
        CALL FUNCTION 'DEQUEUE_ESWB_EO'
          EXPORTING
            objtype = ms_item-obj_type
            objname = ms_item-obj_name.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.


    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lo_wb_object_operator TYPE REF TO object,
      lo_merged_data_all    TYPE REF TO if_wb_object_data_model,
      lv_version            TYPE r3state.

    TRY.
        lo_object_data = get_object_data( io_xml ).
        lo_wb_object_operator = get_wb_object_operator( ).

        IF mv_is_inactive_supported = abap_true.
          lv_version = 'I'.
        ELSE.
          lv_version = 'A'.
        ENDIF.

        tadir_insert( iv_package ).

        IF zif_abapgit_object~exists( ) = abap_false.
          "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
          CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
            EXPORTING
              io_object_data    = lo_object_data
              data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
              version           = lv_version
              package           = iv_package
              transport_request = iv_transport.

        ELSE.

          lo_merged_data_all = merge_object_data( lo_object_data ).
          CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_merged_data_all
              data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
              version           = lv_version
              transport_request = iv_transport.

        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        CALL FUNCTION 'DEQUEUE_ESWB_EO'
          EXPORTING
            objtype = ms_item-obj_type
            objname = ms_item-obj_name.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lo_object_data TYPE REF TO if_wb_object_data_model.

    TRY.
        IF mv_is_inactive_supported = abap_true.
          TRY.
              mi_persistence->get(
                EXPORTING
                  p_object_key     = mv_service_binding_key
                  p_version        = 'I'
                  p_data_selection = 'ST'
                CHANGING
                  p_object_data    = lo_object_data ).

            CATCH cx_root.
              mi_persistence->get(
                EXPORTING
                  p_object_key     = mv_service_binding_key
                  p_version        = 'A'
                  p_data_selection = 'ST'
                CHANGING
                  p_object_data    = lo_object_data ).

          ENDTRY.
        ELSE.

          mi_persistence->get(
            EXPORTING
              p_object_key     = mv_service_binding_key
              p_version        = 'A'
              p_data_selection = 'ST'
            CHANGING
              p_object_data    = lo_object_data ).

        ENDIF.
        rv_bool = boolc( lo_object_data IS NOT INITIAL AND lo_object_data->get_object_key( ) IS NOT INITIAL ).
      CATCH cx_root.
        rv_bool = abap_false.
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
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_service_binding> TYPE any.

    ASSIGN mr_service_binding->* TO <ls_service_binding>.
    ASSERT sy-subrc = 0.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).


        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
            data_selection = 'AL'
          IMPORTING
            eo_object_data = li_object_data_model.

        li_object_data_model->get_data( IMPORTING p_data = <ls_service_binding> ).

        clear_fields( CHANGING cs_service_binding = <ls_service_binding> ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'SRVB'
        ig_data = <ls_service_binding> ).

  ENDMETHOD.
ENDCLASS.
