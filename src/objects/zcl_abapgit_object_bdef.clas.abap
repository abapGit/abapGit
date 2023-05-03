CLASS zcl_abapgit_object_bdef DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

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

    DATA mi_persistence TYPE REF TO if_wb_object_persist .
    DATA mi_wb_object_operator TYPE REF TO object .
    DATA mv_behaviour_definition_key TYPE seu_objkey .
    DATA mr_behaviour_definition TYPE REF TO data .

    METHODS clear_fields
      CHANGING
        !cs_metadata TYPE any .
    METHODS clear_field
      IMPORTING
        !iv_fieldname TYPE csequence
      CHANGING
        !cs_metadata  TYPE any .
    METHODS get_wb_object_operator
      RETURNING
        VALUE(ri_wb_object_operator) TYPE REF TO object
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
ENDCLASS.



CLASS zcl_abapgit_object_bdef IMPLEMENTATION.


  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.


  METHOD clear_fields.

    FIELD-SYMBOLS: <lv_links> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lv_value> TYPE data.
    FIELD-SYMBOLS <ls_item> TYPE any.

    clear_field(
      EXPORTING
        iv_fieldname          = 'VERSION'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CREATED_AT'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CREATED_BY'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CHANGED_AT'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CHANGED_BY'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'RESPONSIBLE'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'PACKAGE_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'CONTAINER_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MASTER_SYSTEM'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CHANGED_AT'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CHANGED_BY'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CREATED_AT'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CREATED_BY'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-RESPONSIBLE'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-PACKAGE_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CONTAINER_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-MASTER_SYSTEM'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'SYNTAX_CONFIGURATION'
      CHANGING
      cs_metadata = cs_metadata ).

    ASSIGN COMPONENT 'LINKS' OF STRUCTURE cs_metadata TO <lv_links>.
    ASSERT sy-subrc = 0.

    LOOP AT <lv_links> ASSIGNING <ls_item>.
      ASSIGN COMPONENT 'COMMON_ATTRIBUTES' OF STRUCTURE <ls_item> TO <lv_value>.
      ASSERT sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_behaviour_definition_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_behaviour_definition TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_BDEF_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        zcx_abapgit_exception=>raise( |BDEF not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_object_data.

    DATA:
      lr_metadata TYPE REF TO data,
      lr_data     TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_metadata_node> TYPE any,
      <ls_metadata>      TYPE any,
      <lv_source>        TYPE any,
      <lg_data>          TYPE any.

    CREATE DATA lr_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE <lg_data> TO <lv_metadata_node>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_metadata  TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA-METADATA').
    ASSIGN lr_metadata->* TO <ls_metadata>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'BDEF'
      CHANGING
        cg_data = <ls_metadata> ).

    <lv_metadata_node> = <ls_metadata>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lv_source>.
    ASSERT sy-subrc = 0.

    <lv_source> = zif_abapgit_object~mo_files->read_string( 'asbdef' ).

    CREATE OBJECT ro_object_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').

    ro_object_data->set_data(  p_data = <lg_data>  ).

  ENDMETHOD.


  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'BDEF'.
    ls_object_type-subtype_wb = 'BDO'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_behaviour_definition_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

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

    CREATE OBJECT lo_object_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
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

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    CREATE OBJECT ro_object_data_merged TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').

    CALL METHOD ro_object_data_merged->('SET_DATA')
      EXPORTING
        p_data = <ls_old>.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lo_object_data_merged TYPE REF TO if_wb_object_data_model,
      lo_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lr_wbobjtype          TYPE REF TO data,
      lr_category           TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_wbobjtype> TYPE any,
      <lv_category>  TYPE any,
      <lv_field>     TYPE any.

    TRY.

        lo_object_data = get_object_data( io_xml ).

        CREATE DATA lr_wbobjtype TYPE ('WBOBJTYPE').
        ASSIGN lr_wbobjtype->* TO <ls_wbobjtype>.
        ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'BDEF'.
        ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'BDO'.

        CREATE DATA lr_category TYPE ('WBADT_RESOURCE_CATEGORY').
        ASSIGN lr_category->* TO <lv_category>.

        CALL METHOD ('CL_BLUE_WB_UTILITY')=>('GET_RESOURCE_CATEGORY')
          EXPORTING
            is_object_type = <ls_wbobjtype>
          RECEIVING
            result         = <lv_category>.

        lo_wb_object_operator = get_wb_object_operator( ).

        tadir_insert( iv_package ).

        IF zif_abapgit_object~exists( ) = abap_false.
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'AL' " if_wb_object_data_selection_co=>c_all_data
                  version           = 'I'
                  package           = iv_package
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
                  version           = 'I'
                  package           = iv_package
                  transport_request = iv_transport.

              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN OTHERS.
          ENDCASE.
        ELSE.
          lo_object_data_merged = merge_object_data( lo_object_data ).
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I'
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key              = mv_behaviour_definition_key
            p_version                 = 'A'
            p_existence_check_only    = abap_true ).
        rv_bool = abap_true.

      CATCH cx_swb_exception.
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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDIC'
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
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_behaviour_definition> TYPE any,
      <lv_metadata>             TYPE any,
      <lv_source>               TYPE string.

    ASSIGN mr_behaviour_definition->* TO <ls_behaviour_definition>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_behaviour_definition>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_behaviour_definition> TO <lv_metadata>.
        ASSERT sy-subrc = 0.
        clear_fields( CHANGING cs_metadata = <lv_metadata> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_behaviour_definition> TO <lv_source>.
        ASSERT sy-subrc = 0.
        lv_source = <lv_source>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'BDEF'
        ig_data = <lv_metadata> ).

    zif_abapgit_object~mo_files->add_string(
        iv_ext    = 'asbdef'
        iv_string = lv_source ).

  ENDMETHOD.
ENDCLASS.
