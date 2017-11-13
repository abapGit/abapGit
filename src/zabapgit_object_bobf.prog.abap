"Internally, BOPF is BOBF. This nomenclature is followed here.
CLASS lcl_object_bobf DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras .

    TYPES: BEGIN OF ty_node,
             node         TYPE /bobf/s_conf_model_api_node,
             root         TYPE abap_bool,
             id           TYPE int4,
             children_ids TYPE int4_table,
           END OF ty_node,
           ty_nodes_tt TYPE STANDARD TABLE OF ty_node.
  PRIVATE SECTION.
    DATA:
      mv_current_id TYPE int4,
      mv_key        TYPE /bobf/obm_bo_key.
    METHODS:
      read_bobf_key
        RETURNING
          VALUE(rv_key) TYPE /bobf/obm_bo_key,
      read_details
        IMPORTING
          iv_key            TYPE /bobf/obm_bo_key
        RETURNING
          VALUE(rs_details) TYPE /bobf/s_conf_model_api_bo
        RAISING
          zcx_abapgit_exception,
      create_bcdata
        IMPORTING
          iv_program       TYPE bdc_prog OPTIONAL
          iv_dynpro        TYPE bdc_dynr OPTIONAL
          iv_dynbegin      TYPE bdc_start OPTIONAL
          iv_fnam          TYPE fnam_____4 OPTIONAL
          iv_fval          TYPE bdc_fval OPTIONAL
        RETURNING
          VALUE(rs_bcdata) TYPE bdcdata,
      deserialize_children_nodes
        IMPORTING
          iv_parent_node TYPE ty_node
        CHANGING
          ct_nodes       TYPE ty_nodes_tt
        RAISING
          zcx_abapgit_exception,
      get_next_id
        RETURNING
          VALUE(rv_id) TYPE i,
      serialize_children
        IMPORTING
          is_parent_node TYPE lcl_object_bobf=>ty_node
          it_api_nodes   TYPE /bobf/t_conf_model_api_node
        CHANGING
          ct_nodes       TYPE ty_nodes_tt.
ENDCLASS.

CLASS lcl_object_bobf IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_key = read_bobf_key( ).
  ENDMETHOD.

  METHOD lif_object~has_changed_since.
    DATA ls_details TYPE /bobf/s_conf_model_api_bo.

    ls_details = read_details( mv_key ).

    IF ls_details-change_time > iv_timestamp.
      rv_changed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~changed_by.
    DATA ls_details TYPE /bobf/s_conf_model_api_bo.

    ls_details = read_details( mv_key ).
    rv_user = ls_details-change_user.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-late_deser = abap_true.
  ENDMETHOD.

  METHOD lif_object~exists.
    IF mv_key IS NOT INITIAL.
      rv_bool = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~jump.
    DATA:
      lv_field_value TYPE bdc_fval,
      lt_bcdata      TYPE STANDARD TABLE OF bdcdata.

    APPEND create_bcdata(
      iv_program  = '/BOBF/CONF_UI'
      iv_dynpro   = '0100'
      iv_dynbegin = 'X'
    ) TO lt_bcdata.
    APPEND create_bcdata(
      iv_fnam = 'BDC_OKCODE'
      iv_fval = '=OPEN'
    ) TO lt_bcdata.
    APPEND create_bcdata(
      iv_fnam = 'BDC_SUBSCR'
      iv_fval = '/BOBF/CONF_UI'
    ) TO lt_bcdata.

    APPEND create_bcdata(
      iv_program  = '/BOBF/CONF_UI'
      iv_dynpro   = '0110'
      iv_dynbegin = 'X'
    ) TO lt_bcdata.
    APPEND create_bcdata(
      iv_fnam = 'BDC_CURSOR'
      iv_fval = '/BOBF/S_CONF_UI-BO_NAME'
    ) TO lt_bcdata.

    lv_field_value = ms_item-obj_name.
    APPEND create_bcdata(
      iv_fnam = '/BOBF/S_CONF_UI-BO_NAME'
      iv_fval = lv_field_value
    ) TO lt_bcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode     = 'BOBX'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bcdata
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from ABAP4_CALL_TRANSACTION, BOBF' ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~delete.
    DATA lv_success TYPE abap_bool.
    lv_success = /bobf/cl_conf_model_api=>delete_business_object(
      iv_bo_key                     = mv_key
      iv_delete_constants_interface = abap_true
      iv_delete_ddic_types          = abap_true
      iv_delete_all_entities        = abap_true ).

    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( `error when deleting BOBF ` && ms_item-obj_name ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~serialize.
    DATA:
      ls_details   TYPE /bobf/s_conf_model_api_bo,
      lt_api_nodes TYPE /bobf/t_conf_model_api_node,
      ls_api_node  TYPE /bobf/s_conf_model_api_node,
      lt_nodes     TYPE ty_nodes_tt,
      ls_root_node TYPE ty_node,
      ls_node      TYPE ty_node,
      "@TODO move lv_success to handle in private methods
      lv_success   TYPE abap_bool,
      lv_id        TYPE int4.
    FIELD-SYMBOLS: <fs_node> TYPE ty_node.

    "IDs are used to reference nodes in the BO.
    "Nodes own UUID keys are not used. Reason: They are generated every time
    "the BO is created, this would cause irrelevant delta.
    CLEAR: mv_current_id.

    ls_details = read_details( mv_key ).

    /bobf/cl_conf_model_api=>get_node_tab(
      EXPORTING
        iv_bo_key        = mv_key
      IMPORTING
        et_node_tab      = lt_api_nodes
        ev_success       = lv_success ).
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( 'error from BOBF, GET_BO' ).
    ENDIF.

    READ TABLE lt_api_nodes INTO ls_root_node-node
      WITH KEY parent_node_key = '00000000000000000000000000000000'.

    ls_root_node-id = get_next_id( ).
    ls_root_node-root = abap_true.

    APPEND ls_root_node TO lt_nodes.

    serialize_children(
      EXPORTING
        is_parent_node = ls_root_node
        it_api_nodes   = lt_api_nodes
      CHANGING
        ct_nodes       = lt_nodes ).

    SORT lt_nodes BY id.

    "Clearing uuid keys since they are not relevant because of ids.
    CLEAR: ls_details-bo_key, ls_details-root_node_key.
    LOOP AT lt_nodes ASSIGNING <fs_node>.
      CLEAR:
        <fs_node>-node-node_key,
        <fs_node>-node-parent_key,
        <fs_node>-node-parent_node_key,
        <fs_node>-node-origin_bo_key.
    ENDLOOP.

    io_xml->add( iv_name = 'DETAILS'
                 ig_data = ls_details ).
    io_xml->add( iv_name = 'NODES'
                 ig_data = lt_nodes ).
  ENDMETHOD.

  METHOD lif_object~deserialize.
    DATA:
      lt_nodes     TYPE ty_nodes_tt,
      ls_node      LIKE LINE OF lt_nodes,
      ls_details   TYPE /bobf/s_conf_model_api_bo,
      lv_root_name TYPE /bobf/obm_name,
      "@TODO move to private methods
      lv_success   TYPE abap_bool.

    FIELD-SYMBOLS: <fs_root_node> LIKE LINE OF lt_nodes.

    io_xml->read( EXPORTING iv_name = 'DETAILS'
                  CHANGING cg_data = ls_details ).
    io_xml->read( EXPORTING iv_name = 'NODES'
                  CHANGING cg_data = lt_nodes ).
*
    READ TABLE lt_nodes ASSIGNING <fs_root_node>
      WITH KEY root = abap_true.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( `Error when deserializing BOBF: Root not found or two roots`).
    ENDIF.
*
    lv_root_name = <fs_root_node>-node-node_name.
    /bobf/cl_conf_model_api=>create_business_object(
      EXPORTING
        iv_business_object_name  = ls_details-bo_name
        iv_description           = ls_details-description
        iv_constant_interface    = ls_details-const_interface
        iv_root_name             = lv_root_name
        iv_root_description      = <fs_root_node>-node-description
        iv_root_data_type        = <fs_root_node>-node-data_type
        iv_root_data_data_type   = <fs_root_node>-node-data_data_type
        iv_root_data_data_type_t = <fs_root_node>-node-data_data_type_t
        iv_root_data_table_type  = <fs_root_node>-node-data_table_type
        iv_root_database_table   = <fs_root_node>-node-database_table
      IMPORTING
        ev_bo_key                = mv_key
        ev_success               = lv_success ).
    <fs_root_node>-node-node_key = mv_key.
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( `Error when deserializing BOBF ` && ms_item-obj_name ).
    ENDIF.

    deserialize_children_nodes(
      EXPORTING
        iv_parent_node = <fs_root_node>
      CHANGING
        ct_nodes       = lt_nodes ).
  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.

  METHOD read_bobf_key.
    DATA lv_bobf_name TYPE /bobf/obm_name.
    lv_bobf_name = ms_item-obj_name.
    rv_key = /bobf/cl_conf_model_api_reuse=>get_bo_key_by_name( lv_bobf_name ).
  ENDMETHOD.

  METHOD read_details.
    DATA lv_success TYPE abap_bool.
    /bobf/cl_conf_model_api=>get_bo(
      EXPORTING
        iv_bo_key  = iv_key
      IMPORTING
        es_bo      = rs_details
        ev_success = lv_success ).
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( `Error from BOBF, GET_BO ` && ms_item-obj_name ).
    ENDIF.
  ENDMETHOD.

  METHOD create_bcdata.
    rs_bcdata-dynbegin = iv_dynbegin.
    rs_bcdata-dynpro   = iv_dynpro.
    rs_bcdata-fnam     = iv_fnam.
    rs_bcdata-fval     = iv_fval.
    rs_bcdata-program  = iv_program.
  ENDMETHOD.


  METHOD deserialize_children_nodes.
    DATA:
      ls_details   TYPE /bobf/s_conf_model_api_bo,
      ls_root_node TYPE /bobf/s_conf_model_api_node,
      lv_root_name TYPE /bobf/obm_name,
      lv_node_name TYPE /bobf/obm_name,
      lt_nodes     TYPE ty_nodes_tt,
      ls_node      TYPE LINE OF ty_nodes_tt,
      lv_success   TYPE abap_bool,
      lv_child_id  TYPE int4,
      lv_node_key  TYPE /bobf/obm_bo_key.

    FIELD-SYMBOLS: <fs_node> TYPE ty_node.

    LOOP AT iv_parent_node-children_ids INTO lv_child_id.
      READ TABLE lt_nodes ASSIGNING <fs_node>
        WITH KEY id = lv_child_id.
      lv_node_name = <fs_node>-node-node_name.

      /bobf/cl_conf_model_api=>create_node(
        EXPORTING
          iv_bo_key              = mv_key
*           iv_version_key         =     " NodeID
          iv_parent_node_key     = iv_parent_node-node-node_key
          iv_node_name           = lv_node_name
          iv_description         = <fs_node>-node-description
          iv_transient           = <fs_node>-node-transient
          iv_is_extendible       = <fs_node>-node-extensible
*           iv_cardinality         =     " Association Cardinality
          iv_data_type           = <fs_node>-node-data_type
          iv_data_data_type      = <fs_node>-node-data_data_type
          iv_data_table_type     = <fs_node>-node-data_table_type
          iv_data_data_type_t    = <fs_node>-node-data_data_type_t
          iv_database_table      = <fs_node>-node-database_table
          iv_ext_incl_name       = <fs_node>-node-ext_incl_name
          iv_ext_incl_name_t     = <fs_node>-node-ext_incl_name_t
*           iv_gen_data_type       = ABAP_TRUE    " Generate combined structure
*           iv_gen_data_table_type = ABAP_TRUE    " Generate combiend table type
*           iv_gen_database        = ABAP_TRUE    " Generate database table
        IMPORTING
          ev_node_key = lv_node_key
          ev_success = lv_success ).
      IF lv_success = abap_false.
        zcx_abapgit_exception=>raise( `Error when deserializing BOBF ` && ms_item-obj_name ).
      ENDIF.

      <fs_node>-node-node_key = lv_node_key.

      deserialize_children_nodes(
        EXPORTING
          iv_parent_node = <fs_node>
        CHANGING
          ct_nodes       = ct_nodes ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_next_id.
    mv_current_id = mv_current_id + 1.
    rv_id = mv_current_id.
  ENDMETHOD.


  METHOD serialize_children.
    DATA:
      ls_api_node TYPE /bobf/s_conf_model_api_node,
      ls_node     TYPE ty_node.

    FIELD-SYMBOLS: <fs_parent_node> LIKE is_parent_node.

    READ TABLE ct_nodes ASSIGNING <fs_parent_node>
      WITH KEY id = is_parent_node-id.

    LOOP AT it_api_nodes INTO ls_api_node
      WHERE parent_node_key = is_parent_node-node-node_key.

      CLEAR ls_node.
      ls_node-node = ls_api_node.
      ls_node-id = get_next_id( ).
      APPEND ls_node-id TO <fs_parent_node>-children_ids.
      APPEND ls_node TO ct_nodes.

      serialize_children(
        EXPORTING
          is_parent_node = ls_node
          it_api_nodes   = it_api_nodes
        CHANGING
          ct_nodes       = ct_nodes ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
