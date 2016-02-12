*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_map DEFINITION INHERITING FROM /bobf/cl_conf_model_api_map.
  PUBLIC SECTION.
    METHODS map_action_for_create REDEFINITION.

  PROTECTED SECTION.
    METHODS map_action_data
      IMPORTING
        !is_action_api        TYPE /bobf/s_conf_model_api_action
        !iv_bo_key            TYPE /bobf/obm_bo_key
        !iv_node_key          TYPE /bobf/obm_node_key
      RETURNING
        VALUE(rr_action_data) TYPE REF TO /bobf/s_conf_act_list .
ENDCLASS.

CLASS lcl_map IMPLEMENTATION.

  METHOD map_action_data.

    "the node key must be supplied also for create scenarios in order to
    "maintain parent - child node relationships
    CREATE DATA rr_action_data.

    "create node
    rr_action_data->* = VALUE #(
        origin_bo_key     = is_action_api-origin_bo_key
        act_cat           = SWITCH #( is_action_api-act_cat
                                    WHEN '' THEN /bobf/if_conf_c=>sc_action_standard
                                    ELSE is_action_api-act_cat )
        node_key          = iv_node_key
        act_cardinality   = is_action_api-act_cardinality
        param_data_type   = is_action_api-param_data_type
        act_name          = is_action_api-act_name
        description       = is_action_api-description
        act_class         = is_action_api-act_class
        extendible        = is_action_api-extendible
    ).
    IF rr_action_data->origin_bo_key IS INITIAL.
      rr_action_data->origin_bo_key = iv_bo_key.
    ENDIF.

  ENDMETHOD.
  METHOD map_action_for_create.

*    BOPF uses node categories in order to differentiate between framework-actions and standard actions
*    in addition to the action type => we need to support this as well.
    DATA ls_modification TYPE /bobf/s_frw_modification.
    DATA lt_node_cat     TYPE /bobf/t_conf_node_cat.
    DATA ls_action_conf  TYPE REF TO /bobf/s_conf_act_conf.

    "create modify container for action
    ls_modification = VALUE #(
      association = /bobf/if_conf_obj_c=>sc_association-version-action
      change_mode = /bobf/if_frw_c=>sc_modify_create
      key = is_action-act_key
      node = /bobf/if_conf_obj_c=>sc_node-action
      source_key = iv_version_key
      source_node = /bobf/if_conf_obj_c=>sc_node-version
      node_cat = SWITCH #( is_action-act_cat
                            WHEN /bobf/if_conf_c=>sc_action_adopt_numbers       THEN /bobf/if_conf_obj_c=>sc_node_category-action-adopt_foreign_numbers
                            WHEN /bobf/if_conf_c=>sc_action_archive             THEN /bobf/if_conf_obj_c=>sc_node_category-action-archive
                            WHEN /bobf/if_conf_c=>sc_action_create              THEN /bobf/if_conf_obj_c=>sc_node_category-action-create
                            WHEN /bobf/if_conf_c=>sc_action_delete              THEN /bobf/if_conf_obj_c=>sc_node_category-action-delete
                            WHEN /bobf/if_conf_c=>sc_action_enhancement_post    THEN /bobf/if_conf_obj_c=>sc_node_category-action-post_enhancement
                            WHEN /bobf/if_conf_c=>sc_action_enhancement_pre     THEN /bobf/if_conf_obj_c=>sc_node_category-action-pre_enhancement
                            WHEN /bobf/if_conf_c=>sc_action_handle_bo_events    THEN /bobf/if_conf_obj_c=>sc_node_category-action-handle_business_object_events
                            WHEN /bobf/if_conf_c=>sc_action_lock                THEN /bobf/if_conf_obj_c=>sc_node_category-action-lock
                            WHEN /bobf/if_conf_c=>sc_action_unlock              THEN /bobf/if_conf_obj_c=>sc_node_category-action-unlock
                            WHEN /bobf/if_conf_c=>sc_action_update              THEN /bobf/if_conf_obj_c=>sc_node_category-action-update
                            WHEN /bobf/if_conf_c=>sc_action_save              THEN /bobf/if_conf_obj_c=>sc_node_category-action-save
                            ELSE /bobf/if_conf_obj_c=>sc_node_category-action-standard
                            )
      data = map_action_data(
          is_action_api    = is_action
          iv_bo_key        = iv_bo_key
          iv_node_key      = iv_node_key
      )
    ).

    INSERT ls_modification INTO TABLE ct_modification.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_conf_model_api_adt_copy DEFINITION CREATE PUBLIC INHERITING FROM  /bobf/cl_conf_model_api_adt.
*    @sap: Changes with respect to /BOBF/CL_CONF_MODEL_API_ADT
*    constructor injection of dependencies in order to get a custom mapper for framework-action-support
*    update_node maps root node key as parent node

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_map               TYPE REF TO /bobf/cl_conf_model_api_map  OPTIONAL
        !io_compare           TYPE REF TO /bobf/cl_conf_model_adt_comp OPTIONAL
        !iv_package           TYPE devclass OPTIONAL
        !iv_transport_request TYPE trkorr OPTIONAL .
    METHODS update_business_object REDEFINITION.

  PROTECTED SECTION.
    DATA mo_map                        TYPE REF TO /bobf/cl_conf_model_api_map.
    DATA mo_compare                    TYPE REF TO /bobf/cl_conf_model_adt_comp.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF gty_bo_name_key .
    TYPES  name TYPE /bobf/obm_name.
    TYPES  key TYPE /bobf/obm_bo_key.
    TYPES END OF gty_bo_name_key .
    TYPES:
      gtt_bo_name_key TYPE SORTED TABLE OF gty_bo_name_key WITH UNIQUE KEY name .

    CLASS-DATA gt_bo_locked TYPE gtt_bo_name_key .

    METHODS prepare_modification .
    METHODS finalize_nodes
      IMPORTING
        !iv_bo_key             TYPE /bobf/obm_bo_key
        !iv_version_key        TYPE /bobf/obm_obj_key
        !it_node_current       TYPE /bobf/t_conf_model_adt_node
        !it_node_new           TYPE /bobf/t_conf_model_adt_node
        !it_idx_create_new     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update_new     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete_current TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      EXPORTING
        !et_modification       TYPE /bobf/t_frw_modification
        !ev_success            TYPE boole_d .
    METHODS process_actions
      IMPORTING
        !iv_bo_key             TYPE /bobf/obm_bo_key
        !iv_version_key        TYPE /bobf/obm_obj_key
        !iv_node_key           TYPE /bobf/obm_node_key
        !it_action_current     TYPE /bobf/t_conf_model_adt_action
        !it_action_new         TYPE /bobf/t_conf_model_adt_action
        !it_idx_create_new     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update_new     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete_current TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_modification       TYPE /bobf/t_frw_modification .
    METHODS process_queries
      IMPORTING
        !iv_bo_key             TYPE /bobf/obm_bo_key
        !iv_version_key        TYPE /bobf/obm_obj_key
        !iv_node_key           TYPE /bobf/obm_node_key
        !it_query_current      TYPE /bobf/t_conf_model_adt_query
        !it_query_new          TYPE /bobf/t_conf_model_adt_query
        !it_idx_create_new     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update_new     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete_current TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_modification       TYPE /bobf/t_frw_modification .
    METHODS process_associations
      IMPORTING
        !iv_bo_key              TYPE /bobf/obm_bo_key
        !iv_version_key         TYPE /bobf/obm_obj_key
        !is_source_node         TYPE /bobf/s_conf_model_api_node
        !it_assoc_current       TYPE /bobf/t_conf_model_adt_assoc
        !it_assoc_new           TYPE /bobf/t_conf_model_adt_assoc
        !it_idx_create          TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update          TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete          TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_node_new            TYPE /bobf/t_conf_model_adt_node
        !it_alternative_key     TYPE /bobf/t_conf_model_adt_alt
      CHANGING
        !ct_representation_node TYPE /bobf/t_conf_node
        !ct_modification        TYPE /bobf/t_frw_modification .
    METHODS process_nodes
      IMPORTING
        !iv_bo_key                 TYPE /bobf/obm_bo_key
        !iv_version_key            TYPE /bobf/obm_obj_key
        !it_node_current           TYPE /bobf/t_conf_model_adt_node
        !it_node_new               TYPE /bobf/t_conf_model_adt_node
        !it_idx_create_new         TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update_new         TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete_current     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_comp_subentity_new TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_representation_node    TYPE /bobf/t_conf_node
        !ct_modification           TYPE /bobf/t_frw_modification .
    METHODS get_highest_priority_message
      IMPORTING
        !io_message       TYPE REF TO /bobf/if_frw_message
      RETURNING
        VALUE(ro_message) TYPE REF TO /bobf/cm_frw .
    METHODS process_alternative_keys
      IMPORTING
        !iv_bo_key         TYPE /bobf/obm_bo_key
        !iv_version_key    TYPE /bobf/obm_obj_key
        !iv_node_key       TYPE /bobf/obm_node_key
        !it_altkey_current TYPE /bobf/t_conf_model_adt_alt OPTIONAL
        !it_altkey_new     TYPE /bobf/t_conf_model_adt_alt
        !is_node_new       TYPE /bobf/s_conf_model_api_node
        !it_idx_create     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete     TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_modification   TYPE /bobf/t_frw_modification .
    METHODS process_altkey_validation
      IMPORTING
        !iv_bo_key       TYPE /bobf/obm_bo_key
        !iv_version_key  TYPE /bobf/obm_obj_key
        !iv_node_key     TYPE /bobf/obm_node_key
        !it_altkey_new   TYPE /bobf/t_conf_model_adt_alt
        !is_node_new     TYPE /bobf/s_conf_model_api_node
      CHANGING
        !ct_modification TYPE /bobf/t_frw_modification .
    METHODS process_properties
      IMPORTING
        !iv_bo_key       TYPE /bobf/obm_bo_key
        !iv_version_key  TYPE /bobf/obm_obj_key
        !iv_node_key     TYPE /bobf/obm_node_key
        !it_prop_current TYPE /bobf/t_conf_model_adt_sprop
        !it_prop_new     TYPE /bobf/t_conf_model_adt_sprop
        !is_node_new     TYPE /bobf/s_conf_model_api_node
      CHANGING
        !ct_modification TYPE /bobf/t_frw_modification .
    METHODS process_authorizations
      IMPORTING
        !iv_bo_key       TYPE /bobf/obm_bo_key
        !iv_version_key  TYPE /bobf/obm_obj_key
        !iv_node_key     TYPE /bobf/obm_node_key
        !it_auth_current TYPE /bobf/t_conf_model_adt_auth
        !it_auth_new     TYPE /bobf/t_conf_model_adt_auth
        !is_node_new     TYPE /bobf/s_conf_model_api_node
        !it_idx_create   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_modification TYPE /bobf/t_frw_modification .
    METHODS process_validations
      IMPORTING
        !iv_bo_key       TYPE /bobf/obm_bo_key
        !iv_version_key  TYPE /bobf/obm_obj_key
        !iv_node_key     TYPE /bobf/obm_node_key
        !it_val_current  TYPE /bobf/t_conf_model_adt_val
        !it_val_new      TYPE /bobf/t_conf_model_adt_val
        !is_node_new     TYPE /bobf/s_conf_model_api_node
        !it_idx_create   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_modification TYPE /bobf/t_frw_modification .
    METHODS process_determinations
      IMPORTING
        !iv_bo_key       TYPE /bobf/obm_bo_key
        !iv_version_key  TYPE /bobf/obm_obj_key
        !iv_node_key     TYPE /bobf/obm_node_key
        !it_det_current  TYPE /bobf/t_conf_model_adt_det
        !it_det_new      TYPE /bobf/t_conf_model_adt_det
        !is_node_new     TYPE /bobf/s_conf_model_api_node
        !it_idx_create   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_update   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
        !it_idx_delete   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index
      CHANGING
        !ct_modification TYPE /bobf/t_frw_modification .
    METHODS get_dependent_objects
      IMPORTING
        !it_node            TYPE /bobf/t_conf_model_api_node
      EXPORTING
        et_dependent_object TYPE /bobf/t_conf_model_adt_bo .
    METHODS collapse_delegated_nodes
      IMPORTING
        !is_updated_business_object TYPE /bobf/s_conf_model_adt_bo
        !is_current_business_object TYPE /bobf/s_conf_model_adt_bo
      EXPORTING
        !es_updated_business_object TYPE /bobf/s_conf_model_adt_bo.
ENDCLASS.

CLASS lcl_conf_model_api_adt_copy IMPLEMENTATION.
  METHOD collapse_delegated_nodes.

    " correspondes to method /BOBF/CL_CONF_MODEL_API_MAP->MAP_DELEGATED_NODE_FOR_READ...
    " correspondes to method /BOBF/CL_CONF_MODEL_API_MAP->MAP_DELEGATED_ASSOC_FOR_READ...

    " check if there're Delegated Nodes at all. if not, there's nothing to do...
    READ TABLE is_updated_business_object-nodes WITH KEY node-node_type = /bobf/if_conf_c=>sc_node_type_do
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      es_updated_business_object = is_updated_business_object.
      RETURN. " !!!!
    ENDIF.


    CLEAR es_updated_business_object.


    " >>>>> header...
    es_updated_business_object-header = is_updated_business_object-header.


    " >>>>> adjust nodes and compositions...
    LOOP AT is_updated_business_object-nodes INTO DATA(ls_node).

      " skip (remove) all DO nodes (cascading), the exception is the Delegated Node itself...
      FIND ALL OCCURRENCES OF '.' IN ls_node-node-node_name MATCH COUNT DATA(lv_count).
      IF lv_count = 1.
        SPLIT ls_node-node-node_name AT '.' INTO DATA(lv_prefix) DATA(lv_suffix).
        IF lv_suffix <> 'ROOT'.
          CONTINUE.
        ENDIF.
      ELSEIF lv_count > 1.
        CONTINUE.
      ENDIF.

      " adjust Delegated Node...
      IF ls_node-node-node_type = /bobf/if_conf_c=>sc_node_type_do.
        " >>>>> delete subentities like actions, associations, ...
        DATA(ls_delegated_node_node) = ls_node-node.
        CLEAR ls_node.
        ls_node-node = ls_delegated_node_node.
        " clear transient key, description and DDIC fields, otherwise the compare function will mark node as modified...
        CLEAR ls_node-node-do_embedding_key.
        CLEAR ls_node-node-description.
        CLEAR ls_node-node-data_data_type.
        CLEAR ls_node-node-data_data_type_t.
        CLEAR ls_node-node-data_type.
        CLEAR ls_node-node-data_table_type.
        CLEAR ls_node-node-database_table.
        " >>>>> restore the node key - potentially it's the DO ROOT node key, but has to be the node key of the Delegated Node.
        " >>>>> otherwise the subsequent compare function will mark the composition for re-creation.
        READ TABLE is_current_business_object-nodes ASSIGNING FIELD-SYMBOL(<ls_current_node>)
          WITH KEY node-node_type = ls_node-node-node_type
                   node-do_embedding_name = ls_node-node-do_embedding_name.
        IF sy-subrc = 0.
          ls_node-node-node_key = <ls_current_node>-node-node_key.
        ENDIF.
      ENDIF.

      " adjust composition to Delegated Node...
      LOOP AT ls_node-association ASSIGNING FIELD-SYMBOL(<ls_delegated_association>)
        WHERE assoc_cat = /bobf/if_conf_c=>sc_assoccat_object.
        " clear transient key, otherwise the compare function will mark composition as modified...
        CLEAR <ls_delegated_association>-target_do_embedding_key.
        " >>>>> restore the target node key - potentially it's the DO ROOT node key, but has to be the node key of the Delegated Node.
        " >>>>> otherwise the subsequent compare function will mark the composition for re-creation.
        READ TABLE is_current_business_object-nodes ASSIGNING <ls_current_node>
          WITH KEY node-node_key = <ls_delegated_association>-source_node_key.
        IF sy-subrc = 0.
          READ TABLE <ls_current_node>-association ASSIGNING FIELD-SYMBOL(<ls_current_association>)
            WITH KEY assoc_key = <ls_delegated_association>-assoc_key.
          IF sy-subrc = 0.
            <ls_delegated_association>-target_node_key = <ls_current_association>-target_node_key.
          ENDIF.
        ENDIF.
      ENDLOOP.

      INSERT ls_node INTO TABLE es_updated_business_object-nodes.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mv_package = iv_package.
    mv_transport = iv_transport_request.

    IF io_compare IS INITIAL.
      CREATE OBJECT mo_compare.
    ELSE.
      mo_compare = io_compare.
    ENDIF.

    IF io_map IS INITIAL.
      CREATE OBJECT mo_map.
    ELSE.
      mo_map = io_map.
    ENDIF.

  ENDMETHOD.


  METHOD finalize_nodes.

    CLEAR et_modification.
    ev_success = abap_true.

    LOOP AT it_idx_create_new INTO DATA(lv_index).
      READ TABLE it_node_new ASSIGNING FIELD-SYMBOL(<ls_node_create>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      ev_success = /bobf/cl_conf_model_api_reuse=>create_ext_node_sysadm_trigger(
        EXPORTING
          iv_bo_key          = iv_bo_key
          iv_version_key     = iv_version_key
          iv_node_key        = <ls_node_create>-node-node_key
          iv_parent_node_key = <ls_node_create>-node-parent_node_key
            iv_node_name       = CONV #( <ls_node_create>-node-node_name )
        CHANGING
          ct_modification    = et_modification ).
      IF ev_success = abap_false.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.



  METHOD get_dependent_objects.

    CLEAR et_dependent_object.

    DATA lt_do_key TYPE STANDARD TABLE OF /bobf/obm_bo_key.
    LOOP AT it_node ASSIGNING FIELD-SYMBOL(<ls_node>)
      WHERE node_type = /bobf/if_conf_c=>sc_node_type_do
        AND ref_bo_key IS NOT INITIAL.
      APPEND <ls_node>-ref_bo_key TO lt_do_key.
    ENDLOOP.

    SORT lt_do_key.
    DELETE ADJACENT DUPLICATES FROM lt_do_key.


    LOOP AT lt_do_key ASSIGNING FIELD-SYMBOL(<lv_do_key>).

      DATA(lv_do_name) = /bobf/cl_conf_model_api_reuse=>get_bo_name_by_key( <lv_do_key> ).
      CHECK lv_do_name IS NOT INITIAL.

      NEW /bobf/cl_conf_model_api_adt( )->get_business_object(
        EXPORTING
          iv_business_object_name = lv_do_name
          iv_edit_mode            = abap_false
          iv_expand_dependent_objects = abap_true " a DO may also include a DO...
        IMPORTING
          es_business_object      = DATA(ls_dependent_object) ).
      CHECK ls_dependent_object IS NOT INITIAL.

      APPEND ls_dependent_object TO et_dependent_object.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_highest_priority_message.
    IF io_message IS INITIAL.
      RETURN.
    ENDIF.

    io_message->get_messages(
      EXPORTING
        iv_severity             = /bobf/cm_frw=>co_severity_error
      IMPORTING
        et_message              = DATA(lt_message)
    ).

    READ TABLE lt_message INTO DATA(ls_message) INDEX 1.
    IF sy-subrc = 0.
      ro_message = ls_message-message.
    ENDIF.
  ENDMETHOD.


  METHOD prepare_modification.

    /bobf/cl_conf_toolbox=>sv_devclass        = mv_package.
    /bobf/cl_conf_toolbox=>sv_corr_num        = mv_transport.
    "suppress transport dialogs:
    /bobf/cl_conf_toolbox=>sv_genflag         = abap_true.
    /bobf/cl_conf_toolbox=>sv_suppress_dialog = abap_true.

    " set active/inactive handling enabled
    /bobf/cl_conf_toolbox=>sv_activation_handling = abap_true.

    "cleanup transaction, because modify works stateless
    DATA(lo_transaction_manager) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
    lo_transaction_manager->cleanup( ).

  ENDMETHOD.


  METHOD process_actions.

    DATA lo_map            TYPE REF TO /bobf/cl_conf_model_api_map.
    DATA lv_index          TYPE i.

    CREATE OBJECT lo_map.

    LOOP AT it_idx_create_new INTO lv_index.

      READ TABLE it_action_new ASSIGNING FIELD-SYMBOL(<ls_action_create>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      lo_map->map_action_for_create(
        EXPORTING
          is_action       = <ls_action_create>
          iv_bo_key       = iv_bo_key
          iv_node_key     = iv_node_key
          iv_version_key  = iv_version_key
        CHANGING
          ct_modification = ct_modification
      ).

    ENDLOOP.

    LOOP AT it_idx_update_new INTO lv_index.

      READ TABLE it_action_new ASSIGNING FIELD-SYMBOL(<ls_action_update>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      lo_map->map_action_for_update(
        EXPORTING
          is_action       = <ls_action_update>
          iv_bo_key       = iv_bo_key
          iv_node_key     = iv_node_key
        CHANGING
          ct_modification = ct_modification
      ).

    ENDLOOP.

    LOOP AT it_idx_delete_current INTO lv_index.

      READ TABLE it_action_current ASSIGNING FIELD-SYMBOL(<ls_action_delete>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      lo_map->map_action_for_delete(
        EXPORTING
          is_action       = <ls_action_delete>
        CHANGING
          ct_modification = ct_modification
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_alternative_keys.

    DATA(lo_map) = NEW /bobf/cl_conf_model_api_map( ).


    LOOP AT it_idx_create ASSIGNING FIELD-SYMBOL(<lv_index>).

      READ TABLE it_altkey_new ASSIGNING FIELD-SYMBOL(<ls_altkey_create>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      lo_map->map_altkey_for_create(
        EXPORTING
          iv_create_with_header = abap_true " !!!
          is_alternative_key    = <ls_altkey_create>
          iv_node_key           = iv_node_key
          iv_bo_key             = iv_bo_key
          iv_version_key        = iv_version_key
        CHANGING
          ct_modification       = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_update ASSIGNING <lv_index>.

      READ TABLE it_altkey_new ASSIGNING FIELD-SYMBOL(<ls_altkey_update>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_altkey_subent_keys(
        EXPORTING
          iv_altkey_key          = <ls_altkey_update>-altkey_key
        IMPORTING
          et_ak_field_key_delete = DATA(lt_ak_field_key_delete) ).

      lo_map->map_altkey_for_update(
        EXPORTING
          is_alternative_key     = <ls_altkey_update>
          iv_bo_key              = iv_bo_key
          iv_node_key            = iv_node_key
          iv_version_key         = iv_version_key
          it_ak_field_key_delete = lt_ak_field_key_delete
        CHANGING
          ct_modification        = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_delete ASSIGNING <lv_index>.

      READ TABLE it_altkey_current ASSIGNING FIELD-SYMBOL(<ls_altkey_delete>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_altkey_subent_keys(
        EXPORTING
          iv_altkey_key          = <ls_altkey_delete>-altkey_key
        IMPORTING
          et_ak_field_key_delete = lt_ak_field_key_delete ).

      lo_map->map_altkey_for_delete(
        EXPORTING
          iv_with_header         = abap_true " !!!
          is_alternative_key     = <ls_altkey_delete>
          it_ak_field_key_delete = lt_ak_field_key_delete
        CHANGING
          ct_modification        = ct_modification ).

    ENDLOOP.


    " >>>>> create/remove action validation for altkey uniqueness check...
    me->process_altkey_validation(
      EXPORTING
        iv_bo_key       = iv_bo_key
        iv_version_key  = iv_version_key
        iv_node_key     = iv_node_key
        it_altkey_new   = it_altkey_new
        is_node_new     = is_node_new
      CHANGING
        ct_modification = ct_modification ).

  ENDMETHOD.


  METHOD process_altkey_validation.

    " >>>>> create/remove action validation for altkey uniqueness check (with library class)...

    " check if the special action validation is needed...
    DATA(lv_unique_check_needed) = abap_false.
    LOOP AT it_altkey_new TRANSPORTING NO FIELDS
      WHERE uniqueness_check = /bobf/if_conf_c=>sc_altkey_uniqcheck_after_mod. "#EC CI_SORTSEQ
      lv_unique_check_needed = abap_true.
      EXIT.
    ENDLOOP.

    " check existence of the special action validation...
    DATA(lv_altkey_uniq_check_val_key) = /bobf/cl_conf_model_api_reuse=>get_val_key_altkey_uniq_check( iv_node_key ).


    IF lv_unique_check_needed = abap_true AND lv_altkey_uniq_check_val_key IS INITIAL.

      " create action validation...
      NEW /bobf/cl_conf_model_api_map( )->map_val_act_uniq_for_create(
        EXPORTING
          is_node         = is_node_new
          iv_version_key  = iv_version_key
          iv_bo_key       = iv_bo_key
          iv_node_key     = iv_node_key
        CHANGING
          ct_modification = ct_modification ).

    ELSEIF lv_unique_check_needed = abap_false AND lv_altkey_uniq_check_val_key IS NOT INITIAL.

      " remove action validation...
      /bobf/cl_conf_model_api_reuse=>get_validation_subent_keys(
        EXPORTING
          iv_val_key                    = lv_altkey_uniq_check_val_key
        IMPORTING
          et_val_trigger_key_delete     = DATA(lt_val_trigger_key_delete)
          et_val_conf_key_delete        = DATA(lt_val_conf_key_delete)
          et_val_group_conf_key_delete  = DATA(lt_val_group_conf_key_delete) ).
      NEW /bobf/cl_conf_model_api_map( )->map_val_for_delete(
        EXPORTING
          iv_with_header               = abap_true " !!!
          iv_val_key                   = lv_altkey_uniq_check_val_key
          it_val_trigger_key_delete    = lt_val_trigger_key_delete
          it_val_conf_key_delete       = lt_val_conf_key_delete
          it_val_group_conf_key_delete = lt_val_group_conf_key_delete
        CHANGING
          ct_modification              = ct_modification ).

    ENDIF.

  ENDMETHOD.


  METHOD process_associations.

    DATA ls_target_node TYPE /bobf/s_conf_model_adt_node.


    DATA(lo_map) = NEW /bobf/cl_conf_model_api_map( ).


    LOOP AT it_idx_create ASSIGNING FIELD-SYMBOL(<lv_index>).

      READ TABLE it_assoc_new ASSIGNING FIELD-SYMBOL(<ls_assoc_create>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      CLEAR ls_target_node.
      READ TABLE it_node_new WITH KEY node-node_key = <ls_assoc_create>-target_node_key INTO ls_target_node.

      lo_map->map_assoc_for_create(
        EXPORTING
          iv_create_with_header  = abap_true " !!!
          is_association         = <ls_assoc_create>
          iv_version_key         = iv_version_key
          iv_bo_key              = iv_bo_key
          is_source_node         = is_source_node
          is_target_node         = ls_target_node-node
        CHANGING
          ct_representation_node = ct_representation_node
          ct_modification        = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_update ASSIGNING <lv_index>.

      READ TABLE it_assoc_new ASSIGNING FIELD-SYMBOL(<ls_assoc_update>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for subentities...
      /bobf/cl_conf_model_api_reuse=>get_association_subent_keys(
        EXPORTING
          iv_assoc_key                = <ls_assoc_update>-assoc_key
        IMPORTING
          et_assoc_binding_key_delete = DATA(lt_assoc_binding_key_delete) ).

      CLEAR ls_target_node.
      READ TABLE it_node_new WITH KEY node-node_key = <ls_assoc_update>-target_node_key INTO ls_target_node.

      lo_map->map_assoc_for_update(
        EXPORTING
          is_association              = <ls_assoc_update>
          iv_version_key              = iv_version_key
          iv_bo_key                   = iv_bo_key
          it_assoc_binding_key_delete = lt_assoc_binding_key_delete
          is_source_node              = is_source_node
          is_target_node              = ls_target_node-node
        CHANGING
          ct_representation_node      = ct_representation_node
          ct_modification             = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_delete ASSIGNING <lv_index>.

      READ TABLE it_assoc_current ASSIGNING FIELD-SYMBOL(<ls_assoc_delete>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for subentities...
      /bobf/cl_conf_model_api_reuse=>get_association_subent_keys(
        EXPORTING
          iv_assoc_key                = <ls_assoc_delete>-assoc_key
        IMPORTING
          et_assoc_binding_key_delete = lt_assoc_binding_key_delete ).

      lo_map->map_assoc_for_delete(
        EXPORTING
          iv_with_header              = abap_true " !!!
          is_association              = <ls_assoc_delete>
          it_assoc_binding_key_delete = lt_assoc_binding_key_delete
        CHANGING
          ct_modification             = ct_modification ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_authorizations.

    DATA(lo_map) = NEW /bobf/cl_conf_model_api_map( ).


    LOOP AT it_idx_create ASSIGNING FIELD-SYMBOL(<lv_index>).

      READ TABLE it_auth_new ASSIGNING FIELD-SYMBOL(<ls_auth_create>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      lo_map->map_auth_for_create(
        EXPORTING
          iv_create_with_header = abap_true " !!!
          is_authorization      = <ls_auth_create>
          is_node               = is_node_new
          iv_bo_key             = iv_bo_key
          iv_node_key           = iv_node_key
          iv_version_key        = iv_version_key
        CHANGING
          ct_modification       = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_update ASSIGNING <lv_index>.

      READ TABLE it_auth_new ASSIGNING FIELD-SYMBOL(<ls_auth_update>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_authorization_subent_keys(
        EXPORTING
          iv_version_key        = iv_version_key
          iv_node_key           = iv_node_key
          iv_auth_obj_name      = <ls_auth_update>-auth_obj_name
        IMPORTING
          et_acf_map_key_delete = DATA(lt_acf_map_key_delete) ).

      lo_map->map_auth_for_update(
        EXPORTING
          is_authorization      = <ls_auth_update>
          iv_bo_key             = iv_bo_key
          iv_node_key           = iv_node_key
          is_node               = is_node_new
          iv_version_key        = iv_version_key
          it_acf_map_key_delete = lt_acf_map_key_delete
        CHANGING
          ct_modification       = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_delete ASSIGNING <lv_index>.

      READ TABLE it_auth_current ASSIGNING FIELD-SYMBOL(<ls_auth_delete>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_authorization_subent_keys(
        EXPORTING
          iv_version_key        = iv_version_key
          iv_node_key           = iv_node_key
          iv_auth_obj_name      = <ls_auth_delete>-auth_obj_name
        IMPORTING
          et_acf_map_key_delete = lt_acf_map_key_delete ).

      lo_map->map_auth_for_delete(
        EXPORTING
          iv_with_header        = abap_true " !!!
          is_authorization      = <ls_auth_delete>
          it_acf_map_key_delete = lt_acf_map_key_delete
        CHANGING
          ct_modification       = ct_modification ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_determinations.

    " >>>>> remark: DETERMINATION_CONF and *special handling* regarding DETERMINATION_TRIGGER and DETERMINATION_WRITE is
    " >>>>> processed later on in determinations DETERMINATION_PATTERN_CREATE / _UPDATE (@ conf model node DETERMINATION)...


    DATA(lo_map) = NEW /bobf/cl_conf_model_api_map( ).


    LOOP AT it_idx_create ASSIGNING FIELD-SYMBOL(<lv_index>).

      READ TABLE it_det_new ASSIGNING FIELD-SYMBOL(<ls_det_create>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      lo_map->map_det_for_create(
        EXPORTING
          iv_create_with_header = abap_true " !!!
          is_determination      = <ls_det_create>
          is_node               = is_node_new
          iv_bo_key             = iv_bo_key
          iv_node_key           = iv_node_key
          iv_version_key        = iv_version_key
        CHANGING
          ct_modification       = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_update ASSIGNING <lv_index>.

      READ TABLE it_det_new ASSIGNING FIELD-SYMBOL(<ls_det_update>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_determination_subent_keys(
        EXPORTING
          iv_det_key                 = <ls_det_update>-det_key
        IMPORTING
          et_det_trigger_key_delete  = DATA(lt_det_trigger_key_delete)
          et_det_write_key_delete    = DATA(lt_det_write_key_delete)
          et_det_conf_key_delete     = DATA(lt_det_conf_key_delete)
          et_det_succ_dep_key_delete = DATA(lt_det_succ_dep_key_delete)
          et_det_pred_dep_key_delete = DATA(lt_det_pred_dep_key_delete) ).

      lo_map->map_det_for_update(
        EXPORTING
          is_determination           = <ls_det_update>
          iv_bo_key                  = iv_bo_key
          iv_node_key                = iv_node_key
          is_node                    = is_node_new
          iv_version_key             = iv_version_key
          it_det_trigger_key_delete  = lt_det_trigger_key_delete
          it_det_write_key_delete    = lt_det_write_key_delete
          it_det_conf_key_delete     = lt_det_conf_key_delete
          it_det_succ_dep_key_delete = lt_det_succ_dep_key_delete
          it_det_pred_dep_key_delete = lt_det_pred_dep_key_delete
        CHANGING
          ct_modification            = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_delete ASSIGNING <lv_index>.

      READ TABLE it_det_current ASSIGNING FIELD-SYMBOL(<ls_det_delete>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_determination_subent_keys(
        EXPORTING
          iv_det_key                 = <ls_det_delete>-det_key
        IMPORTING
          et_det_trigger_key_delete  = lt_det_trigger_key_delete
          et_det_write_key_delete    = lt_det_write_key_delete
          et_det_conf_key_delete     = lt_det_conf_key_delete
          et_det_succ_dep_key_delete = lt_det_succ_dep_key_delete
          et_det_pred_dep_key_delete = lt_det_pred_dep_key_delete ).

      lo_map->map_det_for_delete(
        EXPORTING
          iv_with_header             = abap_true " !!!
          is_determination           = <ls_det_delete>
          it_det_trigger_key_delete  = lt_det_trigger_key_delete
          it_det_write_key_delete    = lt_det_write_key_delete
          it_det_conf_key_delete     = lt_det_conf_key_delete
          it_det_succ_dep_key_delete = lt_det_succ_dep_key_delete
          it_det_pred_dep_key_delete = lt_det_pred_dep_key_delete
        CHANGING
          ct_modification            = ct_modification ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_nodes.
    DATA lv_index                      TYPE i.
    DATA lt_idx_create_action_new      TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_update_action_new      TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_delete_action_current  TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_create_query_new       TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_update_query_new       TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_delete_query_current   TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.


    "process new nodes
    LOOP AT it_idx_create_new INTO lv_index.

      READ TABLE it_node_new ASSIGNING FIELD-SYMBOL(<ls_node_create>) INDEX lv_index.
      ASSERT sy-subrc = 0.
*      @sap: An business object which has been created with create_business_object might have a different ROOT-node_key,
*        as the consumer cannot supply the node_key.
*        Thus, lateron, subnodes of the root node refer to the wrong parent_node.
      IF it_node_current IS NOT INITIAL.
        READ TABLE it_node_current ASSIGNING FIELD-SYMBOL(<ls_current_root_node>) WITH KEY node-parent_node_key = ''.
        ASSERT sy-subrc = 0. "there has to be a root node

        READ TABLE it_node_new ASSIGNING FIELD-SYMBOL(<ls_new_parent_node>) WITH KEY node-node_key = <ls_node_create>-node-node_key.
        IF sy-subrc = 0.
          IF <ls_new_parent_node>-node-parent_node_key IS INITIAL.
            "<ls_node_create> is a subnote of the root-node
            <ls_node_create>-node-parent_node_key = <ls_current_root_node>-node-node_key.
          ENDIF.
        ENDIF.
      ENDIF.

      mo_map->map_node_for_create(
        EXPORTING
          is_node         = <ls_node_create>-node
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
        CHANGING
          ct_modification = ct_modification ).

      "collect subentities to be created
      "- actions
      LOOP AT <ls_node_create>-action ASSIGNING FIELD-SYMBOL(<ls_action>).
        mo_map->map_action_for_create(
          EXPORTING
            is_action       = <ls_action>
            iv_version_key  = iv_version_key
            iv_bo_key       = iv_bo_key
            iv_node_key     = <ls_node_create>-node-node_key
          CHANGING
            ct_modification = ct_modification
        ).
      ENDLOOP.

      "- queries
      LOOP AT <ls_node_create>-query ASSIGNING FIELD-SYMBOL(<ls_query>).
        mo_map->map_query_for_create(
          EXPORTING
            is_query        = <ls_query>
            iv_version_key  = iv_version_key
            iv_bo_key       = iv_bo_key
            iv_node_key     = <ls_node_create>-node-node_key
          CHANGING
            ct_modification = ct_modification
        ).
      ENDLOOP.

      "- associations
      LOOP AT <ls_node_create>-association ASSIGNING FIELD-SYMBOL(<ls_assoc>).
        DATA ls_target_node TYPE /bobf/s_conf_model_adt_node.
        CLEAR ls_target_node.
        READ TABLE it_node_new WITH KEY node-node_key = <ls_assoc>-target_node_key INTO ls_target_node.
        mo_map->map_assoc_for_create(
          EXPORTING
            iv_create_with_header  = abap_true " !!!
            is_association         = <ls_assoc>
            iv_version_key         = iv_version_key
            iv_bo_key              = iv_bo_key
            is_source_node         = <ls_node_create>-node
            is_target_node         = ls_target_node-node
          CHANGING
            ct_representation_node = ct_representation_node
            ct_modification        = ct_modification
        ).
      ENDLOOP.

      "- alternative keys
      LOOP AT <ls_node_create>-alternative_key ASSIGNING FIELD-SYMBOL(<ls_altkey>).
        mo_map->map_altkey_for_create(
          EXPORTING
            iv_create_with_header = abap_true " !!!
            is_alternative_key    = <ls_altkey>
            iv_version_key        = iv_version_key
            iv_bo_key             = iv_bo_key
            iv_node_key           = <ls_node_create>-node-node_key
          CHANGING
            ct_modification       = ct_modification ).
        IF <ls_altkey>-uniqueness_check = /bobf/if_conf_c=>sc_altkey_uniqcheck_after_mod.
          " create action validation for uniqueness check...
          mo_map->map_val_act_uniq_for_create(
            EXPORTING
              is_node         = <ls_node_create>-node
              iv_version_key  = iv_version_key
              iv_bo_key       = iv_bo_key
              iv_node_key     = <ls_node_create>-node-node_key
            CHANGING
              ct_modification = ct_modification ).
        ENDIF.
      ENDLOOP.

      "- determinations
      LOOP AT <ls_node_create>-determination ASSIGNING FIELD-SYMBOL(<ls_determination>).
        mo_map->map_det_for_create(
          EXPORTING
            iv_create_with_header = abap_true " !!!
            is_determination      = <ls_determination>
            is_node               = <ls_node_create>-node
            iv_version_key        = iv_version_key
            iv_bo_key             = iv_bo_key
            iv_node_key           = <ls_node_create>-node-node_key
          CHANGING
            ct_modification       = ct_modification ).
      ENDLOOP.

      "- validations
      LOOP AT <ls_node_create>-validation ASSIGNING FIELD-SYMBOL(<ls_validation>).
        mo_map->map_val_for_create(
          EXPORTING
            iv_create_with_header = abap_true " !!!
            is_validation         = <ls_validation>
            is_node               = <ls_node_create>-node
            iv_version_key        = iv_version_key
            iv_bo_key             = iv_bo_key
            iv_node_key           = <ls_node_create>-node-node_key
          CHANGING
            ct_modification       = ct_modification ).
      ENDLOOP.

      "- static properties
      LOOP AT <ls_node_create>-property ASSIGNING FIELD-SYMBOL(<ls_property>).
        mo_map->map_prop_for_create(
          EXPORTING
            is_property     = <ls_property>
            is_node         = <ls_node_create>-node
            iv_version_key  = iv_version_key
            iv_bo_key       = iv_bo_key
            iv_node_key     = <ls_node_create>-node-node_key
          CHANGING
            ct_modification = ct_modification ).
      ENDLOOP.

      "- authorizations
      LOOP AT <ls_node_create>-authorization ASSIGNING FIELD-SYMBOL(<ls_authorization>).
        mo_map->map_auth_for_create(
          EXPORTING
            iv_create_with_header = abap_true " !!!
            is_authorization      = <ls_authorization>
            is_node               = <ls_node_create>-node
            iv_version_key        = iv_version_key
            iv_bo_key             = iv_bo_key
            iv_node_key           = <ls_node_create>-node-node_key
          CHANGING
            ct_modification       = ct_modification ).
      ENDLOOP.

    ENDLOOP.

    "process updated nodes
    LOOP AT it_idx_update_new INTO lv_index.
      READ TABLE it_node_new ASSIGNING FIELD-SYMBOL(<ls_node_update>) INDEX lv_index.
      ASSERT sy-subrc = 0.
      mo_map->map_node_for_update(
        EXPORTING
          is_node         = <ls_node_update>-node
          iv_bo_key       = iv_bo_key
        CHANGING
          ct_modification = ct_modification ).

      "no handling of subentities; The nodes are redundandly contained in it_idx_comp_subentitiy
    ENDLOOP.

    "process deleted nodes
    LOOP AT it_idx_delete_current INTO lv_index.

      READ TABLE it_node_current ASSIGNING FIELD-SYMBOL(<ls_node_delete>) INDEX lv_index.
      ASSERT sy-subrc = 0.
      mo_map->map_node_for_delete(
        EXPORTING
          is_node         = <ls_node_delete>-node
        CHANGING
          ct_modification = ct_modification
      ).
      "no handling of subentities; BOBF should take care of this automatically
    ENDLOOP.

    "process subentities
    LOOP AT it_idx_comp_subentity_new INTO lv_index.

      READ TABLE it_node_new ASSIGNING FIELD-SYMBOL(<ls_node_comp_new>) INDEX lv_index.
      ASSERT sy-subrc = 0.
      READ TABLE it_node_current ASSIGNING FIELD-SYMBOL(<ls_node_comp_current>) WITH KEY
          node-node_key = <ls_node_comp_new>-node-node_key
          node-do_embedding_key = <ls_node_comp_new>-node-do_embedding_key.
      ASSERT sy-subrc = 0.

      "process actions
      mo_compare->compare_tables_for_changes(
        EXPORTING
          it_subentity_new            = <ls_node_comp_new>-action
          it_subentity_current        = <ls_node_comp_current>-action
          it_update_rel_element       = VALUE #( ( |UPDATABLE| ) )
        IMPORTING
          et_index_create_new         = lt_idx_create_action_new
          et_index_update_new         = lt_idx_update_action_new
          et_index_delete_current     = lt_idx_delete_action_current
      ).
      process_actions(
        EXPORTING
          iv_bo_key             = iv_bo_key
          iv_version_key        = iv_version_key
          iv_node_key           = <ls_node_comp_new>-node-node_key
          it_action_current     = <ls_node_comp_current>-action
          it_action_new         = <ls_node_comp_new>-action
          it_idx_create_new     = lt_idx_create_action_new
          it_idx_update_new     = lt_idx_update_action_new
          it_idx_delete_current = lt_idx_delete_action_current
        CHANGING
          ct_modification       = ct_modification
      ).

      "process queries
      mo_compare->compare_tables_for_changes(
        EXPORTING
          it_subentity_new           = <ls_node_comp_new>-query
          it_subentity_current       = <ls_node_comp_current>-query
          it_update_rel_element      = VALUE #( ( |UPDATABLE| ) )
        IMPORTING
          et_index_create_new        = lt_idx_create_query_new
          et_index_update_new        = lt_idx_update_query_new
          et_index_delete_current    = lt_idx_delete_query_current
      ).
      process_queries(
        EXPORTING
          iv_bo_key             = iv_bo_key
          iv_version_key        = iv_version_key
          iv_node_key           = <ls_node_comp_new>-node-node_key
          it_query_current      = <ls_node_comp_current>-query
          it_query_new          = <ls_node_comp_new>-query
          it_idx_create_new     = lt_idx_create_query_new
          it_idx_update_new     = lt_idx_update_query_new
          it_idx_delete_current = lt_idx_delete_query_current
        CHANGING
          ct_modification       = ct_modification
      ).

      " process alternative keys
      mo_compare->compare_tables_for_changes(
        EXPORTING
          it_subentity_new        = <ls_node_comp_new>-alternative_key
          it_subentity_current    = <ls_node_comp_current>-alternative_key
          it_update_rel_element   = VALUE #( ( |UPDATABLE| ) )
        IMPORTING
          et_index_create_new     = DATA(lt_idx_create_altkey)
          et_index_update_new     = DATA(lt_idx_update_altkey)
          et_index_delete_current = DATA(lt_idx_delete_altkey) ).
      process_alternative_keys(
        EXPORTING
          iv_bo_key         = iv_bo_key
          iv_version_key    = iv_version_key
          iv_node_key       = <ls_node_comp_new>-node-node_key
          it_altkey_current = <ls_node_comp_current>-alternative_key
          it_altkey_new     = <ls_node_comp_new>-alternative_key
          is_node_new       = <ls_node_comp_new>-node
          it_idx_create     = lt_idx_create_altkey
          it_idx_update     = lt_idx_update_altkey
          it_idx_delete     = lt_idx_delete_altkey
        CHANGING
          ct_modification   = ct_modification ).

      " process associations
      mo_compare->compare_associations(
        EXPORTING
          it_assoc_new     = <ls_node_comp_new>-association
          it_assoc_current = <ls_node_comp_current>-association
        IMPORTING
          et_index_create  = DATA(lt_idx_create_assoc)
          et_index_update  = DATA(lt_idx_update_assoc)
          et_index_delete  = DATA(lt_idx_delete_assoc) ).
      process_associations(
        EXPORTING
          iv_bo_key              = iv_bo_key
          iv_version_key         = iv_version_key
          is_source_node         = <ls_node_comp_new>-node
          it_assoc_current       = <ls_node_comp_current>-association
          it_assoc_new           = <ls_node_comp_new>-association
          it_idx_create          = lt_idx_create_assoc
          it_idx_update          = lt_idx_update_assoc
          it_idx_delete          = lt_idx_delete_assoc
          it_node_new            = it_node_new
          it_alternative_key     = <ls_node_comp_new>-alternative_key
        CHANGING
          ct_representation_node = ct_representation_node
          ct_modification        = ct_modification ).

      " process determinations
      mo_compare->compare_determinations(
        EXPORTING
          it_det_new      = <ls_node_comp_new>-determination
          it_det_current  = <ls_node_comp_current>-determination
        IMPORTING
          et_index_create = DATA(lt_idx_create_det)
          et_index_update = DATA(lt_idx_update_det)
          et_index_delete = DATA(lt_idx_delete_det) ).
      process_determinations(
        EXPORTING
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
          iv_node_key     = <ls_node_comp_new>-node-node_key
          it_det_current  = <ls_node_comp_current>-determination
          it_det_new      = <ls_node_comp_new>-determination
          is_node_new     = <ls_node_comp_new>-node
          it_idx_create   = lt_idx_create_det
          it_idx_update   = lt_idx_update_det
          it_idx_delete   = lt_idx_delete_det
        CHANGING
          ct_modification = ct_modification ).

      " process validations
      mo_compare->compare_validations(
        EXPORTING
          it_val_new      = <ls_node_comp_new>-validation
          it_val_current  = <ls_node_comp_current>-validation
        IMPORTING
          et_index_create = DATA(lt_idx_create_val)
          et_index_update = DATA(lt_idx_update_val)
          et_index_delete = DATA(lt_idx_delete_val) ).
      process_validations(
        EXPORTING
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
          iv_node_key     = <ls_node_comp_new>-node-node_key
          it_val_current  = <ls_node_comp_current>-validation
          it_val_new      = <ls_node_comp_new>-validation
          is_node_new     = <ls_node_comp_new>-node
          it_idx_create   = lt_idx_create_val
          it_idx_update   = lt_idx_update_val
          it_idx_delete   = lt_idx_delete_val
        CHANGING
          ct_modification = ct_modification ).

      " process static properties
      DATA(lv_properties_are_changed) = mo_compare->compare_properties(
        it_prop_new     = <ls_node_comp_new>-property
        it_prop_current = <ls_node_comp_current>-property ).
      IF lv_properties_are_changed = abap_true.
        process_properties(
        EXPORTING
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
          iv_node_key     = <ls_node_comp_new>-node-node_key
          it_prop_current = <ls_node_comp_current>-property
          it_prop_new     = <ls_node_comp_new>-property
          is_node_new     = <ls_node_comp_new>-node
        CHANGING
          ct_modification = ct_modification ).
      ENDIF.

      " process authorizations (no special compare_auth() method needed, since field map table is already sorted)
      mo_compare->compare_tables_for_changes(
        EXPORTING
          it_subentity_new        = <ls_node_comp_new>-authorization
          it_subentity_current    = <ls_node_comp_current>-authorization
          it_update_rel_element   = VALUE #( ( |UPDATABLE| ) )
        IMPORTING
          et_index_create_new     = DATA(lt_idx_create_auth)
          et_index_update_new     = DATA(lt_idx_update_auth)
          et_index_delete_current = DATA(lt_idx_delete_auth) ).
      process_authorizations(
        EXPORTING
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
          iv_node_key     = <ls_node_comp_new>-node-node_key
          it_auth_current = <ls_node_comp_current>-authorization
          it_auth_new     = <ls_node_comp_new>-authorization
          is_node_new     = <ls_node_comp_new>-node
          it_idx_create   = lt_idx_create_auth
          it_idx_update   = lt_idx_update_auth
          it_idx_delete   = lt_idx_delete_auth
        CHANGING
          ct_modification = ct_modification ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_properties.

    " >>>>> remark: adjustment of NODE_CAT will be done later on in determination STATIC_PROPERTY_ADJUST
    " >>>>> (@ CONF MODEL node STATIC_PRTY)...


    CHECK it_prop_new <> it_prop_current. " !!!


    DATA(lo_map) = NEW /bobf/cl_conf_model_api_map( ).


    " delete current entries upfront...
    IF lines( it_prop_current ) > 0.

      DATA(lt_prop_key_delete) = /bobf/cl_conf_model_api_reuse=>get_node_static_property_keys(
        iv_version_key = iv_version_key
        iv_node_key    = iv_node_key ).

      LOOP AT lt_prop_key_delete ASSIGNING FIELD-SYMBOL(<ls_prop_key_delete>).
        lo_map->map_prop_for_delete(
          EXPORTING
            iv_property_key = <ls_prop_key_delete>-key
          CHANGING
            ct_modification = ct_modification ).
      ENDLOOP.

    ENDIF.


    " create new entries...
    LOOP AT it_prop_new ASSIGNING FIELD-SYMBOL(<ls_property>).
      lo_map->map_prop_for_create(
        EXPORTING
          is_property     = <ls_property>
          is_node         = is_node_new
          iv_version_key  = iv_version_key
          iv_bo_key       = iv_bo_key
          iv_node_key     = iv_node_key
        CHANGING
          ct_modification = ct_modification ).
    ENDLOOP.

  ENDMETHOD.


  METHOD process_queries.

    DATA lo_map            TYPE REF TO /bobf/cl_conf_model_api_map.
    DATA lv_index          TYPE i.
    DATA lt_query_new      TYPE /bobf/t_conf_model_api_query.
    DATA lt_query_current  TYPE /bobf/t_conf_model_api_query.

    CREATE OBJECT lo_map.

    lt_query_new = it_query_new.
    lt_query_current = it_query_current.

    LOOP AT it_idx_create_new INTO lv_index.

      READ TABLE lt_query_new ASSIGNING FIELD-SYMBOL(<ls_query_create>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      lo_map->map_query_for_create(
        EXPORTING
          is_query        = <ls_query_create>
          iv_node_key     = iv_node_key
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
        CHANGING
          ct_modification = ct_modification
      ).

    ENDLOOP.

    LOOP AT it_idx_delete_current INTO lv_index.

      READ TABLE lt_query_current ASSIGNING FIELD-SYMBOL(<ls_query_delete>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      lo_map->map_query_for_delete(
        EXPORTING
          is_query        = <ls_query_delete>
        CHANGING
          ct_modification = ct_modification
      ).

    ENDLOOP.

    LOOP AT it_idx_update_new INTO lv_index.

      READ TABLE lt_query_new ASSIGNING FIELD-SYMBOL(<ls_query_update>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      READ TABLE lt_query_current ASSIGNING FIELD-SYMBOL(<ls_query_current>) INDEX lv_index.
      ASSERT sy-subrc = 0.

      lo_map->map_query_for_update(
        EXPORTING
          is_query        = <ls_query_update>
          iv_bo_key       = iv_bo_key
          iv_version_key  = iv_version_key
        CHANGING
          ct_modification = ct_modification
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_validations.

    " >>>>> remark: VALIDATION_CONF and *special handling* regarding consistency group (save prevention) is processed
    " >>>>> later on in determinations CONSISTENCY_VALIDATION_CREATE / _UPDATE (@ conf model node VALIDATION) for
    " >>>>> for consistency validations only (!)...

    " >>>>> remark: adjustment of VALIDATION_CONF for action validations (!) will take place in determination
    " >>>>> ACTION_VALIDATION_CREATE / _UPDATE (@ conf model node VALIDATION)...


    DATA(lo_map) = NEW /bobf/cl_conf_model_api_map( ).


    LOOP AT it_idx_create ASSIGNING FIELD-SYMBOL(<lv_index>).

      READ TABLE it_val_new ASSIGNING FIELD-SYMBOL(<ls_val_create>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      lo_map->map_val_for_create(
        EXPORTING
          iv_create_with_header = abap_true " !!!
          is_validation         = <ls_val_create>
          is_node               = is_node_new
          iv_bo_key             = iv_bo_key
          iv_node_key           = iv_node_key
          iv_version_key        = iv_version_key
        CHANGING
          ct_modification       = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_update ASSIGNING <lv_index>.

      READ TABLE it_val_new ASSIGNING FIELD-SYMBOL(<ls_val_update>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_validation_subent_keys(
        EXPORTING
          iv_val_key                   = <ls_val_update>-val_key
        IMPORTING
          et_val_trigger_key_delete    = DATA(lt_val_trigger_key_delete)
          et_val_conf_key_delete       = DATA(lt_val_conf_key_delete)
          et_val_group_conf_key_delete = DATA(lt_val_group_conf_key_delete) ).

      lo_map->map_val_for_update(
        EXPORTING
          is_validation                = <ls_val_update>
          iv_bo_key                    = iv_bo_key
          iv_node_key                  = iv_node_key
          is_node                      = is_node_new
          iv_version_key               = iv_version_key
          it_val_trigger_key_delete    = lt_val_trigger_key_delete
          it_val_conf_key_delete       = lt_val_conf_key_delete
          it_val_group_conf_key_delete = lt_val_group_conf_key_delete
        CHANGING
          ct_modification              = ct_modification ).

    ENDLOOP.


    LOOP AT it_idx_delete ASSIGNING <lv_index>.

      READ TABLE it_val_current ASSIGNING FIELD-SYMBOL(<ls_val_delete>) INDEX <lv_index>.
      ASSERT sy-subrc = 0.

      " get delete keys for all subentities...
      /bobf/cl_conf_model_api_reuse=>get_validation_subent_keys(
        EXPORTING
          iv_val_key                   = <ls_val_delete>-val_key
        IMPORTING
          et_val_trigger_key_delete    = lt_val_trigger_key_delete
          et_val_conf_key_delete       = lt_val_conf_key_delete
          et_val_group_conf_key_delete = lt_val_group_conf_key_delete ).

      lo_map->map_val_for_delete(
        EXPORTING
          iv_with_header               = abap_true " !!!
          iv_val_key                   = <ls_val_delete>-val_key
          it_val_trigger_key_delete    = lt_val_trigger_key_delete
          it_val_conf_key_delete       = lt_val_conf_key_delete
          it_val_group_conf_key_delete = lt_val_group_conf_key_delete
        CHANGING
          ct_modification              = ct_modification ).

    ENDLOOP.

  ENDMETHOD.


  METHOD update_business_object.

    DATA lo_compare                 TYPE REF TO /bobf/cl_conf_model_adt_comp.
    DATA lo_map                     TYPE REF TO /bobf/cl_conf_model_api_map.
    DATA lt_modification            TYPE /bobf/t_frw_modification.
    DATA lt_idx_node_create         TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_node_delete         TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_node_update         TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lt_idx_node_comp_subs_new  TYPE /bobf/cl_conf_model_adt_comp=>gtt_index.
    DATA lv_bo_key                  TYPE /bobf/obm_bo_key.
    DATA lo_message_container       TYPE REF TO /bobf/if_frw_message.
    DATA lo_service_manager         TYPE REF TO /bobf/if_tra_service_manager.
    DATA lo_transaction_manager     TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA lt_representation_node     TYPE /bobf/t_conf_node.

    "bo key or bo name must be provided
    ASSERT is_business_object-header-bo_key IS NOT INITIAL OR
           is_business_object-header-bo_name IS NOT INITIAL.

    ev_success = abap_true.
    CLEAR eo_message.

    prepare_modification( ).
    IF NOT is_business_object-header-bo_key IS INITIAL.
      lv_bo_key = is_business_object-header-bo_key.
    ELSE.
      lv_bo_key = /bobf/cl_conf_model_api_reuse=>get_bo_key_by_name( iv_bo_name = is_business_object-header-bo_name ).
    ENDIF.

    "get version
    DATA(lv_version_key) = NEW /bobf/cl_conf_model_vrs_ctrl( )->get_version_for_modify( lv_bo_key ).

    "get current bo data for comparison
    get_business_object(
      EXPORTING
        iv_business_object_name = is_business_object-header-bo_name
        iv_edit_mode            = abap_true
      IMPORTING
        es_business_object      = DATA(ls_current_bo)
        ev_success              = ev_success
        eo_message              = eo_message
    ).

    IF ev_success = abap_false.
      RETURN.
    ENDIF.

    lo_service_manager = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /bobf/if_conf_obj_c=>sc_bo_key ).
    "retrieve additional technical bo data not contained in the ADT BO structure
    lo_service_manager->retrieve_by_association(
      EXPORTING
        iv_node_key             = /bobf/if_conf_obj_c=>sc_node-version
        it_key                  = VALUE #( ( key = lv_version_key ) )
        iv_association          = /bobf/if_conf_obj_c=>sc_association-version-node
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 = lt_representation_node
    ).
    DELETE lt_representation_node WHERE node_type <> /bobf/if_conf_c=>sc_node_type_bo. "#EC CI_SORTSEQ

    "suppress locking (locking must be done seperately by method lock_business_object)
    "This has to come after get_business_object, because get_business_object does cleanup
    lo_service_manager->do_action(
      EXPORTING
        iv_act_key           = /bobf/if_conf_obj_c=>sc_action-root-mark_business_object_as_locked
        it_key               = VALUE #( ( key = lv_bo_key ) )
      IMPORTING
       eo_message            = lo_message_container
       et_failed_action_key =  DATA(lt_failed_key)
    ).

    IF NOT lt_failed_key IS INITIAL.
      ev_success = abap_false.
      eo_message = get_highest_priority_message( io_message = lo_message_container ).
      RETURN.
    ENDIF.

    "process BO header
    CREATE OBJECT lo_compare.
    CREATE OBJECT lo_map.

    " strip down Delegated Nodes, since they may have subnodes/-entities from last
    " GET_BUSINESS_OBJECT( IV_EXPAND_DEPENDENT_OBJECTS = X ) call...
    collapse_delegated_nodes( EXPORTING is_updated_business_object = is_business_object
                                        is_current_business_object = ls_current_bo
                              IMPORTING es_updated_business_object = DATA(ls_business_object) ).

    " determine authorization check relevant flag for header...
    lo_compare->compare_nodes_auth_check(
      EXPORTING
          it_node_new               = ls_business_object-nodes
        it_node_current           = ls_current_bo-nodes
      IMPORTING
        ev_auth_check_activated   = DATA(lv_auth_check_activated)
        ev_auth_check_deactivated = DATA(lv_auth_check_deactivated) ).

    IF ls_business_object-header-description <> ls_current_bo-header-description OR
       ls_business_object-header-const_interface <> ls_current_bo-header-const_interface OR
     lv_auth_check_activated = abap_true OR lv_auth_check_deactivated = abap_true.

      lo_map->map_bo_header_for_update(
        EXPORTING
          iv_version_key         = lv_version_key
            is_header              = ls_business_object-header
          iv_auth_check_relevant = lv_auth_check_activated
        CHANGING
          ct_modification        = lt_modification
      ).

    ENDIF.

    "compare nodes
    lo_compare->compare_tables_for_changes(
      EXPORTING
          it_subentity_new            = ls_business_object-nodes
        it_subentity_current        = ls_current_bo-nodes
        it_update_rel_element       = VALUE #( ( |NODE-UPDATABLE| ) )
      IMPORTING
        et_index_create_new         = lt_idx_node_create
        et_index_update_new         = lt_idx_node_update
        et_index_delete_current     = lt_idx_node_delete
        et_index_comp_subentity_new = lt_idx_node_comp_subs_new
    ).

    process_nodes( "including subentities
      EXPORTING
        iv_version_key            = lv_version_key
        iv_bo_key                 = lv_bo_key
        it_node_new               = ls_business_object-nodes
        it_node_current           = ls_current_bo-nodes
        it_idx_update_new         = lt_idx_node_update
        it_idx_create_new         = lt_idx_node_create
        it_idx_delete_current     = lt_idx_node_delete
        it_idx_comp_subentity_new = lt_idx_node_comp_subs_new
      CHANGING
        ct_representation_node    = lt_representation_node
        ct_modification           = lt_modification
    ).

    "apply changes
    lo_transaction_manager = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    IF NOT lt_modification IS INITIAL.

      lo_service_manager->modify(
        EXPORTING
          it_modification = lt_modification
        IMPORTING
          eo_change       = DATA(lo_change)
          eo_message      = lo_message_container ).
      IF lo_change->has_failed_changes( ) = abap_true.
        ev_success = abap_false.
        eo_message = get_highest_priority_message( io_message = lo_message_container ).
        /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->cleanup( ).
        RETURN.
      ENDIF.

      " process additional steps, which need to read new metadata from buffer.
      " therefore this is processed here, after the modify() call...
      finalize_nodes(
        EXPORTING
          iv_version_key        = lv_version_key
          iv_bo_key             = lv_bo_key
            it_node_new           = ls_business_object-nodes
          it_node_current       = ls_current_bo-nodes
          it_idx_update_new     = lt_idx_node_update
          it_idx_create_new     = lt_idx_node_create
          it_idx_delete_current = lt_idx_node_delete
        IMPORTING
          et_modification       = DATA(lt_modification_finalize)
          ev_success            = ev_success ).
      IF ev_success = abap_false.
        /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->cleanup( ).
        RETURN.
      ENDIF.

      IF lt_modification_finalize IS NOT INITIAL.
        lo_service_manager->modify(
          EXPORTING
            it_modification = lt_modification_finalize
          IMPORTING
            eo_change       = DATA(lo_change_finalize)
            eo_message      = DATA(lo_message_finalize) ).
        IF lo_change_finalize->has_failed_changes( ) = abap_true.
          ev_success = abap_false.
          eo_message = get_highest_priority_message( io_message = lo_message_finalize ).
          /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->cleanup( ).
          RETURN.
        ENDIF.
      ENDIF.

      " remove _lost_ artifacts: e.g. ITEM was deleted, with CHECK_CORRECT_LOST_ELEMENTS the ITEM queries are deleted too...
      lo_service_manager->do_action(
        iv_act_key = /bobf/if_conf_obj_c=>sc_action-version-check_correct_lost_elements
        it_key     = VALUE #( ( key = lv_version_key ) ) ).

      lo_transaction_manager->save(
        IMPORTING
          ev_rejected            = DATA(lv_save_rejected)
          eo_message             = lo_message_container
      ).

      IF lv_save_rejected = abap_true.
        ev_success = abap_false.
        lo_transaction_manager->cleanup( ).
        eo_message = get_highest_priority_message( io_message = lo_message_container ).
        RETURN.
      ENDIF.
    ENDIF.

    lo_transaction_manager->cleanup( ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_complete_model_api DEFINITION INHERITING FROM lcl_conf_model_api_adt_copy.
  PUBLIC SECTION.
    METHODS get_business_object REDEFINITION.
ENDCLASS.

CLASS lcl_complete_model_api IMPLEMENTATION.
  METHOD get_business_object.
    super->get_business_object(
      EXPORTING
        iv_business_object_name     = iv_business_object_name    " Business Object
        iv_edit_mode                = iv_edit_mode    " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
        iv_expand_dependent_objects = iv_expand_dependent_objects    " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
      IMPORTING
        es_business_object          = es_business_object    " ADT BO
        ev_success                  = ev_success    " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
        eo_message                  = eo_message    " Interface of Message Object
    ).

    CHECK ev_success = abap_true.

* Get BO name by given key
    DATA(lv_bo_key) = /bobf/cl_conf_model_api_reuse=>get_bo_key_by_name( iv_bo_name = iv_business_object_name ).
    IF lv_bo_key IS INITIAL.
      ev_success = abap_false.
      eo_message =  NEW /bobf/cm_conf_model_api_adt(
           textid     = /bobf/cm_conf_model_api=>bo_not_found
           severity   = /bobf/cm_frw=>co_severity_error
           symptom    = /bobf/if_frw_message_symptoms=>co_bo_inconsistency
           mv_bo_name = iv_business_object_name ).
      RETURN.
    ENDIF.

    /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->cleanup( ).
    DATA(lo_service_manager) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /bobf/if_conf_obj_c=>sc_bo_key ).

    DATA(lv_version_key) = NEW /bobf/cl_conf_model_vrs_ctrl(
        )->get_version_for_retrieve(
        EXPORTING
          iv_bo_key      = lv_bo_key
          iv_bo_name     = iv_business_object_name
          iv_edit_mode   = /bobf/cl_conf_model_api_reuse=>convert_edit_mode( iv_edit_mode )
    ).

*      add framework-actions
    /bobf/cl_conf_model_api_reuse=>get_action_tab(
      EXPORTING
        iv_bo_key      = lv_bo_key
        iv_version_key = lv_version_key
        iv_framework_actions  = abap_true
      IMPORTING
        et_action_tab  = DATA(lt_action)
        ev_success     = ev_success
    ).

    LOOP AT es_business_object-nodes ASSIGNING FIELD-SYMBOL(<ls_bo_node>).
      LOOP AT lt_action ASSIGNING FIELD-SYMBOL(<ls_action>) WHERE node_key = <ls_bo_node>-node-node_key.
        READ TABLE <ls_bo_node>-action TRANSPORTING NO FIELDS WITH KEY act_key = <ls_action>-act_key.
        IF sy-subrc <> 0.
          INSERT <ls_action> INTO TABLE <ls_bo_node>-action.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.