CLASS zcl_abapgit_object_bobf DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_xml_element_model TYPE string VALUE 'MODEL'.

    METHODS get_conf_model_api
      RETURNING VALUE(ro_api) TYPE REF TO /bobf/cl_conf_model_api_adt
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_bobf IMPLEMENTATION.


  METHOD get_conf_model_api.

    ro_api = NEW /bobf/cl_conf_model_api_adt( iv_package = ms_item-devclass ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    me->get_conf_model_api( )->delete_business_object(
      EXPORTING
        iv_business_object_name = CONV #( ms_item )
      IMPORTING
        eo_message              = DATA(lo_message)
        ev_success              = DATA(lv_success)
    ).

    IF lv_success EQ abap_false.
      zcx_abapgit_exception=>raise( 'error deleting BOPF object' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA ls_model TYPE /bobf/s_conf_model_adt_bo.

    io_xml->read( EXPORTING iv_name = me->c_xml_element_model
                  CHANGING cg_data = ls_model ).

    DATA(lo_api) = me->get_conf_model_api( ).

    LOOP AT ls_model-nodes ASSIGNING FIELD-SYMBOL(<ls_root_node>) WHERE node-parent_node_key IS INITIAL.
      EXIT.
    ENDLOOP.

    ASSERT <ls_root_node> IS NOT INITIAL.

    lo_api->create_business_object(
      EXPORTING
        iv_business_object_name       = ls_model-header-bo_name
        iv_description                = ls_model-header-description
        iv_constant_interface         = ls_model-header-const_interface
        iv_root_name                  = CONV #( <ls_root_node>-node-node_name )
        iv_root_description           = <ls_root_node>-node-description
        iv_root_data_type             = <ls_root_node>-node-data_type
        iv_root_data_data_type        = <ls_root_node>-node-data_data_type
        iv_root_data_data_type_t      = <ls_root_node>-node-data_data_type_t
        iv_root_data_table_type       = <ls_root_node>-node-data_table_type
        iv_root_database_table        = <ls_root_node>-node-database_table
      IMPORTING
        ev_bo_key                     = ls_model-header-bo_key
        ev_version_key                = DATA(lv_version_key)
        ev_success                    = DATA(lv_bo_created)
    ).

*    This generated a root node. Now we need to replace the node key of the root node imported with the one created
    lo_api->get_business_object(
      EXPORTING
        iv_business_object_name     = ls_model-header-bo_name
      IMPORTING
        es_business_object          = DATA(ls_just_created_model)
    ).
    ASSERT lines( ls_just_created_model-nodes ) = 1.
    DATA(ls_new_root_node) = ls_just_created_model-nodes[ 1 ].
    DELETE ls_model-nodes WHERE node-node_key = <ls_root_node>-node-node_key.

    LOOP AT ls_model-nodes ASSIGNING FIELD-SYMBOL(<ls_node>).
      <ls_node>-node-parent_node_key = ls_new_root_node-node-node_key.
    ENDLOOP.

    INSERT ls_new_root_node INTO TABLE ls_model-nodes.

*    also, we need to "move" all model elements below the just created version node
*    TODO!

    lo_api->update_business_object(
      EXPORTING
        is_business_object       = ls_model
*        iv_update_active_version = ABAP_FALSE
*        iv_cleanup_transaction   = ABAP_TRUE
*        iv_write_generation_info =     " Categories of writing generation infos
      IMPORTING
        eo_message               = DATA(lo_message)
        ev_success               = DATA(lv_model_imported)
        et_modification          = DATA(lt_mod)
    ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    me->get_conf_model_api( )->get_business_object(
      EXPORTING
        iv_business_object_name     = CONV #( ms_item-obj_name )
        iv_header_only              = abap_true
      IMPORTING
        ev_success                  = rv_bool
    ).

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

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    me->get_conf_model_api( )->get_business_object(
      EXPORTING
        iv_business_object_name     = CONV #( ms_item-obj_name )
      IMPORTING
        es_business_object          = DATA(ls_model)
    ).

    io_xml->add( iv_name = me->c_xml_element_model
                 ig_data = ls_model ).

  ENDMETHOD.
ENDCLASS.
