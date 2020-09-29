CLASS zcl_abapgit_object_pdts DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor IMPORTING is_item     TYPE zif_abapgit_definitions=>ty_item
                                  iv_language TYPE spras
                        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.

    CONSTANTS: c_object_type_task       TYPE hr_sotype VALUE 'TS'.

    DATA ms_objkey TYPE hrsobject.
    DATA mv_objid TYPE hrobjid.

    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   zcx_abapgit_exception.

    METHODS is_experimental RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS get_container_xml
      IMPORTING
        ii_task                 TYPE REF TO lif_task_definition
      RETURNING
        VALUE(ri_first_element) TYPE REF TO if_ixml_element
      RAISING
        zcx_abapgit_exception.

    METHODS extract_container IMPORTING io_xml           TYPE REF TO zif_abapgit_xml_input
                              RETURNING VALUE(rv_result) TYPE xstring.

ENDCLASS.


CLASS zcl_abapgit_object_pdts IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    IF is_experimental( ) = abap_false.
      "Alpha version, known issues:
      "- Container texts not de/serialized properly (functionally OK)
      "- Container handling still a bad hack, needs refactoring with lots of debugging time
      "- Probably has a few more bugs, more testing needed
      zcx_abapgit_exception=>raise( 'PDTS not fully implemented, enable experimental features to test it' ).
    ENDIF.

    ms_objkey-otype = c_object_type_task.
    ms_objkey-objid = ms_item-obj_name.

    mv_objid = ms_item-obj_name.  "Todo: Obsolete

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA li_task TYPE REF TO lif_task_definition.

    li_task = lcl_task_definition=>load( mv_objid ).
    li_task->clear_origin_data( ).
    io_xml->add( iv_name = 'PDTS'
                 ig_data = li_task->get_definition( ) ).

    io_xml->add_xml( iv_name = 'CONTAINER'
                     ii_xml  = get_container_xml( li_task ) ).

  ENDMETHOD.


  METHOD get_container_xml.

    DATA li_xml_dom TYPE REF TO if_ixml_document.
    DATA li_elements TYPE REF TO if_ixml_node_collection.
    DATA li_iterator TYPE REF TO if_ixml_node_iterator.
    DATA li_element TYPE REF TO if_ixml_node.
    DATA li_children TYPE REF TO if_ixml_node_list.
    DATA li_child_iterator TYPE REF TO if_ixml_node_iterator.
    DATA li_attributes TYPE REF TO if_ixml_named_node_map.
    DATA lv_name TYPE string.

    "Todo: get_user_container strips out system elements, but to_xml adds them back in (hardcoded internally)
    "      Dirty hack further down to remove them from XML until we get this to work properly
    ii_task->get_user_container( )->to_xml(
      EXPORTING
        include_null_values        = abap_true
        include_initial_values     = abap_true
        include_typenames          = abap_true
        include_change_data        = abap_true
        include_texts              = abap_false  "Todo: Get texts to work properly
        include_extension_elements = abap_true
        save_delta_handling_info   = abap_true
        use_xslt                   = abap_false
      IMPORTING
        xml_dom                    = li_xml_dom
      EXCEPTIONS
        conversion_error           = 1
        OTHERS                     = 2 ).                 "#EC SUBRC_OK

    check_subrc_for( `TO_XML` ).

    ri_first_element ?= li_xml_dom->get_first_child( ).
    li_elements = ri_first_element->get_elements_by_tag_name( name = 'ELEMENTS' ).
    li_iterator = li_elements->create_iterator( ).

    DO.
      li_element = li_iterator->get_next( ).

      IF li_element IS NOT BOUND.
        EXIT.
      ENDIF.

      li_children = li_element->get_children( ).
      li_child_iterator = li_children->create_iterator( ).

      DO.

        li_element = li_child_iterator->get_next( ).
        IF li_element IS NOT BOUND.
          EXIT.
        ENDIF.

        "Remove system container elements - causing too much trouble
        "Todo: This is a bad hack, but obsolete if we can fix todo above
        li_attributes = li_element->get_attributes( ).
        lv_name = li_attributes->get_named_item( name  = 'NAME' )->get_value( ).
        IF lv_name(1) = '_'.
          li_element->remove_node( ).
          li_child_iterator->reset( ).
          CONTINUE.
        ENDIF.

        li_attributes->remove_named_item( name = 'CHGDTA' ).

      ENDDO.

    ENDDO.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_task       TYPE lif_task_definition=>ty_task_data,
          lv_xml_string TYPE xstring,
          li_task       TYPE REF TO lif_task_definition.

    io_xml->read( EXPORTING iv_name = 'PDTS'
      CHANGING cg_data = ls_task ).

    li_task = lcl_task_definition=>create(
                      iv_objid     = mv_objid
                      is_task_data = ls_task ).

    li_task->create_task( ).
    li_task->change_wi_text( ).
    li_task->change_method( ).

    lv_xml_string = extract_container( io_xml ).
    li_task->import_container( lv_xml_string ).

    li_task->change_start_events( ).
    li_task->change_terminating_events( ).
    li_task->change_text( ).

    li_task->save( iv_package ).

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD extract_container.

    DATA li_stream TYPE REF TO if_ixml_ostream.
    DATA li_container_element TYPE REF TO if_ixml_element.
    DATA li_document TYPE REF TO if_ixml_document.

    li_document = io_xml->get_raw( ).

    li_container_element = li_document->find_from_name_ns( 'CONTAINER' ).

    IF li_container_element IS BOUND.

      li_document = cl_ixml=>create( )->create_document( ).

      li_stream = cl_ixml=>create( )->create_stream_factory( )->create_ostream_xstring( rv_result ).

      li_document->append_child( li_container_element ).

      cl_ixml=>create( )->create_renderer(
          document = li_document
          ostream  = li_stream
      )->render( ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'RH_HRSOBJECT_DELETE'
      EXPORTING
        act_otype           = c_object_type_task
        act_objid           = mv_objid
        no_confirmation_msg = abap_true
      EXCEPTIONS
        enqueue_failed      = 1
        object_not_deleted  = 2
        object_not_found    = 3
        OTHERS              = 4.       "#EC SUBRC_OK

    check_subrc_for( `RH_HRSOBJECT_DELETE` ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'RH_READ_OBJECT'
      EXPORTING
        plvar     = '01'
        otype     = c_object_type_task
        objid     = mv_objid
        istat     = '1'
        begda     = sy-datum
        endda     = '99991231'
        ointerval = 'X'
        read_db   = 'X'
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'HRSOBJECT'
                                            iv_argument = c_object_type_task && mv_objid ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE uname
      INTO rv_user
      FROM hrs1201
      WHERE otype = c_object_type_task AND
            objid = ms_item-obj_name.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS_REMOTE'
      STARTING NEW TASK 'GIT'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type
      EXCEPTIONS
        OTHERS      = 0.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD check_subrc_for.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ).
    ENDIF.
  ENDMETHOD.


  METHOD is_experimental.

    DATA lo_settings TYPE REF TO zcl_abapgit_settings.
    DATA lo_settings_persistence TYPE REF TO zcl_abapgit_persist_settings.

    lo_settings_persistence = zcl_abapgit_persist_settings=>get_instance( ).
    lo_settings = lo_settings_persistence->read( ).
    rv_result = lo_settings->get_experimental_features( ).

  ENDMETHOD.

ENDCLASS.
