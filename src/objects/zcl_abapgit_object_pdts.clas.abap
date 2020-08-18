CLASS zcl_abapgit_object_pdts DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor IMPORTING is_item     TYPE zif_abapgit_definitions=>ty_item
                                  iv_language TYPE spras.

    TYPES:
      BEGIN OF ty_task,
        short_text                 TYPE hr_mcshort,
        plvar                      TYPE plvar,
        wi_text                    TYPE witext,
        method                     TYPE hrs1201,
        method_binding             TYPE hrsmtbind,
        starting_events            TYPE hrsevtab,
        starting_events_binding    TYPE hrsevbind,
        terminating_events         TYPE hrsetmtab,
        terminating_events_binding TYPE hrsevbind,
        descriptions               TYPE wstexts,
      END OF ty_task .

  PRIVATE SECTION.

    CONSTANTS:
      c_subty_task_description TYPE hr_s_subty VALUE '0120',
      c_object_type_task       TYPE hr_sotype VALUE 'TS'.

    DATA mv_objid TYPE hrobjid. "hrobject-objid.

    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_pdts IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_objid = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_task           TYPE ty_task,
          lo_inst           TYPE REF TO cl_workflow_task_ts,
          li_first_element  TYPE REF TO if_ixml_element,
          li_xml_dom        TYPE REF TO if_ixml_document,
          li_elements       TYPE REF TO if_ixml_node_collection,
          li_iterator       TYPE REF TO if_ixml_node_iterator,
          li_element        TYPE REF TO if_ixml_node,
          li_children       TYPE REF TO if_ixml_node_list,
          li_child_iterator TYPE REF TO if_ixml_node_iterator,
          li_attributes     TYPE REF TO if_ixml_named_node_map,
          lv_length         TYPE i,
          lv_xml_stream     TYPE xstring.

    FIELD-SYMBOLS: <ls_description>             TYPE hrs1002,
                   <ls_method_binding>          LIKE LINE OF ls_task-method_binding,
                   <ls_starting_events_binding> TYPE hrs1212,
                   <ls_term_events_binding>     TYPE hrs1212.

    "Todo: Delete info note once it all works
    "Original code from https://app.assembla.com/spaces/saplink-plugins/subversion/source/HEAD/trunk/Workflow
    "called function 'RH_TASK_ATTRIBUTES_RUNTIME'

    cl_workflow_factory=>create_ts(
      EXPORTING
        objid                        = mv_objid
      RECEIVING
        ts_inst                      = lo_inst
      EXCEPTIONS
        standard_task_does_not_exist = 1
        object_could_not_be_locked   = 2
        objid_not_given              = 3
        OTHERS                       = 4 )  ##SUBRC_OK.

    check_subrc_for( `CREATE_TS` ).

    ls_task-wi_text                    = lo_inst->wi_text.
    ls_task-short_text                 = lo_inst->short_text.
    ls_task-plvar                      = lo_inst->plvar.
    ls_task-method                     = lo_inst->method.
    ls_task-method_binding             = lo_inst->method_binding.
    ls_task-starting_events            = lo_inst->starting_events.
    ls_task-starting_events_binding    = lo_inst->starting_events_binding.
    ls_task-terminating_events         = lo_inst->terminating_events.
    ls_task-terminating_events_binding = lo_inst->terminating_events_binding.
    ls_task-descriptions               = lo_inst->descriptions.

    CLEAR: ls_task-method-aedtm,
           ls_task-method-uname.

    LOOP AT ls_task-method_binding ASSIGNING <ls_method_binding>.

      CLEAR: <ls_method_binding>-aedtm,
             <ls_method_binding>-uname.

    ENDLOOP.

    LOOP AT ls_task-starting_events_binding ASSIGNING <ls_starting_events_binding>.

      CLEAR: <ls_starting_events_binding>-aedtm,
             <ls_starting_events_binding>-uname.

    ENDLOOP.

    LOOP AT ls_task-descriptions ASSIGNING <ls_description>.

      CLEAR: <ls_description>-aedtm,
             <ls_description>-uname.

    ENDLOOP.

    LOOP AT ls_task-terminating_events_binding ASSIGNING <ls_term_events_binding>.

      CLEAR: <ls_term_events_binding>-aedtm,
             <ls_term_events_binding>-uname.

    ENDLOOP.

    io_xml->add( iv_name = 'PDTS'
                 ig_data = ls_task ).

    lo_inst->container->to_xml(
      EXPORTING
        include_null_values        = abap_true
        include_initial_values     = abap_true
        include_typenames          = abap_true
        include_change_data        = abap_true
        include_texts              = abap_true
        include_extension_elements = abap_true
        save_delta_handling_info   = abap_true
        use_xslt                   = abap_false
      IMPORTING
        xml_dom                    = li_xml_dom
        xml_stream                 = lv_xml_stream
      EXCEPTIONS
        conversion_error           = 1
        OTHERS                     = 2 ).

    check_subrc_for( `TO_XML` ).

    li_first_element ?= li_xml_dom->get_first_child( ).

    li_elements = li_first_element->get_elements_by_tag_name( name = 'ELEMENTS' ). "#EC NOTEXT

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

        li_attributes = li_element->get_attributes( ).

        lv_length = li_attributes->get_length( ).
        li_attributes->remove_named_item( name = 'CHGDTA' )  ##no_text.
        lv_length = li_attributes->get_length( ).

      ENDDO.

    ENDDO.

    io_xml->add_xml( iv_name = 'CONTAINER'
                     ii_xml  = li_first_element ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_task              TYPE ty_task,
          ls_hrsobject         TYPE hrsobject,
          lo_inst              TYPE REF TO cl_workflow_task_ts,
          lo_gen_task          TYPE REF TO cl_workflow_general_task_def,
          lv_xml_string        TYPE xstring,
          li_document          TYPE REF TO if_ixml_document,
          li_container_element TYPE REF TO if_ixml_element,
          li_stream            TYPE REF TO if_ixml_ostream,
          lt_exception_list    TYPE swf_cx_tab.

    FIELD-SYMBOLS: <ls_method_binding> TYPE hrs1214.

    io_xml->read(
      EXPORTING
        iv_name = 'PDTS'
      CHANGING
        cg_data = ls_task ).

    cl_workflow_factory=>create_new_ts(
      EXPORTING
        short_text          = |{ ls_task-short_text }|
        text                = |{ ls_task-wi_text }|
      RECEIVING
        task_object         = lo_inst
      EXCEPTIONS
        text_exists_already = 1
        OTHERS              = 2 ).

    check_subrc_for( `CREATE_NEW_TS` ).

    lo_gen_task ?= lo_inst.

    lcl_abapgit_object_pdts_helper=>set_objid( iv_objid = mv_objid
                                               io_task  = lo_gen_task ).

    lcl_abapgit_object_pdts_helper=>set_container_id( iv_id    = |{ c_object_type_task }{ mv_objid }|
                                                      io_task  = lo_gen_task ).

    lo_inst->change_wi_text(
      EXPORTING
        new_wi_text        = ls_task-wi_text
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    check_subrc_for( `CHANGE_WI_TEXT` ).

    lo_inst->change_method(
      EXPORTING
        new_method                   = ls_task-method    " New Method or Settings
      EXCEPTIONS
        no_changes_allowed           = 1
        problem_method_web_enabling  = 2
        problem_method_phon_enabling = 3
        OTHERS                       = 4 ).

    check_subrc_for( `CHANGE_METHOD` ).                     "#EC NOTEXT

    LOOP AT ls_task-method_binding ASSIGNING <ls_method_binding>.

      lo_inst->change_method_binding(
        EXPORTING
          binding                       = <ls_method_binding>
          delete                        = abap_false
          insert                        = abap_true
        EXCEPTIONS
          no_changes_allowed            = 1
          desired_action_not_clear      = 2
          ts_cnt_element_does_not_exist = 3
          binding_could_not_be_deleted  = 4
          OTHERS                        = 5 ).            "#EC SUBRC_OK

      check_subrc_for( `CHANGE_METHOD_BINDING` ).           "#EC NOTEXT

    ENDLOOP.

    lo_inst->change_start_events_complete(
      EXPORTING
        starting_events    = ls_task-starting_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    check_subrc_for( `CHANGE_START_EVENTS_COMPLETE` ).      "#EC NOTEXT

    lo_inst->change_start_evt_bind_complete(
      EXPORTING
        new_bindings       = ls_task-starting_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    check_subrc_for( `CHANGE_START_EVT_BIND_COMPLETE` ).    "#EC NOTEXT

    lo_inst->change_term_events_complete(
      EXPORTING
        terminating_events = ls_task-terminating_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    check_subrc_for( `CHANGE_TEXT` ).                       "#EC NOTEXT

    lo_inst->change_term_evt_bind_complete(
      EXPORTING
        new_bindings       = ls_task-terminating_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    check_subrc_for( `CHANGE_TERM_EVENTS_COMPLETE` ).       "#EC NOTEXT

    lo_inst->change_text(
      EXPORTING
        subty              = c_subty_task_description
        new_text           = ls_task-descriptions
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    check_subrc_for( `CHANGE_TEXT` ).                       "#EC NOTEXT

    ls_hrsobject-otype = c_object_type_task.
    ls_hrsobject-objid = mv_objid.
    INSERT hrsobject FROM ls_hrsobject.

    lo_inst->save_standard_task(
      EXPORTING
        development_class          = iv_package
        iv_force_gen               = abap_true
      EXCEPTIONS
        no_changes_allowed         = 1
        no_client_indep_maint      = 2
        update_error               = 3
        insert_error_new_ts        = 4
        new_ts_could_not_be_locked = 5
        save_abort_by_user         = 6
        OTHERS                     = 7 ).

    check_subrc_for( `SAVE_STANDARD_TASK` ).                "#EC NOTEXT

    li_document = io_xml->get_raw( ).

    li_container_element = li_document->find_from_name_ns( 'CONTAINER' ).

    IF li_container_element IS BOUND.

      li_document = cl_ixml=>create( )->create_document( ).

      li_stream = cl_ixml=>create( )->create_stream_factory( )->create_ostream_xstring( lv_xml_string ).

      li_document->append_child( li_container_element ).

      cl_ixml=>create( )->create_renderer(
          document = li_document
          ostream  = li_stream
      )->render( ).

      lo_inst->container->import_from_xml(
        EXPORTING
          xml_stream     = lv_xml_string
        IMPORTING
          exception_list = lt_exception_list ).

    ENDIF.

    tadir_insert( iv_package ).

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
        OTHERS              = 4.  "#EC SUBRC_OK

    check_subrc_for( `RH_HRSOBJECT_DELETE` ).

    "Todo: delete this comment when stable
    "Previous code used FUNCTION 'RH_TASK_DELETE'

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_endda TYPE plog-endda.

    lv_endda = '99991231'.

    CALL FUNCTION 'RH_READ_OBJECT'
      EXPORTING
        plvar     = '01'
        otype     = c_object_type_task
        objid     = mv_objid
        istat     = '1'
        begda     = sy-datum
        endda     = lv_endda
        ointerval = 'X'
        read_db   = 'X'
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    CALL FUNCTION 'ENQUEUE_HRSOBJECT'
      EXPORTING
        objid          = mv_objid
        otype          = c_object_type_task
        x_objid        = ' '
        x_otype        = ' '
        _scope         = '2'
        _wait          = ' '
      EXCEPTIONS
        foreign_lock   = 01
        system_failure = 02.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE uname
      FROM hrs1201
      WHERE otype = 'TS' AND
            objid = @ms_item-obj_name
      INTO @rv_user.

    IF sy-subrc = 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS_REMOTE'
      STARTING NEW TASK 'GIT'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

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
      zcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ). "#EC NOTEXT
    ENDIF.
  ENDMETHOD.


ENDCLASS.
