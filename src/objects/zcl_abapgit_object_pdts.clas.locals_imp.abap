CLASS lcl_attribute_setter DEFINITION
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC
  FINAL.
  PUBLIC SECTION.

    CLASS-METHODS set_objid IMPORTING iv_objid TYPE hrobject-objid
                                      io_task  TYPE REF TO cl_workflow_general_task_def.

    CLASS-METHODS set_container_id IMPORTING iv_id   TYPE guid_32
                                             io_task TYPE REF TO cl_workflow_general_task_def. "#EC NEEDED
ENDCLASS.

CLASS lcl_attribute_setter IMPLEMENTATION.

  METHOD set_container_id.

    FIELD-SYMBOLS <lv_object> TYPE REF TO if_swf_cnt_container.

    ASSIGN ('IO_TASK->CONTAINER') TO <lv_object>.
    ASSERT sy-subrc = 0.

    CALL METHOD <lv_object>->('SET_GUID')
      EXPORTING
        guid_32 = iv_id.

  ENDMETHOD.


  METHOD set_objid.
    io_task->objid = iv_objid.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_task_definition DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.

    INTERFACES lif_task_definition.

    CLASS-METHODS load IMPORTING iv_objid         TYPE hrobject-objid
                       RETURNING VALUE(ri_result) TYPE REF TO lif_task_definition
                       RAISING   zcx_abapgit_exception.

    CLASS-METHODS create IMPORTING iv_objid         TYPE hrobject-objid
                                   is_task_data     TYPE lif_task_definition=>ty_task_data
                         RETURNING VALUE(ri_result) TYPE REF TO lif_task_definition
                         RAISING   zcx_abapgit_exception.


  PRIVATE SECTION.
    CONSTANTS c_subty_task_description TYPE hr_s_subty VALUE '0120'.

    DATA mo_taskdef TYPE REF TO cl_workflow_task_ts.
    DATA ms_task TYPE lif_task_definition=>ty_task_data.

    DATA: mv_objid                      TYPE hrobjid.

    METHODS supply_instance RAISING zcx_abapgit_exception.
    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   zcx_abapgit_exception.

ENDCLASS.


CLASS lcl_task_definition IMPLEMENTATION.

  METHOD load.

    DATA lo_taskdef TYPE REF TO lcl_task_definition.

    CREATE OBJECT lo_taskdef.
    lo_taskdef->mv_objid = iv_objid.
    lo_taskdef->supply_instance( ).

    ri_result = lo_taskdef.

  ENDMETHOD.

  METHOD check_subrc_for.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ).
    ENDIF.
  ENDMETHOD.


  METHOD supply_instance.

    cl_workflow_factory=>create_ts(
      EXPORTING
        objid                        = mv_objid
      RECEIVING
        ts_inst                      = mo_taskdef
      EXCEPTIONS
        standard_task_does_not_exist = 1
        object_could_not_be_locked   = 2
        objid_not_given              = 3
        OTHERS                       = 4 )  ##SUBRC_OK.

    check_subrc_for( 'CREATE_TS' ).

    ms_task-wi_text                    = mo_taskdef->wi_text.
    ms_task-short_text                 = mo_taskdef->short_text.
    ms_task-plvar                      = mo_taskdef->plvar.
    ms_task-method                     = mo_taskdef->method.
    ms_task-method_binding             = mo_taskdef->method_binding.
    ms_task-starting_events            = mo_taskdef->starting_events.
    ms_task-starting_events_binding    = mo_taskdef->starting_events_binding.
    ms_task-terminating_events         = mo_taskdef->terminating_events.
    ms_task-terminating_events_binding = mo_taskdef->terminating_events_binding.
    ms_task-descriptions               = mo_taskdef->descriptions.

  ENDMETHOD.

  METHOD lif_task_definition~clear_origin_data.

    FIELD-SYMBOLS: <ls_description>             TYPE hrs1002,
                   <ls_method_binding>          TYPE hrs1214,
                   <ls_starting_events_binding> TYPE hrs1212,
                   <ls_term_events_binding>     TYPE hrs1212.

    CLEAR: ms_task-method-aedtm,
           ms_task-method-uname.

    LOOP AT ms_task-method_binding ASSIGNING <ls_method_binding>.
      CLEAR: <ls_method_binding>-aedtm,
             <ls_method_binding>-uname.
    ENDLOOP.

    LOOP AT ms_task-starting_events_binding ASSIGNING <ls_starting_events_binding>.
      CLEAR: <ls_starting_events_binding>-aedtm,
             <ls_starting_events_binding>-uname.
    ENDLOOP.

    LOOP AT ms_task-descriptions ASSIGNING <ls_description>.
      CLEAR: <ls_description>-aedtm,
             <ls_description>-uname.
    ENDLOOP.

    LOOP AT ms_task-terminating_events_binding ASSIGNING <ls_term_events_binding>.
      CLEAR: <ls_term_events_binding>-aedtm,
             <ls_term_events_binding>-uname.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_task_definition~get_definition.
    rs_result = me->ms_task.
  ENDMETHOD.

  METHOD lif_task_definition~get_container.
    ri_result = mo_taskdef->container.
  ENDMETHOD.

  METHOD lif_task_definition~get_user_container.

    DATA: li_container       TYPE REF TO if_swf_cnt_element_access_1,
          lt_user_elements   TYPE swfdnamtab,
          lt_system_elements TYPE swfdnamtab,
          lv_element         TYPE swfdname.

    li_container = mo_taskdef->container.
    lt_user_elements = li_container->all_elements_list( ).
    lt_system_elements = li_container->all_elements_list( list_system = abap_true ).

    LOOP AT lt_system_elements INTO lv_element.
      READ TABLE lt_user_elements WITH KEY table_line = lv_element TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        TRY.
            li_container->element_remove( name = lv_element ).
          CATCH cx_swf_cnt_container.
            "Shouldn't happen, doesn't matter if it does
        ENDTRY.
      ENDIF.
    ENDLOOP.

    ri_result ?= li_container.

  ENDMETHOD.

  METHOD create.
    DATA lo_task TYPE REF TO lcl_task_definition.

    CREATE OBJECT lo_task TYPE lcl_task_definition.
    lo_task->mv_objid = iv_objid.
    lo_task->ms_task = is_task_data.
    ri_result = lo_task.

  ENDMETHOD.


  METHOD lif_task_definition~import_container.

    DATA lt_exception_list TYPE swf_cx_tab.
    DATA lx_exception TYPE REF TO cx_swf_ifs_exception.

    mo_taskdef->container->import_from_xml(
            EXPORTING xml_stream     = iv_xml_string
            IMPORTING exception_list = lt_exception_list ).

    IF lt_exception_list IS NOT INITIAL.
      READ TABLE lt_exception_list INDEX 1 INTO lx_exception.
      zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_task_definition~create_task.

    cl_workflow_factory=>create_new_ts(
      EXPORTING
        short_text          = |{ ms_task-short_text }|
        text                = |{ ms_task-wi_text }|
      RECEIVING
        task_object         = mo_taskdef
      EXCEPTIONS
        text_exists_already = 1
        OTHERS              = 2 ).                        "#EC SUBRC_OK

    check_subrc_for( `CREATE_NEW_TS` ).

    lcl_attribute_setter=>set_objid( iv_objid = mv_objid
                                           io_task  = mo_taskdef ).

    lcl_attribute_setter=>set_container_id( iv_id    = |TS{ mv_objid }|
                                            io_task  = mo_taskdef ).

  ENDMETHOD.

  METHOD lif_task_definition~change_start_events.

    mo_taskdef->change_start_events_complete(
      EXPORTING
        starting_events    = ms_task-starting_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_START_EVENTS_COMPLETE` ).

    mo_taskdef->change_start_evt_bind_complete(
      EXPORTING
        new_bindings       = ms_task-starting_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_START_EVT_BIND_COMPLETE` ).

  ENDMETHOD.


  METHOD lif_task_definition~save.

    DATA ls_hrsobject TYPE hrsobject.
    ls_hrsobject-otype = 'TS'. "swfco_org_standard_task - todo: linter can't resolve this
    ls_hrsobject-objid = mv_objid.
    INSERT hrsobject FROM ls_hrsobject.

    mo_taskdef->save_standard_task(
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
        OTHERS                     = 7 ).                 "#EC SUBRC_OK

    check_subrc_for( `SAVE_STANDARD_TASK` ).

  ENDMETHOD.


  METHOD lif_task_definition~change_wi_text.

    mo_taskdef->change_wi_text(
      EXPORTING
        new_wi_text        = ms_task-wi_text
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_WI_TEXT` ).

  ENDMETHOD.


  METHOD lif_task_definition~change_method.

    FIELD-SYMBOLS <ls_method_binding> TYPE hrs1214.

    mo_taskdef->change_method(
      EXPORTING
        new_method                   = ms_task-method    " New Method or Settings
      EXCEPTIONS
        no_changes_allowed           = 1
        problem_method_web_enabling  = 2
        problem_method_phon_enabling = 3
        OTHERS                       = 4 ).               "#EC SUBRC_OK

    check_subrc_for( `CHANGE_METHOD` ).

    LOOP AT ms_task-method_binding ASSIGNING <ls_method_binding>.

      mo_taskdef->change_method_binding(
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

      check_subrc_for( `CHANGE_METHOD_BINDING` ).

    ENDLOOP.

  ENDMETHOD.


  METHOD lif_task_definition~change_terminating_events.

    mo_taskdef->change_term_events_complete(
      EXPORTING
        terminating_events = ms_task-terminating_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_TERM_EVENTS_COMPLETE` ).

    mo_taskdef->change_term_evt_bind_complete(
      EXPORTING
        new_bindings       = ms_task-terminating_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_TERM_EVT_BIND_COMPLETE` ).

  ENDMETHOD.


  METHOD lif_task_definition~change_text.

    mo_taskdef->change_text(
      EXPORTING
        subty              = c_subty_task_description
        new_text           = ms_task-descriptions
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_TEXT` ).

  ENDMETHOD.

ENDCLASS.
