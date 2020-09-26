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


INTERFACE lif_task_definition.

  TYPES: BEGIN OF ty_task_data,
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
         END OF ty_task_data.

  METHODS clear_origin_data.
  METHODS get_definition RETURNING VALUE(rs_result) TYPE ty_task_data.
  METHODS get_container RETURNING VALUE(ri_result) TYPE REF TO if_swf_cnt_container.

ENDINTERFACE.


CLASS lcl_task_definition DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.

    INTERFACES lif_task_definition.
    ALIASES: clear_origin_data FOR lif_task_definition~clear_origin_data,
             get_definition FOR lif_task_definition~get_definition.

    CLASS-METHODS create IMPORTING iv_objid         TYPE hrobject-objid
                         RETURNING VALUE(ri_result) TYPE REF TO lif_task_definition
                         RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mo_taskdef TYPE REF TO cl_workflow_task_ts.
    DATA ms_task TYPE lif_task_definition=>ty_task_data.

    DATA: mv_objid                      TYPE hrobjid,
          mv_short_text                 TYPE hr_mcshort,
          mv_plvar                      TYPE plvar,
          mv_wi_text                    TYPE witext,
          ms_method                     TYPE hrs1201,
          mt_method_binding             TYPE hrsmtbind,
          mt_starting_events            TYPE hrsevtab,
          mt_starting_events_binding    TYPE hrsevbind,
          mt_terminating_events         TYPE hrsetmtab,
          mt_terminating_events_binding TYPE hrsevbind,
          mt_descriptions               TYPE wstexts.

    METHODS supply_instance RAISING zcx_abapgit_exception.
    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   zcx_abapgit_exception.

ENDCLASS.


CLASS lcl_task_definition IMPLEMENTATION.

  METHOD create.

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
                   <ls_method_binding>          LIKE LINE OF mt_method_binding,
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

ENDCLASS.
