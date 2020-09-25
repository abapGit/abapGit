CLASS lcl_task_definition DEFINITION
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING iv_objid TYPE hrobject-objid.

    CLASS-METHODS set_objid IMPORTING iv_objid TYPE hrobject-objid
                                      io_task  TYPE REF TO cl_workflow_general_task_def.

    CLASS-METHODS set_container_id IMPORTING iv_id   TYPE guid_32
                                             io_task TYPE REF TO cl_workflow_general_task_def. "#EC NEEDED

  PRIVATE SECTION.

ENDCLASS.



CLASS lcl_task_definition IMPLEMENTATION.


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

  METHOD create.

  ENDMETHOD.

ENDCLASS.
