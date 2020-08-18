CLASS lcl_abapgit_object_pdts_helper DEFINITION
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.

    CLASS-METHODS set_objid IMPORTING iv_objid TYPE hrobject-objid
                                      io_task  TYPE REF TO cl_workflow_general_task_def.

    CLASS-METHODS set_container_id IMPORTING iv_id   TYPE guid_32
                                             io_task TYPE REF TO cl_workflow_general_task_def.

ENDCLASS.



CLASS lcl_abapgit_object_pdts_helper IMPLEMENTATION.


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

CLASS lcx_fm DEFINITION INHERITING FROM zcx_abapgit_exception.
  PUBLIC SECTION.
    CLASS-METHODS check_subrc IMPORTING iv_fm TYPE string
                              RAISING   zcx_abapgit_exception.
ENDCLASS.

CLASS lcx_fm IMPLEMENTATION.

  METHOD check_subrc.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Function module ' && iv_fm && ' returned ' && sy-subrc ). "#EC NOTEXT
    ENDIF.
  ENDMETHOD.

ENDCLASS.
