CLASS zcl_abapgit_services_bkg DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS update_task
      IMPORTING is_bg_task TYPE zcl_abapgit_persist_background=>ty_background
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_SERVICES_BKG IMPLEMENTATION.


  METHOD update_task.

    DATA lo_persistence TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_persistence.

    IF is_bg_task-method = zcl_abapgit_persist_background=>c_method-nothing.
      lo_persistence->delete( is_bg_task-key ).
    ELSE.
      lo_persistence->modify( is_bg_task ).
    ENDIF.

    MESSAGE 'Saved' TYPE 'S' ##NO_TEXT.

    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
