*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SERVICES_BACKGROUND
*&---------------------------------------------------------------------*

CLASS lcl_services_bkg DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS update_task
      IMPORTING is_bg_task TYPE zcl_abapgit_persist_background=>ty_background
      RAISING   zcx_abapgit_exception.

ENDCLASS. "lcl_services_background

CLASS lcl_services_bkg IMPLEMENTATION.

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

  ENDMETHOD.  "update_task

ENDCLASS. "lcl_services_background
