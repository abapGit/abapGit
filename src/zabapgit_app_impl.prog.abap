*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_APP_IMPL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD gui.

    IF go_gui IS NOT BOUND.
      CREATE OBJECT go_gui.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.      "gui

ENDCLASS.   "lcl_app
