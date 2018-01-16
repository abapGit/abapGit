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

  METHOD repo_srv.

    IF go_repo_srv IS NOT BOUND.
      CREATE OBJECT go_repo_srv.
    ENDIF.
    ro_repo_srv = go_repo_srv.

  ENDMETHOD.      "repo_srv

  METHOD proxy.
    IF go_proxy IS NOT BOUND.
      CREATE OBJECT go_proxy.
    ENDIF.
    ro_proxy = go_proxy.
  ENDMETHOD.

ENDCLASS.   "lcl_app
