*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_APP_IMPL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    IF sy-batch = abap_true.
      lcl_background=>run( ).
    ELSE.
      gui( )->go_home( ).
      CALL SELECTION-SCREEN 1001. " trigger screen
    ENDIF.

  ENDMETHOD.      "run

  METHOD gui.

    IF go_gui IS NOT BOUND.
      CREATE OBJECT go_gui.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.      "gui

  METHOD user.

    IF iv_user = sy-uname ##USER_OK.
      IF go_current_user IS NOT BOUND.
        CREATE OBJECT go_current_user.
      ENDIF.
      ro_user = go_current_user.
    ELSE.
      CREATE OBJECT ro_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.      "user

  METHOD repo_srv.

    IF go_repo_srv IS NOT BOUND.
      CREATE OBJECT go_repo_srv.
    ENDIF.
    ro_repo_srv = go_repo_srv.

  ENDMETHOD.      "repo_srv

  METHOD db.

    IF go_db IS NOT BOUND.
      CREATE OBJECT go_db.
    ENDIF.
    ro_db = go_db.

  ENDMETHOD.      "repo_srv

  METHOD settings.
    IF go_settings IS NOT BOUND.
      CREATE OBJECT go_settings.
    ENDIF.
    ro_settings = go_settings.
  ENDMETHOD.

ENDCLASS.   "lcl_app
