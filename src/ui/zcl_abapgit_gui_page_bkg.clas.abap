CLASS zcl_abapgit_gui_page_bkg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key .

    METHODS zif_abapgit_gui_page~on_event
        REDEFINITION .
  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA mv_key TYPE zif_abapgit_persistence=>ty_repo-key .

    METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS update_task
      IMPORTING
        !is_bg_task TYPE zcl_abapgit_persist_background=>ty_background
      RAISING
        zcx_abapgit_exception .
    METHODS render_data
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_BKG IMPLEMENTATION.


  METHOD build_menu.
    CREATE OBJECT ro_menu.
    ro_menu->add( iv_txt = 'Run background logic'
                  iv_act = zif_abapgit_definitions=>gc_action-go_background_run ) ##NO_TEXT.
  ENDMETHOD. "build_menu


  METHOD constructor.

    super->constructor( ).

    mv_key = iv_key.
    ms_control-page_title = 'BACKGROUND'.
    ms_control-page_menu  = build_menu( ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( render_data( ) ).

  ENDMETHOD.  "render_content


  METHOD render_data.

    DATA: lo_repo    TYPE REF TO zcl_abapgit_repo_online,
          lo_per     TYPE REF TO zcl_abapgit_persist_background,
          lt_per     TYPE zcl_abapgit_persist_background=>tt_background,
          ls_per     LIKE LINE OF lt_per,
          lv_nothing TYPE string,
          lv_push    TYPE string,
          lv_pull    TYPE string,
          lv_afixed  TYPE string,
          lv_aauto   TYPE string,
          lv_auser   TYPE string.


    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).

    CREATE OBJECT lo_per.
    lt_per = lo_per->list( ).

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_key ).

    READ TABLE lt_per INTO ls_per WITH KEY key = lo_repo->get_key( ).
    IF sy-subrc <> 0.
      CLEAR ls_per.
    ENDIF.

    IF ls_per-aname IS INITIAL.
      ls_per-aname = 'foobar' ##NO_TEXT.
    ENDIF.
    IF ls_per-amail IS INITIAL.
      ls_per-amail = 'foo@bar.com' ##NO_TEXT.
    ENDIF.

    CASE ls_per-method.
      WHEN zcl_abapgit_persist_background=>c_method-push.
        lv_push = ' checked' ##NO_TEXT.
      WHEN zcl_abapgit_persist_background=>c_method-pull.
        lv_pull = ' checked' ##NO_TEXT.
      WHEN OTHERS.
        lv_nothing = ' checked' ##NO_TEXT.
    ENDCASE.

    CASE ls_per-amethod.
      WHEN zcl_abapgit_persist_background=>c_amethod-user.
        lv_auser = ' checked' ##NO_TEXT.
      WHEN zcl_abapgit_persist_background=>c_amethod-auto.
        lv_aauto = ' checked' ##NO_TEXT.
      WHEN OTHERS.
        lv_afixed = ' checked' ##NO_TEXT.
    ENDCASE.

    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( lo_repo ) ).
    ro_html->add( '<br>' ).

    ro_html->add( '<u>Method</u><br>' ) ##NO_TEXT.
    ro_html->add( |<form method="get" action="sapevent:{ zif_abapgit_definitions=>gc_action-bg_update }">| ).
    ro_html->add( '<input type="radio" name="method" value="nothing"' &&
      lv_nothing && '>Do nothing<br>' ) ##NO_TEXT.
    ro_html->add( '<input type="radio" name="method" value="push"' &&
      lv_push && '>Automatic push<br>' ) ##NO_TEXT.
    ro_html->add( '<input type="radio" name="method" value="pull"' &&
      lv_pull && '>Automatic pull<br>' ) ##NO_TEXT.
    ro_html->add( '<br>' ).

    ro_html->add( '<u>HTTP Authentication, optional</u><br>' ) ##NO_TEXT.
    ro_html->add( '(password will be saved in clear text)<br>' ) ##NO_TEXT.
    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Username:</td>' ).
    ro_html->add( '<td><input type="text" name="username" value="' &&
      ls_per-username && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Password:</td>' ).
    ro_html->add( '<td><input type="text" name="password" value="' &&
      ls_per-password && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).

    ro_html->add( '<u>Commit author</u><br>' ).
    ro_html->add( '<input type="radio" name="amethod" value="fixed"' &&
      lv_afixed && '>Fixed<br>' ) ##NO_TEXT.
    ro_html->add( '<input type="radio" name="amethod" value="auto"' &&
      lv_aauto && '>Automatic<br>' ) ##NO_TEXT.
    ro_html->add( '<input type="radio" name="amethod" value="user"' &&
      lv_auser && '>Automatic using SU01 user details<br>' ) ##NO_TEXT.
    ro_html->add( '<br>' ).

    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Name:</td>' ).
    ro_html->add( '<td><input type="text" name="aname" value="' &&
      ls_per-aname && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Email:</td>' ).
    ro_html->add( '<td><input type="text" name="amail" value="' &&
      ls_per-amail && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).
    ro_html->add( '<input type="submit" value="Save">' ).

    ro_html->add( '</form>' ).
    ro_html->add( '<br>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.


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


  METHOD zif_abapgit_gui_page~on_event.

    DATA ls_bg_task TYPE zcl_abapgit_persist_background=>ty_background.

    CASE iv_action.
      WHEN zif_abapgit_definitions=>gc_action-bg_update.
        ls_bg_task     = zcl_abapgit_html_action_utils=>decode_bg_update( iv_getdata ).
        ls_bg_task-key = mv_key.
        update_task( ls_bg_task ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
