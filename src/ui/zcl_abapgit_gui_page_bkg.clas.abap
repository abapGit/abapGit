CLASS zcl_abapgit_gui_page_bkg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    METHODS constructor
      IMPORTING
        iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    METHODS read_persist
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rs_persist) TYPE zcl_abapgit_persist_background=>ty_background
      RAISING
        zcx_abapgit_exception .
    METHODS render_methods
      IMPORTING
        !is_per        TYPE zcl_abapgit_persist_background=>ty_background
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_settings
      IMPORTING
        !is_per        TYPE zcl_abapgit_persist_background=>ty_background
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS update
      IMPORTING
        !is_bg_task TYPE zcl_abapgit_persist_background=>ty_background
      RAISING
        zcx_abapgit_exception .
    METHODS render
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS decode
      IMPORTING
        !iv_getdata      TYPE clike
      RETURNING
        VALUE(rs_fields) TYPE zcl_abapgit_persist_background=>ty_background .

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    DATA mv_key TYPE zif_abapgit_persistence=>ty_repo-key .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_BKG IMPLEMENTATION.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Run background logic'
                  iv_act = zif_abapgit_definitions=>c_action-go_background_run ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mv_key = iv_key.
    ms_control-page_title = 'BACKGROUND'.
    ms_control-page_menu  = build_menu( ).

  ENDMETHOD.


  METHOD decode.

    DATA: lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_setting> LIKE LINE OF rs_fields-settings.


    rs_fields-key = mv_key.

    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( iv_getdata ).

    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'METHOD'
        it_field   = lt_fields
      CHANGING
        cg_field   = rs_fields ).
    IF rs_fields-method IS INITIAL.
      RETURN.
    ENDIF.

    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'USERNAME'
        it_field   = lt_fields
      CHANGING
        cg_field   = rs_fields ).

    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'PASSWORD'
        it_field   = lt_fields
      CHANGING
        cg_field   = rs_fields ).


    CALL METHOD (rs_fields-method)=>zif_abapgit_background~get_settings
      CHANGING
        ct_settings = rs_fields-settings.
    LOOP AT rs_fields-settings ASSIGNING <ls_setting>.
      zcl_abapgit_html_action_utils=>get_field(
        EXPORTING
          iv_name = <ls_setting>-key
          it_field   = lt_fields
        CHANGING
          cg_field   = <ls_setting>-value ).
    ENDLOOP.

    ASSERT NOT rs_fields IS INITIAL.

  ENDMETHOD.


  METHOD read_persist.

    DATA: lo_per TYPE REF TO zcl_abapgit_persist_background,
          lt_per TYPE zcl_abapgit_persist_background=>tt_background.


    CREATE OBJECT lo_per.
    lt_per = lo_per->list( ).

    READ TABLE lt_per INTO rs_persist WITH KEY key = io_repo->get_key( ).
    IF sy-subrc <> 0.
      CLEAR rs_persist.
    ENDIF.

  ENDMETHOD.


  METHOD render.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          ls_per  TYPE zcl_abapgit_persist_background=>ty_background.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_key ).
    ls_per = read_persist( lo_repo ).


    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).

    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( lo_repo ) ).
    ro_html->add( '<br>' ).

    ro_html->add( render_methods( ls_per ) ).

    ro_html->add( '<u>HTTP Authentication, optional</u><br>' ) ##NO_TEXT.
    ro_html->add( '(password will be saved in clear text)<br>' ) ##NO_TEXT.
    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Username:</td>' ).
    ro_html->add( '<td><input type="text" name="username" value="' && ls_per-username && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Password:</td>' ).
    ro_html->add( '<td><input type="text" name="password" value="' && ls_per-password && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).

    ro_html->add( render_settings( ls_per ) ).

    ro_html->add( '<br>' ).
    ro_html->add( '<input type="submit" value="Save">' ).

    ro_html->add( '</form>' ).
    ro_html->add( '<br>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( render( ) ).

  ENDMETHOD.


  METHOD render_methods.

    DATA: lt_methods TYPE zcl_abapgit_background=>ty_methods_tt,
          ls_method  LIKE LINE OF lt_methods,
          lv_checked TYPE string.


    CREATE OBJECT ro_html.

    lt_methods = zcl_abapgit_background=>list_methods( ).

    ro_html->add( '<u>Method</u><br>' ) ##NO_TEXT.
    ro_html->add( |<form method="get" action="sapevent:{ zif_abapgit_definitions=>c_action-bg_update }">| ).

    IF is_per-method IS INITIAL.
      lv_checked = ' checked' ##NO_TEXT.
    ENDIF.

    ro_html->add( '<input type="radio" name="method" value=""' && lv_checked && '>Do nothing<br>' ) ##NO_TEXT.

    LOOP AT lt_methods INTO ls_method.
      CLEAR lv_checked.
      IF is_per-method = ls_method-class.
        lv_checked = ' checked' ##NO_TEXT.
      ENDIF.

      ro_html->add( '<input type="radio" name="method" value="' &&
        ls_method-class && '"' &&
        lv_checked && '>' &&
        ls_method-description && '<br>' ) ##NO_TEXT.
    ENDLOOP.

    ro_html->add( '<br>' ).

  ENDMETHOD.


  METHOD render_settings.

    DATA: lt_settings LIKE is_per-settings,
          ls_setting  LIKE LINE OF lt_settings.


    CREATE OBJECT ro_html.

    IF is_per-method IS INITIAL.
      RETURN.
    ENDIF.

    lt_settings = is_per-settings.

* skip invalid values, from old background logic
    IF is_per-method <> 'push' AND is_per-method <> 'pull' AND is_per-method <> 'nothing'.
      CALL METHOD (is_per-method)=>zif_abapgit_background~get_settings
        CHANGING
          ct_settings = lt_settings.
    ENDIF.

    IF lines( lt_settings ) = 0.
      RETURN.
    ENDIF.

    ro_html->add( '<table>' ).
    LOOP AT lt_settings INTO ls_setting.
      ro_html->add( '<tr>' ).
      ro_html->add( '<td>' && ls_setting-key && ':</td>' ).
      ro_html->add( '<td><input type="text" name="' &&
        ls_setting-key && '" value="' &&
        ls_setting-value && '"></td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</table>' ).

  ENDMETHOD.


  METHOD update.

    DATA lo_persistence TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_persistence.

    IF is_bg_task-method IS INITIAL.
      lo_persistence->delete( is_bg_task-key ).
    ELSE.
      lo_persistence->modify( is_bg_task ).
    ENDIF.

    MESSAGE 'Saved' TYPE 'S' ##NO_TEXT.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN zif_abapgit_definitions=>c_action-bg_update.
        update( decode( iv_getdata ) ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
