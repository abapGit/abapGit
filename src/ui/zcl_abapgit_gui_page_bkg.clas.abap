CLASS zcl_abapgit_gui_page_bkg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
                iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception.

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
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_settings
      IMPORTING
        !is_per        TYPE zcl_abapgit_persist_background=>ty_background
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
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
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
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
                  iv_act = zif_abapgit_definitions=>c_action-go_background_run ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mv_key = iv_key.
    ms_control-page_title = 'Background'.
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

    DATA lo_per TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_per.

    TRY.
        rs_persist = lo_per->get_by_key( io_repo->get_key( ) ).
      CATCH zcx_abapgit_not_found.
        CLEAR rs_persist.
    ENDTRY.

  ENDMETHOD.


  METHOD render.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          ls_per  TYPE zcl_abapgit_persist_background=>ty_background.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_key ).
    ls_per = read_persist( lo_repo ).


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="toc" class="settings_container">' ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( lo_repo ) ).
    ri_html->add( '<br>' ).

    ri_html->add( render_methods( ls_per ) ).

    ri_html->add( '<u>HTTP Authentication, optional</u><br>' ).
    ri_html->add( '(password will be saved in clear text)<br>' ).
    ri_html->add( '<table>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Username:</td>' ).
    ri_html->add( '<td><input type="text" name="username" value="' && ls_per-username && '"></td>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Password:</td>' ).
    ri_html->add( '<td><input type="text" name="password" value="' && ls_per-password && '"></td>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</table>' ).

    ri_html->add( '<br>' ).

    ri_html->add( render_settings( ls_per ) ).

    ri_html->add( '<br>' ).
    ri_html->add( '<input type="submit" value="Save">' ).

    ri_html->add( '</form>' ).
    ri_html->add( '<br>' ).

    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( render( ) ).

  ENDMETHOD.


  METHOD render_methods.

    DATA: lt_methods TYPE zcl_abapgit_background=>ty_methods,
          ls_method  LIKE LINE OF lt_methods,
          lv_checked TYPE string.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_methods = zcl_abapgit_background=>list_methods( ).

    ri_html->add( '<u>Method</u><br>' ).
    ri_html->add( |<form method="get" action="sapevent:{ zif_abapgit_definitions=>c_action-bg_update }">| ).

    IF is_per-method IS INITIAL.
      lv_checked = ' checked'.
    ENDIF.

    ri_html->add( '<input type="radio" name="method" value=""' && lv_checked && '>Do nothing<br>' ).

    LOOP AT lt_methods INTO ls_method.
      CLEAR lv_checked.
      IF is_per-method = ls_method-class.
        lv_checked = ' checked'.
      ENDIF.

      ri_html->add( '<input type="radio" name="method" value="' &&
        ls_method-class && '"' &&
        lv_checked && '>' &&
        ls_method-description && '<br>' ).
    ENDLOOP.

    ri_html->add( '<br>' ).

  ENDMETHOD.


  METHOD render_settings.

    DATA: lt_settings LIKE is_per-settings,
          ls_setting  LIKE LINE OF lt_settings.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

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

    ri_html->add( '<table>' ).
    LOOP AT lt_settings INTO ls_setting.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td>' && ls_setting-key && ':</td>' ).
      ri_html->add( '<td><input type="text" name="' &&
        ls_setting-key && '" value="' &&
        ls_setting-value && '"></td>' ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</table>' ).

  ENDMETHOD.


  METHOD update.

    DATA lo_persistence TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_persistence.

    IF is_bg_task-method IS INITIAL.
      lo_persistence->delete( is_bg_task-key ).
    ELSE.
      lo_persistence->modify( is_bg_task ).
    ENDIF.

    MESSAGE 'Saved' TYPE 'S'.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA ls_fields TYPE zcl_abapgit_persist_background=>ty_background.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-bg_update.
        ls_fields = decode( ii_event->mv_getdata ).
        update( ls_fields ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
