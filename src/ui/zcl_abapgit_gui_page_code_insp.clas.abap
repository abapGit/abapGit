CLASS zcl_abapgit_gui_page_code_insp DEFINITION PUBLIC FINAL CREATE PUBLIC
    INHERITING FROM zcl_abapgit_gui_page_codi_base.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    METHODS:
      constructor
        IMPORTING
          io_repo  TYPE REF TO zcl_abapgit_repo
          io_stage TYPE REF TO zcl_abapgit_stage OPTIONAL
        RAISING
          zcx_abapgit_exception,

      zif_abapgit_gui_page~on_event
        REDEFINITION,

      zif_abapgit_gui_page~render
        REDEFINITION.

  PROTECTED SECTION.

    METHODS:
      render_content   REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_actions,
        stage  TYPE string VALUE 'stage' ##NO_TEXT,
        commit TYPE string VALUE 'commit' ##NO_TEXT,
        rerun  TYPE string VALUE 'rerun' ##NO_TEXT,
      END OF c_actions.
    DATA:
      mo_stage  TYPE REF TO zcl_abapgit_stage.

    METHODS:
      build_menu
        RETURNING
          VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar
        RAISING
          zcx_abapgit_exception,

      run_code_inspector
        RAISING
          zcx_abapgit_exception,

      has_inspection_errors
        RETURNING
          VALUE(rv_has_inspection_errors) TYPE abap_bool,

      is_stage_allowed
        RETURNING
          VALUE(rv_is_stage_allowed) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_gui_page_code_insp IMPLEMENTATION.


  METHOD build_menu.

    DATA: lv_opt TYPE c LENGTH 1.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Re-Run'
                  iv_act = c_actions-rerun
                  iv_cur = abap_false ) ##NO_TEXT.

    IF is_stage_allowed( ) = abap_false.
      lv_opt = zif_abapgit_definitions=>c_html_opt-crossout.
    ENDIF.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    IF mo_stage IS BOUND.

      " Staging info already available, we can directly
      " offer to commit

      ro_menu->add( iv_txt = 'Commit'
                    iv_act = c_actions-commit
                    iv_cur = abap_false
                    iv_opt = lv_opt ) ##NO_TEXT.

    ELSE.

      ro_menu->add( iv_txt = 'Stage'
                    iv_act = c_actions-stage
                    iv_cur = abap_false
                    iv_opt = lv_opt ) ##NO_TEXT.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mo_repo ?= io_repo.
    mo_stage = io_stage.
    ms_control-page_title = 'Code Inspector'.
    run_code_inspector( ).
  ENDMETHOD.  " constructor.


  METHOD has_inspection_errors.

    READ TABLE mt_result TRANSPORTING NO FIELDS
                         WITH KEY kind = 'E'.
    rv_has_inspection_errors = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_stage_allowed.

    rv_is_stage_allowed =  boolc( NOT ( mo_repo->get_local_settings( )-block_commit = abap_true
                                           AND has_inspection_errors( ) = abap_true ) ).

  ENDMETHOD.

  METHOD render_content.

    DATA: lv_check_variant TYPE sci_chkv,
          lv_class         TYPE string,
          lv_line          TYPE string.
    FIELD-SYMBOLS: <ls_result> TYPE scir_alvlist.

    CREATE OBJECT ro_html.

    lv_check_variant = mo_repo->get_local_settings( )-code_inspector_check_variant.

    IF lv_check_variant IS INITIAL.
      ro_html->add( |No check variant maintained in repo settings.| ).
      RETURN.
    ENDIF.

    ro_html->add( '<div class="toc"><br/>' ).

    ro_html->add( |Code inspector check variant: {
                    mo_repo->get_local_settings( )-code_inspector_check_variant
                  }<br/>| ).

    IF lines( mt_result ) = 0.
      ro_html->add( '<br/><div class="success">No code inspector findings</div>' ).
    ENDIF.

    ro_html->add( |<br/>| ).

    LOOP AT mt_result ASSIGNING <ls_result>.
      render_result( ro_html = ro_html iv_result = <ls_result> ).
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

  METHOD run_code_inspector.

    mt_result = mo_repo->run_code_inspector( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-name           = |Code Inspector: Stage|.
    ls_hotkey_action-action         = c_actions-stage.
    ls_hotkey_action-default_hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-name           = |Code Inspector: Re-Run|.
    ls_hotkey_action-action         = c_actions-rerun.
    ls_hotkey_action-default_hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.
    DATA: lo_repo_online   TYPE REF TO zcl_abapgit_repo_online,
          ls_item          TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item      TYPE zif_abapgit_definitions=>ty_item.
    DATA: lv_main_object   TYPE string.
    DATA: lv_sub_object    TYPE string.
    DATA: lv_line_number_s TYPE string.
    DATA: lv_line_number   TYPE i.

    CASE iv_action.
      WHEN c_actions-stage.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.
          " we need to refresh as the source might have changed
          lo_repo_online->refresh( ).

          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_stage
            EXPORTING
              io_repo = lo_repo_online.
          ev_state = zif_abapgit_definitions=>c_event_state-new_page.

        ELSE.

          ei_page = me.
          ev_state = zif_abapgit_definitions=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-commit.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.

          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
            EXPORTING
              io_repo  = lo_repo_online
              io_stage = mo_stage.
          ev_state = zif_abapgit_definitions=>c_event_state-new_page.

        ELSE.

          ei_page = me.
          ev_state = zif_abapgit_definitions=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-rerun.

        run_code_inspector( ).

        ei_page = me.
        ev_state = zif_abapgit_definitions=>c_event_state-re_render.
      WHEN OTHERS.
        super->zif_abapgit_gui_page~on_event(
          EXPORTING
            iv_action             = iv_action
            iv_prev_page          = iv_prev_page
            iv_getdata            = iv_getdata
            it_postdata           = it_postdata
          IMPORTING
            ei_page               = ei_page
            ev_state              = ev_state ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~render.

    ms_control-page_menu = build_menu( ).
    ro_html = super->zif_abapgit_gui_page~render( ).

  ENDMETHOD.
ENDCLASS.
