CLASS zcl_abapgit_gui_page_code_insp DEFINITION PUBLIC FINAL CREATE PUBLIC
    INHERITING FROM zcl_abapgit_gui_page_codi_base.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_hotkeys.

    METHODS:
      constructor
        IMPORTING
          io_repo  TYPE REF TO zcl_abapgit_repo
          io_stage TYPE REF TO zcl_abapgit_stage OPTIONAL
        RAISING
          zcx_abapgit_exception,

      zif_abapgit_gui_event_handler~on_event
        REDEFINITION,

      zif_abapgit_gui_renderable~render
        REDEFINITION.

  PROTECTED SECTION.

    METHODS:
      render_content   REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mo_stage         TYPE REF TO zcl_abapgit_stage,
      mv_check_variant TYPE sci_chkv.

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
          VALUE(rv_is_stage_allowed) TYPE abap_bool,

      ask_user_for_check_variant
        RETURNING
          VALUE(rv_check_variant) TYPE sci_chkv
        RAISING
          zcx_abapgit_exception,

      determine_check_variant
        RAISING
          zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_CODE_INSP IMPLEMENTATION.


  METHOD ask_user_for_check_variant.

    DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

    FIELD-SYMBOLS: <ls_return> LIKE LINE OF lt_return.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'SCI_DYNP'
        fieldname         = 'CHKV'
      TABLES
        return_tab        = lt_return
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_return ASSIGNING <ls_return>
                         WITH KEY retfield = 'SCI_DYNP-CHKV'.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Please select a check variant.| ).
    ENDIF.

    rv_check_variant = <ls_return>-fieldval.

  ENDMETHOD.


  METHOD build_menu.

    DATA: lv_opt TYPE c LENGTH 1.

    ro_menu = build_base_menu( ).

    IF is_stage_allowed( ) = abap_false.
      lv_opt = zif_abapgit_html=>c_html_opt-crossout.
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
                    iv_opt = lv_opt ).

    ELSE.

      ro_menu->add( iv_txt = 'Stage'
                    iv_act = c_actions-stage
                    iv_cur = abap_false
                    iv_opt = lv_opt ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
    mo_stage = io_stage.
    ms_control-page_title = 'Code Inspector'.
    determine_check_variant( ).
    run_code_inspector( ).
  ENDMETHOD.


  METHOD determine_check_variant.

    mv_check_variant = mo_repo->get_local_settings( )-code_inspector_check_variant.

    IF mv_check_variant IS INITIAL.
      mv_check_variant = ask_user_for_check_variant( ).
    ENDIF.

  ENDMETHOD.


  METHOD has_inspection_errors.

    READ TABLE mt_result TRANSPORTING NO FIELDS
                         WITH KEY kind = 'E'.
    rv_has_inspection_errors = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_stage_allowed.

    rv_is_stage_allowed = boolc( NOT ( mo_repo->get_local_settings( )-block_commit = abap_true
                                           AND has_inspection_errors( ) = abap_true ) ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( io_repo        = mo_repo
                                                              iv_show_commit = abap_false ) ).
    ri_html->add( `</div>` ).

    IF mv_check_variant IS INITIAL.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error( iv_error = 'No check variant supplied.' ) ).
      RETURN.
    ENDIF.

    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    ri_html->add( render_variant( mv_check_variant ) ).

    IF lines( mt_result ) = 0.
      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No code inspector findings' ).
      ri_html->add( '</div>' ).
    ELSE.
      render_result(
        ii_html   = ri_html
        it_result = mt_result ).
    ENDIF.

  ENDMETHOD.


  METHOD run_code_inspector.

    DATA: li_code_inspector TYPE REF TO zif_abapgit_code_inspector.

    li_code_inspector = zcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) ).

    mt_result = li_code_inspector->run(
      iv_variant = |{ mv_check_variant }|
      iv_save    = abap_true ).

    DELETE mt_result WHERE kind = 'N'.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online.

    CASE ii_event->mv_action.
      WHEN c_actions-stage.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.
          " we need to refresh as the source might have changed
          lo_repo_online->refresh( ).

          CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_stage
            EXPORTING
              io_repo = lo_repo_online.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

        ELSE.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-commit.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.

          CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_commit
            EXPORTING
              io_repo  = lo_repo_online
              io_stage = mo_stage.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

        ELSE.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-rerun.

        run_code_inspector( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Code inspector'.

    ls_hotkey_action-description = |Stage|.
    ls_hotkey_action-action = c_actions-stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Re-Run|.
    ls_hotkey_action-action = c_actions-rerun.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    ms_control-page_menu = build_menu( ).
    ri_html = super->zif_abapgit_gui_renderable~render( ).

  ENDMETHOD.
ENDCLASS.
