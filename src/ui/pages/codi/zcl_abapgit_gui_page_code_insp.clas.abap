CLASS zcl_abapgit_gui_page_code_insp DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page_codi_base
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_page_title,
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        ii_repo                  TYPE REF TO zif_abapgit_repo
        io_stage                 TYPE REF TO zcl_abapgit_stage OPTIONAL
        iv_check_variant         TYPE sci_chkv OPTIONAL
        iv_raise_when_no_results TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_page)           TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        ii_repo                  TYPE REF TO zif_abapgit_repo
        io_stage                 TYPE REF TO zcl_abapgit_stage OPTIONAL
        iv_check_variant         TYPE sci_chkv OPTIONAL
        iv_raise_when_no_results TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_stage         TYPE REF TO zcl_abapgit_stage.
    DATA mv_check_variant TYPE sci_chkv.

    METHODS:
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

    METHODS status
      RETURNING
        VALUE(rv_status) TYPE zif_abapgit_definitions=>ty_sci_result.

ENDCLASS.



CLASS zcl_abapgit_gui_page_code_insp IMPLEMENTATION.


  METHOD ask_user_for_check_variant.

    rv_check_variant = zcl_abapgit_ui_factory=>get_popups( )->choose_code_insp_check_variant( ).

    IF rv_check_variant IS INITIAL.
      zcx_abapgit_exception=>raise( |Please select a check variant.| ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mi_repo = ii_repo.
    mo_stage = io_stage.
    mv_check_variant = iv_check_variant.
    determine_check_variant( ).
    run_code_inspector( ).

    IF mt_result IS INITIAL AND iv_raise_when_no_results = abap_true.
      zcx_abapgit_exception=>raise( 'No results' ).
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_code_insp.

    CREATE OBJECT lo_component
      EXPORTING
        ii_repo                  = ii_repo
        io_stage                 = io_stage
        iv_check_variant         = iv_check_variant
        iv_raise_when_no_results = iv_raise_when_no_results.

    ri_page = zcl_abapgit_gui_page_hoc=>create( lo_component ).

  ENDMETHOD.


  METHOD determine_check_variant.

    IF mv_check_variant IS NOT INITIAL.
      RETURN.
    ENDIF.

    mv_check_variant = mi_repo->get_local_settings( )-code_inspector_check_variant.

    IF mv_check_variant IS INITIAL.
      mv_check_variant = ask_user_for_check_variant( ).
    ENDIF.

  ENDMETHOD.


  METHOD has_inspection_errors.

    READ TABLE mt_result TRANSPORTING NO FIELDS WITH KEY kind = 'E'.
    rv_has_inspection_errors = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_stage_allowed.

    rv_is_stage_allowed = boolc( NOT (
      mi_repo->get_local_settings( )-block_commit = abap_true AND has_inspection_errors( ) = abap_true ) ).

  ENDMETHOD.


  METHOD run_code_inspector.

    DATA li_code_inspector TYPE REF TO zif_abapgit_code_inspector.

    li_code_inspector = zcl_abapgit_code_inspector=>get_code_inspector( mi_repo->get_package( ) ).

    mt_result = li_code_inspector->run(
      iv_variant = |{ mv_check_variant }|
      iv_save    = abap_true ).

    mv_summary = li_code_inspector->get_summary( ).

    DELETE mt_result WHERE kind = 'N'.

  ENDMETHOD.


  METHOD status.

    READ TABLE mt_result TRANSPORTING NO FIELDS WITH KEY kind = 'E'.
    IF sy-subrc = 0.
      rv_status = zif_abapgit_definitions=>c_sci_result-failed.
    ELSE.
      READ TABLE mt_result TRANSPORTING NO FIELDS WITH KEY kind = 'W'.
      IF sy-subrc = 0.
        rv_status = zif_abapgit_definitions=>c_sci_result-warning.
      ELSE.
        rv_status = zif_abapgit_definitions=>c_sci_result-passed.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA li_repo_online TYPE REF TO zif_abapgit_repo_online.

    CASE ii_event->mv_action.
      WHEN c_actions-stage.

        li_repo_online ?= mi_repo.

        IF is_stage_allowed( ) = abap_true.

          rs_handled-page   = zcl_abapgit_gui_page_stage=>create(
            ii_repo_online = li_repo_online
            iv_sci_result  = status( ) ).

          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

        ELSE.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-commit.

        li_repo_online ?= mi_repo.

        IF is_stage_allowed( ) = abap_true.

          rs_handled-page = zcl_abapgit_gui_page_commit=>create(
            ii_repo_online = li_repo_online
            io_stage       = mo_stage ).

          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

        ELSE.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-rerun.

        run_code_inspector( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        rs_handled = on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Code Inspector'.

    ls_hotkey_action-description = |Stage|.
    ls_hotkey_action-action = c_actions-stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Re-Run|.
    ls_hotkey_action-action = c_actions-rerun.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA: lv_opt TYPE c LENGTH 1.

    ro_toolbar = build_base_menu( ).

    IF is_stage_allowed( ) = abap_false.
      lv_opt = zif_abapgit_html=>c_html_opt-crossout.
    ENDIF.

    IF mi_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    IF mo_stage IS BOUND.

      " Staging info already available, we can directly
      " offer to commit

      ro_toolbar->add(
        iv_txt = 'Commit'
        iv_act = c_actions-commit
        iv_opt = lv_opt ).

    ELSE.

      ro_toolbar->add(
        iv_txt = 'Stage'
        iv_act = c_actions-stage
        iv_opt = lv_opt ).

    ENDIF.

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = 'Code Inspector'.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->div(
      iv_class = 'repo'
      ii_content = zcl_abapgit_gui_chunk_lib=>render_repo_top(
        ii_repo        = mi_repo
        iv_show_commit = abap_false ) ).

    IF mv_check_variant IS INITIAL.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error( iv_error = 'No check variant supplied.' ) ).
      RETURN.
    ENDIF.

    render_ci_report(
      ii_html    = ri_html
      iv_variant = mv_check_variant
      iv_success_msg = 'No code inspector findings' ).

  ENDMETHOD.
ENDCLASS.
