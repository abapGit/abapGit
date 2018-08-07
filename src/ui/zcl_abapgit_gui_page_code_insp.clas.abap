CLASS zcl_abapgit_gui_page_code_insp DEFINITION PUBLIC FINAL CREATE PUBLIC
    INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
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
    DATA: mo_repo TYPE REF TO zcl_abapgit_repo.

    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_actions,
        stage  TYPE string VALUE 'stage' ##NO_TEXT,
        commit TYPE string VALUE 'commit' ##NO_TEXT,
        rerun  TYPE string VALUE 'rerun' ##NO_TEXT,
      END OF c_actions.

    DATA:
      mt_result TYPE scit_alvlist,
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
          VALUE(rv_is_stage_allowed) TYPE abap_bool,
      jump
        IMPORTING
          is_item TYPE zif_abapgit_definitions=>ty_item
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_code_insp IMPLEMENTATION.


  METHOD build_menu.

    DATA: lv_opt TYPE c LENGTH 1.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Re-Run'
                  iv_act = c_actions-rerun
                  iv_cur = abap_false ) ##NO_TEXT.

    IF is_stage_allowed( ) = abap_false.
      lv_opt = zif_abapgit_definitions=>gc_html_opt-crossout.
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


  METHOD jump.

    DATA: lo_test               TYPE REF TO cl_ci_test_root,
          li_code_inspector     TYPE REF TO zif_abapgit_code_inspector,
          ls_info               TYPE scir_rest,
          lo_result             TYPE REF TO cl_ci_result_root,
          lv_check_variant_name TYPE sci_chkv,
          lv_package            TYPE devclass.

    FIELD-SYMBOLS: <ls_result> TYPE scir_alvlist.

    READ TABLE mt_result WITH KEY objtype = is_item-obj_type
                                  objname = is_item-obj_name
                         ASSIGNING <ls_result>.
    ASSERT sy-subrc = 0.

    lv_package = mo_repo->get_package( ).
    lv_check_variant_name = mo_repo->get_local_settings( )-code_inspector_check_variant.

    li_code_inspector = zcl_abapgit_factory=>get_code_inspector(
        iv_package            = lv_package
        iv_check_variant_name = lv_check_variant_name ).

    " see SCI_LCL_DYNP_530 / HANDLE_DOUBLE_CLICK

    MOVE-CORRESPONDING <ls_result> TO ls_info.

    TRY.
        lo_test ?= cl_ci_tests=>get_test_ref( <ls_result>-test ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Jump to object not supported in your NW release|  ).
    ENDTRY.

    lo_result = lo_test->get_result_node( <ls_result>-kind ).

    lo_result->set_info( ls_info ).
    lo_result->if_ci_test~navigate( ).


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

      ro_html->add( '<div>' ).
      ro_html->add_a( iv_txt = |{ <ls_result>-objtype } { <ls_result>-objname }|
                      iv_act = |{ <ls_result>-objtype }{ <ls_result>-objname }|
                      iv_typ = zif_abapgit_definitions=>gc_action_type-sapevent ).
      ro_html->add( '</div>' ).

      CASE <ls_result>-kind.
        WHEN 'E'.
          lv_class = 'error'.
        WHEN 'W'.
          lv_class = 'warning'.
        WHEN OTHERS.
          lv_class = 'grey'.
      ENDCASE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <ls_result>-line
        IMPORTING
          output = lv_line.

      ro_html->add( |<div class="{ lv_class }">Line { lv_line }: { <ls_result>-text }</div><br>| ).
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content


  METHOD run_code_inspector.

    mt_result = mo_repo->run_code_inspector( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
          ls_item        TYPE zif_abapgit_definitions=>ty_item.


    CASE iv_action.
      WHEN c_actions-stage.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.
          " we need to refresh as the source might have changed
          lo_repo_online->refresh( ).

          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_stage
            EXPORTING
              io_repo = lo_repo_online.
          ev_state = zif_abapgit_definitions=>gc_event_state-new_page.

        ELSE.

          ei_page = me.
          ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-commit.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.

          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
            EXPORTING
              io_repo  = lo_repo_online
              io_stage = mo_stage.
          ev_state = zif_abapgit_definitions=>gc_event_state-new_page.

        ELSE.

          ei_page = me.
          ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-rerun.

        run_code_inspector( ).

        ei_page = me.
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.

      WHEN OTHERS.

        ls_item-obj_type = iv_action(4).
        ls_item-obj_name = iv_action+4(*).

        jump( ls_item ).

*        zcl_abapgit_objects=>jump( ls_item ).

        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~render.

    ms_control-page_menu = build_menu( ).
    ro_html = super->zif_abapgit_gui_page~render( ).

  ENDMETHOD.
ENDCLASS.
