CLASS zcl_abapgit_gui_page_patch DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page_diff
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_hotkeys.

    METHODS:
      constructor
        IMPORTING
          iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
          is_file       TYPE zif_abapgit_definitions=>ty_file OPTIONAL
          is_object     TYPE zif_abapgit_definitions=>ty_item OPTIONAL
          iv_patch_mode TYPE abap_bool OPTIONAL
        RAISING
          zcx_abapgit_exception,

      zif_abapgit_gui_event_handler~on_event REDEFINITION.

    CLASS-METHODS:
      get_patch_data
        IMPORTING
          iv_patch      TYPE string
        EXPORTING
          ev_filename   TYPE string
          ev_line_index TYPE string
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      add_menu_end REDEFINITION,
      add_menu_begin REDEFINITION,
      render_table_head_non_unified REDEFINITION,
      render_beacon_begin_of_row REDEFINITION,
      render_diff_head_after_state REDEFINITION,
      insert_nav REDEFINITION,
      render_line_split_row REDEFINITION.


  PRIVATE SECTION.
    TYPES:
      ty_patch_action TYPE string .

    CONSTANTS:
      BEGIN OF c_actions,
        stage                TYPE string VALUE 'patch_stage',
        refresh              TYPE string VALUE 'patch_refresh',
        refresh_local        TYPE string VALUE 'patch_refresh_local',
        refresh_local_object TYPE string VALUE 'patch_refresh_local_object',
      END OF c_actions,
      BEGIN OF c_patch_action,
        add    TYPE ty_patch_action VALUE 'add',
        remove TYPE ty_patch_action VALUE 'remove',
      END OF c_patch_action .

    DATA:
      mo_stage         TYPE REF TO zcl_abapgit_stage,
      mv_section_count TYPE i,
      mv_pushed        TYPE abap_bool,
      mo_repo_online   TYPE REF TO zcl_abapgit_repo_online.

    METHODS:
      render_patch
        IMPORTING
          io_html      TYPE REF TO zcl_abapgit_html
          iv_filename  TYPE string
          is_diff_line TYPE zif_abapgit_definitions=>ty_diff
          iv_fstate    TYPE char1
          iv_index     TYPE sy-tabix
        RAISING
          zcx_abapgit_exception,

      render_patch_head
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html
          is_diff TYPE ty_file_diff,

      start_staging
        IMPORTING
          it_postdata TYPE cnht_post_data_tab
        RAISING
          zcx_abapgit_exception,

      apply_patch_from_form_fields
        IMPORTING
          it_postdata TYPE cnht_post_data_tab
        RAISING
          zcx_abapgit_exception,

      restore_patch_flags
        IMPORTING
          it_diff_files_old TYPE tt_file_diff
        RAISING
          zcx_abapgit_exception,

      add_to_stage
        RAISING
          zcx_abapgit_exception,

      refresh
        IMPORTING
          iv_action TYPE clike
        RAISING
          zcx_abapgit_exception,

      refresh_full
        RAISING
          zcx_abapgit_exception,

      refresh_local
        RAISING
          zcx_abapgit_exception,

      refresh_local_object
        IMPORTING
          iv_action TYPE clike
        RAISING
          zcx_abapgit_exception,

      apply_patch_all
        IMPORTING
          iv_patch      TYPE string
          iv_patch_flag TYPE abap_bool
        RAISING
          zcx_abapgit_exception,

      are_all_lines_patched
        IMPORTING
          it_diff                         TYPE zif_abapgit_definitions=>ty_diffs_tt
        RETURNING
          VALUE(rv_are_all_lines_patched) TYPE abap_bool,

      apply_patch_for
        IMPORTING
          iv_filename   TYPE string
          iv_line_index TYPE string
          iv_patch_flag TYPE abap_bool
        RAISING
          zcx_abapgit_exception,

      get_diff_object
        IMPORTING
          iv_filename    TYPE string
        RETURNING
          VALUE(ro_diff) TYPE REF TO zcl_abapgit_diff
        RAISING
          zcx_abapgit_exception,

      get_diff_line
        IMPORTING
          io_diff        TYPE REF TO zcl_abapgit_diff
          iv_line_index  TYPE string
        RETURNING
          VALUE(rs_diff) TYPE zif_abapgit_definitions=>ty_diff
        RAISING
          zcx_abapgit_exception,

      is_every_changed_line_patched
        RETURNING
          VALUE(rv_everything_patched) TYPE abap_bool.

    CLASS-METHODS:
      is_patch_line_possible
        IMPORTING
          is_diff_line                     TYPE zif_abapgit_definitions=>ty_diff
          iv_fstate                        TYPE char1
        RETURNING
          VALUE(rv_is_patch_line_possible) TYPE abap_bool.

    METHODS render_scripts
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_PATCH IMPLEMENTATION.


  METHOD add_menu_begin.

    io_menu->add(
        iv_txt   = |Refresh local|
        iv_typ   = zif_abapgit_html=>c_action_type-dummy
        iv_act   = c_actions-refresh_local
        iv_id    = c_actions-refresh_local
        iv_title = |Refresh all local objects, without refreshing the remote| ).

    io_menu->add(
        iv_txt   = |Refresh|
        iv_typ   = zif_abapgit_html=>c_action_type-dummy
        iv_act   = c_actions-refresh
        iv_id    = c_actions-refresh
        iv_title = |Complete refresh of all objects, local and remote| ).

  ENDMETHOD.


  METHOD add_menu_end.

    io_menu->add( iv_txt = 'Stage'
                  iv_act = c_actions-stage
                  iv_id  = 'stage'
                  iv_typ = zif_abapgit_html=>c_action_type-dummy
                   ) ##NO_TEXT.

  ENDMETHOD.


  METHOD add_to_stage.

    DATA: lt_diff              TYPE zif_abapgit_definitions=>ty_diffs_tt,
          lv_something_patched TYPE abap_bool,
          ls_status            TYPE zif_abapgit_definitions=>ty_result,
          lv_patch             TYPE xstring,
          lo_git_add_patch     TYPE REF TO zcl_abapgit_git_add_patch.

    FIELD-SYMBOLS: <ls_diff_file> TYPE ty_file_diff.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.

      IF <ls_diff_file>-o_diff IS NOT BOUND.
        " When we deal with binary files we don't have a diff object.
        " There's nothing to do because they cannot be patched
        CONTINUE.
      ENDIF.

      lt_diff = <ls_diff_file>-o_diff->get( ).

      READ TABLE lt_diff TRANSPORTING NO FIELDS
                         WITH KEY patch_flag = abap_true.
      CHECK sy-subrc = 0.

      lv_something_patched = abap_true.

      CREATE OBJECT lo_git_add_patch
        EXPORTING
          it_diff = <ls_diff_file>-o_diff->get( ).

      lv_patch = lo_git_add_patch->get_patch_binary( ).

      IF <ls_diff_file>-lstate = 'D' AND are_all_lines_patched( lt_diff ) = abap_true.

        ls_status-lstate = zif_abapgit_definitions=>c_state-deleted.
        mo_stage->rm(
          iv_path     = <ls_diff_file>-path
          is_status   = ls_status
          iv_filename = <ls_diff_file>-filename ).

      ELSE.

        IF <ls_diff_file>-lstate = 'A' AND are_all_lines_patched( lt_diff ) = abap_true.
          ls_status-lstate = zif_abapgit_definitions=>c_state-added.
        ELSE.
          ls_status-lstate = zif_abapgit_definitions=>c_state-modified.
        ENDIF.

        mo_stage->add(
          iv_path     = <ls_diff_file>-path
          iv_filename = <ls_diff_file>-filename
          is_status   = ls_status
          iv_data     = lv_patch ).

      ENDIF.

    ENDLOOP.

    IF lv_something_patched = abap_false.
      zcx_abapgit_exception=>raise( |Nothing added| ).
    ENDIF.

  ENDMETHOD.


  METHOD apply_patch_all.

    DATA: lv_filename   TYPE string,
          lt_patch      TYPE string_table,
          lv_line_index TYPE string.

    FIELD-SYMBOLS: <lv_patch>     TYPE LINE OF string_table.

    SPLIT iv_patch AT ',' INTO TABLE lt_patch.

    LOOP AT lt_patch ASSIGNING <lv_patch>.

      get_patch_data(
        EXPORTING
          iv_patch      = <lv_patch>
        IMPORTING
          ev_filename   = lv_filename
          ev_line_index = lv_line_index ).

      apply_patch_for( iv_filename   = lv_filename
                       iv_line_index = lv_line_index
                       iv_patch_flag = iv_patch_flag ).

    ENDLOOP.

  ENDMETHOD.


  METHOD apply_patch_for.

    DATA: lo_diff      TYPE REF TO zcl_abapgit_diff,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff,
          lv_line      TYPE i.

    lo_diff = get_diff_object( iv_filename ).

    ls_diff_line = get_diff_line( io_diff       = lo_diff
                                  iv_line_index = iv_line_index ).

    CASE ls_diff_line-result.
      WHEN zif_abapgit_definitions=>c_diff-update
        OR zif_abapgit_definitions=>c_diff-insert.

        lv_line = ls_diff_line-new_num.

        lo_diff->set_patch_new( iv_line_new   = lv_line
                                iv_patch_flag = iv_patch_flag ).

      WHEN zif_abapgit_definitions=>c_diff-delete.

        lv_line = ls_diff_line-old_num.

        lo_diff->set_patch_old( iv_line_old   = lv_line
                                iv_patch_flag = iv_patch_flag ).

    ENDCASE.

  ENDMETHOD.


  METHOD apply_patch_from_form_fields.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp,
          lv_add    TYPE string,
          lv_remove TYPE string.

    lv_string = zcl_abapgit_utils=>translate_postdata( it_postdata ).
    lt_fields = zcl_abapgit_html_action_utils=>parse_fields( lv_string ).

    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name  = c_patch_action-add
                                                        it_field = lt_fields
                                              CHANGING  cg_field = lv_add ).

    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name  = c_patch_action-remove
                                                        it_field = lt_fields
                                              CHANGING  cg_field = lv_remove ).

    apply_patch_all( iv_patch      = lv_add
                     iv_patch_flag = abap_true ).

    apply_patch_all( iv_patch      = lv_remove
                     iv_patch_flag = abap_false ).

  ENDMETHOD.


  METHOD are_all_lines_patched.

    DATA: lv_patch_count TYPE i.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    LOOP AT it_diff ASSIGNING <ls_diff>
                    WHERE patch_flag = abap_true.
      lv_patch_count = lv_patch_count + 1.
    ENDLOOP.

    rv_are_all_lines_patched = boolc( lv_patch_count = lines( it_diff ) ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      iv_key    = iv_key
      is_file   = is_file
      is_object = is_object ).

    IF mo_repo->is_offline( ) = abap_true.
      zcx_abapgit_exception=>raise( |Can't patch offline repos| ).
    ENDIF.

    mo_repo_online ?= mo_repo.

    " While patching we always want to be in split mode
    CLEAR: mv_unified.
    CREATE OBJECT mo_stage.

    ms_control-page_menu = build_menu( ).

  ENDMETHOD.


  METHOD get_diff_line.

    DATA: lt_diff       TYPE zif_abapgit_definitions=>ty_diffs_tt,
          lv_line_index TYPE sy-tabix.


    lv_line_index = iv_line_index.
    lt_diff = io_diff->get( ).

    READ TABLE lt_diff INTO rs_diff
                       INDEX lv_line_index.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid line index { lv_line_index }| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_diff_object.

    FIELD-SYMBOLS: <ls_diff_file> LIKE LINE OF mt_diff_files.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.
      IF get_normalized_fname_with_path( <ls_diff_file> ) = iv_filename.
        ro_diff = <ls_diff_file>-o_diff.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ro_diff IS NOT BOUND.
      zcx_abapgit_exception=>raise( |Invalid filename { iv_filename }| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_patch_data.

    DATA: lv_section TYPE string.

    CLEAR: ev_filename, ev_line_index.

    FIND FIRST OCCURRENCE OF REGEX `patch_line` && `_(.*)_(\d)+_(\d+)`
         IN iv_patch
         SUBMATCHES ev_filename lv_section ev_line_index.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid patch| ).
    ENDIF.

  ENDMETHOD.


  METHOD insert_nav.

    " add beacon at beginning of file
    rv_insert_nav = abap_true.

  ENDMETHOD.


  METHOD is_every_changed_line_patched.

    DATA: lt_diff TYPE zif_abapgit_definitions=>ty_diffs_tt.

    FIELD-SYMBOLS:
      <ls_diff_file> TYPE zcl_abapgit_gui_page_diff=>ty_file_diff,
      <ls_diff>      TYPE zif_abapgit_definitions=>ty_diff.

    rv_everything_patched = abap_true.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.

      lt_diff = <ls_diff_file>-o_diff->get( ).

      LOOP AT lt_diff ASSIGNING <ls_diff>
                      WHERE result IS NOT INITIAL
                      AND   patch_flag = abap_false.
        rv_everything_patched = abap_false.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_patch_line_possible.

    IF is_diff_line-result = zif_abapgit_definitions=>c_diff-update
    OR is_diff_line-result = zif_abapgit_definitions=>c_diff-insert
    OR is_diff_line-result = zif_abapgit_definitions=>c_diff-delete.
      rv_is_patch_line_possible = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD refresh.

    DATA:
      lt_diff_files_old TYPE tt_file_diff.

    lt_diff_files_old = mt_diff_files.

    CASE iv_action.
      WHEN c_actions-refresh.
        refresh_full( ).
      WHEN c_actions-refresh_local.
        refresh_local( ).
      WHEN OTHERS.
        refresh_local_object( iv_action ).
    ENDCASE.

    calculate_diff( ).
    restore_patch_flags( lt_diff_files_old ).

  ENDMETHOD.


  METHOD refresh_full.
    mo_repo->refresh( abap_true ).
  ENDMETHOD.


  METHOD refresh_local.
    mo_repo->refresh_local_objects( ).
  ENDMETHOD.


  METHOD refresh_local_object.

    DATA:
      lv_regex    TYPE string,
      lv_obj_type TYPE tadir-object,
      lv_obj_name TYPE tadir-obj_name.

    lv_regex = c_actions-refresh_local_object && `_(\w{4})_(.*)`.

    FIND FIRST OCCURRENCE OF REGEX lv_regex
      IN iv_action
      SUBMATCHES lv_obj_type lv_obj_name.

    IF sy-subrc = 0.
      mo_repo->refresh_local_object(
          iv_obj_type = lv_obj_type
          iv_obj_name = lv_obj_name ).
    ELSE.
      zcx_abapgit_exception=>raise( |Invalid refresh action { iv_action }| ).
    ENDIF.

  ENDMETHOD.


  METHOD render_beacon_begin_of_row.

    mv_section_count = mv_section_count + 1.

    io_html->add( |<th class="patch">| ).
    io_html->add_checkbox( iv_id = |patch_section_{ get_normalized_fname_with_path( is_diff ) }_{ mv_section_count }| ).
    io_html->add( '</th>' ).

  ENDMETHOD.


  METHOD render_content.

    CLEAR: mv_section_count.

    IF mv_pushed = abap_true.
      refresh_full( ).
      calculate_diff( ).
      CLEAR: mv_pushed.
    ENDIF.

    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).
    ri_html = super->render_content( ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.


  METHOD render_diff_head_after_state.

    DATA: lv_act_id TYPE string.

    IF is_diff-obj_type IS NOT INITIAL AND is_diff-obj_name IS NOT INITIAL.

      lv_act_id = |{ c_actions-refresh_local_object }_{ is_diff-obj_type }_{ is_diff-obj_name }|.

      io_html->add_a(
          iv_txt   = |Refresh|
          iv_typ   = zif_abapgit_html=>c_action_type-dummy
          iv_act   = lv_act_id
          iv_id    = lv_act_id
          iv_title = |Local refresh of this object| ).

    ENDIF.

    super->render_diff_head_after_state(
        io_html = io_html
        is_diff = is_diff ).

  ENDMETHOD.


  METHOD render_line_split_row.

    render_patch( io_html      = io_html
                  iv_filename  = iv_filename
                  is_diff_line = is_diff_line
                  iv_fstate    = iv_fstate
                  iv_index     = iv_index ).

    super->render_line_split_row(
        io_html      = io_html
        iv_filename  = iv_filename
        is_diff_line = is_diff_line
        iv_index     = iv_index
        iv_fstate    = iv_fstate
        iv_new       = iv_new
        iv_old       = iv_old ).

  ENDMETHOD.


  METHOD render_patch.

    CONSTANTS:
      BEGIN OF lc_css_class,
        patch TYPE string VALUE `patch` ##NO_TEXT,
      END OF lc_css_class.

    DATA:
      lv_id                TYPE string,
      lv_patched           TYPE abap_bool,
      lv_is_patch_possible TYPE abap_bool.

    lv_patched = get_diff_object( iv_filename )->is_line_patched( iv_index ).

    lv_is_patch_possible = is_patch_line_possible(
                               is_diff_line = is_diff_line
                               iv_fstate    = iv_fstate ).

    IF lv_is_patch_possible = abap_true.

      lv_id = |{ iv_filename }_{ mv_section_count }_{ iv_index }|.

      io_html->add( |<td class="{ lc_css_class-patch }">| ).
      io_html->add_checkbox(
          iv_id      = |patch_line_{ lv_id }|
          iv_checked = lv_patched ).
      io_html->add( |</td>| ).

    ELSE.

      io_html->add( |<td class="{ lc_css_class-patch }">| ).
      io_html->add( |</td>| ).

    ENDIF.

  ENDMETHOD.


  METHOD render_patch_head.

    io_html->add( |<th class="patch">| ).
    io_html->add_checkbox( iv_id = |patch_file_{ get_normalized_fname_with_path( is_diff ) }| ).
    io_html->add( '</th>' ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ro_html.

    ro_html->zif_abapgit_html~set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ro_html->add( 'preparePatch();' ).
    ro_html->add( 'registerStagePatch();' ).

  ENDMETHOD.


  METHOD render_table_head_non_unified.

    render_patch_head( io_html = io_html
                       is_diff = is_diff ).

    super->render_table_head_non_unified(
        io_html = io_html
        is_diff = is_diff ).

  ENDMETHOD.


  METHOD restore_patch_flags.

    DATA:
      lt_diff_old TYPE zif_abapgit_definitions=>ty_diffs_tt.

    FIELD-SYMBOLS:
      <ls_diff_file>     TYPE ty_file_diff,
      <ls_diff_file_old> TYPE ty_file_diff,
      <ls_diff_old>      TYPE zif_abapgit_definitions=>ty_diff.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.

      READ TABLE it_diff_files_old ASSIGNING <ls_diff_file_old>
                                   WITH KEY secondary
                                   COMPONENTS path     = <ls_diff_file>-path
                                              filename = <ls_diff_file>-filename.
      IF sy-subrc <> 0.
        CONTINUE. " e.g. new objects
      ENDIF.

      lt_diff_old = <ls_diff_file_old>-o_diff->get( ).

      LOOP AT lt_diff_old ASSIGNING <ls_diff_old>
                          WHERE patch_flag = abap_true.

        <ls_diff_file>-o_diff->set_patch_by_old_diff(
            is_diff_old   = <ls_diff_old>
            iv_patch_flag = abap_true ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD start_staging.

    apply_patch_from_form_fields( it_postdata ).
    add_to_stage( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_actions-stage.

        start_staging( it_postdata ).

        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
          EXPORTING
            io_repo  = mo_repo_online
            io_stage = mo_stage.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.

        FIND FIRST OCCURRENCE OF REGEX |^{ c_actions-refresh }| IN iv_action.
        IF sy-subrc = 0.

          apply_patch_from_form_fields( it_postdata ).
          refresh( iv_action ).
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.

        ELSE.

          super->zif_abapgit_gui_event_handler~on_event(
             EXPORTING
               iv_action    = iv_action
               iv_getdata   = iv_getdata
               it_postdata  = it_postdata
             IMPORTING
               ei_page      = ei_page
               ev_state     = ev_state ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Patch'.

    ls_hotkey_action-description = |Stage changes|.
    ls_hotkey_action-action      = |stagePatch|.
    ls_hotkey_action-hotkey      = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Refresh local|.
    ls_hotkey_action-action      = |refreshLocal|.
    ls_hotkey_action-hotkey      = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
