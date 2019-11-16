CLASS zcl_abapgit_gui_page_diff DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_page_hotkey.

    TYPES:
      BEGIN OF ty_file_diff,
        path       TYPE string,
        filename   TYPE string,
        lstate     TYPE char1,
        rstate     TYPE char1,
        fstate     TYPE char1, " FILE state - Abstraction for shorter ifs
        o_diff     TYPE REF TO zcl_abapgit_diff,
        changed_by TYPE xubname,
        type       TYPE string,
      END OF ty_file_diff.
    TYPES:
      tt_file_diff TYPE STANDARD TABLE OF ty_file_diff.

    CONSTANTS:
      BEGIN OF c_fstate,
        local  TYPE char1 VALUE 'L',
        remote TYPE char1 VALUE 'R',
        both   TYPE char1 VALUE 'B',
      END OF c_fstate.

    METHODS constructor
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !is_file       TYPE zif_abapgit_definitions=>ty_file OPTIONAL
        !is_object     TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        !iv_patch_mode TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts REDEFINITION.

  PRIVATE SECTION.
    TYPES: ty_patch_action TYPE string.

    CONSTANTS: BEGIN OF c_actions,
                 stage          TYPE string VALUE 'patch_stage',
                 toggle_unified TYPE string VALUE 'toggle_unified',
               END OF c_actions,
               BEGIN OF c_patch_action,
                 add    TYPE ty_patch_action VALUE 'add',
                 remove TYPE ty_patch_action VALUE 'remove',
               END OF c_patch_action.

    DATA: mt_diff_files    TYPE tt_file_diff,
          mt_delayed_lines TYPE zif_abapgit_definitions=>ty_diffs_tt,
          mv_unified       TYPE abap_bool VALUE abap_true,
          mv_repo_key      TYPE zif_abapgit_persistence=>ty_repo-key,
          mv_seed          TYPE string, " Unique page id to bind JS sessionStorage
          mv_patch_mode    TYPE abap_bool,
          mo_stage         TYPE REF TO zcl_abapgit_stage,
          mv_section_count TYPE i.

    METHODS render_diff
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_diff_head
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_table_head
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_lines
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_beacon
      IMPORTING is_diff_line   TYPE zif_abapgit_definitions=>ty_diff
                is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_line_split
      IMPORTING is_diff_line   TYPE zif_abapgit_definitions=>ty_diff
                iv_filename    TYPE string
                iv_fstate      TYPE char1
                iv_index       TYPE sy-tabix
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_line_unified
      IMPORTING is_diff_line   TYPE zif_abapgit_definitions=>ty_diff OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS append_diff
      IMPORTING it_remote TYPE zif_abapgit_definitions=>ty_files_tt
                it_local  TYPE zif_abapgit_definitions=>ty_files_item_tt
                is_status TYPE zif_abapgit_definitions=>ty_result
      RAISING   zcx_abapgit_exception.
    METHODS build_menu
      RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.
    METHODS is_binary
      IMPORTING iv_d1         TYPE xstring
                iv_d2         TYPE xstring
      RETURNING VALUE(rv_yes) TYPE abap_bool.
    METHODS add_to_stage
      RAISING
        zcx_abapgit_exception.
    METHODS render_patch
      IMPORTING
        io_html                TYPE REF TO zcl_abapgit_html
        iv_patch_line_possible TYPE abap_bool
        iv_filename            TYPE string
        is_diff_line           TYPE zif_abapgit_definitions=>ty_diff
        iv_index               TYPE sy-tabix.
    METHODS start_staging
      IMPORTING
        it_postdata TYPE cnht_post_data_tab
      RAISING
        zcx_abapgit_exception.
    METHODS apply_patch_all
      IMPORTING
        iv_patch      TYPE string
        iv_patch_flag TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS render_patch_head
      IMPORTING
        io_html TYPE REF TO zcl_abapgit_html
        is_diff TYPE zcl_abapgit_gui_page_diff=>ty_file_diff.
    METHODS apply_patch_for
      IMPORTING
        iv_filename   TYPE string
        iv_line_index TYPE string
        iv_patch_flag TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS get_diff_object
      IMPORTING
        iv_filename    TYPE string
      RETURNING
        VALUE(ro_diff) TYPE REF TO zcl_abapgit_diff
      RAISING
        zcx_abapgit_exception.
    METHODS get_diff_line
      IMPORTING
        io_diff        TYPE REF TO zcl_abapgit_diff
        iv_line_index  TYPE string
      RETURNING
        VALUE(rs_diff) TYPE zif_abapgit_definitions=>ty_diff
      RAISING
        zcx_abapgit_exception.
    METHODS are_all_lines_patched
      IMPORTING
        it_diff                         TYPE zif_abapgit_definitions=>ty_diffs_tt
      RETURNING
        VALUE(rv_are_all_lines_patched) TYPE abap_bool.
    METHODS add_jump_sub_menu
      IMPORTING
        io_menu TYPE REF TO zcl_abapgit_html_toolbar.
    METHODS add_filter_sub_menu
      IMPORTING
        io_menu TYPE REF TO zcl_abapgit_html_toolbar.
    CLASS-METHODS get_patch_data
      IMPORTING
        iv_patch      TYPE string
      EXPORTING
        ev_filename   TYPE string
        ev_line_index TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DIFF IMPLEMENTATION.


  METHOD add_filter_sub_menu.

    DATA:
      lo_sub_filter TYPE REF TO zcl_abapgit_html_toolbar,
      lt_types      TYPE string_table,
      lt_users      TYPE string_table.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff_files,
                   <lv_i>    TYPE string.
    " Get unique
    LOOP AT mt_diff_files ASSIGNING <ls_diff>.
      APPEND <ls_diff>-type TO lt_types.
      APPEND <ls_diff>-changed_by TO lt_users.
    ENDLOOP.

    SORT lt_types.
    DELETE ADJACENT DUPLICATES FROM lt_types.

    SORT lt_users.
    DELETE ADJACENT DUPLICATES FROM lt_users.

    IF lines( lt_types ) > 1 OR lines( lt_users ) > 1.
      CREATE OBJECT lo_sub_filter EXPORTING iv_id = 'diff-filter'.

      " File types
      IF lines( lt_types ) > 1.
        lo_sub_filter->add( iv_txt = 'TYPE' iv_typ = zif_abapgit_html=>c_action_type-separator ).
        LOOP AT lt_types ASSIGNING <lv_i>.
          lo_sub_filter->add( iv_txt = <lv_i>
                       iv_typ = zif_abapgit_html=>c_action_type-onclick
                       iv_aux = 'type'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      " Changed by
      IF lines( lt_users ) > 1.
        lo_sub_filter->add( iv_txt = 'CHANGED BY' iv_typ = zif_abapgit_html=>c_action_type-separator ).
        LOOP AT lt_users ASSIGNING <lv_i>.
          lo_sub_filter->add( iv_txt = <lv_i>
                       iv_typ = zif_abapgit_html=>c_action_type-onclick
                       iv_aux = 'changed-by'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      io_menu->add( iv_txt = 'Filter'
                    io_sub = lo_sub_filter ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.


  METHOD add_jump_sub_menu.

    DATA: lo_sub_jump TYPE REF TO zcl_abapgit_html_toolbar.
    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff_files.

    CREATE OBJECT lo_sub_jump EXPORTING iv_id = 'jump'.

    LOOP AT mt_diff_files ASSIGNING <ls_diff>.

      lo_sub_jump->add(
          iv_id  = |li_jump_{ sy-tabix }|
          iv_txt = <ls_diff>-filename
          iv_typ = zif_abapgit_html=>c_action_type-onclick ).

    ENDLOOP.

    io_menu->add( iv_txt = 'Jump'
                  io_sub = lo_sub_jump ) ##NO_TEXT.

  ENDMETHOD.


  METHOD add_to_stage.

    DATA: lo_repo              TYPE REF TO zcl_abapgit_repo_online,
          lt_diff              TYPE zif_abapgit_definitions=>ty_diffs_tt,
          lv_something_patched TYPE abap_bool,
          ls_status            TYPE zif_abapgit_definitions=>ty_result,
          lv_patch             TYPE xstring,
          lo_git_add_patch     TYPE REF TO zcl_abapgit_git_add_patch.

    FIELD-SYMBOLS: <ls_diff_file> TYPE zcl_abapgit_gui_page_diff=>ty_file_diff.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).

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


  METHOD append_diff.

    DATA:
      lv_offs    TYPE i,
      lv_is_binary TYPE abap_bool,
      ls_r_dummy LIKE LINE OF it_remote ##NEEDED,
      ls_l_dummy LIKE LINE OF it_local  ##NEEDED.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote,
                   <ls_local>  LIKE LINE OF it_local,
                   <ls_diff>   LIKE LINE OF mt_diff_files.


    READ TABLE it_remote ASSIGNING <ls_remote>
      WITH KEY filename = is_status-filename
               path     = is_status-path.
    IF sy-subrc <> 0.
      ASSIGN ls_r_dummy TO <ls_remote>.
    ENDIF.

    READ TABLE it_local ASSIGNING <ls_local>
      WITH KEY file-filename = is_status-filename
               file-path     = is_status-path.
    IF sy-subrc <> 0.
      ASSIGN ls_l_dummy TO <ls_local>.
    ENDIF.

    IF <ls_local> IS INITIAL AND <ls_remote> IS INITIAL.
      zcx_abapgit_exception=>raise( |DIFF: file not found { is_status-filename }| ).
    ENDIF.

    APPEND INITIAL LINE TO mt_diff_files ASSIGNING <ls_diff>.
    <ls_diff>-path     = is_status-path.
    <ls_diff>-filename = is_status-filename.
    <ls_diff>-lstate   = is_status-lstate.
    <ls_diff>-rstate   = is_status-rstate.

    IF <ls_diff>-lstate IS NOT INITIAL AND <ls_diff>-rstate IS NOT INITIAL.
      <ls_diff>-fstate = c_fstate-both.
    ELSEIF <ls_diff>-lstate IS NOT INITIAL.
      <ls_diff>-fstate = c_fstate-local.
    ELSE. "rstate IS NOT INITIAL, lstate = empty.
      <ls_diff>-fstate = c_fstate-remote.
    ENDIF.

    " Changed by
    IF <ls_local>-item-obj_type IS NOT INITIAL.
      <ls_diff>-changed_by = to_lower( zcl_abapgit_objects=>changed_by( <ls_local>-item ) ).
    ENDIF.

    " Extension
    IF <ls_local>-file-filename IS NOT INITIAL.
      <ls_diff>-type = reverse( <ls_local>-file-filename ).
    ELSE.
      <ls_diff>-type = reverse( <ls_remote>-filename ).
    ENDIF.

    FIND FIRST OCCURRENCE OF '.' IN <ls_diff>-type MATCH OFFSET lv_offs.
    <ls_diff>-type = reverse( substring( val = <ls_diff>-type len = lv_offs ) ).
    IF <ls_diff>-type <> 'xml' AND <ls_diff>-type <> 'abap'.
      <ls_diff>-type = 'other'.
    ENDIF.

    IF <ls_diff>-type = 'other'
       AND is_binary( iv_d1 = <ls_remote>-data iv_d2 = <ls_local>-file-data ) = abap_true.
      <ls_diff>-type = 'binary'.
    ENDIF.

    " Diff data
    IF <ls_diff>-type <> 'binary'.
      IF <ls_diff>-fstate = c_fstate-remote. " Remote file leading changes
        CREATE OBJECT <ls_diff>-o_diff
          EXPORTING
            iv_new = <ls_remote>-data
            iv_old = <ls_local>-file-data.
      ELSE.             " Local leading changes or both were modified
        CREATE OBJECT <ls_diff>-o_diff
          EXPORTING
            iv_new = <ls_local>-file-data
            iv_old = <ls_remote>-data.
      ENDIF.
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


  METHOD are_all_lines_patched.

    DATA: lv_patch_count TYPE i.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    LOOP AT it_diff ASSIGNING <ls_diff>
                    WHERE patch_flag = abap_true.
      lv_patch_count = lv_patch_count + 1.
    ENDLOOP.

    rv_are_all_lines_patched = boolc( lv_patch_count = lines( it_diff ) ).

  ENDMETHOD.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    add_jump_sub_menu( ro_menu ).
    add_filter_sub_menu( ro_menu ).

    IF mv_patch_mode = abap_true.
      ro_menu->add( iv_txt = 'Stage'
                    iv_act = c_actions-stage
                    iv_id  = 'stage'
                    iv_typ = zif_abapgit_html=>c_action_type-dummy
                     ) ##NO_TEXT.
    ELSE.
      ro_menu->add( iv_txt = 'Split/Unified view'
                    iv_act = c_actions-toggle_unified ) ##NO_TEXT.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.

    DATA: lt_remote TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_local  TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_status TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_repo   TYPE REF TO zcl_abapgit_repo,
          lv_ts     TYPE timestamp.

    FIELD-SYMBOLS: <ls_status> LIKE LINE OF lt_status.

    super->constructor( ).
    ms_control-page_title = 'DIFF'.
    mv_unified            = zcl_abapgit_persistence_user=>get_instance( )->get_diff_unified( ).
    mv_repo_key           = iv_key.
    mv_patch_mode         = iv_patch_mode.

    IF mv_patch_mode = abap_true.
      " While patching we always want to be in split mode
      CLEAR: mv_unified.
      CREATE OBJECT mo_stage.
    ENDIF.

    GET TIME STAMP FIELD lv_ts.
    mv_seed = |diff{ lv_ts }|. " Generate based on time

    ASSERT is_file IS INITIAL OR is_object IS INITIAL. " just one passed

    lo_repo   = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lt_remote = lo_repo->get_files_remote( ).
    lt_local  = lo_repo->get_files_local( ).
    lt_status = lo_repo->status( ).

    IF is_file IS NOT INITIAL.        " Diff for one file

      READ TABLE lt_status ASSIGNING <ls_status>
        WITH KEY path = is_file-path filename = is_file-filename.

      append_diff( it_remote = lt_remote
                   it_local  = lt_local
                   is_status = <ls_status> ).

    ELSEIF is_object IS NOT INITIAL.  " Diff for whole object

      LOOP AT lt_status ASSIGNING <ls_status>
          WHERE obj_type = is_object-obj_type
          AND obj_name = is_object-obj_name
          AND match IS INITIAL.
        append_diff( it_remote = lt_remote
                     it_local  = lt_local
                     is_status = <ls_status> ).
      ENDLOOP.

    ELSE.                             " Diff for the whole repo
      SORT lt_status BY
        path ASCENDING
        filename ASCENDING.
      LOOP AT lt_status ASSIGNING <ls_status> WHERE match IS INITIAL.
        append_diff( it_remote = lt_remote
                     it_local  = lt_local
                     is_status = <ls_status> ).
      ENDLOOP.

    ENDIF.

    IF lines( mt_diff_files ) = 0.
      zcx_abapgit_exception=>raise( 'PAGE_DIFF ERROR: No diff files found' ).
    ENDIF.

    ms_control-page_menu  = build_menu( ).

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

    READ TABLE mt_diff_files ASSIGNING <ls_diff_file>
                             WITH KEY filename = iv_filename.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid filename { iv_filename }| ).
    ENDIF.

    ro_diff = <ls_diff_file>-o_diff.

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


  METHOD is_binary.

    FIELD-SYMBOLS <lv_data> LIKE iv_d1.

    IF iv_d1 IS NOT INITIAL. " One of them might be new and so empty
      ASSIGN iv_d1 TO <lv_data>.
    ELSE.
      ASSIGN iv_d2 TO <lv_data>.
    ENDIF.

    rv_yes = zcl_abapgit_utils=>is_binary( <lv_data> ).

  ENDMETHOD.


  METHOD render_beacon.

    DATA: lv_beacon  TYPE string,
          lt_beacons TYPE zif_abapgit_definitions=>ty_string_tt.

    CREATE OBJECT ro_html.

    mv_section_count = mv_section_count + 1.

    IF is_diff_line-beacon > 0.
      lt_beacons = is_diff-o_diff->get_beacons( ).
      READ TABLE lt_beacons INTO lv_beacon INDEX is_diff_line-beacon.
    ELSE.
      lv_beacon = '---'.
    ENDIF.

    ro_html->add( '<thead class="nav_line">' ).
    ro_html->add( '<tr>' ).

    IF mv_patch_mode = abap_true.

      ro_html->add( |<th class="patch">| ).
      ro_html->add_checkbox( iv_id = |patch_section_{ is_diff-filename }_{ mv_section_count }| ).
      ro_html->add( '</th>' ).

    ELSE.
      ro_html->add( '<th class="num"></th>' ).
    ENDIF.
    IF mv_unified = abap_true.
      ro_html->add( '<th class="num"></th>' ).
      ro_html->add( |<th>@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ELSE.
      ro_html->add( |<th colspan="3">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ENDIF.

    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_content.

    DATA: ls_diff_file LIKE LINE OF mt_diff_files,
          li_progress  TYPE REF TO zif_abapgit_progress.


    CREATE OBJECT ro_html.

    CLEAR: mv_section_count.

    li_progress = zcl_abapgit_progress=>get_instance( lines( mt_diff_files ) ).

    ro_html->add( |<div id="diff-list" data-repo-key="{ mv_repo_key }">| ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_js_error_banner( ) ).
    LOOP AT mt_diff_files INTO ls_diff_file.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Render Diff - { ls_diff_file-filename }| ).

      ro_html->add( render_diff( ls_diff_file ) ).
    ENDLOOP.
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( |<div class="diff" data-type="{ is_diff-type
      }" data-changed-by="{ is_diff-changed_by
      }" data-file="{ is_diff-path && is_diff-filename }">| ). "#EC NOTEXT
    ro_html->add( render_diff_head( is_diff ) ).

    " Content
    IF is_diff-type <> 'binary'.
      ro_html->add( '<div class="diff_content">' ).         "#EC NOTEXT
      ro_html->add( |<table class="diff_tab syntax-hl" id={ is_diff-filename }>| ). "#EC NOTEXT
      ro_html->add( render_table_head( is_diff ) ).
      ro_html->add( render_lines( is_diff ) ).
      ro_html->add( '</table>' ).                           "#EC NOTEXT
    ELSE.
      ro_html->add( '<div class="diff_content paddings center grey">' ). "#EC NOTEXT
      ro_html->add( 'The content seems to be binary.' ).    "#EC NOTEXT
      ro_html->add( 'Cannot display as diff.' ).            "#EC NOTEXT
    ENDIF.
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD render_diff_head.

    DATA: ls_stats TYPE zif_abapgit_definitions=>ty_count.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff_head">' ).              "#EC NOTEXT

    IF is_diff-type <> 'binary'.
      ls_stats = is_diff-o_diff->stats( ).
      IF is_diff-fstate = c_fstate-both. " Merge stats into 'update' if both were changed
        ls_stats-update = ls_stats-update + ls_stats-insert + ls_stats-delete.
        CLEAR: ls_stats-insert, ls_stats-delete.
      ENDIF.

      ro_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
      ro_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
      ro_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ENDIF.

    ro_html->add( |<span class="diff_name">{ is_diff-path }{ is_diff-filename }</span>| ). "#EC NOTEXT
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state(
      iv_lstate = is_diff-lstate
      iv_rstate = is_diff-rstate ) ).

    IF is_diff-fstate = c_fstate-both AND mv_unified = abap_true.
      ro_html->add( '<span class="attention pad-sides">Attention: Unified mode'
                 && ' highlighting for MM assumes local file is newer ! </span>' ). "#EC NOTEXT
    ENDIF.

    ro_html->add( |<span class="diff_changed_by">last change by: <span class="user">{
      is_diff-changed_by }</span></span>| ).

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD render_lines.

    DATA: lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter,
          lt_diffs       TYPE zif_abapgit_definitions=>ty_diffs_tt,
          lv_insert_nav  TYPE abap_bool,
          lv_tabix       TYPE syst-tabix.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.

    lo_highlighter = zcl_abapgit_syntax_highlighter=>create( is_diff-filename ).
    CREATE OBJECT ro_html.

    lt_diffs = is_diff-o_diff->get( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.

      lv_tabix = sy-tabix.

      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        ro_html->add( render_beacon( is_diff_line = <ls_diff> is_diff = is_diff ) ).
        lv_insert_nav = abap_false.
      ENDIF.

      IF lo_highlighter IS BOUND.
        <ls_diff>-new = lo_highlighter->process_line( <ls_diff>-new ).
        <ls_diff>-old = lo_highlighter->process_line( <ls_diff>-old ).
      ELSE.
        <ls_diff>-new = escape( val = <ls_diff>-new format = cl_abap_format=>e_html_attr ).
        <ls_diff>-old = escape( val = <ls_diff>-old format = cl_abap_format=>e_html_attr ).
      ENDIF.

      CONDENSE <ls_diff>-new_num. "get rid of leading spaces
      CONDENSE <ls_diff>-old_num.

      IF mv_unified = abap_true.
        ro_html->add( render_line_unified( is_diff_line = <ls_diff> ) ).
      ELSE.
        ro_html->add( render_line_split( is_diff_line = <ls_diff>
                                         iv_filename  = is_diff-filename
                                         iv_fstate    = is_diff-fstate
                                         iv_index     = lv_tabix ) ).
      ENDIF.

    ENDLOOP.

    IF mv_unified = abap_true.
      ro_html->add( render_line_unified( ) ). " Release delayed lines
    ENDIF.

  ENDMETHOD.


  METHOD render_line_split.

    DATA: lv_new                 TYPE string,
          lv_old                 TYPE string,
          lv_mark                TYPE string,
          lv_bg                  TYPE string,
          lv_patch_line_possible TYPE abap_bool.

    CREATE OBJECT ro_html.

    " New line
    lv_mark = ` `.
    IF is_diff_line-result IS NOT INITIAL.
      IF iv_fstate = c_fstate-both OR is_diff_line-result = zif_abapgit_definitions=>c_diff-update.
        lv_bg = ' diff_upd'.
        lv_mark = `~`.
      ELSEIF is_diff_line-result = zif_abapgit_definitions=>c_diff-insert.
        lv_bg = ' diff_ins'.
        lv_mark = `+`.
      ENDIF.
    ENDIF.
    lv_new = |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-new }</td>|.

    IF lv_mark <> ` `.
      lv_patch_line_possible = abap_true.
    ENDIF.

    " Old line
    CLEAR lv_bg.
    lv_mark = ` `.
    IF is_diff_line-result IS NOT INITIAL.
      IF iv_fstate = c_fstate-both OR is_diff_line-result = zif_abapgit_definitions=>c_diff-update.
        lv_bg = ' diff_upd'.
        lv_mark = `~`.
      ELSEIF is_diff_line-result = zif_abapgit_definitions=>c_diff-delete.
        lv_bg = ' diff_del'.
        lv_mark = `-`.
      ENDIF.
    ENDIF.
    lv_old = |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-old }</td>|.

    IF lv_mark <> ` `.
      lv_patch_line_possible = abap_true.
    ENDIF.

    " render line, inverse sides if remote is newer
    ro_html->add( '<tr>' ).                                 "#EC NOTEXT

    IF mv_patch_mode = abap_true.

      render_patch( io_html                = ro_html
                    iv_patch_line_possible = lv_patch_line_possible
                    iv_filename            = iv_filename
                    is_diff_line           = is_diff_line
                    iv_index               = iv_index ).

    ENDIF.

    IF iv_fstate = c_fstate-remote. " Remote file leading changes
      ro_html->add( lv_old ). " local
      ro_html->add( lv_new ). " remote
    ELSE.             " Local leading changes or both were modified
      ro_html->add( lv_new ). " local
      ro_html->add( lv_old ). " remote
    ENDIF.

    ro_html->add( '</tr>' ).                                "#EC NOTEXT

  ENDMETHOD.


  METHOD render_line_unified.

    FIELD-SYMBOLS <ls_diff_line> LIKE LINE OF mt_delayed_lines.

    CREATE OBJECT ro_html.

    " Release delayed subsequent update lines
    IF is_diff_line-result <> zif_abapgit_definitions=>c_diff-update.
      LOOP AT mt_delayed_lines ASSIGNING <ls_diff_line>.
        ro_html->add( '<tr>' ).                             "#EC NOTEXT
        ro_html->add( |<td class="num" line-num="{ <ls_diff_line>-old_num }"></td>|
                   && |<td class="num" line-num=""></td>|
                   && |<td class="code diff_del">-{ <ls_diff_line>-old }</td>| ).
        ro_html->add( '</tr>' ).                            "#EC NOTEXT
      ENDLOOP.
      LOOP AT mt_delayed_lines ASSIGNING <ls_diff_line>.
        ro_html->add( '<tr>' ).                             "#EC NOTEXT
        ro_html->add( |<td class="num" line-num=""></td>|
                   && |<td class="num" line-num="{ <ls_diff_line>-new_num }"></td>|
                   && |<td class="code diff_ins">+{ <ls_diff_line>-new }</td>| ).
        ro_html->add( '</tr>' ).                            "#EC NOTEXT
      ENDLOOP.
      CLEAR mt_delayed_lines.
    ENDIF.

    ro_html->add( '<tr>' ).                                 "#EC NOTEXT
    CASE is_diff_line-result.
      WHEN zif_abapgit_definitions=>c_diff-update.
        APPEND is_diff_line TO mt_delayed_lines. " Delay output of subsequent updates
      WHEN zif_abapgit_definitions=>c_diff-insert.
        ro_html->add( |<td class="num" line-num=""></td>|
                   && |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="code diff_ins">+{ is_diff_line-new }</td>| ).
      WHEN zif_abapgit_definitions=>c_diff-delete.
        ro_html->add( |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num" line-num=""></td>|
                   && |<td class="code diff_del">-{ is_diff_line-old }</td>| ).
      WHEN OTHERS. "none
        ro_html->add( |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="code"> { is_diff_line-old }</td>| ).
    ENDCASE.
    ro_html->add( '</tr>' ).                                "#EC NOTEXT

  ENDMETHOD.


  METHOD render_patch.

    CONSTANTS:
      BEGIN OF c_css_class,
        patch TYPE string VALUE `patch` ##NO_TEXT,
      END OF c_css_class.

    DATA: lv_id          TYPE string,
          lv_left_class  TYPE string,
          lv_right_class TYPE string,
          lv_object      TYPE string.

    lv_object = iv_filename.

    IF iv_patch_line_possible = abap_true.

      lv_id = |{ lv_object }_{ mv_section_count }_{ iv_index }|.

      io_html->add( |<td class="{ c_css_class-patch }">| ).
      io_html->add_checkbox( iv_id = |patch_line_{ lv_id }| ).
      io_html->add( |</td>| ).

    ELSE.

      io_html->add( |<td class="{ c_css_class-patch }">| ).
      io_html->add( |</td>| ).

    ENDIF.

  ENDMETHOD.


  METHOD render_patch_head.

    io_html->add( |<th class="patch">| ).
    io_html->add_checkbox( iv_id = |patch_file_{ is_diff-filename }| ).
    io_html->add( '</th>' ).

  ENDMETHOD.


  METHOD render_table_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<thead class="header">' ).               "#EC NOTEXT
    ro_html->add( '<tr>' ).                                 "#EC NOTEXT

    IF mv_unified = abap_true.
      ro_html->add( '<th class="num">old</th>' ).           "#EC NOTEXT
      ro_html->add( '<th class="num">new</th>' ).           "#EC NOTEXT
      ro_html->add( '<th>code</th>' ).                      "#EC NOTEXT
    ELSE.

      IF mv_patch_mode = abap_true.

        render_patch_head( io_html = ro_html
                           is_diff = is_diff ).

      ENDIF.

      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>LOCAL</th>' ).                     "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>REMOTE</th>' ).                    "#EC NOTEXT

    ENDIF.

    ro_html->add( '</tr>' ).                                "#EC NOTEXT
    ro_html->add( '</thead>' ).                             "#EC NOTEXT

  ENDMETHOD.


  METHOD scripts.

    ro_html = super->scripts( ).

    ro_html->add( 'var gHelper = new DiffHelper({' ).
    ro_html->add( |  seed:        "{ mv_seed }",| ).
    ro_html->add( '  ids: {' ).
    ro_html->add( '    jump:        "jump",' ).
    ro_html->add( '    diffList:    "diff-list",' ).
    ro_html->add( '    filterMenu:  "diff-filter",' ).
    ro_html->add( '  }' ).
    ro_html->add( '});' ).

    IF mv_patch_mode = abap_true.
      ro_html->add( 'preparePatch();' ).
      ro_html->add( 'registerStagePatch();' ).
    ENDIF.

    ro_html->add( 'addMarginBottom();' ).

    ro_html->add( 'var gGoJumpPalette = new CommandPalette(enumerateJumpAllFiles, {' ).
    ro_html->add( '  toggleKey: "F2",' ).
    ro_html->add( '  hotkeyDescription: "Jump to file ..."' ).
    ro_html->add( '});' ).

  ENDMETHOD.


  METHOD start_staging.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp,
          lv_add    TYPE string,
          lv_remove TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_string.
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

    add_to_stage( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    CASE iv_action.
      WHEN c_actions-toggle_unified. " Toggle file diplay

        mv_unified = zcl_abapgit_persistence_user=>get_instance( )->toggle_diff_unified( ).
        ev_state   = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-stage.

        start_staging( it_postdata ).

        lo_repo  ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
          EXPORTING
            io_repo  = lo_repo
            io_stage = mo_stage.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.

        super->zif_abapgit_gui_event_handler~on_event(
           EXPORTING
             iv_action    = iv_action
             iv_prev_page = iv_prev_page
             iv_getdata   = iv_getdata
             it_postdata  = it_postdata
           IMPORTING
             ei_page      = ei_page
             ev_state     = ev_state ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-name   = |Stage changes|.
    ls_hotkey_action-action = |stagePatch|.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
