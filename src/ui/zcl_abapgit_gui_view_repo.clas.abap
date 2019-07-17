CLASS zcl_abapgit_gui_view_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_page_hotkey.

    ALIASES render
      FOR zif_abapgit_gui_renderable~render .

    CONSTANTS:
      BEGIN OF c_actions,
        change_dir        TYPE string VALUE 'change_dir' ##NO_TEXT,
        toggle_hide_files TYPE string VALUE 'toggle_hide_files' ##NO_TEXT,
        toggle_folders    TYPE string VALUE 'toggle_folders' ##NO_TEXT,
        toggle_changes    TYPE string VALUE 'toggle_changes' ##NO_TEXT,
        display_more      TYPE string VALUE 'display_more' ##NO_TEXT,
      END OF c_actions .

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_repo         TYPE REF TO zcl_abapgit_repo,
          mv_cur_dir      TYPE string,
          mv_hide_files   TYPE abap_bool,
          mv_max_lines    TYPE i,
          mv_max_setting  TYPE i,
          mv_show_folders TYPE abap_bool,
          mv_changes_only TYPE abap_bool.

    METHODS:
      render_head_line
        IMPORTING iv_lstate      TYPE char1
                  iv_rstate      TYPE char1
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception,
      build_head_menu
        IMPORTING iv_lstate         TYPE char1
                  iv_rstate         TYPE char1
        RETURNING VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar
        RAISING   zcx_abapgit_exception,
      build_grid_menu
        RETURNING VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar
        RAISING   zcx_abapgit_exception,
      render_item
        IMPORTING is_item              TYPE zif_abapgit_definitions=>ty_repo_item
                  iv_render_transports TYPE abap_bool
        RETURNING VALUE(ro_html)       TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception,
      render_item_files
        IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      render_item_command
        IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      get_item_class
        IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      get_item_icon
        IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      render_item_lock_column
        IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      render_empty_package
        RETURNING VALUE(rv_html) TYPE string,
      render_parent_dir
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception.

    METHODS:
      build_obj_jump_link
        IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      build_dir_jump_link
        IMPORTING iv_path        TYPE string
        RETURNING VALUE(rv_html) TYPE string,
      build_inactive_object_code
        IMPORTING is_item                      TYPE zif_abapgit_definitions=>ty_repo_item
        RETURNING VALUE(rv_inactive_html_code) TYPE string,
      open_in_master_language
        RAISING zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_VIEW_REPO IMPLEMENTATION.


  METHOD build_dir_jump_link.

    DATA: lv_path   TYPE string,
          lv_encode TYPE string.

    lv_path = iv_path.
    REPLACE FIRST OCCURRENCE OF mv_cur_dir IN lv_path WITH ''.
    lv_encode = zcl_abapgit_html_action_utils=>dir_encode( lv_path ).

    rv_html = zcl_abapgit_html=>a( iv_txt = lv_path
                                   iv_act = |{ c_actions-change_dir }?{ lv_encode }| ).

  ENDMETHOD.


  METHOD build_grid_menu.

    CREATE OBJECT ro_toolbar.

    IF mo_repo->has_remote_source( ) = abap_true.
      ro_toolbar->add(  " Show/Hide files
        iv_txt = 'Show files'
        iv_chk = boolc( NOT mv_hide_files = abap_true )
        iv_act = c_actions-toggle_hide_files ).

      ro_toolbar->add(  " Show changes only
        iv_txt = 'Show changes only'
        iv_chk = mv_changes_only
        iv_act = c_actions-toggle_changes ).
    ENDIF.

    ro_toolbar->add(  " Show/Hide folders
      iv_txt = 'Show folders'
      iv_chk = mv_show_folders
      iv_act = c_actions-toggle_folders ).

  ENDMETHOD.


  METHOD build_head_menu.

    DATA: lo_tb_advanced TYPE REF TO zcl_abapgit_html_toolbar,
          lo_tb_branch   TYPE REF TO zcl_abapgit_html_toolbar,
          lo_tb_tag      TYPE REF TO zcl_abapgit_html_toolbar,
          lv_key         TYPE zif_abapgit_persistence=>ty_value,
          lv_wp_opt      LIKE zif_abapgit_html=>c_html_opt-crossout,
          lv_crossout    LIKE zif_abapgit_html=>c_html_opt-crossout,
          lv_pull_opt    LIKE zif_abapgit_html=>c_html_opt-crossout,
          li_log         TYPE REF TO zif_abapgit_log.

    CREATE OBJECT ro_toolbar.
    CREATE OBJECT lo_tb_branch.
    CREATE OBJECT lo_tb_advanced.
    CREATE OBJECT lo_tb_tag.

    lv_key = mo_repo->get_key( ).

    IF mo_repo->get_local_settings( )-write_protected = abap_true.
      lv_wp_opt   = zif_abapgit_html=>c_html_opt-crossout.
      lv_pull_opt = zif_abapgit_html=>c_html_opt-crossout.
    ELSE.
      lv_pull_opt = zif_abapgit_html=>c_html_opt-strong.
    ENDIF.

    " Build branch drop-down ========================
    IF mo_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_branch->add( iv_txt = 'Overview'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_branch_overview }?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Switch'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_switch }?{ lv_key }|
                         iv_opt = lv_wp_opt ).
      lo_tb_branch->add( iv_txt = 'Create'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_create }?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Delete'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_delete }?{ lv_key }| ).

      lo_tb_tag->add( iv_txt = 'Overview'
                      iv_act = |{ zif_abapgit_definitions=>c_action-go_tag_overview }?{ lv_key }| ).
      lo_tb_tag->add( iv_txt = 'Switch'
                      iv_act = |{ zif_abapgit_definitions=>c_action-git_tag_switch }?{ lv_key }|
                      iv_opt = lv_wp_opt ).
      lo_tb_tag->add( iv_txt = 'Create'
                      iv_act = |{ zif_abapgit_definitions=>c_action-git_tag_create }?{ lv_key }| ).
      lo_tb_tag->add( iv_txt = 'Delete'
                      iv_act = |{ zif_abapgit_definitions=>c_action-git_tag_delete }?{ lv_key }| ).

    ENDIF.

    " Build advanced drop-down ========================
    IF iv_rstate IS NOT INITIAL OR iv_lstate IS NOT INITIAL. " In case of asyncronicities
      lo_tb_advanced->add( iv_txt = 'Reset local'
                           iv_act = |{ zif_abapgit_definitions=>c_action-git_reset }?{ lv_key }|
                           iv_opt = lv_wp_opt ).
    ENDIF.
    IF mo_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_advanced->add( iv_txt = 'Background mode'
                           iv_act = |{ zif_abapgit_definitions=>c_action-go_background }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Change remote'
                           iv_act = |{ zif_abapgit_definitions=>c_action-repo_remote_change }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Make off-line'
                           iv_act = |{ zif_abapgit_definitions=>c_action-repo_remote_detach }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Force stage'
                           iv_act = |{ zif_abapgit_definitions=>c_action-go_stage }?{ lv_key }| ).

      CLEAR lv_crossout.
      IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-transport_to_branch ) = abap_false.
        lv_crossout = zif_abapgit_html=>c_html_opt-crossout.
      ENDIF.
      lo_tb_advanced->add( iv_txt = 'Transport to Branch'
                           iv_act = |{ zif_abapgit_definitions=>c_action-repo_transport_to_branch }?{ lv_key }|
                           iv_opt = lv_crossout ).

    ELSE.
      lo_tb_advanced->add( iv_txt = 'Make on-line'
                           iv_act = |{ zif_abapgit_definitions=>c_action-repo_remote_attach }?{ lv_key }| ).
    ENDIF.
    lo_tb_advanced->add( iv_txt = 'Syntax Check'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_syntax_check }?{ lv_key }| ).
    lo_tb_advanced->add( iv_txt = 'Run Code Inspector'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_code_inspector }?{ lv_key }| ).
    lo_tb_advanced->add( iv_txt = 'Repo settings'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_settings }?{ lv_key }| ).

    CLEAR lv_crossout.
    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-update_local_checksum ) = abap_false.
      lv_crossout = zif_abapgit_html=>c_html_opt-crossout.
    ENDIF.
    lo_tb_advanced->add( iv_txt = 'Update local checksums'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_refresh_checksums }?{ lv_key }|
                         iv_opt = lv_crossout ).

    IF mo_repo->get_dot_abapgit( )->get_master_language( ) <> sy-langu.
      lo_tb_advanced->add( iv_txt = 'Open in master language'
                           iv_act = |{ zif_abapgit_definitions=>c_action-repo_open_in_master_lang }?{ lv_key }| ).
    ENDIF.

    lo_tb_advanced->add( iv_txt = 'Remove'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_remove }?{ lv_key }| ).

    CLEAR lv_crossout.
    IF mo_repo->get_local_settings( )-write_protected = abap_true
        OR zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-uninstall ) = abap_false.
      lv_crossout = zif_abapgit_html=>c_html_opt-crossout.
    ENDIF.
    lo_tb_advanced->add( iv_txt = 'Uninstall'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_purge }?{ lv_key }|
                         iv_opt = lv_crossout ).

    " Build main toolbar ==============================
    IF mo_repo->is_offline( ) = abap_false. " Online ?
      IF iv_rstate IS NOT INITIAL. " Something new at remote
        ro_toolbar->add( iv_txt = 'Pull'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_pull }?{ lv_key }|
                         iv_opt = lv_pull_opt ).
      ENDIF.
      IF iv_lstate IS NOT INITIAL. " Something new at local
        ro_toolbar->add( iv_txt = 'Stage'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_stage }?{ lv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      IF iv_rstate IS NOT INITIAL OR iv_lstate IS NOT INITIAL. " Any changes
        ro_toolbar->add( iv_txt = 'Show diff'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_diff }?key={ lv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      li_log = mo_repo->get_log( ).
      IF li_log IS BOUND AND li_log->count( ) > 0.
        ro_toolbar->add( iv_txt = 'Log'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_log }?{ lv_key }| ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Branch'
                       io_sub = lo_tb_branch ) ##NO_TEXT.
      ro_toolbar->add( iv_txt = 'Tag'
                       io_sub = lo_tb_tag ) ##NO_TEXT.
    ELSE.
      IF mo_repo->has_remote_source( ) = abap_true AND iv_rstate IS NOT INITIAL.
        ro_toolbar->add( iv_txt = 'Pull <sup>zip</sup>'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_pull }?{ lv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Show diff'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_diff }?key={ lv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Import <sup>zip</sup>'
                       iv_act = |{ zif_abapgit_definitions=>c_action-zip_import }?{ lv_key }|
                       iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ro_toolbar->add( iv_txt = 'Export <sup>zip</sup>'
                       iv_act = |{ zif_abapgit_definitions=>c_action-zip_export }?{ lv_key }|
                       iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      li_log = mo_repo->get_log( ).
      IF li_log IS BOUND AND li_log->count( ) > 0.
        ro_toolbar->add( iv_txt = 'Log'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_log }?{ lv_key }| ).
      ENDIF.
    ENDIF.

    ro_toolbar->add( iv_txt = 'Advanced'
                     io_sub = lo_tb_advanced ) ##NO_TEXT.
    ro_toolbar->add( iv_txt = 'Refresh'
                     iv_act = |{ zif_abapgit_definitions=>c_action-repo_refresh }?{ lv_key }| ).
    ro_toolbar->add( iv_txt = zcl_abapgit_html=>icon( iv_name = 'cog/grey70' )
                     io_sub = build_grid_menu( ) ).

  ENDMETHOD.


  METHOD build_inactive_object_code.

    IF is_item-inactive = abap_true.
      rv_inactive_html_code = zcl_abapgit_html=>icon(
        iv_name  = 'bolt/orange'
        iv_hint  = 'Object or object part is inactive'
        iv_class = 'inactive' ).
    ENDIF.

  ENDMETHOD.


  METHOD build_obj_jump_link.

    DATA: lv_encode TYPE string.

    lv_encode = zcl_abapgit_html_action_utils=>jump_encode( iv_obj_type = is_item-obj_type
                                                    iv_obj_name = is_item-obj_name ).

    rv_html = zcl_abapgit_html=>a( iv_txt = |{ is_item-obj_name }|
                                   iv_act = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.


  METHOD constructor.

    DATA lo_settings TYPE REF TO zcl_abapgit_settings.

    super->constructor( ).

    mo_repo         = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    mv_cur_dir      = '/'. " Root
    mv_hide_files   = zcl_abapgit_persistence_user=>get_instance( )->get_hide_files( ).
    mv_changes_only = zcl_abapgit_persistence_user=>get_instance( )->get_changes_only( ).

    " Read global settings to get max # of objects to be listed
    lo_settings     = zcl_abapgit_persist_settings=>get_instance( )->read( ).
    mv_max_lines    = lo_settings->get_max_lines( ).
    mv_max_setting  = mv_max_lines.

  ENDMETHOD.


  METHOD get_item_class.

    DATA lt_class TYPE TABLE OF string.

    IF is_item-is_dir = abap_true.
      APPEND 'folder' TO lt_class.
    ELSEIF is_item-changes > 0.
      APPEND 'modified' TO lt_class.
    ELSEIF is_item-obj_name IS INITIAL.
      APPEND 'unsupported' TO lt_class.
    ENDIF.

    IF lines( lt_class ) > 0.
      rv_html = | class="{ concat_lines_of( table = lt_class sep = ` ` ) }"|.
    ENDIF.

  ENDMETHOD.


  METHOD get_item_icon.

    CASE is_item-obj_type.
      WHEN 'PROG' OR 'CLAS' OR 'FUGR'.
        rv_html = zcl_abapgit_html=>icon( 'file-code/darkgrey' ).
      WHEN 'W3MI' OR 'W3HT'.
        rv_html = zcl_abapgit_html=>icon( 'file-image/darkgrey' ).
      WHEN ''.
        rv_html = space. " no icon
      WHEN OTHERS.
        rv_html = zcl_abapgit_html=>icon( 'file-alt/darkgrey' ).
    ENDCASE.

    IF is_item-is_dir = abap_true.
      rv_html = zcl_abapgit_html=>icon( 'folder/darkgrey' ).
    ENDIF.

  ENDMETHOD.


  METHOD open_in_master_language.

    CONSTANTS:
      lc_abapgit_tcode TYPE tcode VALUE `ZABAPGIT` ##NO_TEXT.

    DATA:
      lv_master_language TYPE spras,
      lt_spagpa          TYPE STANDARD TABLE OF rfc_spagpa,
      ls_spagpa          LIKE LINE OF lt_spagpa,
      ls_item            TYPE zif_abapgit_definitions=>ty_item,
      lv_subrc           TYPE syst-subrc,
      lv_save_sy_langu   TYPE sy-langu.

    " https://blogs.sap.com/2017/01/13/logon-language-sy-langu-and-rfc/

    lv_master_language = mo_repo->get_dot_abapgit( )->get_master_language( ).

    IF lv_master_language = sy-langu.
      zcx_abapgit_exception=>raise( |Repo already opened in master language| ).
    ENDIF.

    ls_item-obj_name = lc_abapgit_tcode.
    ls_item-obj_type = |TRAN|.

    IF zcl_abapgit_objects=>exists( ls_item ) = abap_false.
      zcx_abapgit_exception=>raise( |Please install the abapGit repository| ).
    ENDIF.

    lv_save_sy_langu = sy-langu.
    SET LOCALE LANGUAGE lv_master_language.

    ls_spagpa-parid  = zif_abapgit_definitions=>c_spagpa_param_repo_key.
    ls_spagpa-parval = mo_repo->get_key( ).
    INSERT ls_spagpa INTO TABLE lt_spagpa.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      DESTINATION 'NONE'
      STARTING NEW TASK 'ABAPGIT'
      EXPORTING
        tcode                   = lc_abapgit_tcode
      TABLES
        spagpa_tab              = lt_spagpa
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        communication_failure   = 3
        system_failure          = 4
        OTHERS                  = 5.

    lv_subrc = sy-subrc.

    SET LOCALE LANGUAGE lv_save_sy_langu.

    IF lv_subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from ABAP4_CALL_TRANSACTION. Subrc = { lv_subrc }| ).
    ENDIF.

    MESSAGE 'Repository opened in a new window' TYPE 'S'.

  ENDMETHOD.


  METHOD render_empty_package.

    DATA: lv_text TYPE string.

    IF mv_changes_only = abap_true.
      lv_text = |No changes|.
    ELSE.
      lv_text = |Empty package|.
    ENDIF.

    rv_html = |<tr class="unsupported"><td class="paddings">|
           && |  <center>{ lv_text }</center>|
           && |</td></tr>|.

  ENDMETHOD.


  METHOD render_head_line.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ro_html.
    lo_toolbar = build_head_menu( iv_lstate = iv_lstate iv_rstate = iv_rstate ).

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( '<table class="w100"><tr>' ).

    IF mv_show_folders = abap_true.
      ro_html->add( |<td class="current_dir">{ mv_cur_dir }</td>| ).
    ENDIF.

    ro_html->add( '<td class="right">' ).
    ro_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_item.

    DATA: lv_link    TYPE string,
          lv_colspan TYPE i.

    CREATE OBJECT ro_html.

    IF iv_render_transports = abap_false.
      lv_colspan = 2.
    ELSE.
      lv_colspan = 3.
    ENDIF.

    ro_html->add( |<tr{ get_item_class( is_item ) }>| ).

    IF is_item-obj_name IS INITIAL AND is_item-is_dir = abap_false.
      ro_html->add( |<td colspan="{ lv_colspan }"></td>|
                 && '<td class="object">'
                 && '<i class="grey">non-code and meta files</i>'
                 && '</td>' ).
    ELSE.
      ro_html->add( |<td class="icon">{ get_item_icon( is_item ) }</td>| ).
      IF iv_render_transports = abap_true.
        ro_html->add( render_item_lock_column( is_item ) ).
      ENDIF.

      IF is_item-is_dir = abap_true. " Subdir
        lv_link = build_dir_jump_link( is_item-path ).
        ro_html->add( |<td class="dir" colspan="2">{ lv_link }</td>| ).
      ELSE.
        lv_link = build_obj_jump_link( is_item ).
        ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ro_html->add( |<td class="object">{ lv_link } { build_inactive_object_code( is_item ) }</td>| ).
      ENDIF.
    ENDIF.

    " Files
    ro_html->add( '<td class="files">' ).
    ro_html->add( render_item_files( is_item ) ).
    ro_html->add( '</td>' ).

    " Command
    IF mo_repo->has_remote_source( ) = abap_true.
      ro_html->add( '<td class="cmd">' ).
      ro_html->add( render_item_command( is_item ) ).
      ro_html->add( '</td>' ).
    ENDIF.

    ro_html->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_item_command.

    DATA: lv_difflink TYPE string,
          ls_file     LIKE LINE OF is_item-files.

    CREATE OBJECT ro_html.

    IF is_item-is_dir = abap_true. " Directory

      ro_html->add( '<div>' ).
      ro_html->add( |<span class="grey">{ is_item-changes } changes</span>| ).
      ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_item-lstate
                                                          iv_rstate = is_item-rstate ) ).
      ro_html->add( '</div>' ).

    ELSEIF is_item-changes > 0.

      IF mv_hide_files = abap_true AND is_item-obj_name IS NOT INITIAL.

        lv_difflink = zcl_abapgit_html_action_utils=>obj_encode(
          iv_key    = mo_repo->get_key( )
          ig_object = is_item ).

        ro_html->add( '<div>' ).
        ro_html->add_a( iv_txt = |view diff ({ is_item-changes })|
                        iv_act = |{ zif_abapgit_definitions=>c_action-go_diff }?{ lv_difflink }| ).
        ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_item-lstate
                                                            iv_rstate = is_item-rstate ) ).
        ro_html->add( '</div>' ).

      ELSE.
        LOOP AT is_item-files INTO ls_file.

          ro_html->add( '<div>' ).
          IF ls_file-is_changed = abap_true.
            lv_difflink = zcl_abapgit_html_action_utils=>file_encode(
              iv_key  = mo_repo->get_key( )
              ig_file = ls_file ).
            ro_html->add_a( iv_txt = 'view diff'
                            iv_act = |{ zif_abapgit_definitions=>c_action-go_diff }?{ lv_difflink }| ).
            ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = ls_file-lstate
                                                                iv_rstate = ls_file-rstate ) ).
          ELSE.
            ro_html->add( '&nbsp;' ).
          ENDIF.
          ro_html->add( '</div>' ).

        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD render_item_files.

    DATA: ls_file     LIKE LINE OF is_item-files.

    CREATE OBJECT ro_html.

    IF mv_hide_files = abap_true AND is_item-obj_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT is_item-files INTO ls_file.
      ro_html->add( |<div>{ ls_file-path && ls_file-filename }</div>| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_item_lock_column.
    DATA: li_cts_api          TYPE REF TO zif_abapgit_cts_api,
          lv_transport        TYPE trkorr,
          lv_transport_string TYPE string,
          lv_icon_html        TYPE string.

    li_cts_api = zcl_abapgit_factory=>get_cts_api( ).

    TRY.
        IF is_item-obj_type IS INITIAL OR is_item-obj_name IS INITIAL OR
           li_cts_api->is_object_type_lockable( is_item-obj_type ) = abap_false OR
           li_cts_api->is_object_locked_in_transport( iv_object_type = is_item-obj_type
                                                      iv_object_name = is_item-obj_name ) = abap_false.
          rv_html = |<td class="icon"></td>|.
        ELSE.
          lv_transport = li_cts_api->get_current_transport_for_obj( iv_object_type             = is_item-obj_type
                                                                    iv_object_name             = is_item-obj_name
                                                                    iv_resolve_task_to_request = abap_false ).
          lv_transport_string = lv_transport.
          lv_icon_html = zcl_abapgit_html=>a( iv_txt = zcl_abapgit_html=>icon( iv_name = 'briefcase/darkgrey'
                                                                               iv_hint = lv_transport_string )
                                              iv_act = |{ zif_abapgit_definitions=>c_action-jump_transport }?| &&
                                                       lv_transport ).
          rv_html = |<td class="icon">| &&
                    |{ lv_icon_html }| &&
                    |</td>|.
        ENDIF.
      CATCH zcx_abapgit_exception.
        ASSERT 1 = 2.
    ENDTRY.
  ENDMETHOD.


  METHOD render_parent_dir.

    CREATE OBJECT ro_html.

    ro_html->add( '<tr class="folder">' ).
    ro_html->add( |<td class="icon">{ zcl_abapgit_html=>icon( 'folder' ) }</td>| ).
    ro_html->add( |<td class="object" colspan="4">{ build_dir_jump_link( '..' ) }</td>| ).
    IF mo_repo->has_remote_source( ) = abap_true.
      ro_html->add( |<td colspan="1"></td>| ). " Dummy for online
    ENDIF.
    ro_html->add( '</tr>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lv_path TYPE string.

    CASE iv_action.
      WHEN c_actions-toggle_hide_files. " Toggle file diplay
        mv_hide_files   = zcl_abapgit_persistence_user=>get_instance( )->toggle_hide_files( ).
        ev_state        = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-change_dir.        " Change dir
        lv_path         = zcl_abapgit_html_action_utils=>dir_decode( iv_getdata ).
        mv_cur_dir      = zcl_abapgit_path=>change_dir( iv_cur_dir = mv_cur_dir iv_cd = lv_path ).
        ev_state        = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-toggle_folders.    " Toggle folder view
        mv_show_folders = boolc( mv_show_folders <> abap_true ).
        mv_cur_dir      = '/'. " Root
        ev_state        = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-toggle_changes.    " Toggle changes only view
        mv_changes_only = zcl_abapgit_persistence_user=>get_instance( )->toggle_changes_only( ).
        ev_state        = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-display_more.      " Increase MAX lines limit
        mv_max_lines    = mv_max_lines + mv_max_setting.
        ev_state        = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_open_in_master_lang.
        open_in_master_language( ).
        ev_state        = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA: lt_repo_items        TYPE zif_abapgit_definitions=>tt_repo_items,
          lo_browser           TYPE REF TO zcl_abapgit_repo_content_list,
          lx_error             TYPE REF TO zcx_abapgit_exception,
          lv_lstate            TYPE char1,
          lv_rstate            TYPE char1,
          lv_max               TYPE abap_bool,
          lv_max_str           TYPE string,
          lv_add_str           TYPE string,
          li_log               TYPE REF TO zif_abapgit_log,
          lv_render_transports TYPE abap_bool.


    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    " Reinit, for the case of type change
    mo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).

    CREATE OBJECT ro_html TYPE zcl_abapgit_html.

    TRY.

        lv_render_transports = zcl_abapgit_factory=>get_cts_api(
          )->is_chrec_possible_for_package( mo_repo->get_package( ) ).

        CREATE OBJECT lo_browser
          EXPORTING
            io_repo = mo_repo.

        lt_repo_items = lo_browser->list( iv_path         = mv_cur_dir
                                          iv_by_folders   = mv_show_folders
                                          iv_changes_only = mv_changes_only ).

        LOOP AT lt_repo_items ASSIGNING <ls_item>.
          zcl_abapgit_state=>reduce( EXPORTING iv_cur = <ls_item>-lstate
                                     CHANGING cv_prev = lv_lstate ).
          zcl_abapgit_state=>reduce( EXPORTING iv_cur = <ls_item>-rstate
                                     CHANGING cv_prev = lv_rstate ).
        ENDLOOP.

        ro_html->add( render_head_line( iv_lstate = lv_lstate
                                        iv_rstate = lv_rstate ) ).

        li_log = lo_browser->get_log( ).
        IF mo_repo->is_offline( ) = abap_false AND li_log->count( ) > 0.
          ro_html->add( '<div class="log">' ).
          ro_html->add( zcl_abapgit_log_viewer=>to_html( li_log ) ). " shows eg. list of unsupported objects
          ro_html->add( '</div>' ).
        ENDIF.

        ro_html->add( '<div class="repo_container">' ).

        " Offline match banner
        IF mo_repo->is_offline( ) = abap_true
            AND mo_repo->has_remote_source( ) = abap_true
            AND lv_lstate IS INITIAL AND lv_rstate IS INITIAL.
          ro_html->add(
            |<div class="repo_banner panel success">|
            && |ZIP source is attached and completely <b>matches</b> to the local state|
            && |</div>| ).
        ENDIF.

        " Repo content table
        ro_html->add( '<table class="repo_tab">' ).

        IF zcl_abapgit_path=>is_root( mv_cur_dir ) = abap_false.
          ro_html->add( render_parent_dir( ) ).
        ENDIF.

        IF lines( lt_repo_items ) = 0.
          ro_html->add( render_empty_package( ) ).
        ELSE.
          LOOP AT lt_repo_items ASSIGNING <ls_item>.
            IF mv_max_lines > 0 AND sy-tabix > mv_max_lines.
              lv_max = abap_true.
              EXIT. " current loop
            ENDIF.
            ro_html->add( render_item( is_item = <ls_item> iv_render_transports = lv_render_transports ) ).
          ENDLOOP.
        ENDIF.

        ro_html->add( '</table>' ).

        IF lv_max = abap_true.
          ro_html->add( '<div class = "dummydiv">' ).
          IF mv_max_lines = 1.
            lv_max_str = '1 object'.
          ELSE.
            lv_max_str = |first { mv_max_lines } objects|.
          ENDIF.
          lv_add_str = |+{ mv_max_setting }|.
          ro_html->add( |Only { lv_max_str } shown in list. Display {
            zcl_abapgit_html=>a( iv_txt = lv_add_str iv_act = c_actions-display_more )
            } more. (Set in Advanced > {
            zcl_abapgit_html=>a( iv_txt = 'Settings' iv_act = zif_abapgit_definitions=>c_action-go_settings )
            } )| ).
          ro_html->add( '</div>' ).
        ENDIF.

        ro_html->add( '</div>' ).

      CATCH zcx_abapgit_exception INTO lx_error.
        ro_html->add( render_head_line( iv_lstate = lv_lstate iv_rstate = lv_rstate ) ).
        ro_html->add( zcl_abapgit_gui_chunk_lib=>render_error( ix_error = lx_error ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
