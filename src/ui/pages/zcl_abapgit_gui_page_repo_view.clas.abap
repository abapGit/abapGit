CLASS zcl_abapgit_gui_page_repo_view DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable.

    CONSTANTS:
      BEGIN OF c_actions,
        change_dir        TYPE string VALUE 'change_dir' ##NO_TEXT,
        toggle_hide_files TYPE string VALUE 'toggle_hide_files' ##NO_TEXT,
        toggle_folders    TYPE string VALUE 'toggle_folders' ##NO_TEXT,
        toggle_changes    TYPE string VALUE 'toggle_changes' ##NO_TEXT,
        toggle_diff_first TYPE string VALUE 'toggle_diff_first ' ##NO_TEXT,
        display_more      TYPE string VALUE 'display_more' ##NO_TEXT,
        go_data           TYPE string VALUE 'go_data',
        go_unit           TYPE string VALUE 'go_unit',
      END OF c_actions .

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA mo_repo_aggregated_state TYPE REF TO zcl_abapgit_item_state.
    DATA mv_cur_dir TYPE string .
    DATA mv_hide_files TYPE abap_bool .
    DATA mv_max_lines TYPE i .
    DATA mv_max_setting TYPE i .
    DATA mv_show_folders TYPE abap_bool .
    DATA mv_changes_only TYPE abap_bool .
    DATA mv_order_by TYPE string .
    DATA mv_order_descending TYPE abap_bool .
    DATA mv_diff_first TYPE abap_bool .
    DATA mv_key TYPE zif_abapgit_persistence=>ty_value .
    DATA mv_are_changes_recorded_in_tr TYPE abap_bool .

    METHODS render_head_line
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS build_view_dropdown
      RETURNING
        VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception .
    METHODS render_item
      IMPORTING
        !is_item              TYPE zif_abapgit_definitions=>ty_repo_item
        !iv_render_transports TYPE abap_bool
      RETURNING
        VALUE(ri_html)        TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_item_files
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_item_command
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_file_command
      IMPORTING
        !is_file       TYPE zif_abapgit_definitions=>ty_repo_file
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS get_item_class
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_repo_item
        iv_is_object_row TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_html)   TYPE string .
    METHODS render_item_transport
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_parent_dir
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS build_dir_jump_link
      IMPORTING
        !iv_path       TYPE string
      RETURNING
        VALUE(rv_html) TYPE string .
    METHODS build_inactive_object_code
      IMPORTING
        !is_item                     TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(rv_inactive_html_code) TYPE string .
    METHODS build_srcsystem_code
      IMPORTING
        !is_item                      TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(rv_srcsystem_html_code) TYPE string .
    METHODS build_origlang_code
      IMPORTING
        !is_item                      TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(rv_html_code) TYPE string .
    METHODS open_in_main_language
      RAISING
        zcx_abapgit_exception .
    METHODS render_order_by
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS apply_order_by
      CHANGING
        !ct_repo_items TYPE zif_abapgit_definitions=>ty_repo_item_tt .
    METHODS build_branch_dropdown
      RETURNING
        VALUE(ro_branch_dropdown) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception .
    METHODS build_tag_dropdown
      RETURNING
        VALUE(ro_tag_dropdown) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception .
    METHODS build_advanced_dropdown
      RETURNING
        VALUE(ro_advanced_dropdown) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception .
    METHODS build_main_toolbar
      RETURNING
        VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception .
    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS is_repo_lang_logon_lang
      RETURNING
        VALUE(rv_repo_lang_is_logon_lang) TYPE abap_bool .

    METHODS render_item_changed_by
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS order_files
      CHANGING
        ct_files TYPE zif_abapgit_definitions=>ty_repo_file_tt.

    METHODS get_crossout
      IMPORTING
        !iv_authorization  TYPE zif_abapgit_auth=>ty_authorization OPTIONAL
        !iv_protected      TYPE abap_bool DEFAULT abap_false
        !iv_strong         TYPE abap_bool DEFAULT abap_false
          PREFERRED PARAMETER iv_authorization
      RETURNING
        VALUE(rv_crossout) LIKE zif_abapgit_html=>c_html_opt-crossout.

    METHODS check_branch
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_repo_view IMPLEMENTATION.


  METHOD apply_order_by.

    DATA:
      lt_sort                        TYPE abap_sortorder_tab,
      ls_sort                        LIKE LINE OF lt_sort,
      lt_non_code_and_metadata_items LIKE ct_repo_items,
      lt_code_items                  LIKE ct_repo_items,
      lt_diff_items                  LIKE ct_repo_items.

    FIELD-SYMBOLS:
      <ls_repo_item> TYPE zif_abapgit_definitions=>ty_repo_item.

    IF mv_order_by IS INITIAL.
      RETURN.
    ENDIF.

    " we want to preserve non-code and metadata files at the top,
    " so we isolate them and and sort only the code artifacts
    LOOP AT ct_repo_items ASSIGNING <ls_repo_item>.

      IF <ls_repo_item>-obj_type IS INITIAL AND <ls_repo_item>-is_dir = abap_false.
        INSERT <ls_repo_item> INTO TABLE lt_non_code_and_metadata_items.
      ELSE.
        INSERT <ls_repo_item> INTO TABLE lt_code_items.
      ENDIF.

    ENDLOOP.

    IF mv_diff_first = abap_true.
      " fix diffs on the top, right after non-code and metadata
      LOOP AT lt_code_items ASSIGNING <ls_repo_item>
                            WHERE changes > 0.
        INSERT <ls_repo_item> INTO TABLE lt_diff_items.
      ENDLOOP.

      DELETE lt_code_items WHERE changes > 0.
    ENDIF.

    CLEAR: ct_repo_items.

    ls_sort-descending = mv_order_descending.
    ls_sort-astext     = abap_true.
    ls_sort-name       = mv_order_by.
    INSERT ls_sort INTO TABLE lt_sort.

    " Combine state fields for order of 'Status' column
    IF mv_order_by = 'LSTATE'.
      ls_sort-name = 'RSTATE'.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    " Use object name as secondary sort criteria
    IF mv_order_by <> 'OBJ_NAME'.
      ls_sort-name = 'OBJ_NAME'.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    SORT lt_code_items STABLE BY (lt_sort).
    SORT lt_diff_items STABLE BY (lt_sort).

    INSERT LINES OF lt_non_code_and_metadata_items INTO TABLE ct_repo_items.
    INSERT LINES OF lt_diff_items INTO TABLE ct_repo_items.
    INSERT LINES OF lt_code_items INTO TABLE ct_repo_items.

    " Files are listed under the object names so we always sort them by name
    LOOP AT ct_repo_items ASSIGNING <ls_repo_item>.
      order_files( CHANGING ct_files = <ls_repo_item>-files ).
    ENDLOOP.

  ENDMETHOD.


  METHOD build_advanced_dropdown.

    CREATE OBJECT ro_advanced_dropdown.

    ro_advanced_dropdown->add( iv_txt = 'Activate Objects'
                               iv_act = |{ zif_abapgit_definitions=>c_action-repo_activate_objects }?key={ mv_key }| ).

    IF mo_repo->is_offline( ) = abap_false. " Online ?
      ro_advanced_dropdown->add(
        iv_txt = 'Transport to Branch'
        iv_act = |{ zif_abapgit_definitions=>c_action-repo_transport_to_branch }?key={ mv_key }|
        iv_opt = get_crossout( zif_abapgit_auth=>c_authorization-transport_to_branch ) ).
    ENDIF.

    IF mv_are_changes_recorded_in_tr = abap_true.
      ro_advanced_dropdown->add(
        iv_txt = 'Add All Objects to Transport'
        iv_act = |{ zif_abapgit_definitions=>c_action-repo_add_all_obj_to_trans_req }?key={ mv_key }| ).
    ENDIF.
    IF mo_repo->is_offline( ) = abap_true.
      ro_advanced_dropdown->add( iv_txt = 'Export by Transport'
                                 iv_act = |{ zif_abapgit_definitions=>c_action-zip_export_transport }?key={ mv_key }| ).
    ELSE.
      ro_advanced_dropdown->add( iv_txt = 'Stage by Transport'
                                 iv_act = |{ zif_abapgit_definitions=>c_action-go_stage_transport }?key={ mv_key }| ).
    ENDIF.

    ro_advanced_dropdown->add( iv_txt = 'Quality Assurance'
                               iv_typ = zif_abapgit_html=>c_action_type-separator ).

    ro_advanced_dropdown->add( iv_txt = 'Syntax Check'
                               iv_act = |{ zif_abapgit_definitions=>c_action-repo_syntax_check }?key={ mv_key }| ).
    ro_advanced_dropdown->add( iv_txt = 'Unit Test'
                               iv_act = |{ c_actions-go_unit }| ).
    ro_advanced_dropdown->add( iv_txt = 'Run Code Inspector'
                               iv_act = |{ zif_abapgit_definitions=>c_action-repo_code_inspector }?key={ mv_key }| ).

    ro_advanced_dropdown->add( iv_txt = 'Very Advanced'
                               iv_typ = zif_abapgit_html=>c_action_type-separator ).

    ro_advanced_dropdown->add( iv_txt = 'Update Local Checksums'
                               iv_act = |{ zif_abapgit_definitions=>c_action-repo_refresh_checksums }?key={ mv_key }|
                               iv_opt = get_crossout( zif_abapgit_auth=>c_authorization-update_local_checksum ) ).

    ro_advanced_dropdown->add( iv_txt = 'Beta - Data'
                               iv_act = |{ c_actions-go_data }?key={ mv_key }| ).

    IF is_repo_lang_logon_lang( ) = abap_false AND zcl_abapgit_services_abapgit=>get_abapgit_tcode( ) IS NOT INITIAL.
      ro_advanced_dropdown->add(
        iv_txt = 'Open in Main Language'
        iv_act = |{ zif_abapgit_definitions=>c_action-repo_open_in_master_lang }?key={ mv_key }| ).
    ENDIF.

    ro_advanced_dropdown->add( iv_txt = 'Danger'
                               iv_typ = zif_abapgit_html=>c_action_type-separator ).

    ro_advanced_dropdown->add( iv_txt   = 'Remove'
                               iv_title = `Remove abapGit's records of the repository (the system's `
                                          && `development objects will remain unaffected)`
                               iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remove }?key={ mv_key }| ).

    ro_advanced_dropdown->add( iv_txt   = 'Uninstall'
                               iv_title = `Delete all development objects belonging to this package `
                                          && `(and subpackages) from the system`
                               iv_act   = |{ zif_abapgit_definitions=>c_action-repo_purge }?key={ mv_key }|
                               iv_opt   = get_crossout(
                                            iv_authorization = zif_abapgit_auth=>c_authorization-uninstall
                                            iv_protected     = abap_true ) ).

  ENDMETHOD.


  METHOD build_branch_dropdown.

    CREATE OBJECT ro_branch_dropdown.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    ro_branch_dropdown->add( iv_txt = 'Switch'
                             iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_switch }?key={ mv_key }| ).
    ro_branch_dropdown->add( iv_txt = 'Create'
                             iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_create }?key={ mv_key }| ).
    ro_branch_dropdown->add( iv_txt = 'Delete'
                             iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_delete }?key={ mv_key }| ).
    ro_branch_dropdown->add( iv_txt = 'Merge'
                             iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_merge }?key={ mv_key }| ).

  ENDMETHOD.


  METHOD build_dir_jump_link.

    DATA lv_path   TYPE string.
    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO zif_abapgit_html.

    CREATE OBJECT li_html TYPE zcl_abapgit_html.

    lv_path = iv_path.
    REPLACE FIRST OCCURRENCE OF mv_cur_dir IN lv_path WITH ''.
    lv_encode = zcl_abapgit_html_action_utils=>dir_encode( lv_path ).

    " remove leading and trailing / for display
    IF lv_path <> '/'.
      IF lv_path(1) = '/'.
        lv_path = lv_path+1.
      ENDIF.
      IF substring( val = reverse( lv_path )
                    len = 1 ) = '/'.
        lv_path = substring( val = lv_path
                             len = strlen( lv_path ) - 1 ).
      ENDIF.
    ENDIF.

    rv_html = li_html->a(
      iv_txt = lv_path
      iv_act = |{ c_actions-change_dir }?{ lv_encode }| ).

  ENDMETHOD.


  METHOD build_inactive_object_code.

    IF is_item-inactive = abap_true.
      rv_inactive_html_code = zcl_abapgit_html=>icon(
        iv_name  = 'bolt/orange'
        iv_hint  = 'Object or object part is inactive'
        iv_class = 'inactive' ).
    ENDIF.

  ENDMETHOD.


  METHOD build_main_toolbar.

    DATA:
      li_log TYPE REF TO zif_abapgit_log.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-repo'.

    IF mo_repo->is_offline( ) = abap_false.
      " online repo

      IF mo_repo_aggregated_state->is_unchanged( ) = abap_false. " Any changes
        ro_toolbar->add( iv_txt = 'Pull'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_pull }?key={ mv_key }|
                         iv_opt = get_crossout( iv_protected = abap_true
                                                iv_strong    = abap_true ) ).
        ro_toolbar->add( iv_txt = 'Stage'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_stage }?key={ mv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Patch'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_patch }?key={ mv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Diff'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_repo_diff }?key={ mv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      li_log = mo_repo->get_log( ).
      IF li_log IS BOUND AND li_log->count( ) > 0.
        ro_toolbar->add( iv_txt = 'Log'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_log }?key={ mv_key }| ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Branch'
                       io_sub = build_branch_dropdown( ) ).
      ro_toolbar->add( iv_txt = 'Tag'
                       io_sub = build_tag_dropdown( ) ).

    ELSE.
      " offline repo

      IF mo_repo->has_remote_source( ) = abap_true AND mo_repo_aggregated_state->is_unchanged( ) = abap_false.
        ro_toolbar->add( iv_txt = 'Pull <sup>zip</sup>'
                         iv_act = |{ zif_abapgit_definitions=>c_action-git_pull }?key={ mv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Diff'
                         iv_act = |{ zif_abapgit_definitions=>c_action-go_repo_diff }?key={ mv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Import <sup>zip</sup>'
                       iv_act = |{ zif_abapgit_definitions=>c_action-zip_import }?key={ mv_key }|
                       iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      IF mo_repo->get_local_settings( )-write_protected = abap_true.
        ro_toolbar->add( iv_txt = 'Compare <sup>rfc</sup>'
                         iv_act = |{ zif_abapgit_definitions=>c_action-rfc_compare }?key={ mv_key }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Export <sup>zip</sup>'
                       iv_act = |{ zif_abapgit_definitions=>c_action-zip_export }?key={ mv_key }|
                       iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      li_log = mo_repo->get_log( ).
      IF li_log IS BOUND AND li_log->count( ) > 0.
        ro_toolbar->add( iv_txt = 'Log'
                         iv_act = |{ zif_abapgit_definitions=>c_action-repo_log }?key={ mv_key }| ).
      ENDIF.

    ENDIF.

    ro_toolbar->add( iv_txt = 'Advanced'
                     io_sub = build_advanced_dropdown( ) ).

    ro_toolbar->add( iv_txt = 'View'
                     io_sub = build_view_dropdown( ) ).

    ro_toolbar->add( iv_txt = 'Refresh'
                     iv_act = |{ zif_abapgit_definitions=>c_action-repo_refresh }?key={ mv_key }|
                     iv_opt = zif_abapgit_html=>c_html_opt-strong ).

    ro_toolbar->add( iv_txt   = 'Settings'
                     iv_act   = |{ zif_abapgit_definitions=>c_action-repo_settings }?key={ mv_key }|
                     iv_opt   = zif_abapgit_html=>c_html_opt-strong
                     iv_title = `Repository Settings` ).

  ENDMETHOD.


  METHOD build_origlang_code.

    IF is_item-origlang IS NOT INITIAL AND is_item-origlang <> mo_repo->get_dot_abapgit( )->get_main_language( ).
      rv_html_code = zcl_abapgit_html=>icon(
        iv_name  = 'language-solid/grey'
        iv_hint  = |Original language: { is_item-origlang }|
        iv_class = 'cursor-pointer' ).
    ENDIF.

  ENDMETHOD.


  METHOD build_srcsystem_code.

    IF is_item-srcsystem IS NOT INITIAL AND is_item-srcsystem <> sy-sysid.
      rv_srcsystem_html_code = zcl_abapgit_html=>icon(
        iv_name  = 'server-solid/grey'
        iv_hint  = |Original system: { is_item-srcsystem }|
        iv_class = 'cursor-pointer' ).
    ENDIF.

  ENDMETHOD.


  METHOD build_tag_dropdown.

    CREATE OBJECT ro_tag_dropdown.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    ro_tag_dropdown->add( iv_txt = 'Switch'
                          iv_act = |{ zif_abapgit_definitions=>c_action-git_tag_switch }?key={ mv_key }| ).
    ro_tag_dropdown->add( iv_txt = 'Create'
                          iv_act = |{ zif_abapgit_definitions=>c_action-git_tag_create }?key={ mv_key }| ).
    ro_tag_dropdown->add( iv_txt = 'Delete'
                          iv_act = |{ zif_abapgit_definitions=>c_action-git_tag_delete }?key={ mv_key }| ).


  ENDMETHOD.


  METHOD build_view_dropdown.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Changes First'
      iv_chk = mv_diff_first
      iv_act = c_actions-toggle_diff_first ).

    ro_toolbar->add(
      iv_txt = 'Changes Only'
      iv_chk = mv_changes_only
      iv_act = c_actions-toggle_changes ).

    ro_toolbar->add(
      iv_txt = 'File Paths'
      iv_chk = boolc( NOT mv_hide_files = abap_true )
      iv_act = c_actions-toggle_hide_files ).

    ro_toolbar->add(
      iv_txt = 'Folders'
      iv_chk = mv_show_folders
      iv_act = c_actions-toggle_folders ).

  ENDMETHOD.


  METHOD check_branch.

    DATA lo_repo TYPE REF TO zif_abapgit_repo_online.

    IF mo_repo->is_offline( ) = abap_false.
      lo_repo ?= mo_repo.
      lo_repo->check_for_valid_branch( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA: lo_settings         TYPE REF TO zcl_abapgit_settings,
          lx_error            TYPE REF TO zcx_abapgit_exception,
          lo_persistence_user TYPE REF TO zif_abapgit_persist_user.

    super->constructor( ).

    TRY.
        lo_persistence_user = zcl_abapgit_persistence_user=>get_instance( ).

        mv_key = iv_key.
        mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
        mv_cur_dir = '/'. " Root

        mv_hide_files = lo_persistence_user->get_hide_files( ).
        mv_changes_only = lo_persistence_user->get_changes_only( ).
        mv_order_by = lo_persistence_user->get_order_by( ).
        mv_order_descending = lo_persistence_user->get_order_descending( ).
        mv_diff_first = lo_persistence_user->get_diff_first( ).
        mv_show_folders = lo_persistence_user->get_show_folders( ).

        " Read global settings to get max # of objects to be listed
        lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
        mv_max_lines = lo_settings->get_max_lines( ).
        mv_max_setting = mv_max_lines.

      CATCH zcx_abapgit_exception INTO lx_error.
        " Reset 'last shown repo' so next start will go to repo overview
        " and allow troubleshooting of issue
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).

        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_repo_view.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key = iv_key.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Repository'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD get_crossout.
    IF iv_strong = abap_true.
      rv_crossout = zif_abapgit_html=>c_html_opt-strong.
    ENDIF.
    IF iv_protected = abap_true AND mo_repo->get_local_settings( )-write_protected = abap_true.
      rv_crossout = zif_abapgit_html=>c_html_opt-crossout.
    ENDIF.
    IF iv_authorization IS NOT INITIAL AND zcl_abapgit_auth=>is_allowed( iv_authorization ) = abap_false.
      rv_crossout = zif_abapgit_html=>c_html_opt-crossout.
    ENDIF.
  ENDMETHOD.


  METHOD get_item_class.

    DATA lt_class TYPE TABLE OF string.

    IF iv_is_object_row = abap_true.
      APPEND 'object_row' TO lt_class.
    ELSE.
      APPEND 'file_row' TO lt_class.
    ENDIF.

    IF is_item-is_dir = abap_true.
      APPEND 'folder' TO lt_class.
    ELSEIF is_item-changes > 0.
      APPEND 'modified' TO lt_class.
    ELSEIF is_item-obj_name IS INITIAL.
      APPEND 'unsupported' TO lt_class.
    ENDIF.

    IF lines( lt_class ) > 0.
      rv_html = | class="{ concat_lines_of( table = lt_class
                                            sep = ` ` ) }"|.
    ENDIF.

  ENDMETHOD.


  METHOD is_repo_lang_logon_lang.
    rv_repo_lang_is_logon_lang = boolc( mo_repo->get_dot_abapgit( )->get_main_language( ) = sy-langu ).
  ENDMETHOD.


  METHOD open_in_main_language.

    DATA:
      lv_main_language TYPE spras,
      ls_item          TYPE zif_abapgit_definitions=>ty_item,
      lv_tcode         TYPE tcode.

    lv_main_language = mo_repo->get_dot_abapgit( )->get_main_language( ).
    lv_tcode = zcl_abapgit_services_abapgit=>get_abapgit_tcode( ).
    ASSERT lv_tcode IS NOT INITIAL.

    IF lv_main_language = sy-langu.
      zcx_abapgit_exception=>raise( |Repo already opened in main language| ).
    ENDIF.

    ls_item-obj_name = lv_tcode.
    ls_item-obj_type = |TRAN|.

    IF zcl_abapgit_objects=>exists( ls_item ) = abap_false.
      zcx_abapgit_exception=>raise( |Please install the abapGit repository| ).
    ENDIF.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_abapgit(
      iv_language = lv_main_language
      iv_key      = mo_repo->get_key( ) ).

  ENDMETHOD.


  METHOD order_files.

    DATA:
      lt_sort TYPE abap_sortorder_tab,
      ls_sort LIKE LINE OF lt_sort.

    IF lines( ct_files ) = 0.
      RETURN.
    ENDIF.

    ls_sort-descending = mv_order_descending.
    ls_sort-astext     = abap_true.
    ls_sort-name       = 'PATH'.
    INSERT ls_sort INTO TABLE lt_sort.

    ls_sort-descending = mv_order_descending.
    ls_sort-astext     = abap_true.
    ls_sort-name       = 'FILENAME'.
    INSERT ls_sort INTO TABLE lt_sort.

    SORT ct_files STABLE BY (lt_sort).

  ENDMETHOD.


  METHOD render_file_command.

    DATA: lv_difflink TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div>' ).
    IF is_file-is_changed = abap_true.
      lv_difflink = zcl_abapgit_html_action_utils=>file_encode(
        iv_key  = mo_repo->get_key( )
        ig_file = is_file ).
      ri_html->add_a( iv_txt = 'diff'
                      iv_act = |{ zif_abapgit_definitions=>c_action-go_file_diff }?{ lv_difflink }| ).
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_file-lstate
                                                                  iv_rstate = is_file-rstate ) ).
    ELSE.
      ri_html->add( '&nbsp;' ).
    ENDIF.
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_head_line.

    DATA:
      lo_toolbar      TYPE REF TO zcl_abapgit_html_toolbar,
      ls_settings     TYPE zif_abapgit_definitions=>ty_s_user_settings,
      lo_label_colors TYPE REF TO zcl_abapgit_string_map,
      lt_labels       TYPE string_table.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    lo_toolbar = build_main_toolbar( ).

    ri_html->add( '<div class="paddings">' ).
    ri_html->add( '<table class="w100"><tr>' ).

    IF mv_show_folders = abap_true.
      ri_html->add( '<td class="current_dir">' ).
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_path( mv_cur_dir ) ).
      ri_html->add( '</td>' ).
    ENDIF.

    lt_labels = zcl_abapgit_repo_labels=>split( mo_repo->ms_data-local_settings-labels ).

    IF lines( lt_labels ) > 0.
      ls_settings = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_user_settings( ).
      lo_label_colors = zcl_abapgit_repo_labels=>split_colors_into_map( ls_settings-label_colors ).

      ri_html->td(
        iv_content = zcl_abapgit_gui_chunk_lib=>render_label_list(
                       it_labels = lt_labels
                       io_label_colors = lo_label_colors )
        iv_class   = 'labels' ).
    ENDIF.

    ri_html->add( '<td class="right">' ).
    ri_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ri_html->add( '</td>' ).
    ri_html->add( '</tr></table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_item.

    DATA: lv_link    TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<tr{ get_item_class( is_item = is_item
                                        iv_is_object_row = abap_true ) }>| ).

    IF is_item-obj_name IS INITIAL AND is_item-is_dir = abap_false.
      ri_html->add( |<td colspan="2"></td>|
                 && '<td class="object">'
                 && '<i class="grey">non-code and meta files</i>'
                 && '</td>' ).
    ELSE.
      ri_html->add( |<td class="icon">{ zcl_abapgit_gui_chunk_lib=>get_item_icon( is_item ) }</td>| ).

      IF is_item-is_dir = abap_true. " Subdir
        lv_link = build_dir_jump_link( is_item-path ).
        ri_html->add( |<td class="dir" colspan="2">{ lv_link }</td>| ).
      ELSE.
        lv_link = zcl_abapgit_gui_chunk_lib=>get_item_link( is_item ).
        ri_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ri_html->add( |<td class="object">{ lv_link } { build_inactive_object_code( is_item )
                      } { build_srcsystem_code( is_item ) } { build_origlang_code( is_item ) }</td>| ).
      ENDIF.
    ENDIF.

    " Changed by
    ri_html->add( '<td class="user">' ).
    ri_html->add( render_item_changed_by( is_item ) ).
    ri_html->add( '</td>' ).

    IF iv_render_transports = abap_true.
      ri_html->add( render_item_transport( is_item ) ).
    ENDIF.

    " Command
    ri_html->add( '<td class="cmd">' ).
    IF mo_repo->has_remote_source( ) = abap_true.
      ri_html->add( render_item_command( is_item ) ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '</tr>' ).

    ri_html->add( render_item_files( is_item ) ).

  ENDMETHOD.


  METHOD render_item_changed_by.
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF is_item-changes = 0 OR is_item-changed_by IS INITIAL.
      ri_html->add( '&nbsp;' ).
    ELSE.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_user_name( is_item-changed_by ) ).
    ENDIF.

  ENDMETHOD.


  METHOD render_item_command.

    DATA lv_difflink TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF is_item-is_dir = abap_true. " Directory
      ri_html->add( '<div>' ).
      ri_html->add( |<span class="grey">{ is_item-changes } changes</span>| ).
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_item-lstate
                                                                  iv_rstate = is_item-rstate ) ).
      ri_html->add( '</div>' ).

    ELSEIF is_item-changes > 0.
      IF mv_hide_files = abap_true AND is_item-obj_name IS NOT INITIAL.

        lv_difflink = zcl_abapgit_html_action_utils=>obj_encode(
          iv_key    = mo_repo->get_key( )
          ig_object = is_item ).

        ri_html->add( '<div>' ).
        ri_html->add_a( iv_txt = |diff ({ is_item-changes })|
                        iv_act = |{ zif_abapgit_definitions=>c_action-go_file_diff }?{ lv_difflink }| ).
        ri_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_item-lstate
                                                                    iv_rstate = is_item-rstate ) ).
        ri_html->add( '</div>' ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD render_item_files.

    DATA: ls_file LIKE LINE OF is_item-files.
    DATA li_exit TYPE REF TO zif_abapgit_exit.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF mv_hide_files = abap_true AND is_item-obj_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    li_exit = zcl_abapgit_exit=>get_instance( ).

    LOOP AT is_item-files INTO ls_file.
      ri_html->add( |<tr{ get_item_class( is_item ) }>| ).

      ri_html->add( |<td class="icon"></td>| ).

      ri_html->add( |<td class="type"></td>| ).
      ri_html->add( |<td class="filename darkgrey">| ).

      IF mv_show_folders = abap_true.
        ri_html->add( |<div>{ li_exit->adjust_display_filename( ls_file-filename ) }</div>| ).
      ELSE.
        ri_html->add( |<div>{ li_exit->adjust_display_filename( ls_file-path && ls_file-filename ) }</div>| ).
      ENDIF.

      ri_html->add( |</td>| ).

      " Changed by (not applicable to file)
      ri_html->add( '<td class="user">' ).
      ri_html->add( '</td>' ).

      " Transport (not applicable to file)
      IF mv_are_changes_recorded_in_tr = abap_true.
        ri_html->add( `<td></td>` ).
      ENDIF.

      " Command
      ri_html->add( '<td class="cmd">' ).
      IF mo_repo->has_remote_source( ) = abap_true.
        ri_html->add( render_file_command( ls_file ) ).
      ENDIF.
      ri_html->add( '</td>' ).

      ri_html->add( '</tr>' ).

    ENDLOOP.

  ENDMETHOD.


  METHOD render_item_transport.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<td class="transport">' ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_transport( is_item-transport ) ).

    ri_html->add( '</td>' ).

  ENDMETHOD.


  METHOD render_order_by.

    DATA:
      lv_icon     TYPE string,
      lt_col_spec TYPE zif_abapgit_definitions=>ty_col_spec_tt,
      ls_col_spec TYPE zif_abapgit_definitions=>ty_col_spec.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    " icon
    APPEND INITIAL LINE TO lt_col_spec.

    ls_col_spec-tech_name = 'OBJ_TYPE'.
    ls_col_spec-display_name = 'Type'.
    ls_col_spec-allow_order_by = abap_true.
    APPEND ls_col_spec TO lt_col_spec.

    ls_col_spec-tech_name = 'OBJ_NAME'.
    ls_col_spec-display_name = 'Name'.
    ls_col_spec-allow_order_by = abap_true.
    APPEND ls_col_spec TO lt_col_spec.

    ls_col_spec-tech_name = 'CHANGED_BY'.
    ls_col_spec-display_name = 'Changed by'.
    ls_col_spec-allow_order_by = abap_true.
    APPEND ls_col_spec TO lt_col_spec.

    IF mv_are_changes_recorded_in_tr = abap_true.
      ls_col_spec-tech_name = 'TRANSPORT'.
      ls_col_spec-display_name = 'Transport'.
      ls_col_spec-allow_order_by = abap_true.
      APPEND ls_col_spec TO lt_col_spec.
    ENDIF.

    ls_col_spec-tech_name = 'LSTATE'.
    ls_col_spec-display_name = 'Status'.
    ls_col_spec-allow_order_by = abap_true.
    ls_col_spec-css_class = 'cmd'.
    APPEND ls_col_spec TO lt_col_spec.

    ri_html->add( |<thead>| ).
    ri_html->add( |<tr>| ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_order_by_header_cells(
      it_col_spec         = lt_col_spec
      iv_order_by         = mv_order_by
      iv_order_descending = mv_order_descending ) ).

    IF mv_diff_first = abap_true.
      lv_icon = 'check/blue'.
    ELSE.
      lv_icon = 'check/grey'.
    ENDIF.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_parent_dir.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<tr class="folder">' ).
    ri_html->add( |<td class="icon">{ ri_html->icon( 'folder' ) }</td>| ).
    ri_html->add( |<td class="dir" colspan="4">{ build_dir_jump_link( '..' ) }</td>| ).
    IF mo_repo->has_remote_source( ) = abap_true.
      ri_html->add( |<td colspan="1"></td>| ). " Dummy for online
    ENDIF.
    ri_html->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_palette(
      iv_action = zif_abapgit_definitions=>c_action-go_repo ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_path TYPE string.
    DATA lv_key TYPE zif_abapgit_persistence=>ty_value.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_repo. " Switch to another repo
        rs_handled-page  = create( |{ ii_event->query( )->get( 'KEY' ) }| ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.

      WHEN c_actions-go_data.
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_data
          EXPORTING
            iv_key = |{ ii_event->query( )->get( 'KEY' ) }|.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-go_unit.
        rs_handled-page  = zcl_abapgit_gui_page_runit=>create( mo_repo ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-toggle_hide_files. " Toggle file diplay
        mv_hide_files    = zcl_abapgit_persistence_user=>get_instance( )->toggle_hide_files( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-change_dir.        " Change dir
        lv_path         = ii_event->query( )->get( 'PATH' ).
        mv_cur_dir = zcl_abapgit_path=>change_dir(
          iv_cur_dir = mv_cur_dir
          iv_cd      = lv_path ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-toggle_folders.    " Toggle folder view
        mv_show_folders = zcl_abapgit_persistence_user=>get_instance( )->toggle_show_folders( ).
        mv_cur_dir      = '/'. " Root
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-toggle_changes.    " Toggle changes only view
        mv_changes_only = zcl_abapgit_persistence_user=>get_instance( )->toggle_changes_only( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-toggle_diff_first.
        mv_diff_first = zcl_abapgit_persistence_user=>get_instance( )->set_diff_first(
          boolc( mv_diff_first = abap_false ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-display_more.      " Increase MAX lines limit
        mv_max_lines    = mv_max_lines + mv_max_setting.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-change_order_by.
        mv_order_by = zcl_abapgit_persistence_user=>get_instance( )->set_order_by(
          ii_event->query( )->get( 'ORDERBY' ) ).
        mv_order_descending = zcl_abapgit_persistence_user=>get_instance( )->set_order_descending( abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-direction.
        mv_order_descending = zcl_abapgit_persistence_user=>get_instance( )->set_order_descending(
          boolc( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-repo_open_in_master_lang.
        open_in_main_language( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-go_patch.
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_patch
          EXPORTING
            iv_key = lv_key.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.
    ls_hotkey_action-ui_component = 'Repo'.

    ls_hotkey_action-description   = |Stage|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Switch Branch|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-git_branch_switch.
    ls_hotkey_action-hotkey = |b|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Repository List|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-abapgit_home.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Refresh Repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_refresh.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Pull|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-git_pull.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Patch|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_patch.
    ls_hotkey_action-hotkey = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Diff|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_repo_diff.
    ls_hotkey_action-hotkey = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Uninstall Repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_purge.
    ls_hotkey_action-hotkey = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Run Syntax Check|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_syntax_check.
    ls_hotkey_action-hotkey = |c|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Run Code Inspector|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_code_inspector.
    ls_hotkey_action-hotkey = |i|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Show Log|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_log.
    ls_hotkey_action-hotkey = |l|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = zcl_abapgit_gui_buttons=>repo_list( )
      iv_act = zif_abapgit_definitions=>c_action-abapgit_home
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>help( )
      iv_title = 'Help'
      io_sub = zcl_abapgit_gui_chunk_lib=>help_submenu( ) ).

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
      ro_toolbar->add(
        iv_txt   = zcl_abapgit_gui_buttons=>experimental( )
        iv_title = 'Experimental Features are Enabled'
        iv_act   = zif_abapgit_definitions=>c_action-go_settings ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA: lt_repo_items TYPE zif_abapgit_definitions=>ty_repo_item_tt,
          lo_browser    TYPE REF TO zcl_abapgit_repo_content_list,
          lx_error      TYPE REF TO zcx_abapgit_exception,
          lv_max        TYPE abap_bool,
          lv_max_str    TYPE string,
          lv_add_str    TYPE string,
          li_log        TYPE REF TO zif_abapgit_log,
          lv_msg        TYPE string,
          lo_news       TYPE REF TO zcl_abapgit_news.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    register_handlers( ).

    CREATE OBJECT mo_repo_aggregated_state.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    TRY.
        " Reinit, for the case of type change
        mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).

        check_branch( ).

        mv_are_changes_recorded_in_tr = zcl_abapgit_factory=>get_sap_package( mo_repo->get_package( )
          )->are_changes_recorded_in_tr_req( ).

        lo_news = zcl_abapgit_news=>create( mo_repo ).

        ri_html->add( |<div class="repo" id="repo{ mv_key }">| ).
        ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
          io_repo               = mo_repo
          io_news               = lo_news
          iv_show_edit          = abap_true
          iv_interactive_branch = abap_true ) ).

        ri_html->add( zcl_abapgit_gui_chunk_lib=>render_news( io_news = lo_news ) ).

        zcl_abapgit_exit=>get_instance( )->wall_message_repo(
          is_repo_meta = mo_repo->ms_data
          ii_html      = ri_html ).

        CREATE OBJECT lo_browser
          EXPORTING
            io_repo = mo_repo.

        lt_repo_items = lo_browser->list( iv_path         = mv_cur_dir
                                          iv_by_folders   = mv_show_folders
                                          iv_changes_only = mv_changes_only
                                          iv_transports   = mv_are_changes_recorded_in_tr ).

        apply_order_by( CHANGING ct_repo_items = lt_repo_items ).

        LOOP AT lt_repo_items ASSIGNING <ls_item>.
          mo_repo_aggregated_state->sum_with_repo_item( <ls_item> ).
        ENDLOOP.

        ri_html->add( render_head_line( ) ).

        li_log = lo_browser->get_log( ).
        IF li_log->count( ) > 0.
          ri_html->add( '<div class="log">' ).
          ri_html->add( zcl_abapgit_log_viewer=>to_html( li_log ) ). " shows eg. list of unsupported objects
          ri_html->add( '</div>' ).
        ENDIF.

        ri_html->add( '<div class="repo_container">' ).

        CLEAR lv_msg.

        IF lines( lt_repo_items ) = 0.
          IF mv_changes_only = abap_true.
            IF mo_repo->is_offline( ) = abap_true.
              " Offline match banner
              IF mo_repo->has_remote_source( ) = abap_true.
                lv_msg = 'Local state completely <b>matches</b> the ZIP file'.
              ELSE.
                lv_msg = 'Import a ZIP file to see if there are any changes'.
              ENDIF.
            ELSE.
              " Online match banner
              lv_msg = 'Local state completely <b>matches</b> the remote repository'.
            ENDIF.
          ELSE.
            lv_msg = |Package is empty. Show { build_dir_jump_link( 'parent' ) } package|.
          ENDIF.
        ELSE.
          " Repo content table
          ri_html->add( '<table class="repo_tab">' ).

          ri_html->add( render_order_by( ) ).

          IF zcl_abapgit_path=>is_root( mv_cur_dir ) = abap_false.
            ri_html->add( render_parent_dir( ) ).
          ENDIF.

          LOOP AT lt_repo_items ASSIGNING <ls_item>.
            IF mv_max_lines > 0 AND sy-tabix > mv_max_lines.
              lv_max = abap_true.
              EXIT. " current loop
            ENDIF.
            ri_html->add( render_item( is_item = <ls_item>
                                       iv_render_transports = mv_are_changes_recorded_in_tr ) ).
          ENDLOOP.

          IF mv_changes_only = abap_true.
            ri_html->add( `<tfoot><tr><td class="grey" colspan="5">` ).
            ri_html->add( `(Only changes are shown. ` ).
            ri_html->add( ri_html->a(
              iv_txt   = |Show All|
              iv_act   = |{ c_actions-toggle_changes }| ) ).
            ri_html->add( `)</td></tr></tfoot>` ).
          ENDIF.

          ri_html->add( '</table>' ).
        ENDIF.

        IF NOT lv_msg IS INITIAL.
          ri_html->add( |<div class="panel success repo_banner">{ lv_msg }</div>| ).
        ENDIF.

        IF lv_max = abap_true.
          ri_html->add( '<div class = "dummydiv">' ).
          IF mv_max_lines = 1.
            lv_max_str = '1 object'.
          ELSE.
            lv_max_str = |first { mv_max_lines } objects|.
          ENDIF.
          lv_add_str = |+{ mv_max_setting }|.
          ri_html->add( |Only { lv_max_str } objects shown in list. Display {
            ri_html->a( iv_txt = lv_add_str
                        iv_act = c_actions-display_more )
            } more (change in Settings > {
            ri_html->a( iv_txt = 'Personal Settings'
                        iv_act = zif_abapgit_definitions=>c_action-go_settings_personal )
            })| ).
          ri_html->add( '</div>' ).
        ENDIF.

        ri_html->add( '</div>' ).
        ri_html->add( '</div>' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        " Reset 'last shown repo' so next start will go to repo overview
        " and allow troubleshooting of issue
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).

        ri_html->add( render_head_line( ) ).

        ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error(
          iv_extra_style = 'repo_banner'
          ix_error = lx_error ) ).
    ENDTRY.

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
ENDCLASS.
