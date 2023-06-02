CLASS zcl_abapgit_gui_page_sett_remo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_hotkeys.

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_head_type TYPE c LENGTH 1,
      BEGIN OF ty_remote_settings,
        offline         TYPE zif_abapgit_persistence=>ty_repo-offline,
        url             TYPE zif_abapgit_persistence=>ty_repo-url,
        branch          TYPE zif_abapgit_git_definitions=>ty_git_branch-name,
        tag             TYPE zif_abapgit_git_definitions=>ty_git_tag-name,
        commit          TYPE zif_abapgit_definitions=>ty_commit-sha1,
        pull_request    TYPE string,
        head_type       TYPE ty_head_type,
        switched_origin TYPE zif_abapgit_persistence=>ty_repo-switched_origin,
      END OF ty_remote_settings.
    CONSTANTS:
      BEGIN OF c_head_types,
        branch       TYPE ty_head_type VALUE 'B',
        tag          TYPE ty_head_type VALUE 'T',
        commit       TYPE ty_head_type VALUE 'C',
        pull_request TYPE ty_head_type VALUE 'P',
      END OF c_head_types.
    CONSTANTS:
      BEGIN OF c_id,
        general      TYPE string VALUE 'general',
        repo_type    TYPE string VALUE 'repo_type',
        offline      TYPE string VALUE 'offline',
        url          TYPE string VALUE 'url',
        head_group   TYPE string VALUE 'head_group',
        branch       TYPE string VALUE 'branch',
        tag          TYPE string VALUE 'tag',
        commit       TYPE string VALUE 'commit',
        pull_request TYPE string VALUE 'pull_request',
        head_type    TYPE string VALUE 'head_type',
      END OF c_id.
    CONSTANTS:
      BEGIN OF c_event,
        save                TYPE string VALUE 'save',
        switch              TYPE string VALUE 'switch',
        choose_url          TYPE string VALUE 'choose_url',
        choose_branch       TYPE string VALUE 'choose_branch',
        choose_tag          TYPE string VALUE 'choose_tag',
        choose_commit       TYPE string VALUE 'choose_commit',
        choose_pull_request TYPE string VALUE 'choose_pull_request',
        change_head_type    TYPE string VALUE 'change_head_type',
      END OF c_event .
    CONSTANTS:
      BEGIN OF c_popup,
        pull_request TYPE string VALUE 'popup_pull_request',
      END OF c_popup.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA ms_settings_snapshot TYPE ty_remote_settings.
    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map .
    DATA mv_refresh_on_back TYPE abap_bool.
    DATA mv_offline_switch_saved_url TYPE string.

    DATA mo_popup_picklist TYPE REF TO zcl_abapgit_gui_picklist.

    METHODS get_remote_settings_from_repo
      IMPORTING
        io_repo            TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rs_settings) TYPE ty_remote_settings
      RAISING
        zcx_abapgit_exception.

    METHODS get_remote_settings_from_form
      IMPORTING
        io_form_data       TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rs_settings) TYPE ty_remote_settings
      RAISING
        zcx_abapgit_exception.

    METHODS get_form_schema
      IMPORTING
        io_existing_form_data TYPE REF TO zcl_abapgit_string_map OPTIONAL
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

    METHODS initialize_form_data
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS check_protection
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        io_form_data             TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS save_settings
      RAISING
        zcx_abapgit_exception.

    METHODS choose_url
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS choose_branch
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.
    METHODS choose_tag
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.
    METHODS choose_pr
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.
    METHODS choose_commit
      RETURNING
        VALUE(rv_commit) TYPE ty_remote_settings-commit
      RAISING
        zcx_abapgit_exception.

    METHODS switch_online_offline
      RAISING
        zcx_abapgit_exception.
    METHODS switch_to_branch_tag
      IMPORTING
        !iv_name TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception.
    METHODS switch_to_commit
      IMPORTING
        !iv_revert TYPE abap_bool DEFAULT abap_false
        !iv_commit TYPE ty_remote_settings-commit OPTIONAL
      RAISING
        zcx_abapgit_exception.
    METHODS switch_to_pull_req
      IMPORTING
        !iv_revert TYPE abap_bool DEFAULT abap_false
        !iv_pull   TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception.

    METHODS handle_picklist_state
      RAISING
        zcx_abapgit_exception.

    METHODS render_content
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SETT_REMO IMPLEMENTATION.


  METHOD check_protection.

    IF mo_repo->is_offline( ) = abap_true.
      zcx_abapgit_exception=>raise( 'Unexpected switch for offline repo' ).
    ENDIF.
    IF mo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot switch. Repository is write-protected in local settings' ).
    ENDIF.

  ENDMETHOD.


  METHOD choose_branch.

    DATA lv_url         TYPE zif_abapgit_persistence=>ty_repo-url.
    DATA lv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name.
    DATA ls_branch      TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( c_id-offline ) = abap_true.
        RETURN.
      ENDIF.

      lv_url         = mo_form_data->get( c_id-url ).
      lv_branch_name = mo_form_data->get( c_id-branch ).

      mo_popup_picklist = zcl_abapgit_html_popups=>branch_list(
        iv_show_new_option = abap_false
        iv_url             = lv_url
        iv_default_branch  = lv_branch_name
        )->create_picklist(
        )->set_id( c_event-choose_branch
        )->set_in_page( ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_branch ).
        IF ls_branch IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-branch
            iv_val = ls_branch-display_name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_commit.

    DATA:
      lv_url         TYPE string,
      lv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name,
      li_popups      TYPE REF TO zif_abapgit_popups.

    IF mo_form_data->get( c_id-offline ) = abap_true.
      RETURN.
    ENDIF.

    lv_url = mo_form_data->get( c_id-url ).
    lv_branch_name = zif_abapgit_definitions=>c_git_branch-heads_prefix && mo_form_data->get( c_id-branch ).

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).

    rv_commit = li_popups->commit_list_popup(
      iv_repo_url    = lv_url
      iv_branch_name = lv_branch_name )-sha1.

  ENDMETHOD.


  METHOD choose_pr.

    DATA ls_pull         TYPE zif_abapgit_pr_enum_provider=>ty_pull_request.
    DATA lv_pull_request TYPE ty_remote_settings-pull_request.
    DATA lv_url TYPE ty_remote_settings-url.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( c_id-offline ) = abap_true.
        zcx_abapgit_exception=>raise( 'Not possible for offline repositories' ).
      ENDIF.

      lv_url = mo_form_data->get( c_id-url ).
      mo_popup_picklist = zcl_abapgit_html_popups=>pull_request_list( lv_url
        )->create_picklist(
        )->set_id( c_event-choose_pull_request
        )->set_in_page( abap_true ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_pull ).
        IF ls_pull IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-pull_request
            iv_val = ls_pull-head_url && '@' && ls_pull-head_branch ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_tag.

    DATA ls_tag TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_url TYPE ty_remote_settings-url.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( c_id-offline ) = abap_true.
        RETURN.
      ELSEIF mo_repo->is_offline( ) = abap_true.
        MESSAGE 'Please save conversion to online repository before choosing a tag' TYPE 'S'.
        RETURN.
      ENDIF.

      lv_url = mo_form_data->get( c_id-url ).
      mo_popup_picklist = zcl_abapgit_html_popups=>tag_list( lv_url
        )->create_picklist(
        )->set_id( c_event-choose_tag
        )->set_in_page( ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_tag ).
        IF ls_tag IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-tag
            iv_val = ls_tag-display_name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_url.

    " todo, get url history from DB and show selection popup #3639
    rv_url = ''.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mo_repo              = io_repo.
    ms_settings_snapshot = get_remote_settings_from_repo( mo_repo ).
    mo_form              = get_form_schema( ).
    mo_form_data         = initialize_form_data( ).
    CREATE OBJECT mo_validation_log.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_remo.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Remote Settings'
      io_page_menu       = zcl_abapgit_gui_chunk_lib=>settings_repo_toolbar(
                             iv_key = io_repo->get_key( )
                             iv_act = zif_abapgit_definitions=>c_action-repo_remote_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA:
      lv_button      TYPE string,
      lv_label       TYPE string,
      lv_icon        TYPE string,
      lv_hint        TYPE string,
      lv_placeholder TYPE string,
      lv_offline     TYPE abap_bool,
      lv_head_type   TYPE ty_head_type.

    IF io_existing_form_data IS BOUND AND io_existing_form_data->is_empty( ) = abap_false.
      lv_offline = io_existing_form_data->get( c_id-offline ).
      IF lv_offline = abap_false.
        lv_head_type = io_existing_form_data->get( c_id-head_type ).
      ENDIF.
    ELSE.
      lv_offline   = ms_settings_snapshot-offline.
      lv_head_type = ms_settings_snapshot-head_type.
    ENDIF.

    ro_form = zcl_abapgit_html_form=>create(
      iv_form_id   = 'repo-remote-settings-form'
      iv_help_page = 'https://docs.abapgit.org/settings-remote.html' ).

    IF lv_offline = abap_true.
      lv_button      = 'Switch to Online'.
      lv_icon        = 'plug/darkgrey'.
      lv_label       = 'Repository Name'.
    ELSE.
      lv_button      = 'Switch to Offline'.
      lv_icon        = 'cloud-upload-alt/darkgrey'.
      lv_label       = 'Git Repository URL'.
      lv_hint        = 'URL of original repository'.
      lv_placeholder = 'https://github.com/...git'.
    ENDIF.

    ro_form->start_group(
      iv_name  = c_id-general
      iv_label = 'General'
      iv_hint  = 'Change the general type and origin of the repository'
    )->text(
      iv_name        = c_id-repo_type
      iv_label       = |Type of Repository: { zcl_abapgit_html=>icon( lv_icon ) }|
      iv_readonly    = abap_true
    )->hidden( c_id-offline
    )->text(
      iv_name        = c_id-url
      iv_condense    = abap_true
      iv_label       = lv_label
      iv_hint        = lv_hint
      iv_placeholder = lv_placeholder ).

    IF lv_offline = abap_false.

      ro_form->start_group(
        iv_name  = c_id-head_group
        iv_label = 'Head'
      )->radio(
        iv_label  = 'Type'
        iv_name   = c_id-head_type
        iv_action = c_event-change_head_type
      )->option(
        iv_label = 'Branch'
        iv_value = c_head_types-branch
      )->option(
        iv_label = 'Tag'
        iv_value = c_head_types-tag
      )->option(
        iv_label = 'Commit'
        iv_value = c_head_types-commit
      )->option(
        iv_label = 'Pull Request'
        iv_value = c_head_types-pull_request ).

      IF lv_head_type = c_head_types-branch OR
         lv_head_type = c_head_types-commit.
        ro_form->text(
          iv_name        = c_id-branch
          iv_label       = 'Branch'
          iv_required    = abap_true
          iv_side_action = c_event-choose_branch ).
      ENDIF.

      IF lv_head_type = c_head_types-tag.
        ro_form->text(
          iv_name        = c_id-tag
          iv_label       = 'Tag'
          iv_required    = abap_true
          iv_side_action = c_event-choose_tag ).
      ENDIF.

      IF lv_head_type = c_head_types-commit.
        ro_form->text(
          iv_name        = c_id-commit
          iv_label       = 'Commit'
          iv_required    = abap_true
          iv_min         = 40
          iv_max         = 40
          iv_side_action = c_event-choose_commit ).
      ENDIF.

      IF lv_head_type = c_head_types-pull_request.
        ro_form->text(
          iv_name        = c_id-pull_request
          iv_label       = 'Pull Request'
          iv_required    = abap_true
          iv_side_action = c_event-choose_pull_request ).
      ENDIF.

    ENDIF.

    ro_form->command(
      iv_label    = 'Save Settings'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-save
    )->command(
      iv_label  = lv_button
      iv_action = c_event-switch
    )->command(
      iv_label  = 'Back'
      iv_action = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_remote_settings_from_form.

    rs_settings-url = io_form_data->get( c_id-url ).
    rs_settings-offline = io_form_data->get( c_id-offline ).

    IF rs_settings-offline = abap_false.
      rs_settings-head_type = io_form_data->get( c_id-head_type ).

      CASE rs_settings-head_type.
        WHEN c_head_types-branch.
          rs_settings-branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && io_form_data->get( c_id-branch ).
        WHEN c_head_types-tag.
          rs_settings-tag = zif_abapgit_definitions=>c_git_branch-tags_prefix && io_form_data->get( c_id-tag ).
        WHEN c_head_types-commit.
          rs_settings-branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && io_form_data->get( c_id-branch ).
          rs_settings-commit = io_form_data->get( c_id-commit ).
        WHEN c_head_types-pull_request.
          rs_settings-pull_request = io_form_data->get( c_id-pull_request ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get_remote_settings_from_repo.

    DATA: lo_repo_online  TYPE REF TO zcl_abapgit_repo_online,
          lo_repo_offline TYPE REF TO zcl_abapgit_repo_offline,
          lv_branch       TYPE ty_remote_settings-branch.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      rs_settings-url = lo_repo_online->get_url( ).
      rs_settings-offline = abap_false.
      rs_settings-switched_origin = lo_repo_online->get_switched_origin( ).

      IF lo_repo_online->get_selected_commit( ) IS NOT INITIAL.
        rs_settings-commit = lo_repo_online->get_selected_commit( ).
        rs_settings-branch = lo_repo_online->get_selected_branch( ).
        rs_settings-head_type = c_head_types-commit.
      ELSEIF lo_repo_online->get_switched_origin( ) IS NOT INITIAL.
        " get_switched_origin( ) returns the original repo url + HEAD concatenated with @
        " get_branch( ) returns the branch of the PR in the source repo
        " get_url( ) returns the source repo of the PR branch

        rs_settings-switched_origin = lo_repo_online->get_switched_origin( ).
        SPLIT rs_settings-switched_origin AT '@' INTO rs_settings-url rs_settings-branch.
        IF rs_settings-branch CP zif_abapgit_definitions=>c_git_branch-tags.
          rs_settings-tag = rs_settings-branch.
          CLEAR rs_settings-branch.
        ENDIF.

        lv_branch = lo_repo_online->get_selected_branch( ).
        REPLACE FIRST OCCURRENCE OF zif_abapgit_definitions=>c_git_branch-heads_prefix IN lv_branch WITH space.
        CONDENSE lv_branch.
        rs_settings-pull_request = |{ lo_repo_online->get_url( ) }@{ lv_branch }|.
        rs_settings-head_type = c_head_types-pull_request.
      ELSE.
        rs_settings-branch = lo_repo_online->get_selected_branch( ).
        rs_settings-head_type = c_head_types-branch.

        IF rs_settings-branch CP zif_abapgit_definitions=>c_git_branch-tags.
          rs_settings-head_type = c_head_types-tag.
          rs_settings-tag = rs_settings-branch.
          CLEAR rs_settings-branch.
        ENDIF.
      ENDIF.

    ELSE.
      lo_repo_offline ?= io_repo.

      rs_settings-url = lo_repo_offline->get_name( ).
      rs_settings-offline = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD handle_picklist_state.

    IF mo_popup_picklist IS BOUND AND
      ( mo_popup_picklist->is_fulfilled( ) = abap_true OR mo_popup_picklist->is_in_page( ) = abap_false ).
      " Picklist is either fullfilled OR
      " it was on its own page and user went back from it via F3/ESC and the picklist had no "graceful back" handler
      CASE mo_popup_picklist->id( ).
        WHEN c_event-choose_pull_request.
          choose_pr( iv_is_return = abap_true ).
        WHEN c_event-choose_branch.
          choose_branch( iv_is_return = abap_true ).
        WHEN c_event-choose_tag.
          choose_tag( iv_is_return = abap_true ).
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |Unexpected picklist id { mo_popup_picklist->id( ) }| ).
      ENDCASE.

      CLEAR mo_popup_picklist.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_form_data.

    DATA:
      lv_type TYPE string,
      lv_head TYPE string.

    CREATE OBJECT ro_form_data.

    IF ms_settings_snapshot-offline = abap_true.
      lv_type = 'Offline repository'.
    ELSE.
      lv_type = 'Online repository'.
    ENDIF.

    ro_form_data->set(
      iv_key = c_id-offline
      iv_val = ms_settings_snapshot-offline ).
    ro_form_data->set(
      iv_key = c_id-repo_type
      iv_val = lv_type ).
    ro_form_data->set(
      iv_key = c_id-url
      iv_val = ms_settings_snapshot-url ).

    IF ms_settings_snapshot-offline = abap_false.
      ro_form_data->set(
        iv_key = c_id-head_type
        iv_val = ms_settings_snapshot-head_type ).

      " When pull request is selected the previously selected branch/tag is also loaded to be able to switch back to it
      lv_head = zcl_abapgit_git_branch_list=>get_display_name( ms_settings_snapshot-branch ).
      ro_form_data->set(
        iv_key = c_id-branch
        iv_val = lv_head ).

      lv_head = zcl_abapgit_git_branch_list=>get_display_name( ms_settings_snapshot-tag ).
      ro_form_data->set(
        iv_key = c_id-tag
        iv_val = lv_head ).

      ro_form_data->set(
        iv_key = c_id-commit
        iv_val = ms_settings_snapshot-commit ).

      ro_form_data->set(
        iv_key = c_id-pull_request
        iv_val = ms_settings_snapshot-pull_request ).
    ENDIF.

    " Set for is_dirty check
    zcl_abapgit_html_form_utils=>create( mo_form )->set_data( ro_form_data ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo               = mo_repo
      iv_show_commit        = abap_false
      iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.


  METHOD save_settings.

    DATA:
      lo_repo_online  TYPE REF TO zcl_abapgit_repo_online,
      lo_repo_offline TYPE REF TO zcl_abapgit_repo_offline,
      ls_settings_new TYPE ty_remote_settings.

    ls_settings_new = get_remote_settings_from_form( mo_form_data ).

    " Switch online / offline
    IF ls_settings_new-offline <> ms_settings_snapshot-offline.
      " Remember key, switch, retrieve new instance (todo, refactor #2244)
      mo_repo->switch_repo_type( ls_settings_new-offline ).
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).
    ENDIF.

    IF mo_repo->is_offline( ) = abap_true.
      " Offline: Save repo name
      lo_repo_offline ?= mo_repo.
      lo_repo_offline->set_name( ls_settings_new-url ).
    ELSE.
      " Online: Save url
      lo_repo_online ?= mo_repo.
      lo_repo_online->set_url( ls_settings_new-url ).
    ENDIF.

    CASE ls_settings_new-head_type.
      WHEN c_head_types-branch.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( ls_settings_new-branch ).
      WHEN c_head_types-tag.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( ls_settings_new-tag ).
      WHEN c_head_types-commit.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_commit = ls_settings_new-commit ).
      WHEN c_head_types-pull_request.
        switch_to_commit( iv_revert = abap_true ).
        switch_to_pull_req( iv_pull = ls_settings_new-pull_request ).
    ENDCASE.

    IF mo_repo->is_offline( ) = abap_false AND ls_settings_new-head_type <> c_head_types-pull_request.
      " Switching from PR to something else will reset the URL in repo->switch_origin( space )
      " -> set URL again
      lo_repo_online->set_url( ls_settings_new-url ).
    ENDIF.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mv_refresh_on_back = abap_true.
    ms_settings_snapshot = get_remote_settings_from_repo( mo_repo ).

  ENDMETHOD.


  METHOD switch_online_offline.

    DATA: lv_offline_new TYPE abap_bool,
          lv_url         TYPE ty_remote_settings-url,
          lv_branch      TYPE ty_remote_settings-branch.

    lv_offline_new = boolc( mo_form_data->get( c_id-offline ) = abap_false ).
    mo_form_data->set(
      iv_key = c_id-offline
      iv_val = lv_offline_new ).

    IF lv_offline_new = abap_true.
      lv_url = mo_form_data->get( c_id-url ).
      mv_offline_switch_saved_url = lv_url.
      IF lv_url CP 'http*'.
        lv_url = zcl_abapgit_url=>name( lv_url ).
        mo_form_data->set(
          iv_key = c_id-url
          iv_val = lv_url ).
      ENDIF.

    ELSE.
      IF mv_offline_switch_saved_url IS NOT INITIAL.
        mo_form_data->set(
          iv_key = c_id-url
          iv_val = mv_offline_switch_saved_url ).
      ENDIF.

      lv_url = mo_form_data->get( c_id-url ).
      IF mo_form_data->get( c_id-head_type ) IS INITIAL.
        TRY.
            mo_form_data->set(
              iv_key = c_id-head_type
              iv_val = c_head_types-branch ).

            IF lv_url CP 'http*'.
              lv_branch = zcl_abapgit_git_transport=>branches( lv_url )->get_head_symref( ).
              mo_form_data->set(
                iv_key = c_id-branch
                iv_val = lv_branch ).
            ENDIF.
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD switch_to_branch_tag.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo_online.

    check_protection( ).
    lo_repo ?= mo_repo.
    lo_repo->select_branch( iv_name ).

  ENDMETHOD.


  METHOD switch_to_commit.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo_online.

    check_protection( ).

    lo_repo ?= mo_repo.

    IF iv_revert = abap_true.
      lo_repo->select_commit( '' ).
    ELSE.
      lo_repo->select_commit( iv_commit ).
    ENDIF.

  ENDMETHOD.


  METHOD switch_to_pull_req.

    DATA:
      lo_repo   TYPE REF TO zcl_abapgit_repo_online,
      lv_url    TYPE ty_remote_settings-url,
      lv_branch TYPE ty_remote_settings-branch.

    check_protection( ).

    lo_repo ?= mo_repo.

    " Switching twice does not work so reset to original repo first
    lo_repo->switch_origin( '' ).

    IF iv_revert = abap_false.
      SPLIT iv_pull AT '@' INTO lv_url lv_branch.
      lo_repo->switch_origin(
        iv_url    = lv_url
        iv_branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && lv_branch ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_form.

    DATA:
      lx_error                 TYPE REF TO zcx_abapgit_exception,
      lo_branch_list           TYPE REF TO zcl_abapgit_git_branch_list,
      lo_url                   TYPE REF TO zcl_abapgit_git_url,
      lv_offline               TYPE abap_bool,
      lv_head_type             TYPE ty_head_type,
      lv_branch                TYPE ty_remote_settings-branch,
      lv_url                   TYPE ty_remote_settings-url,
      lv_branch_check_error_id TYPE string,
      lv_pull_request          TYPE ty_remote_settings-pull_request,
      lv_commit                TYPE ty_remote_settings-commit.

    ro_validation_log = zcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).
    lv_offline = io_form_data->get( c_id-offline ).
    lv_url = io_form_data->get( c_id-url ).

    IF lv_offline = abap_true AND lv_url IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Enter a name for the repository and save' ).
    ENDIF.

    IF lv_offline = abap_false AND lv_url NP 'http*'.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Enter the URL of the repository and save' ).
    ELSEIF lv_offline = abap_false.
      TRY.
          zcl_abapgit_url=>name(
            iv_url      = lv_url
            iv_validate = abap_true ).

          " Provider-specific URL check
          CREATE OBJECT lo_url.
          lo_url->validate_url( lv_url ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-url
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF lv_offline = abap_false.
      lv_head_type = io_form_data->get( c_id-head_type ).

      CASE lv_head_type.
        WHEN c_head_types-branch.
          lv_branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && io_form_data->get( c_id-branch ).
          CONDENSE lv_branch.
          lv_branch_check_error_id = c_id-branch.
        WHEN c_head_types-tag.
          lv_branch = zif_abapgit_definitions=>c_git_branch-tags_prefix && io_form_data->get( c_id-tag ).
          CONDENSE lv_branch.
          lv_branch_check_error_id = c_id-tag.
        WHEN c_head_types-pull_request.
          lv_pull_request = io_form_data->get( c_id-pull_request ).
          SPLIT lv_pull_request AT '@' INTO lv_url lv_branch.
          IF lv_branch IS NOT INITIAL.
            lv_branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && lv_branch.
          ENDIF.
          lv_branch_check_error_id = c_id-pull_request.
        WHEN c_head_types-commit.
          lv_commit = io_form_data->get( c_id-commit ).

          " Cannot check for commit existence currently (needs API that doesn't rely on finding the first commit
          " in the branch), check format instead
          IF lv_commit CN '0123456789abcdef'.
            ro_validation_log->set(
              iv_key = c_id-commit
              iv_val = 'Commit needs to be hexadecimal and in lowercase' ).
          ENDIF.
        WHEN OTHERS.
          ro_validation_log->set(
            iv_key = c_id-head_type
            iv_val = 'Unknown head type' ).
      ENDCASE.

      TRY.
          IF lv_branch IS NOT INITIAL.
            lo_branch_list = zcl_abapgit_git_transport=>branches( lv_url ).
            lo_branch_list->find_by_name( lv_branch ).
          ENDIF.
        CATCH zcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = lv_branch_check_error_id
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_url    TYPE ty_remote_settings-url,
      lv_branch TYPE ty_remote_settings-branch,
      lv_tag    TYPE ty_remote_settings-tag,
      lv_commit TYPE ty_remote_settings-commit.

    mo_form_data->merge( zcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        IF mv_refresh_on_back = abap_true.
          " Note this doesn't trigger if the tab is switched first
          mo_repo->refresh( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data = mo_form_data
          io_check_changes_versus = initialize_form_data( ) ).

      WHEN c_event-choose_url.
        lv_url = choose_url( ).

        IF lv_url IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-url
            iv_val = lv_url ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-change_head_type.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        mo_validation_log->clear( ).

      WHEN c_event-choose_branch.
        choose_branch( ). " Unformly handle state below

      WHEN c_event-choose_tag.
        choose_tag( ). " Unformly handle state below

      WHEN c_event-choose_pull_request.
        choose_pr( ). " Unformly handle state below

      WHEN c_event-choose_commit.
        lv_commit = choose_commit( ).

        IF lv_commit IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-commit
            iv_val = lv_commit ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-switch.
        switch_online_offline( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-save.
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

    IF mo_popup_picklist IS BOUND. " Uniform popup state handling
      " This should happen only for a new popup because
      " on the first re-render main component event handling is blocked
      " and not called again until the popup distruction
      IF mo_popup_picklist->is_in_page( ) = abap_true.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      ELSE.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
        rs_handled-page  = zcl_abapgit_gui_page_hoc=>create(
          ii_child_component = mo_popup_picklist
          iv_show_as_modal   = abap_true ).
      ENDIF.
    ENDIF.

    " If staying on form, initialize it with current settings
    IF rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render AND mo_popup_picklist IS NOT BOUND.
      " Switching tabs must change the form layout
      mo_form = get_form_schema( io_existing_form_data = mo_form_data ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions,
          lv_head_type     TYPE ty_head_type,
          lv_offline       TYPE abap_bool.

    IF mo_form_data IS BOUND AND mo_form_data->is_empty( ) = abap_false.
      lv_offline = mo_form_data->get( c_id-offline ).
      IF lv_offline = abap_false.
        lv_head_type = mo_form_data->get( c_id-head_type ).
      ENDIF.
    ELSE.
      lv_offline = ms_settings_snapshot-offline.
      IF lv_offline = abap_false.
        lv_head_type = ms_settings_snapshot-head_type.
      ENDIF.
    ENDIF.

    ls_hotkey_action-ui_component = 'Remote'.

    ls_hotkey_action-description = 'Choose URL'.
    ls_hotkey_action-action      = c_event-choose_url.
    ls_hotkey_action-hotkey      = 'u'.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    IF lv_head_type = c_head_types-branch OR
       lv_head_type = c_head_types-commit.
      ls_hotkey_action-description = 'Choose Branch'.
      ls_hotkey_action-action      = c_event-choose_branch.
      ls_hotkey_action-hotkey      = 'b'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = c_head_types-tag.
      ls_hotkey_action-description = 'Choose Tag'.
      ls_hotkey_action-action      = c_event-choose_tag.
      ls_hotkey_action-hotkey      = 't'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = c_head_types-commit.
      ls_hotkey_action-description = 'Choose Commit'.
      ls_hotkey_action-action      = c_event-choose_commit.
      ls_hotkey_action-hotkey      = 'c'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = c_head_types-pull_request.
      ls_hotkey_action-description = 'Choose Pull Request'.
      ls_hotkey_action-action      = c_event-choose_pull_request.
      ls_hotkey_action-hotkey      = 'p'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_offline = abap_true.
      ls_hotkey_action-description = 'Switch to Online'.
      ls_hotkey_action-action      = c_event-switch.
      ls_hotkey_action-hotkey      = 'o'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ELSE.
      ls_hotkey_action-description = 'Switch to Offline'.
      ls_hotkey_action-action      = c_event-switch.
      ls_hotkey_action-hotkey      = 'o'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    handle_picklist_state( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->wrap(
      iv_tag     = 'div'
      iv_class   = 'repo' " It's OK because it's repo settings ... for now
      ii_content = render_content( ) ).

    IF mo_popup_picklist IS NOT BOUND OR mo_popup_picklist->is_in_page( ) = abap_false.
      register_handlers( ).
    ELSEIF mo_popup_picklist->is_in_page( ) = abap_true.
      " Block usual page events if the popup is an in-page popup
      ri_html->add( zcl_abapgit_gui_in_page_modal=>create( mo_popup_picklist ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
