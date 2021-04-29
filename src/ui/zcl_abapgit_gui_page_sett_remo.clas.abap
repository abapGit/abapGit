CLASS zcl_abapgit_gui_page_sett_remo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

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
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_commit_value_tab,
        sha1     TYPE zif_abapgit_definitions=>ty_sha1,
        message  TYPE c LENGTH 50,
        datetime TYPE c LENGTH 20,
      END OF ty_commit_value_tab .
    TYPES:
      ty_commit_value_tab_tt TYPE STANDARD TABLE OF ty_commit_value_tab WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF c_mode,
        offline      TYPE i VALUE 0,
        branch       TYPE i VALUE 1,
        tag          TYPE i VALUE 2,
        commit       TYPE i VALUE 3,
        pull_request TYPE i VALUE 4,
      END OF c_mode .
    CONSTANTS:
      BEGIN OF c_id,
        general      TYPE string VALUE 'general',
        repo_type    TYPE string VALUE 'repo_type',
        head         TYPE string VALUE 'head',
        url          TYPE string VALUE 'url',
        branch       TYPE string VALUE 'branch',
        tag          TYPE string VALUE 'tag',
        commit       TYPE string VALUE 'commmit',
        pull_request TYPE string VALUE 'pull_request',
      END OF c_id .
    CONSTANTS:
      BEGIN OF c_event,
        go_back         TYPE string VALUE 'go-back',
        save            TYPE string VALUE 'save',
        switch          TYPE string VALUE 'switch',
        choose_url      TYPE string VALUE 'choose_url',
        choose_branch   TYPE string VALUE 'choose_branch',
        choose_tag      TYPE string VALUE 'choose_tag',
        choose_commit   TYPE string VALUE 'choose_commit',
        choose_pull_req TYPE string VALUE 'choose_pull_req',
      END OF c_event .
    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils .
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA ms_repo_current TYPE zif_abapgit_persistence=>ty_repo .
    DATA ms_repo_new TYPE zif_abapgit_persistence=>ty_repo .
    DATA mo_dot TYPE REF TO zcl_abapgit_dot_abapgit .
    DATA mv_pull_req TYPE string .
    DATA mv_mode TYPE i .
    DATA mv_original_url TYPE string .

    METHODS init
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    METHODS choose_url
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS choose_branch
      RETURNING
        VALUE(rv_branch) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS choose_tag
      RETURNING
        VALUE(rv_tag) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS choose_commit
      RETURNING
        VALUE(rv_commit) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS choose_pull_req
      RETURNING
        VALUE(rv_pull) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS check_protection
      RAISING
        zcx_abapgit_exception .
    METHODS switch_online_offline
      RAISING
        zcx_abapgit_exception .
    METHODS switch_to_branch_tag
      IMPORTING
        !iv_name TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS switch_to_commit
      IMPORTING
        !iv_revert TYPE abap_bool DEFAULT abap_false
        !iv_commit TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS switch_to_pull_req
      IMPORTING
        !iv_revert TYPE abap_bool DEFAULT abap_false
        !iv_pull   TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception .
    METHODS read_settings
      RAISING
        zcx_abapgit_exception .
    METHODS save_settings
      RAISING
        zcx_abapgit_exception .
    METHODS checkout_commit_build_list
      IMPORTING
        !io_repo      TYPE REF TO zcl_abapgit_repo_online
      EXPORTING
        !et_value_tab TYPE ty_commit_value_tab_tt
        !et_commits   TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception .
    METHODS checkout_commit_build_popup
      IMPORTING
        !it_commits               TYPE zif_abapgit_definitions=>ty_commit_tt
        !it_value_tab             TYPE ty_commit_value_tab_tt
      RETURNING
        VALUE(rs_selected_commit) TYPE zif_abapgit_definitions=>ty_commit
      RAISING
        zcx_abapgit_exception .
    METHODS get_mode
      IMPORTING
        !is_repo       TYPE zif_abapgit_persistence=>ty_repo
      RETURNING
        VALUE(rv_mode) TYPE i .
ENDCLASS.



CLASS zcl_abapgit_gui_page_sett_remo IMPLEMENTATION.


  METHOD checkout_commit_build_list.

    DATA: lv_unix_time   TYPE zcl_abapgit_time=>ty_unixtime,
          lv_date        TYPE sy-datum,
          lv_date_string TYPE c LENGTH 12,
          lv_time        TYPE sy-uzeit,
          lv_time_string TYPE c LENGTH 10.

    FIELD-SYMBOLS: <ls_commit>    TYPE zif_abapgit_definitions=>ty_commit,
                   <ls_value_tab> TYPE ty_commit_value_tab.

    CLEAR: et_commits, et_value_tab.

    et_commits = zcl_abapgit_git_commit=>get_by_branch( iv_branch_name  = io_repo->get_selected_branch( )
                                                        iv_repo_url     = io_repo->get_url( )
                                                        iv_deepen_level = 99 )-commits.

    DELETE et_commits WHERE sha1 = io_repo->get_selected_commit( ).
    SORT et_commits BY time DESCENDING.

    IF et_commits IS INITIAL.
      zcx_abapgit_exception=>raise( |No commits are available in this branch.| ).
    ENDIF.

    LOOP AT et_commits ASSIGNING <ls_commit>.

      APPEND INITIAL LINE TO et_value_tab ASSIGNING <ls_value_tab>.
      <ls_value_tab>-sha1    = <ls_commit>-sha1.
      <ls_value_tab>-message = <ls_commit>-message.
      lv_unix_time = <ls_commit>-time.
      zcl_abapgit_time=>get_utc(
        EXPORTING
          iv_unix = lv_unix_time
        IMPORTING
          ev_time = lv_time
          ev_date = lv_date ).
      WRITE: lv_date TO lv_date_string,
             lv_time TO lv_time_string.
      <ls_value_tab>-datetime = |{ lv_date_string }, | &&
                                |{ lv_time_string }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD checkout_commit_build_popup.

    DATA: lt_columns         TYPE zif_abapgit_definitions=>ty_alv_column_tt,
          li_popups          TYPE REF TO zif_abapgit_popups,
          lt_selected_values TYPE ty_commit_value_tab_tt.

    FIELD-SYMBOLS: <ls_value_tab> TYPE ty_commit_value_tab,
                   <ls_column>    TYPE zif_abapgit_definitions=>ty_alv_column.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'SHA1'.
    <ls_column>-text   = 'Hash'.
    <ls_column>-length = 8.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'MESSAGE'.
    <ls_column>-text = 'Message'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DATETIME'.
    <ls_column>-text = 'Datetime'.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = it_value_tab
        iv_title              = |Checkout Commit|
        iv_end_column         = 83
        iv_striped_pattern    = abap_true
        iv_optimize_col_width = abap_false
        iv_selection_mode     = if_salv_c_selection_mode=>single
        it_columns_to_display = lt_columns
      IMPORTING
        et_list              = lt_selected_values ).

    IF lt_selected_values IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    READ TABLE lt_selected_values
      ASSIGNING <ls_value_tab>
      INDEX 1.

    IF <ls_value_tab> IS NOT ASSIGNED.
      zcx_abapgit_exception=>raise( |Though result set of popup wasn't empty selected value couldn't retrieved.| ).
    ENDIF.

    READ TABLE it_commits
      INTO rs_selected_commit
      WITH KEY sha1 = <ls_value_tab>-sha1.

  ENDMETHOD.


  METHOD check_protection.

    IF mo_repo->is_offline( ) = abap_true.
      zcx_abapgit_exception=>raise( 'Unexpected switch for offline repo' ).
    ENDIF.
    IF mo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot switch. Repository is write-protected in local settings' ).
    ENDIF.

  ENDMETHOD.


  METHOD choose_branch.

    DATA:
      lo_repo   TYPE REF TO zcl_abapgit_repo_online,
      ls_branch TYPE zif_abapgit_definitions=>ty_git_branch.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo ?= mo_repo.

    ls_branch = zcl_abapgit_ui_factory=>get_popups( )->branch_list_popup(
      iv_url             = lo_repo->get_url( )
      iv_default_branch  = lo_repo->get_selected_branch( )
      iv_show_new_option = abap_false ).

    IF ls_branch IS NOT INITIAL.
      rv_branch = ls_branch-name.
    ENDIF.

  ENDMETHOD.


  METHOD choose_commit.

    DATA:
      lo_repo            TYPE REF TO zcl_abapgit_repo_online,
      lt_value_tab       TYPE ty_commit_value_tab_tt,
      lt_commits         TYPE zif_abapgit_definitions=>ty_commit_tt,
      ls_selected_commit TYPE zif_abapgit_definitions=>ty_commit.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo ?= mo_repo.

    checkout_commit_build_list(
      EXPORTING
        io_repo        = lo_repo
      IMPORTING
        et_value_tab   = lt_value_tab
        et_commits     = lt_commits ).

    ls_selected_commit = checkout_commit_build_popup(
      it_commits   = lt_commits
      it_value_tab = lt_value_tab ).

    IF ls_selected_commit IS NOT INITIAL.
      rv_commit = ls_selected_commit-sha1.
    ENDIF.

  ENDMETHOD.


  METHOD choose_pull_req.

    DATA:
      lo_repo  TYPE REF TO zcl_abapgit_repo_online,
      lt_pulls TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests,
      ls_pull  LIKE LINE OF lt_pulls.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo ?= mo_repo.

    lt_pulls = zcl_abapgit_pr_enumerator=>new( lo_repo )->get_pulls( ).

    IF lines( lt_pulls ) = 0.
      MESSAGE 'No pull requests found' TYPE 'S'.
      RETURN.
    ENDIF.

    ls_pull = zcl_abapgit_ui_factory=>get_popups( )->choose_pr_popup( lt_pulls ).

    IF ls_pull IS NOT INITIAL.
      rv_pull = ls_pull-head_url && '@' && ls_pull-head_branch.
    ENDIF.

  ENDMETHOD.


  METHOD choose_tag.

    DATA:
      lo_repo TYPE REF TO zcl_abapgit_repo_online,
      ls_tag  TYPE zif_abapgit_definitions=>ty_git_tag.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo ?= mo_repo.

    ls_tag = zcl_abapgit_ui_factory=>get_tag_popups( )->tag_select_popup( lo_repo ).

    IF ls_tag IS NOT INITIAL.
      rv_tag = ls_tag-name.
    ENDIF.

  ENDMETHOD.


  METHOD choose_url.

    " todo, get url history from DB and show selection popup #3639

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.

    init( io_repo ).
    mv_original_url = ms_repo_current-url.

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    read_settings( ).

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
      lv_placeholder TYPE string.

    ro_form = zcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-remote-settings-form'
                iv_help_page = 'https://docs.abapgit.org/ref-dot-abapgit.html' ).

    IF mv_mode = c_mode-offline.
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
      iv_name        = c_id-general
      iv_label       = 'General'
      iv_hint        = 'Change the general type and origin of the repository'
    )->text(
      iv_name        = c_id-repo_type
      iv_label       = |Type of Repository: { zcl_abapgit_html=>icon( lv_icon ) }|
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-url
      iv_label       = lv_label
      iv_hint        = lv_hint
      iv_placeholder = lv_placeholder ).

    IF mv_mode <> c_mode-offline.

      ro_form->start_group(
        iv_name        = c_id-head
        iv_label       = 'Head'
      )->text(
        iv_name        = c_id-branch
        iv_label       = 'Branch'
        iv_hint        = 'Switch to a branch of the repository'
        iv_readonly    = abap_true
        iv_side_action = c_event-choose_branch
      )->text(
        iv_name        = c_id-tag
        iv_label       = 'Tag'
        iv_hint        = 'Switch to a tag of the repository'
        iv_readonly    = abap_true
        iv_side_action = c_event-choose_tag
      )->text(
        iv_name        = c_id-commit
        iv_label       = 'Commit'
        iv_hint        = 'Switch to a commit of the repository'
        iv_readonly    = abap_true
        iv_side_action = c_event-choose_commit
      )->text(
        iv_name        = c_id-pull_request
        iv_label       = 'Pull Request'
        iv_hint        = 'Switch to a pull request of the repository or its forks'
        iv_readonly    = abap_true
        iv_side_action = c_event-choose_pull_req ).

    ENDIF.

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = lv_button
      iv_action      = c_event-switch
    )->command(
      iv_label       = 'Back'
      iv_action      = c_event-go_back ).

  ENDMETHOD.


  METHOD get_mode.

    IF is_repo-offline = abap_true.
      rv_mode = c_mode-offline.
    ELSEIF is_repo-selected_commit IS NOT INITIAL.
      rv_mode = c_mode-commit.
    ELSEIF is_repo-switched_origin IS NOT INITIAL.
      rv_mode = c_mode-pull_request.
    ELSEIF is_repo-branch_name CP zif_abapgit_definitions=>c_git_branch-tags.
      rv_mode = c_mode-tag.
    ELSE.
      rv_mode = c_mode-branch.
    ENDIF.

  ENDMETHOD.


  METHOD init.

    DATA lv_branch TYPE string.

    mo_repo = io_repo.

    " Get repo settings when starting dialog
    TRY.
        ms_repo_current = zcl_abapgit_persist_factory=>get_repo( )->read( mo_repo->get_key( ) ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'Repository not found' ).
    ENDTRY.

    " Initialize new repo settings which are modified using this form (and saved)
    mv_mode = get_mode( ms_repo_current ).

    IF mv_mode = c_mode-pull_request.
      lv_branch = ms_repo_current-branch_name.
      REPLACE zif_abapgit_definitions=>c_git_branch-heads_prefix IN lv_branch WITH ''.
      mv_pull_req = ms_repo_current-url && '@' && lv_branch.
      SPLIT ms_repo_current-switched_origin AT '@' INTO ms_repo_current-url ms_repo_current-branch_name.
    ENDIF.

    ms_repo_new = ms_repo_current.

  ENDMETHOD.


  METHOD read_settings.

    DATA:
      lv_type     TYPE string,
      lv_url      TYPE string,
      lv_rest     TYPE string,
      lv_branch   TYPE string,
      lv_tag      TYPE string,
      lv_commit   TYPE string,
      lv_pull_req TYPE string.

    lv_type = 'Online repository'.
    lv_url = ms_repo_new-url.

    CASE mv_mode.
      WHEN c_mode-offline.
        lv_type = 'Offline repository'.
      WHEN c_mode-branch.
        lv_branch = ms_repo_new-branch_name.
      WHEN c_mode-tag.
        lv_tag = ms_repo_new-branch_name.
      WHEN c_mode-commit.
        lv_commit = ms_repo_new-selected_commit.
      WHEN c_mode-pull_request.
        SPLIT ms_repo_new-switched_origin AT '@' INTO lv_url lv_rest."original repo URL
        lv_pull_req = mv_pull_req.
    ENDCASE.

    mo_form_data->set(
      iv_key = c_id-repo_type
      iv_val = lv_type ).
    mo_form_data->set(
      iv_key = c_id-url
      iv_val = lv_url ).
    mo_form_data->set(
      iv_key = c_id-branch
      iv_val = lv_branch ).
    mo_form_data->set(
      iv_key = c_id-tag
      iv_val = lv_tag ).
    mo_form_data->set(
      iv_key = c_id-commit
      iv_val = lv_commit ).
    mo_form_data->set(
      iv_key = c_id-pull_request
      iv_val = lv_pull_req ).

    " Set for is_dirty check
    mo_form_util->set_data( mo_form_data ).

  ENDMETHOD.


  METHOD save_settings.

    DATA:
      lv_key          TYPE zif_abapgit_persistence=>ty_repo-key,
      lo_repo_online  TYPE REF TO zcl_abapgit_repo_online,
      lo_repo_offline TYPE REF TO zcl_abapgit_repo_offline,
      lo_branch_list  TYPE REF TO zcl_abapgit_git_branch_list,
      lv_url          TYPE string,
      lv_branch       TYPE string,
      lv_tag          TYPE string,
      lv_commit       TYPE string,
      lv_pull         TYPE string.

    lv_url    = mo_form_data->get( c_id-url ).
    lv_branch = mo_form_data->get( c_id-branch ).
    lv_tag    = mo_form_data->get( c_id-tag ).
    lv_commit = mo_form_data->get( c_id-commit ).
    lv_pull   = mo_form_data->get( c_id-pull_request ).

    " Switch online / offline
    IF ms_repo_new-offline <> ms_repo_current-offline.
      " Remember key, switch, retrieve new instance (todo, refactor #2244)
      lv_key = ms_repo_current-key.
      mo_repo->switch_repo_type( ms_repo_new-offline ).
      mo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
    ENDIF.

    IF mo_repo->is_offline( ) = abap_true.
      " Offline: Save repo name
      lo_repo_offline ?= mo_repo.
      lo_repo_offline->set_name( lv_url ).
    ELSE.
      " Online: Save url
      lo_repo_online ?= mo_repo.
      lo_repo_online->set_url( lv_url ).

      " Check branch/tag and reset to default if not found
      lo_branch_list = zcl_abapgit_git_transport=>branches( lv_url ).
      TRY.
          IF lv_branch IS INITIAL AND lv_tag IS INITIAL.
            lv_branch = lo_branch_list->get_head_symref( ).
          ELSEIF lv_tag IS INITIAL.
            lo_branch_list->find_by_name( lv_branch ).
          ELSE.
            lo_branch_list->find_by_name( lv_tag ).
          ENDIF.
        CATCH zcx_abapgit_exception.
          lv_branch = lo_branch_list->get_head_symref( ).
      ENDTRY.

      mv_original_url = lv_url.
    ENDIF.

    CASE mv_mode.
      WHEN c_mode-branch.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( lv_branch ).
      WHEN c_mode-tag.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( lv_tag ).
      WHEN c_mode-commit.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_commit = lv_commit ).
      WHEN c_mode-pull_request.
        switch_to_commit( iv_revert = abap_true ).
        switch_to_pull_req( iv_pull = lv_pull ).
    ENDCASE.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    init( mo_repo ).

  ENDMETHOD.


  METHOD switch_online_offline.

    DATA lv_url TYPE string.

    ms_repo_new-offline = boolc( ms_repo_new-offline = abap_false ).
    mv_mode = get_mode( ms_repo_new ).

    lv_url = mo_form_data->get( c_id-url ).

    IF mv_mode = c_mode-offline AND lv_url CP 'http*'.
      " Switch from URL to name
      lv_url = zcl_abapgit_url=>name( lv_url ).
    ENDIF.

    IF mv_mode <> c_mode-offline AND lv_url NP 'http*' AND mv_original_url CP 'http*'.
      " Switch back to original URL
      lv_url = mv_original_url.
    ENDIF.

    mo_form_data->set(
      iv_key = c_id-url
      iv_val = lv_url ).

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
      lo_repo->select_commit( |{ iv_commit }| ).
    ENDIF.

  ENDMETHOD.


  METHOD switch_to_pull_req.

    DATA:
      lo_repo   TYPE REF TO zcl_abapgit_repo_online,
      lv_url    TYPE string,
      lv_branch TYPE string.

    check_protection( ).

    lo_repo ?= mo_repo.

    IF iv_revert = abap_true.
      lo_repo->switch_origin( '' ).
    ELSE.
      SPLIT iv_pull AT '@' INTO lv_url lv_branch.
      lo_repo->switch_origin(
        iv_url    = lv_url
        iv_branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && lv_branch ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_form.

    DATA:
      lv_url   TYPE string,
      lx_error TYPE REF TO zcx_abapgit_exception.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    lv_url = mo_form_data->get( c_id-url ).

    IF mv_mode = c_mode-offline AND lv_url IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Enter a name for the repository and save' ).
    ENDIF.

    IF mv_mode <> c_mode-offline AND lv_url NP 'http*'.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Enter the URL of the repository and save' ).
    ELSEIF mv_mode <> c_mode-offline.
      TRY.
          zcl_abapgit_url=>name(
            iv_url      = lv_url
            iv_validate = abap_true ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-url
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_url    TYPE string,
      lv_branch TYPE string,
      lv_tag    TYPE string,
      lv_pull   TYPE string,
      lv_commit TYPE string.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-go_back.
        IF ms_repo_new <> ms_repo_current.
          mo_repo->refresh( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.

      WHEN c_event-choose_url.

        lv_url = choose_url( ).

        IF lv_url IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          ms_repo_new-url = lv_url.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-choose_branch.

        lv_branch = choose_branch( ).

        IF lv_branch IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          ms_repo_new-branch_name = lv_branch.
          mv_mode = c_mode-branch.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-choose_tag.

        lv_tag = choose_tag( ).

        IF lv_tag IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          ms_repo_new-branch_name = lv_tag.
          mv_mode = c_mode-tag.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-choose_commit.

        lv_commit = choose_commit( ).

        IF lv_commit IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          ms_repo_new-selected_commit = lv_commit.
          mv_mode = c_mode-commit.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-choose_pull_req.

        lv_pull = choose_pull_req( ).

        IF lv_pull IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          ms_repo_new-switched_origin = ms_repo_new-url.
          mv_pull_req = lv_pull.
          mv_mode = c_mode-pull_request.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-switch OR c_event-save.

        IF ii_event->mv_action = c_event-switch.
          switch_online_offline( ).
        ENDIF.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

    " If staying on form, initialize it with current settings
    IF rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      mo_form = get_form_schema( ).
      read_settings( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    gui_services( )->register_event_handler( me ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      read_settings( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
ENDCLASS.
