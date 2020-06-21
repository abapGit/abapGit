CLASS zcl_abapgit_gui_page_addonline DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      " TODO importing prefilled form data
      RETURNING
        VALUE(ro_page) TYPE REF TO zcl_abapgit_gui_page_addonline
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event REDEFINITION .

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        url TYPE string VALUE 'url',
        package TYPE string VALUE 'package',
        branch_name TYPE string VALUE 'branch_name',
        display_name TYPE string VALUE 'display_name',
        folder_logic TYPE string VALUE 'folder_logic',
        ignore_subpackages TYPE string VALUE 'ignore_subpackages',
        master_lang_only TYPE string VALUE 'master_lang_only',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back         TYPE string VALUE 'go-back',
        choose_package  TYPE string VALUE 'choose-package',
        create_package  TYPE string VALUE 'create-package',
        choose_branch   TYPE string VALUE 'choose-branch',
        add_online_repo TYPE string VALUE 'add-repo-online',
      END OF c_event.

    DATA ms_form_data TYPE zif_abapgit_services_repo=>ty_repo_params.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.

    METHODS parse_form
      IMPORTING
        it_post_data TYPE cnht_post_data_tab
      RETURNING
        VALUE(rs_form_data) TYPE zif_abapgit_services_repo=>ty_repo_params
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        is_form_data TYPE zif_abapgit_services_repo=>ty_repo_params
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDONLINE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Clone online repository'. " TODO refactor
    CREATE OBJECT mo_validation_log.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_page.
  ENDMETHOD.


  METHOD parse_form.

    DATA lt_form TYPE tihttpnvp.
    DATA ls_field LIKE LINE OF lt_form.

    lt_form = zcl_abapgit_html_action_utils=>parse_post_data( it_post_data ).

    LOOP AT lt_form INTO ls_field.
      CASE ls_field-name.
        WHEN c_id-url.
          rs_form_data-url = ls_field-value.
        WHEN c_id-package.
          rs_form_data-package = ls_field-value.
        WHEN c_id-branch_name.
          rs_form_data-branch_name = ls_field-value.
        WHEN c_id-display_name.
          rs_form_data-display_name = ls_field-value.
        WHEN c_id-folder_logic.
          rs_form_data-folder_logic = ls_field-value.
        WHEN c_id-ignore_subpackages.
          rs_form_data-ignore_subpackages = boolc( ls_field-value = 'on' ).
        WHEN c_id-master_lang_only.
          rs_form_data-master_lang_only = boolc( ls_field-value = 'on' ).
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |Unexpected form field [{ ls_field-name }]| ).
      ENDCASE.
    ENDLOOP.


  ENDMETHOD.


  METHOD render_content.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.

    ri_html = zcl_abapgit_html=>create( ).

    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form->text(
      iv_name        = c_id-url
      iv_value       = ms_form_data-url
      iv_required    = abap_true
      iv_label       = 'Git repository URL'
      iv_hint        = 'HTTPS address of the repository to clone'
      iv_placeholder = 'https://github.com/...git' ).
    lo_form->text(
      iv_name        = c_id-package
      iv_value       = |{ ms_form_data-package }|
      iv_side_action = c_event-choose_package
      iv_required    = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package for the code (should be a dedicated one)'
      iv_placeholder = 'Z... / $...' ).
    lo_form->text(
      iv_name        = c_id-branch_name
      iv_value       = ms_form_data-branch_name
      iv_side_action = c_event-choose_branch
      iv_label       = 'Branch'
      iv_hint        = 'Switch to a specific branch on clone (default: master)'
      iv_placeholder = 'master' ).
    lo_form->text(
      iv_name        = c_id-display_name
      iv_value       = ms_form_data-display_name
      iv_label       = 'Display name'
      iv_hint        = 'Name to show instead of original repo name (optional)' ).
    lo_form->radio(
      iv_name        = c_id-folder_logic
      iv_label       = 'Folder logic'
      iv_hint        = 'Define how package folders are named in the repo (see https://docs.abapgit.org)' ).
    lo_form->option(
      iv_selected    = boolc( ms_form_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
                           OR ms_form_data-folder_logic IS INITIAL  )
      iv_label       = 'Prefix'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-prefix ).
    lo_form->option(
      iv_selected    = boolc( ms_form_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full )
      iv_label       = 'Full'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-full ).
    lo_form->checkbox(
      iv_name        = c_id-ignore_subpackages
      iv_checked     = ms_form_data-ignore_subpackages
      iv_label       = 'Ignore subpackages'
      iv_hint        = 'Syncronize root package only (see https://docs.abapgit.org)' ).
    lo_form->checkbox(
      iv_name        = c_id-master_lang_only
      iv_checked     = ms_form_data-master_lang_only
      iv_label       = 'Master language only'
      iv_hint        = 'Ignore translations, serialize just master language' ).
    lo_form->command(
      iv_label       = 'Clone online repo'
      iv_is_main     = abap_true
      iv_action      = c_event-add_online_repo ).
    lo_form->command(
      iv_label       = 'Create package'
      iv_action      = c_event-create_package ).
    lo_form->command(
      iv_label       = 'Back'
      iv_action      = c_event-go_back ).

    ri_html->add( lo_form->render(
      iv_form_class     = 'dialog w500px'
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.


  METHOD validate_form.

    DATA lx_err TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT ro_validation_log.

    IF is_form_data-url IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Url cannot be empty' ).
    ELSE.
      TRY.
          zcl_abapgit_url=>validate( is_form_data-url ).
        CATCH zcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-url
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF is_form_data-package IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-package
        iv_val = 'Package cannot be empty' ).
    ELSE.
      TRY.
          zcl_abapgit_repo_srv=>get_instance( )->validate_package(
            iv_package    = is_form_data-package
            iv_ign_subpkg = is_form_data-ignore_subpackages ).
        CATCH zcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-package
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF is_form_data-folder_logic <> zif_abapgit_dot_abapgit=>c_folder_logic-prefix
        AND is_form_data-folder_logic <> zif_abapgit_dot_abapgit=>c_folder_logic-full.
      ro_validation_log->set(
        iv_key = c_id-folder_logic
        iv_val = |Invalid folder logic { is_form_data-folder_logic
        }. Must be { zif_abapgit_dot_abapgit=>c_folder_logic-prefix
        } or { zif_abapgit_dot_abapgit=>c_folder_logic-full } | ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_event-go_back.
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-create_package.

        ms_form_data = parse_form( it_postdata ). " import data from html before re-render
        ms_form_data-package = zcl_abapgit_services_basis=>create_package( ).
        IF ms_form_data-package IS NOT INITIAL.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_package.

        ms_form_data = parse_form( it_postdata ).
        ms_form_data-package = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ).
        IF ms_form_data-package IS NOT INITIAL.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_branch.

        ms_form_data = parse_form( it_postdata ).
        mo_validation_log = validate_form( ms_form_data ).
        IF mo_validation_log->has( c_id-url ) = abap_true.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.
        ms_form_data-branch_name = zcl_abapgit_ui_factory=>get_popups( )->branch_list_popup( ms_form_data-url )-name.
        IF ms_form_data-branch_name IS INITIAL.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          ms_form_data-branch_name = replace( " strip technical
            val = ms_form_data-branch_name
            sub = 'refs/heads/'
            with = '' ).
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-add_online_repo.

        ms_form_data = parse_form( it_postdata ).
        mo_validation_log = validate_form( ms_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          zcl_abapgit_services_repo=>new_online( ms_form_data ).
          ev_state = zcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

    IF ev_state IS INITIAL. " TODO !!! Refactor this disaster !!!
      super->zif_abapgit_gui_event_handler~on_event(
        EXPORTING
          iv_action = iv_action
          iv_getdata = iv_getdata
          it_postdata = it_postdata
        IMPORTING
          ei_page = ei_page
          ev_state = ev_state ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
