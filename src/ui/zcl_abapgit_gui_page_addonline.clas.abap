CLASS zcl_abapgit_gui_page_addonline DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      " TODO importing prefilled form data
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        url                TYPE string VALUE 'url',
        package            TYPE string VALUE 'package',
        branch_name        TYPE string VALUE 'branch_name',
        display_name       TYPE string VALUE 'display_name',
        folder_logic       TYPE string VALUE 'folder_logic',
        ignore_subpackages TYPE string VALUE 'ignore_subpackages',
        master_lang_only   TYPE string VALUE 'master_lang_only',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back         TYPE string VALUE 'go-back',
        choose_package  TYPE string VALUE 'choose-package',
        create_package  TYPE string VALUE 'create-package',
        choose_branch   TYPE string VALUE 'choose-branch',
        add_online_repo TYPE string VALUE 'add-repo-online',
      END OF c_event.

    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.

    METHODS parse_form
      IMPORTING
        it_post_data TYPE cnht_post_data_tab
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        io_form_data TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDONLINE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_addonline.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title = 'Clone online repository'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD parse_form.

    DATA lt_form TYPE tihttpnvp.
    DATA ls_field LIKE LINE OF lt_form.

    lt_form = zcl_abapgit_html_action_utils=>parse_post_data( it_post_data ).
    CREATE OBJECT ro_form_data.

    LOOP AT lt_form INTO ls_field.
      CASE ls_field-name.
        WHEN c_id-url OR c_id-package OR c_id-branch_name OR c_id-display_name OR c_id-folder_logic.
          IF ls_field-name = c_id-package.
            ls_field-value = to_upper( ls_field-value ).
          ENDIF.
          ro_form_data->set(
            iv_key = ls_field-name
            iv_val = ls_field-value ).
        WHEN c_id-ignore_subpackages OR c_id-master_lang_only. " Flags
          ro_form_data->set(
            iv_key = ls_field-name
            iv_val = boolc( ls_field-value = 'on' ) ).
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |Unexpected form field [{ ls_field-name }]| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_form.

    DATA lx_err TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT ro_validation_log.

    IF io_form_data->get( c_id-url ) IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Url cannot be empty' ).
    ELSE.
      TRY.
          zcl_abapgit_url=>validate( io_form_data->get( c_id-url ) ).
        CATCH zcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-url
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF io_form_data->get( c_id-package ) IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-package
        iv_val = 'Package cannot be empty' ).
    ELSE.
      TRY.
          zcl_abapgit_repo_srv=>get_instance( )->validate_package(
            iv_package    = |{ io_form_data->get( c_id-package ) }|
            iv_ign_subpkg = |{ io_form_data->get( c_id-ignore_subpackages ) }| ).
        CATCH zcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-package
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF io_form_data->get( c_id-folder_logic ) <> zif_abapgit_dot_abapgit=>c_folder_logic-prefix
        AND io_form_data->get( c_id-folder_logic ) <> zif_abapgit_dot_abapgit=>c_folder_logic-full.
      ro_validation_log->set(
        iv_key = c_id-folder_logic
        iv_val = |Invalid folder logic { io_form_data->get( c_id-folder_logic )
        }. Must be { zif_abapgit_dot_abapgit=>c_folder_logic-prefix
        } or { zif_abapgit_dot_abapgit=>c_folder_logic-full } | ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA ls_repo_params TYPE zif_abapgit_services_repo=>ty_repo_params.

    mo_form_data = parse_form( it_postdata ). " import data from html before re-render

    CASE iv_action.
      WHEN c_event-go_back.
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-create_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_services_basis=>create_package(
            iv_prefill_package = |{ mo_form_data->get( 'package' ) }| ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_branch.

        mo_validation_log = validate_form( mo_form_data ).
        IF mo_validation_log->has( c_id-url ) = abap_true.
          mo_validation_log->set(
            iv_key = c_id-branch_name
            iv_val = 'Check URL issues' ).
          ev_state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
          RETURN.
        ENDIF.
        mo_form_data->set(
          iv_key = c_id-branch_name
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->branch_list_popup( mo_form_data->get( c_id-url ) )-name ).

        IF mo_form_data->get( c_id-branch_name ) IS INITIAL.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-branch_name
            iv_val = replace( " strip technical
              val = mo_form_data->get( c_id-branch_name )
              sub = 'refs/heads/'
              with = '' ) ).
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-add_online_repo.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          mo_form_data->to_abap( CHANGING cs_container = ls_repo_params ).
          zcl_abapgit_services_repo=>new_online( ls_repo_params ).
          ev_state = zcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.

    gui_services( )->register_event_handler( me ).

    ri_html = zcl_abapgit_html=>create( ).

    lo_form = zcl_abapgit_html_form=>create( iv_form_id = 'add-repo-online-form' ).
    lo_form->text(
      iv_name        = c_id-url
      iv_required    = abap_true
      iv_label       = 'Git repository URL'
      iv_hint        = 'HTTPS address of the repository to clone'
      iv_placeholder = 'https://github.com/...git' ).
    lo_form->text(
      iv_name        = c_id-package
      iv_side_action = c_event-choose_package
      iv_required    = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package for the code (should be a dedicated one)'
      iv_placeholder = 'Z... / $...' ).
    lo_form->text(
      iv_name        = c_id-branch_name
      iv_side_action = c_event-choose_branch
      iv_label       = 'Branch'
      iv_hint        = 'Switch to a specific branch on clone (default: master)'
      iv_placeholder = 'master' ).
    lo_form->radio(
      iv_name        = c_id-folder_logic
      iv_default_value = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_label       = 'Folder logic'
      iv_hint        = 'Define how package folders are named in the repo (see https://docs.abapgit.org)' ).
    lo_form->option(
      iv_label       = 'Prefix'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-prefix ).
    lo_form->option(
      iv_label       = 'Full'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-full ).
    lo_form->text(
      iv_name        = c_id-display_name
      iv_label       = 'Display name'
      iv_hint        = 'Name to show instead of original repo name (optional)' ).
    lo_form->checkbox(
      iv_name        = c_id-ignore_subpackages
      iv_label       = 'Ignore subpackages'
      iv_hint        = 'Syncronize root package only (see https://docs.abapgit.org)' ).
    lo_form->checkbox(
      iv_name        = c_id-master_lang_only
      iv_label       = 'Serialize master language only'
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
      iv_form_class     = 'dialog w600px m-em5-sides margin-v1' " to center add wmax600px and auto-center instead
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.
ENDCLASS.
