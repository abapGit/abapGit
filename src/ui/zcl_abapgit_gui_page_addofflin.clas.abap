CLASS zcl_abapgit_gui_page_addofflin DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    " TODO importing prefilled form data
    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        url              TYPE string VALUE 'url',
        package          TYPE string VALUE 'package',
        folder_logic     TYPE string VALUE 'folder_logic',
        master_lang_only TYPE string VALUE 'master_lang_only',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back          TYPE string VALUE 'go-back',
        choose_package   TYPE string VALUE 'choose-package',
        create_package   TYPE string VALUE 'create-package',
        add_offline_repo TYPE string VALUE 'add-repo-offline',
      END OF c_event.

    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.

    METHODS parse_form
      IMPORTING
        it_post_data        TYPE cnht_post_data_tab
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        io_form_data             TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDOFFLIN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_addofflin.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title = 'Create offline repository'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD parse_form.

    DATA lt_form TYPE tihttpnvp.
    DATA ls_field LIKE LINE OF lt_form.

    lt_form = zcl_abapgit_html_action_utils=>parse_post_data( it_post_data ).
    CREATE OBJECT ro_form_data.

    LOOP AT lt_form INTO ls_field.
      CASE ls_field-name.
        WHEN c_id-url OR c_id-package OR c_id-folder_logic.
          IF ls_field-name = c_id-package.
            ls_field-value = to_upper( ls_field-value ).
          ENDIF.
          ro_form_data->set(
            iv_key = ls_field-name
            iv_val = ls_field-value ).
        WHEN c_id-master_lang_only. " Flags
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

    IF io_form_data->get( c_id-package ) IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-package
        iv_val = 'Package cannot be empty' ).
    ELSE.
      TRY.
          zcl_abapgit_repo_srv=>get_instance( )->validate_package(
            iv_package    = |{ io_form_data->get( c_id-package ) }| ).
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

    DATA: ls_repo_params      TYPE zif_abapgit_services_repo=>ty_repo_params,
          lo_new_offline_repo TYPE REF TO zcl_abapgit_repo_offline.

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

      WHEN c_event-add_offline_repo.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          mo_form_data->to_abap( CHANGING cs_container = ls_repo_params ).
          lo_new_offline_repo = zcl_abapgit_services_repo=>new_offline( ls_repo_params ).
          CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_view_repo
            EXPORTING
              iv_key = lo_new_offline_repo->get_key( ).
          ev_state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.

    gui_services( )->register_event_handler( me ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lo_form = zcl_abapgit_html_form=>create( iv_form_id = 'add-repo-offline-form' ).
    lo_form->text(
      iv_name        = c_id-url
      iv_required    = abap_true
      iv_label       = 'Repository name'
      iv_hint        = 'Unique name for repository' ).
    lo_form->text(
      iv_name        = c_id-package
      iv_side_action = c_event-choose_package
      iv_required    = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package for the code (should be a dedicated one)'
      iv_placeholder = 'Z... / $...' ).
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
    lo_form->checkbox(
      iv_name        = c_id-master_lang_only
      iv_label       = 'Serialize master language only'
      iv_hint        = 'Ignore translations, serialize just master language' ).
    lo_form->command(
      iv_label       = 'Create offline repo'
      iv_is_main     = abap_true
      iv_action      = c_event-add_offline_repo ).
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
