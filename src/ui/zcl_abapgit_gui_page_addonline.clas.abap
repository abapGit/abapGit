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

  PROTECTED SECTION.

    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

    CONSTANTS:
      BEGIN OF c_event,
        go_back         TYPE string VALUE 'go-back',
        choose_package  TYPE string VALUE 'choose-package',
        create_package  TYPE string VALUE 'create-package',
        choose_branch   TYPE string VALUE 'choose-branch',
        add_online_repo TYPE string VALUE 'add-repo-online',
      END OF c_event.

    DATA ms_form_data TYPE zif_abapgit_services_repo=>ty_repo_params.

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
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDONLINE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Clone online repository'. " TODO refactor
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
        WHEN 'repo-url'.
          rs_form_data-url = ls_field-value.
        WHEN 'package'.
          rs_form_data-package = ls_field-value.
        WHEN 'branch'.
          rs_form_data-branch_name = ls_field-value.
        WHEN 'display-name'.
          rs_form_data-display_name = ls_field-value.
        WHEN 'folder-logic'.
          rs_form_data-folder_logic = ls_field-value.
        WHEN 'ignore-subpackages'.
          rs_form_data-ignore_subpackages = boolc( ls_field-value = 'on' ).
        WHEN 'master-lang-only'.
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
      iv_name        = 'repo-url'
      iv_required    = abap_true
      iv_label       = 'Git repository URL'
      iv_hint        = 'HTTPS address of the repository to clone'
      iv_value       = |{ ms_form_data-url }|
      iv_placeholder = 'https://github.com/...git' ).
    lo_form->text(
      iv_name        = 'package'
      iv_required    = abap_true
      iv_side_action = c_event-choose_package
      iv_label       = 'Package'
      iv_hint        = 'SAP package for the code (should be a dedicated one)'
      iv_value       = |{ ms_form_data-package }|
      iv_placeholder = 'Z... / $...' ).
    lo_form->text(
      iv_name        = 'branch'
      iv_side_action = c_event-choose_branch
      iv_label       = 'Branch'
      iv_hint        = 'Switch to a specific branch on clone (default: master)'
      iv_value       = |{ ms_form_data-branch_name }|
      iv_placeholder = 'master' ).
    lo_form->text(
      iv_name        = 'display-name'
      iv_label       = 'Display name'
      iv_value       = |{ ms_form_data-display_name }|
      iv_hint        = 'Name to show instead of original repo name (optional)' ).
    lo_form->radio(
      iv_name        = 'folder-logic'
      iv_label       = 'Folder logic'
      iv_hint        = 'Define how package folders are named in the repo (see https://docs.abapgit.org)' ).
    lo_form->option(
      iv_selected    = boolc( ms_form_data-folder_logic = 'prefix' OR ms_form_data-folder_logic IS INITIAL  )
      iv_label       = 'Prefix'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-prefix ).
    lo_form->option(
      iv_selected    = boolc( ms_form_data-folder_logic = 'full' )
      iv_label       = 'Full'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-full ).
    lo_form->checkbox(
      iv_name        = 'ignore-subpackages'
      iv_label       = 'Ignore subpackages'
      iv_checked     = ms_form_data-ignore_subpackages
      iv_hint        = 'Syncronize root package only (see https://docs.abapgit.org)' ).
    lo_form->checkbox(
      iv_name        = 'master-lang-only'
      iv_label       = 'Master language only'
      iv_checked     = ms_form_data-master_lang_only
      iv_hint        = 'Ignore translations, serialize just master language' ).
    lo_form->command(
      iv_is_main     = abap_true
      iv_label       = 'Clone online repo'
      iv_action      = c_event-add_online_repo ).
    lo_form->command(
      iv_label       = 'Create package'
      iv_action      = c_event-create_package ).
    lo_form->command(
      iv_label       = 'Back'
      iv_as_a        = abap_true " ignore required fields
      iv_action      = c_event-go_back ).

    ri_html->add( lo_form->render( iv_form_class = 'dialog w500px' ) ).

  ENDMETHOD.


  METHOD validate_form.

    IF is_form_data-url IS INITIAL.
      zcx_abapgit_exception=>raise( 'Url cannot be empty' ).
    ENDIF.

    zcl_abapgit_url=>validate( |{ is_form_data-url }| ).

    IF is_form_data-package IS INITIAL.
      zcx_abapgit_exception=>raise( 'Package cannot be empty' ).
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->validate_package(
      iv_package    = is_form_data-package
      iv_ign_subpkg = is_form_data-ignore_subpackages ).

    IF is_form_data-folder_logic <> zif_abapgit_dot_abapgit=>c_folder_logic-prefix
        AND is_form_data-folder_logic <> zif_abapgit_dot_abapgit=>c_folder_logic-full.
      zcx_abapgit_exception=>raise( |Invalid folder logic { is_form_data-folder_logic }. | &&
        |Choose either { zif_abapgit_dot_abapgit=>c_folder_logic-prefix } | &&
        |or { zif_abapgit_dot_abapgit=>c_folder_logic-full } | ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_event-go_back.
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-create_package.

        " Move this to services ?

        ms_form_data = parse_form( it_postdata ). " import data from html before re-render

        DATA ls_package_data TYPE scompkdtln.
        DATA lv_create       TYPE abap_bool.
        DATA li_popups       TYPE REF TO zif_abapgit_popups.

        li_popups = zcl_abapgit_ui_factory=>get_popups( ).

        li_popups->popup_to_create_package(
          IMPORTING
            es_package_data = ls_package_data
            ev_create       = lv_create ).
        IF lv_create = abap_false.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          zcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
          COMMIT WORK. " Hmmm ?

          ms_form_data-package = ls_package_data-devclass.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-choose_package.

        ms_form_data = parse_form( it_postdata ). " import data from html before re-render
        ms_form_data-package = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ).
        IF ms_form_data-package IS NOT INITIAL.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-add_online_repo.

        ms_form_data = parse_form( it_postdata ).
        validate_form( ms_form_data ).
        zcl_abapgit_services_repo=>new_online( ms_form_data ).
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

    ENDCASE.

    IF ev_state IS INITIAL. " TODO !!! Refactor this disaster !!!
      super->zif_abapgit_gui_event_handler~on_event(
        exporting
          iv_action = iv_action
          iv_getdata = iv_getdata
          it_postdata = it_postdata
        importing
          ei_page = ei_page
          ev_state = ev_state ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
