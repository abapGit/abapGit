CLASS zcl_abapgit_gui_page_addonline DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
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
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDONLINE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Clone online repository'. " TODO refactor
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_page.
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
      iv_placeholder = 'https://github.com/...git' ).
    lo_form->text(
      iv_name        = 'package'
      iv_required    = abap_true
      iv_side_action = 'choose-package'
      iv_label       = 'Package'
      iv_hint        = 'SAP package for the code (should be a dedicated one)'
      iv_placeholder = 'Z... / $...' ).
    lo_form->text(
      iv_name        = 'branch'
      iv_side_action = 'choose-branch'
      iv_label       = 'Branch'
      iv_hint        = 'Switch to a specific branch on clone (default: master)'
      iv_value       = 'master' ).
    lo_form->text(
      iv_name        = 'display-name'
      iv_label       = 'Display name'
      iv_hint        = 'Name to show instead of original repo name (optional)' ).
    lo_form->radio(
      iv_name        = 'folder-logic'
      iv_label       = 'Folder logic'
      iv_hint        = 'Define how package folders are named in the repo (see https://docs.abapgit.org)' ).
    lo_form->option(
      iv_selected    = abap_true
      iv_label       = 'Prefix'
      iv_value       = 'prefix' ).
    lo_form->option(
      iv_label       = 'Full'
      iv_value       = 'full' ).

    lo_form->checkbox(
      iv_name        = 'ignore-subpackages'
      iv_label       = 'Ignore subpackages'
      iv_hint        = 'Syncronize root package only (see https://docs.abapgit.org)' ).
    lo_form->checkbox(
      iv_name        = 'master-lang-only'
      iv_label       = 'Master language only'
      iv_hint        = 'Ignore translations, serialize just master language' ).

    ri_html->add( lo_form->render(
      iv_form_class = 'dialog w500px'
      iv_action = 'add-repo-online' ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
  ENDMETHOD.
ENDCLASS.
