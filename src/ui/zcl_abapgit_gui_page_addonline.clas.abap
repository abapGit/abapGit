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


  PROTECTED SECTION.

    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDONLINE IMPLEMENTATION.


  METHOD create.
    CREATE OBJECT ro_page.
  ENDMETHOD.


  METHOD render_content.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.

    CREATE OBJECT ro_html.

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
      iv_name        = 'display-name'
      iv_label       = 'Display name'
      iv_hint        = 'Name to show instead of original repo name (optional)' ).
    lo_form->checkbox(
      iv_name        = 'ignore-subpackages'
      iv_label       = 'Ignore subpackages'
      iv_hint        = '??? TODO describe' ).
    lo_form->checkbox(
      iv_name        = 'master-lang-only'
      iv_label       = 'Master language only'
      iv_hint        = 'Ignore translations, serialize just master language' ).

    ro_html->add( lo_form->render(
      iv_form_class = 'dialog w500px'
      iv_action = 'add-repo-online' ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
  ENDMETHOD.
ENDCLASS.
