CLASS zcl_abapgit_gui_page_cr_repo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_form           TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data      TYPE REF TO zcl_abapgit_string_map.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util      TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mv_url TYPE string.

    CONSTANTS:
      BEGIN OF c_id,
        org         TYPE string VALUE 'org',
        name        TYPE string VALUE 'name',
        description TYPE string VALUE 'description',
        private     TYPE string VALUE 'private',
        token       TYPE string VALUE 'token',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        create TYPE string VALUE 'create',
      END OF c_event.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS set_defaults
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_cr_repo IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    mv_url = iv_url.

    set_defaults( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_cr_repo.

    CREATE OBJECT lo_component
      EXPORTING
        iv_url = iv_url.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Create GitHub Repository'
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'create-repository' ).

    ro_form->text(
      iv_label       = 'Organization'
      iv_name        = c_id-org
      iv_required    = abap_true ).

    ro_form->text(
      iv_label       = 'Repository Name'
      iv_name        = c_id-name
      iv_required    = abap_true ).

    ro_form->text(
      iv_label       = 'Description'
      iv_name        = c_id-description ).

    ro_form->checkbox(
      iv_label       = 'Private'
      iv_name        = c_id-private
      iv_hint        = 'Set visibility to private (organization) or public' ).

    ro_form->text(
      iv_label       = 'GitHub Token'
      iv_name        = c_id-token ).

    ro_form->command(
      iv_label       = 'Create'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-create ).

    ro_form->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD set_defaults.

    DATA lv_rest TYPE string ##NEEDED.
    DATA lv_org TYPE string.
    DATA lv_name TYPE string.

    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)'
      IN mv_url
      SUBMATCHES lv_org lv_name.
    IF sy-subrc = 0.
      mo_form_data->set(
        iv_key = c_id-org
        iv_val = lv_org ).

      mo_form_data->set(
        iv_key = c_id-name
        iv_val = lv_name ).
    ENDIF.

    mo_form_data->set(
      iv_key = c_id-private
      iv_val = abap_true ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_url TYPE string.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-create.
        mo_validation_log = mo_form_util->validate( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.

          zcl_abapgit_login_manager=>set_token(
            iv_uri   = 'https://api.github.com/'
            iv_token = mo_form_data->get( c_id-token ) ).

          " So far, this is only implemented for GitHub
          lv_url = |https://github.com/{ mo_form_data->get( c_id-org ) }/{ mo_form_data->get( c_id-name ) }|.

          zcl_abapgit_pr_enumerator=>new( lv_url )->create_repository(
            iv_description = mo_form_data->get( c_id-description )
            iv_private     = |{ mo_form_data->get( c_id-private ) }| ).

          MESSAGE 'Repository created' TYPE 'S'.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.
      WHEN OTHERS.
        " do nothing
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
