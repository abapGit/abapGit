CLASS zcl_abapgit_gui_page_ex_pckage DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
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
        package        TYPE string VALUE 'package',
        folder_logic   TYPE string VALUE 'folder_logic',
        main_lang_only TYPE string VALUE 'main_lang_only',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back        TYPE string VALUE 'go-back',
        export_package TYPE string VALUE 'export-package',
        choose_package TYPE string VALUE 'choose-object-type',
      END OF c_event.


    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS export_package
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_EX_PCKAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.


  METHOD create.
    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_ex_pckage.
    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Export Package to ZIP'
      ii_child_component = lo_component ).
  ENDMETHOD.


  METHOD export_package.
    DATA lv_package TYPE devclass.
    DATA lv_folder_logic TYPE string.
    DATA lv_main_lang_only TYPE abap_bool.

    lv_package        = mo_form_data->get( c_id-package ).
    lv_folder_logic   = mo_form_data->get( c_id-folder_logic ).
    lv_main_lang_only = mo_form_data->get( c_id-main_lang_only ).

    zcl_abapgit_zip=>export_package(
        iv_package        = lv_package
        iv_folder_logic   = lv_folder_logic
        iv_main_lang_only = lv_main_lang_only ).
  ENDMETHOD.


  METHOD get_form_schema.
    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'export-package-to-files' ).

    ro_form->text(
      iv_name          = c_id-package
      iv_label         = 'Package'
      iv_required      = abap_true
      iv_upper_case    = abap_true
      iv_side_action   = c_event-choose_package
    )->radio(
      iv_name          = c_id-folder_logic
      iv_label         = 'Folder Logic'
      iv_default_value = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_hint          = 'Define how package folders are named in repository'
    )->option(
      iv_label         = 'Prefix'
      iv_value         = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
    )->option(
      iv_label         = 'Full'
      iv_value         = zif_abapgit_dot_abapgit=>c_folder_logic-full
    )->option(
      iv_label         = 'Mixed'
      iv_value         = zif_abapgit_dot_abapgit=>c_folder_logic-mixed
    )->checkbox(
      iv_name          = c_id-main_lang_only
      iv_label         = 'Serialize Main Language Only'
      iv_hint          = 'Ignore translations, serialize just main language'
    )->command(
      iv_label         = 'Export Package to ZIP'
      iv_action        = c_event-export_package
      iv_cmd_type      = zif_abapgit_html_form=>c_cmd_type-input_main
    )->command(
      iv_label         = 'Back'
      iv_action        = c_event-go_back ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-go_back.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-export_package.

        mo_validation_log = mo_form_util->validate( mo_form_data ).
        IF mo_validation_log->is_empty( ) = abap_false.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          export_package( ).
          MESSAGE 'Package successfully exported' TYPE 'S'.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
        ENDIF.

      WHEN c_event-choose_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).

        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.
    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
  ENDMETHOD.
ENDCLASS.
