CLASS zcl_abapgit_gui_page_cpackage DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        iv_name        TYPE devclass
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_form           TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data      TYPE REF TO zcl_abapgit_string_map.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util      TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mv_default_name   TYPE devclass.

    CONSTANTS:
      BEGIN OF c_id,
        package            TYPE string VALUE 'package',
        description        TYPE string VALUE 'description',
        software_component TYPE string VALUE 'software_component',
        transport_layer    TYPE string VALUE 'transport_layer',
        super_package      TYPE string VALUE 'super_package',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        create TYPE string VALUE 'create',
      END OF c_event.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS get_defaults
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_cpackage IMPLEMENTATION.
  METHOD zif_abapgit_gui_menu_provider~get_menu.
    RETURN.
  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.

  METHOD get_form_schema.
    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'create-package' ).

    ro_form->text(
      iv_label       = 'Package'
      iv_name        = c_id-package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_max         = 30 ).

    ro_form->text(
      iv_label       = 'Description'
      iv_name        = c_id-description
      iv_required    = abap_true
      iv_max         = 60 ).

    ro_form->text(
      iv_label       = 'Software Component'
      iv_name        = c_id-software_component
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_placeholder = 'typically HOME or LOCAL'
      iv_max         = 20 ).

* not required for local packages
    ro_form->text(
      iv_label       = 'Transport Layer'
      iv_name        = c_id-transport_layer
      iv_upper_case  = abap_true
      iv_max         = 4 ).

    ro_form->text(
      iv_label       = 'Super Package'
      iv_name        = c_id-super_package
      iv_upper_case  = abap_true
      iv_max         = 30 ).

*********

    ro_form->command(
      iv_label       = 'Create'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-create ).

    ro_form->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_cpackage.

    CREATE OBJECT lo_component.
    lo_component->mv_default_name = iv_name.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Create Package'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA ls_create TYPE zif_abapgit_sap_package=>ty_create.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-create.
        mo_validation_log = mo_form_util->validate( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          ls_create-devclass = mo_form_data->get( c_id-package ).
          IF zcl_abapgit_factory=>get_sap_package( ls_create-devclass )->exists( ) = abap_true.
            zcx_abapgit_exception=>raise( |Package { ls_create-devclass } already exists| ).
          ENDIF.
          ls_create-ctext = mo_form_data->get( c_id-description ).
          ls_create-dlvunit = mo_form_data->get( c_id-software_component ).
          ls_create-parentcl = mo_form_data->get( c_id-super_package ).
          ls_create-pdevclass = mo_form_data->get( c_id-transport_layer ).
          ls_create-as4user = sy-uname.

          zcl_abapgit_factory=>get_sap_package( ls_create-devclass )->create( ls_create ).
          MESSAGE 'Package created' TYPE 'S'.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.
      WHEN OTHERS.
        " do nothing
    ENDCASE.

  ENDMETHOD.


  METHOD get_defaults.

    mo_form_data->set(
      iv_key = c_id-transport_layer
      iv_val = zcl_abapgit_factory=>get_sap_package( 'DUMMY' )->get_default_transport_layer( ) ).

    mo_form_data->set(
      iv_key = c_id-package
      iv_val = mv_default_name ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      get_defaults( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
