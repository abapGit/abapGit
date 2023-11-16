CLASS zcl_abapgit_gui_page_pull DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.

    CONSTANTS:
      BEGIN OF c_id,
        transport_request TYPE string VALUE 'transport_request',
      END OF c_id .

    CONSTANTS: BEGIN OF c_action,
                 pull      TYPE string VALUE 'pull',
                 refresh   TYPE string VALUE 'refresh',
                 choose_tr TYPE string VALUE 'choose_tr',
               END OF c_action.

    CLASS-METHODS create
      IMPORTING
        io_repo        TYPE REF TO zcl_abapgit_repo
        iv_trkorr      TYPE trkorr OPTIONAL
        ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        io_repo       TYPE REF TO zcl_abapgit_repo
        iv_trkorr     TYPE trkorr
        ii_obj_filter TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_repo       TYPE REF TO zcl_abapgit_repo.
    DATA mi_obj_filter TYPE REF TO zif_abapgit_object_filter.
    DATA mo_form_data  TYPE REF TO zcl_abapgit_string_map.
    DATA ms_checks     TYPE zif_abapgit_definitions=>ty_deserialize_checks.


    METHODS form
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

    METHODS choose_transport_request
      RAISING
        zcx_abapgit_exception .

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_PULL IMPLEMENTATION.


  METHOD choose_transport_request.

    DATA lv_transport_request TYPE trkorr.

    lv_transport_request = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( ).

    IF lv_transport_request IS NOT INITIAL.
      mo_form_data->set(
        iv_key = c_id-transport_request
        iv_val = lv_transport_request ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_repo       = io_repo.
    mi_obj_filter = ii_obj_filter.

    CREATE OBJECT mo_form_data.
    mo_form_data->set(
      iv_key = c_id-transport_request
      iv_val = iv_trkorr ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_pull.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        iv_trkorr     = iv_trkorr
        ii_obj_filter = ii_obj_filter.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Pull'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD form.

    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF ms_checks-overwrite.


    IF mi_obj_filter IS NOT INITIAL.
      lt_filter = mi_obj_filter->get_filter( ).
    ENDIF.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'pull-form' ).

    ro_form->start_group(
      iv_name  = 'id-objects'
      iv_label = 'Objects' ).

    LOOP AT ms_checks-overwrite ASSIGNING <ls_overwrite>.
      IF lines( lt_filter ) > 0.
        READ TABLE lt_filter WITH KEY object = <ls_overwrite>-obj_type
          obj_name = <ls_overwrite>-obj_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      ro_form->checkbox(
        iv_label = |{ <ls_overwrite>-obj_type } { <ls_overwrite>-obj_name }|
        iv_name  = |{ <ls_overwrite>-obj_type }-{ <ls_overwrite>-obj_name }| ).
    ENDLOOP.

    ro_form->text(
      iv_name        = c_id-transport_request
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_action-choose_tr
      iv_max         = 10
      iv_label       = |Transport Request| ).

    ro_form->command(
      iv_label    = 'Pull'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_action-pull
    )->command(
      iv_label    = 'Back'
      iv_action   = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lo_log TYPE REF TO zcl_abapgit_log.
    DATA lv_value TYPE string.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF ms_checks-overwrite.


    mo_form_data = ii_event->form_data( ).

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        mo_repo->refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-choose_tr.
        choose_transport_request( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-pull.
        ms_checks-transport-transport = mo_form_data->get( c_id-transport_request ).

        LOOP AT ms_checks-overwrite ASSIGNING <ls_overwrite>.
          lv_value = mo_form_data->get( |{ <ls_overwrite>-obj_type }-{ <ls_overwrite>-obj_name }| ).
          IF lv_value = 'on'.
            <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
          ELSE.
            <ls_overwrite>-decision = zif_abapgit_definitions=>c_no.
          ENDIF.
        ENDLOOP.

* todo, show log?
        CREATE OBJECT lo_log.
        mo_repo->deserialize(
          is_checks = ms_checks
          ii_log    = lo_log ).

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    ms_checks = mo_repo->deserialize_checks( ).

    IF lines( ms_checks-overwrite ) = 0.
      zcx_abapgit_exception=>raise(
        'There is nothing to pull. The local state completely matches the remote repository.' ).
    ENDIF.

    ri_html->add( form( )->render( mo_form_data ) ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
