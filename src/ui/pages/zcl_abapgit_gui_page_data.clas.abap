CLASS zcl_abapgit_gui_page_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " This page is different from other dialogs since it contains multiple forms.
    " Existing data config is based on mi_config (and multiple lo_form_data).
    " A new entry is based on mo_form_data. WHen a particular form is submitted,
    " the on event handler will automatically get the corresponding data.
    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF c_event,
        add               TYPE string VALUE 'add',
        update            TYPE string VALUE 'update',
        remove            TYPE string VALUE 'remove',
        add_via_transport TYPE string VALUE 'add_via_transport',
      END OF c_event.
    CONSTANTS:
      BEGIN OF c_id,
        table        TYPE string VALUE 'table',
        where        TYPE string VALUE 'where',
        skip_initial TYPE string VALUE 'skip_initial',
      END OF c_id.

    DATA mi_config TYPE REF TO zif_abapgit_data_config.

  PRIVATE SECTION.

    DATA mi_repo TYPE REF TO zif_abapgit_repo.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map. " only for new config
    DATA mv_validation_tab TYPE tabname.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.

    CLASS-METHODS concatenated_key_to_where
      IMPORTING
        !iv_table       TYPE tabname
        !iv_tabkey      TYPE clike
      RETURNING
        VALUE(rv_where) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.
    METHODS add_via_transport
      RAISING
        zcx_abapgit_exception .
    METHODS build_table_name
      IMPORTING
        !iv_input         TYPE csequence
      RETURNING
        VALUE(rv_tabname) TYPE string.
    METHODS build_where
      IMPORTING
        !iv_where       TYPE string
      RETURNING
        VALUE(rt_where) TYPE string_table .
    METHODS render_for_existing_config
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_for_new_config
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS event_add
      IMPORTING
        !io_form_data TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS event_remove
      IMPORTING
        !io_form_data TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS event_update
      IMPORTING
        !io_form_data TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS config_load
      RAISING
        zcx_abapgit_exception.
    METHODS config_save
      RAISING
        zcx_abapgit_exception.
    METHODS validate_form
      IMPORTING
        !io_form_data TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.
    METHODS validate_table_name
      IMPORTING
        iv_table_name     TYPE tabname
      CHANGING
        co_validation_log TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS validate_where_clause
      IMPORTING
        iv_table_name     TYPE tabname
        it_where          TYPE string_table
      CHANGING
        co_validation_log TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_gui_page_data IMPLEMENTATION.


  METHOD add_via_transport.

    DATA lv_trkorr  TYPE trkorr.
    DATA ls_request TYPE zif_abapgit_cts_api=>ty_transport_data.
    DATA ls_key     LIKE LINE OF ls_request-keys.
    DATA lv_where   TYPE string.
    DATA ls_config  TYPE zif_abapgit_data_config=>ty_config.


    lv_trkorr = zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transport( ).
    IF lv_trkorr IS INITIAL.
      RETURN.
    ENDIF.

    ls_request = zcl_abapgit_factory=>get_cts_api( )->read( lv_trkorr ).

    IF lines( ls_request-keys ) = 0.
      zcx_abapgit_exception=>raise( |No keys found, select task| ).
    ENDIF.

    LOOP AT ls_request-keys INTO ls_key WHERE object = 'TABU'.
      ASSERT ls_key-objname IS NOT INITIAL.
      ASSERT ls_key-tabkey IS NOT INITIAL.

      CLEAR ls_config.
      ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
      ls_config-name = build_table_name( ls_key-objname ).
      lv_where = concatenated_key_to_where(
        iv_table  = ls_key-objname
        iv_tabkey = ls_key-tabkey ).
      APPEND lv_where TO ls_config-where.
      mi_config->add_config( ls_config ).
    ENDLOOP.

  ENDMETHOD.


  METHOD build_table_name.
    rv_tabname = to_upper( condense( iv_input ) ).
  ENDMETHOD.


  METHOD build_where.

    DATA lv_where LIKE LINE OF rt_where.

    rt_where = zcl_abapgit_convert=>split_string( iv_where ).

    DELETE rt_where WHERE table_line IS INITIAL.

    LOOP AT rt_where INTO lv_where.
      IF strlen( lv_where ) <= 2.
        DELETE rt_where INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD concatenated_key_to_where.

    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_typedescr   TYPE REF TO cl_abap_typedescr.
    DATA lt_fields      TYPE zcl_abapgit_data_utils=>ty_names.
    DATA lv_field       LIKE LINE OF lt_fields.
    DATA lv_table       TYPE tadir-obj_name.
    DATA lv_length      TYPE i.
    DATA lv_tabix       TYPE i.
    DATA lv_key         TYPE c LENGTH 900.

    lv_key = iv_tabkey.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( iv_table ).

    lv_table = iv_table.
    lt_fields = zcl_abapgit_data_utils=>list_key_fields( lv_table ).

    LOOP AT lt_fields INTO lv_field.
      lv_tabix = sy-tabix.
      lo_typedescr = cl_abap_typedescr=>describe_by_name( |{ iv_table }-{ lv_field }| ).
      lv_length = lo_typedescr->length / cl_abap_char_utilities=>charsize.

      IF lv_tabix = 1 AND lo_typedescr->get_relative_name( ) = 'MANDT'.
        lv_key = lv_key+lv_length.
        CONTINUE.
      ENDIF.

      IF lv_key = |*|.
        EXIT. " current loop
      ENDIF.
      IF NOT rv_where IS INITIAL.
        rv_where = |{ rv_where } AND |.
      ENDIF.
      rv_where = |{ rv_where }{ to_lower( lv_field ) } = '{ lv_key(lv_length) }'|.
      lv_key = lv_key+lv_length.
    ENDLOOP.

  ENDMETHOD.


  METHOD config_load.
    mi_config->zif_abapgit_data_persistence~load_config( mi_repo->get_key( ) ).
  ENDMETHOD.


  METHOD config_save.
    mi_config->zif_abapgit_data_persistence~save_config( mi_repo->get_key( ) ).
    mi_repo->refresh( ).
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    mi_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    CREATE OBJECT mi_config TYPE zcl_abapgit_data_config.
    config_load( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_data.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key = iv_key.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Data Config'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD event_add.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-type         = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name         = build_table_name( io_form_data->get( c_id-table ) ).
    ls_config-skip_initial = io_form_data->get( c_id-skip_initial ).
    ls_config-where        = build_where( io_form_data->get( c_id-where ) ).

    mi_config->add_config( ls_config ).

  ENDMETHOD.


  METHOD event_remove.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = build_table_name( io_form_data->get( c_id-table ) ).

    mi_config->remove_config( ls_config ).

  ENDMETHOD.


  METHOD event_update.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-type         = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name         = build_table_name( io_form_data->get( c_id-table ) ).
    ls_config-skip_initial = io_form_data->get( c_id-skip_initial ).
    ls_config-where        = build_where( io_form_data->get( c_id-where ) ).

    mi_config->update_config( ls_config ).

  ENDMETHOD.


  METHOD get_form_schema.
    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'data-config' ).

    ro_form->text(
      iv_label    = 'Table'
      iv_name     = c_id-table
      iv_required = abap_true
      iv_max      = 16 ).

    ro_form->checkbox(
      iv_label = 'Skip Initial Values'
      iv_name  = c_id-skip_initial ).

    ro_form->textarea(
      iv_label       = 'Where'
      iv_placeholder = 'Conditions separated by newline'
      iv_name        = c_id-where ).

    ro_form->command(
      iv_label    = 'Add'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-add ).
  ENDMETHOD.


  METHOD render_for_existing_config.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.
    DATA lv_form_id TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_configs = mi_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      lv_form_id = replace(
        val  = ls_config-name
        sub  = '/'
        with = '_'
        occ  = 0 ).
      lo_form = zcl_abapgit_html_form=>create( iv_form_id = lv_form_id ).
      CREATE OBJECT lo_form_data.

      lo_form_data->set(
        iv_key = c_id-table
        iv_val = |{ ls_config-name }| ).
      lo_form->text(
        iv_label    = 'Table'
        iv_name     = c_id-table
        iv_readonly = abap_true
        iv_max      = 16 ).

      lo_form_data->set(
        iv_key = c_id-skip_initial
        iv_val = ls_config-skip_initial ).
      lo_form->checkbox(
        iv_label = 'Skip Initial Values'
        iv_name  = c_id-skip_initial ).

      lo_form_data->set(
        iv_key = c_id-where
        iv_val = concat_lines_of( table = ls_config-where sep = |\n| ) ).
      lo_form->textarea(
        iv_label       = 'Where'
        iv_placeholder = 'Conditions separated by newline'
        iv_name        = c_id-where ).

      lo_form->command(
        iv_label    = 'Update'
        iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
        iv_action   = c_event-update ).
      lo_form->command(
        iv_label  = 'Remove'
        iv_action = c_event-remove ).

      IF ls_config-name = mv_validation_tab.
        ri_html->add( lo_form->render(
          io_values         = lo_form_data
          io_validation_log = mo_validation_log ) ).
      ELSE.
        ri_html->add( lo_form->render( lo_form_data ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD render_for_new_config.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF mv_validation_tab IS INITIAL.
      ri_html->add( mo_form->render(
        io_values         = mo_form_data
        io_validation_log = mo_validation_log ) ).
    ELSE.
      ri_html->add( mo_form->render( mo_form_data ) ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_form.

    mo_validation_log = zcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    mv_validation_tab = build_table_name( io_form_data->get( c_id-table ) ).

    validate_table_name(
      EXPORTING
        iv_table_name     = mv_validation_tab
      CHANGING
        co_validation_log = mo_validation_log ).

    IF mo_validation_log->is_empty( ) = abap_true.
      validate_where_clause(
        EXPORTING
          iv_table_name     = mv_validation_tab
          it_where          = build_where( io_form_data->get( c_id-where ) )
        CHANGING
          co_validation_log = mo_validation_log ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_table_name.

    DATA: ls_item   TYPE zif_abapgit_definitions=>ty_item,
          lv_exists TYPE abap_bool.

    CLEAR ls_item.
    ls_item-obj_name = iv_table_name.
    ls_item-obj_type = 'TABL'.
    lv_exists = zcl_abapgit_objects=>exists( ls_item ).

    IF lv_exists = abap_false.
      co_validation_log->set(
        iv_key = c_id-table
        iv_val = |Table { iv_table_name } does not exist| ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_where_clause.

    DATA:
      lv_where TYPE string,
      lv_count TYPE i,
      lv_total TYPE i,
      lx_error TYPE REF TO cx_root.

    TRY.
        IF it_where IS INITIAL.
          SELECT COUNT(*) FROM (iv_table_name) INTO lv_total.
        ELSE.
          LOOP AT it_where INTO lv_where.
            SELECT COUNT(*) FROM (iv_table_name) INTO lv_count WHERE (lv_where).
            lv_total = lv_total + lv_count.
          ENDLOOP.
        ENDIF.
      CATCH cx_root INTO lx_error.
        co_validation_log->set(
          iv_key = c_id-where
          iv_val = |Invalid where clause for table { iv_table_name }| ).
    ENDTRY.

    IF lv_total > zcl_abapgit_data_serializer=>c_max_records.
      co_validation_log->set(
        iv_key = c_id-where
        iv_val = |Too many records selected from table { iv_table_name } | &&
                 |(selected { lv_total }, max { zcl_abapgit_data_serializer=>c_max_records })| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.

    CASE ii_event->mv_action.
      WHEN c_event-add.
        " For new entries, we add the config only if validation passes
        mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).
        validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          event_add( mo_form_data ).
          config_save( ).

          " Clear form for new entry
          mo_form_data->delete( c_id-table ).
          mo_form_data->delete( c_id-skip_initial ).
          mo_form_data->delete( c_id-where ).
        ELSE.
          CLEAR mv_validation_tab.
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-update.
        " For existing entries, we update the config in memory. If
        " validation does not pass (for the where clause), the user
        " can correct it.
        lo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).
        validate_form( lo_form_data ).

        event_update( lo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          config_save( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-remove.
        lo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).
        event_remove( lo_form_data ).
        config_save( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-add_via_transport.
        add_via_transport( ).
        config_save( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-advanced-data' ).

    ro_toolbar->add( iv_txt = 'Add Via Transport'
                     iv_act = c_event-add_via_transport ).
    ro_toolbar->add( iv_txt = 'Back'
                     iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( render_for_existing_config( ) ).
    ri_html->add( render_for_new_config( ) ).
    ri_html->add( '</div>' ).

    CLEAR: mo_validation_log, mv_validation_tab.

  ENDMETHOD.
ENDCLASS.
