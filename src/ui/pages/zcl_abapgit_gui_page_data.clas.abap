CLASS zcl_abapgit_gui_page_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF c_event,
        add               TYPE string VALUE 'add',
        update            TYPE string VALUE 'update',
        remove            TYPE string VALUE 'remove',
        add_via_transport TYPE string VALUE 'add_via_transport',
      END OF c_event .

    CONSTANTS:
      BEGIN OF c_id,
        table        TYPE string VALUE 'table',
        where        TYPE string VALUE 'where',
        skip_initial TYPE string VALUE 'skip_initial',
      END OF c_id .

    DATA mi_config TYPE REF TO zif_abapgit_data_config .

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .

    CLASS-METHODS concatenated_key_to_where
      IMPORTING
        !iv_table       TYPE tabname
        !iv_tabkey      TYPE clike
      RETURNING
        VALUE(rv_where) TYPE string .
    METHODS add_via_transport
      RAISING
        zcx_abapgit_exception .
    METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    METHODS build_where
      IMPORTING
        !io_map         TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rt_where) TYPE string_table .
    METHODS render_add
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_existing
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS event_add
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception .
    METHODS event_remove
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception .
    METHODS event_update
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_gui_page_data IMPLEMENTATION.


  METHOD add_via_transport.

    DATA lt_trkorr  TYPE trwbo_request_headers.
    DATA ls_trkorr  LIKE LINE OF lt_trkorr.
    DATA ls_request TYPE trwbo_request.
    DATA ls_key     LIKE LINE OF ls_request-keys.
    DATA lv_where   TYPE string.
    DATA ls_config  TYPE zif_abapgit_data_config=>ty_config.


    lt_trkorr = zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    IF lines( lt_trkorr ) <> 1.
      RETURN.
    ENDIF.

    READ TABLE lt_trkorr INDEX 1 INTO ls_trkorr.
    ASSERT sy-subrc = 0.

    ls_request = zcl_abapgit_transport=>read( ls_trkorr ).

    IF lines( ls_request-keys ) = 0.
      zcx_abapgit_exception=>raise( |No keys found, select task| ).
    ENDIF.

    LOOP AT ls_request-keys INTO ls_key WHERE object = 'TABU'.
      ASSERT ls_key-objname IS NOT INITIAL.
      ASSERT ls_key-tabkey IS NOT INITIAL.

      CLEAR ls_config.
      ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
      ls_config-name = to_upper( ls_key-objname ).
      lv_where = concatenated_key_to_where(
        iv_table  = ls_key-objname
        iv_tabkey = ls_key-tabkey ).
      APPEND lv_where TO ls_config-where.
      mi_config->add_config( ls_config ).
    ENDLOOP.

  ENDMETHOD.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Add Via Transport'
                  iv_act = c_event-add_via_transport ).
    ro_menu->add( iv_txt = 'Back'
                  iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD build_where.

    DATA lv_where LIKE LINE OF rt_where.

    SPLIT io_map->get( c_id-where ) AT |\n| INTO TABLE rt_where.

    DELETE rt_where WHERE table_line IS INITIAL.

    LOOP AT rt_where INTO lv_where.
      IF strlen( lv_where ) <= 2.
        DELETE rt_where INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD concatenated_key_to_where.

    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_fields      TYPE ddfields.
    DATA ls_field       LIKE LINE OF lt_fields.
    DATA lv_key         TYPE c LENGTH 900.

    lv_key = iv_tabkey.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( iv_table ).

    lt_fields = lo_structdescr->get_ddic_field_list( ).

    LOOP AT lt_fields INTO ls_field WHERE keyflag = abap_true.
      IF ls_field-position = '0001' AND ls_field-datatype = 'CLNT'.
        lv_key = lv_key+ls_field-leng.
        CONTINUE.
      ENDIF.
      IF lv_key = |*|.
        EXIT. " current loop
      ENDIF.
      IF NOT rv_where IS INITIAL.
        rv_where = |{ rv_where } AND |.
      ENDIF.
      rv_where = |{ rv_where }{ to_lower( ls_field-fieldname ) } = '{ lv_key(ls_field-leng) }'|.
      lv_key = lv_key+ls_field-leng.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    ms_control-page_title = 'Data'.
    ms_control-page_menu = build_menu( ).

    mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    mi_config = mo_repo->get_data_config( ).

  ENDMETHOD.


  METHOD event_add.

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type         = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name         = to_upper( lo_map->get( c_id-table ) ).
    ls_config-skip_initial = lo_map->get( c_id-skip_initial ).
    ls_config-where        = build_where( lo_map ).

    mi_config->add_config( ls_config ).

  ENDMETHOD.


  METHOD event_remove.

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = to_upper( lo_map->get( c_id-table ) ).

    mi_config->remove_config( ls_config ).

  ENDMETHOD.


  METHOD event_update.

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type         = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name         = to_upper( lo_map->get( c_id-table ) ).
    ls_config-skip_initial = lo_map->has( to_upper( c_id-skip_initial ) ).
    ls_config-where        = build_where( lo_map ).

    mi_config->update_config( ls_config ).

  ENDMETHOD.


  METHOD render_add.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_form_data.

    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form->text(
      iv_label    = 'Table'
      iv_name     = c_id-table
      iv_required = abap_true ).

    lo_form->checkbox(
      iv_label = 'Skip Initial Values'
      iv_name  = c_id-skip_initial ).

    lo_form->textarea(
      iv_label       = 'Where'
      iv_placeholder = 'Conditions separated by newline'
      iv_name        = c_id-where ).

    lo_form->command(
      iv_label       = 'Add'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-add ).
    ri_html->add( lo_form->render( lo_form_data ) ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo">' ).
    ri_html->add( render_existing( ) ).
    ri_html->add( render_add( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_existing.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_form_data.

    lt_configs = mi_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      lo_form = zcl_abapgit_html_form=>create(  ).
      CREATE OBJECT lo_form_data.

      lo_form_data->set(
        iv_key = c_id-table
        iv_val = |{ ls_config-name }| ).
      lo_form->text(
        iv_label    = 'Table'
        iv_name     = c_id-table
        iv_readonly = abap_true ).

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
        iv_label       = 'Update'
        iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
        iv_action      = c_event-update ).
      lo_form->command(
        iv_label       = 'Remove'
        iv_action      = c_event-remove ).
      ri_html->add( lo_form->render( lo_form_data ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_event-add.
        event_add( ii_event ).
        mo_repo->refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-update.
        event_update( ii_event ).
        mo_repo->refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-remove.
        event_remove( ii_event ).
        mo_repo->refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-add_via_transport.
        add_via_transport( ).
        mo_repo->refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
