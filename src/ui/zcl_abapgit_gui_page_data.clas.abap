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
        add    TYPE string VALUE 'add',
        update TYPE string VALUE 'update',
        remove TYPE string VALUE 'remove',
      END OF c_event .

    CONSTANTS:
      BEGIN OF c_id,
        table TYPE string VALUE 'table',
        where TYPE string VALUE 'where',
      END OF c_id .

    DATA mi_config TYPE REF TO zif_abapgit_data_config .

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .

    METHODS build_where
      IMPORTING
        !io_map         TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rt_where) TYPE string_table .
    METHODS render_add
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
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


  METHOD constructor.

    super->constructor( ).

    ms_control-page_title = 'Data'.

    mo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    mi_config = mo_repo->get_data_config( ).

  ENDMETHOD.


  METHOD event_add.

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = to_upper( lo_map->get( c_id-table ) ).
    ls_config-where = build_where( lo_map ).

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

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = to_upper( lo_map->get( c_id-table ) ).
    ls_config-where = build_where( lo_map ).

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

    ri_html->add( render_add( ) ).
    ri_html->add( render_existing( ) ).

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
        iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
        iv_action      = c_event-remove ).
      ri_html->add( lo_form->render( lo_form_data ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_event-add.
        event_add( ii_event ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-update.
        event_update( ii_event ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-remove.
        event_remove( ii_event ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
