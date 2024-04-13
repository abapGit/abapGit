CLASS zcl_abapgit_gui_page_whereused DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_page_title,
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable,
      zif_abapgit_html_table.

    CLASS-METHODS create
      IMPORTING
        iv_package     TYPE devclass
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        refresh TYPE string VALUE 'refresh',
      END OF c_action.

    CONSTANTS c_title TYPE string VALUE 'Where Used'.
    DATA mv_package TYPE devclass.
    DATA mi_table TYPE REF TO zcl_abapgit_html_table.

    METHODS init_table_component
      RAISING
        zcx_abapgit_exception.

    METHODS render_filter_help_hint
      RETURNING
        VALUE(rv_html) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_WHEREUSED IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_package = iv_package.

    IF zcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { iv_package } does not exist| ).
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_whereused.

    CREATE OBJECT lo_component
      EXPORTING
        iv_package = iv_package.

    ri_page = zcl_abapgit_gui_page_hoc=>create( lo_component ).

  ENDMETHOD.


  METHOD init_table_component.

    DATA ls_sorting_state TYPE zif_abapgit_html_table=>ty_sorting_state.

    IF mi_table IS BOUND.
      RETURN.
    ENDIF.

    IF ls_sorting_state-column_id IS INITIAL.
      ls_sorting_state-column_id = 'package'.
    ENDIF.

    mi_table = zcl_abapgit_html_table=>create(
      ii_renderer              = me
      is_initial_sorting_state = ls_sorting_state ).
    mi_table->define_column_group(
        iv_group_title = 'Repo object'
        iv_group_id    = '' " No need
      )->define_column(
        iv_column_id    = 'dep_package'
        iv_column_title = 'Package'
      )->define_column(
        iv_column_id    = 'dep_obj_type'
        iv_column_title = 'Type'
      )->define_column(
        iv_column_id    = 'dep_obj_name'
        iv_column_title = 'Name'
      )->define_column(
        iv_column_id    = 'dep_used_obj'
        iv_column_title = 'Used obj' ).
    mi_table->define_column_group(
        iv_group_title = 'Used in'
        iv_group_id    = 'where' " Needed for CSS
      )->define_column(
        iv_column_id    = 'package'
        iv_column_title = 'Package'
      )->define_column(
        iv_column_id    = 'obj_type'
        iv_column_title = 'Type'
      )->define_column(
        iv_column_id    = 'obj_name'
        iv_column_title = 'Name' ).

  ENDMETHOD.


  METHOD render_filter_help_hint.

    DATA li_html TYPE REF TO zif_abapgit_html.

    li_html = zcl_abapgit_html=>create(
      )->add( `<p>This tool cycles through all objects in the repo `
      )->add( `and runs the standard where-used function against it. `
      )->add( `The result is displayed here less the usages `
      )->add( `inside the repo itself.</p>`
      )->add( `<p>The tool can be used to detect `
      )->add( `potential regressions in the code which uses the repo `
      )->add( `and external to it (e.g. when deploying updates `
      )->add( `to a library-like repo).</p>` ).

    rv_html = zcl_abapgit_gui_chunk_lib=>render_help_hint(
      li_html->render( iv_no_line_breaks = abap_true ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    IF mi_table->process_sorting_request( ii_event->mv_action ) = abap_true.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      RETURN.
    ENDIF.

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = c_title.

    ls_hotkey_action-description = |Refresh|.
    ls_hotkey_action-action = c_action-refresh.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( )->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = c_title.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lt_where_used TYPE zcl_abapgit_where_used_tools=>ty_dependency_tt.

    register_handlers( ).
    init_table_component( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->div(
      iv_class   = 'wu-header'
      iv_content = |Where used for package {
        zcl_abapgit_gui_chunk_lib=>render_package_name( mv_package )->render( iv_no_line_breaks = abap_true )
        } and it's subpackages. { render_filter_help_hint( ) }| ).

    lt_where_used = zcl_abapgit_where_used_tools=>new( )->select_external_usages( mv_package ).

    ri_html->div(
      iv_class   = 'wu'
      ii_content = mi_table->render(
        iv_wrap_in_div   = 'default-table-container'
        iv_css_class     = 'default-table'
        iv_with_cids     = abap_true
        it_data          = lt_where_used ) ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.
  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

    FIELD-SYMBOLS <ls_i> TYPE zcl_abapgit_where_used_tools=>ty_dependency.

    ASSIGN is_row TO <ls_i>.

    CASE iv_column_id.
      WHEN 'obj_type'.
        IF <ls_i>-obj_prog_type IS INITIAL.
          rs_render-content = <ls_i>-obj_type.
        ELSE.
          rs_render-content = <ls_i>-obj_type && ':' && <ls_i>-obj_prog_type.
        ENDIF.
      WHEN 'obj_name'.
        rs_render-content = zcl_abapgit_gui_chunk_lib=>get_item_link(
          iv_obj_type = <ls_i>-obj_type
          iv_obj_name = <ls_i>-obj_name ).
      WHEN OTHERS.
        rs_render-content = iv_value.
    ENDCASE.
    " TODO maybe add title for object cls ?

  ENDMETHOD.
ENDCLASS.
