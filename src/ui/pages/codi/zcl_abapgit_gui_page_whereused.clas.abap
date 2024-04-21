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
        iv_package TYPE devclass OPTIONAL
        ii_repo    TYPE REF TO zif_abapgit_repo OPTIONAL
        PREFERRED PARAMETER iv_package
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        ii_repo    TYPE REF TO zif_abapgit_repo OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        refresh TYPE string VALUE 'refresh',
        show_used_obj TYPE string VALUE 'show_used_obj',
      END OF c_action.

    CONSTANTS c_title TYPE string VALUE 'Where Used'.
    DATA mv_package TYPE devclass.
    DATA mv_ignore_subpackages TYPE abap_bool.
    DATA mi_table TYPE REF TO zcl_abapgit_html_table.
    DATA mv_show_used_obj TYPE abap_bool.
    DATA mt_where_used TYPE zcl_abapgit_where_used_tools=>ty_dependency_tt.

    METHODS init_table_component
      RAISING
        zcx_abapgit_exception.

    METHODS render_filter_help_hint
      RETURNING
        VALUE(rv_html) TYPE string.
    METHODS render_header
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
    METHODS run_where_used
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_WHEREUSED IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    IF ii_repo IS BOUND.
      mv_package = ii_repo->get_package( ).
      mv_ignore_subpackages = ii_repo->get_local_settings( )-ignore_subpackages.
    ELSE.
      mv_package = iv_package.
    ENDIF.

    IF mv_package IS INITIAL OR zcl_abapgit_factory=>get_sap_package( mv_package )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { mv_package } does not exist| ).
    ENDIF.

    run_where_used( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_whereused.

    CREATE OBJECT lo_component
      EXPORTING
        ii_repo    = ii_repo
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
        iv_column_title = 'Name' ).
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

    rv_html = zcl_abapgit_gui_chunk_lib=>render_help_hint( li_html->render( iv_no_line_breaks = abap_true ) ).

  ENDMETHOD.


  METHOD render_header.

    DATA lv_sub_hint TYPE string.

    IF mv_ignore_subpackages = abap_false.
      lv_sub_hint = ' and its subpackages'.
    ENDIF.

    ii_html->div(
      iv_class   = 'wu-header'
      iv_content = |Where used for package {
        zcl_abapgit_gui_chunk_lib=>render_package_name( mv_package )->render( iv_no_line_breaks = abap_true )
        }{ lv_sub_hint } in other packages. { render_filter_help_hint( ) }| ).

  ENDMETHOD.


  METHOD run_where_used.
    mt_where_used = zcl_abapgit_where_used_tools=>new( )->select_external_usages(
      iv_ignore_subpackages = mv_ignore_subpackages
      iv_package            = mv_package ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    IF mi_table->process_sorting_request( ii_event->mv_action ) = abap_true.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      RETURN.
    ENDIF.

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        run_where_used( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-show_used_obj.
        mv_show_used_obj = boolc( mv_show_used_obj = abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.
    rt_hotkey_actions = zif_abapgit_gui_menu_provider~get_menu( )->get_hotkeys( c_title ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA lv_show_used_txt TYPE string.

    IF mv_show_used_obj = abap_true.
      lv_show_used_txt = 'Hide Used Type'.
    ELSE.
      lv_show_used_txt = 'Show Used Type'.
    ENDIF.

    ro_toolbar = zcl_abapgit_html_toolbar=>create(
      )->add(
        iv_txt    = lv_show_used_txt
        iv_title  = 'Show/Hide used type or object (when available)'
        iv_act    = c_action-show_used_obj
        iv_hotkey = 'u'
      )->add(
        iv_txt    = 'Refresh'
        iv_act    = c_action-refresh
        iv_hotkey = 'r' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = c_title.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).
    init_table_component( ).

    ri_html = zcl_abapgit_html=>create( ).

    render_header( ri_html ).

    IF mt_where_used IS INITIAL.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_success( 'No usages found' ) ).
    ELSE.
      ri_html->div(
        iv_class   = 'wu'
        ii_content = mi_table->render(
          iv_wrap_in_div   = 'default-table-container'
          iv_css_class     = 'default-table'
          iv_with_cids     = abap_true
          it_data          = mt_where_used ) ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.
  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

    FIELD-SYMBOLS <ls_i> TYPE zcl_abapgit_where_used_tools=>ty_dependency.

    ASSIGN is_row TO <ls_i>.

    CASE iv_column_id.
      WHEN 'dep_obj_name'.
        rs_render-content = <ls_i>-dep_obj_name.
        IF mv_show_used_obj = abap_true.
          rs_render-content = rs_render-content && |<span class='used-obj'>{ <ls_i>-dep_used_obj }</span>|.
        ENDIF.
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
