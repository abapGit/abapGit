CLASS zcl_abapgit_gui_page_db_entry DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_page_title .

    CLASS-METHODS create
      IMPORTING
        !is_key        TYPE zif_abapgit_persistence=>ty_content
        !iv_edit_mode  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !is_key       TYPE zif_abapgit_persistence=>ty_content
        !iv_edit_mode TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        update      TYPE string VALUE 'update',
        switch_mode TYPE string VALUE 'switch_mode',
      END OF c_action .

    CONSTANTS c_edit_form_id TYPE string VALUE `db_form`.
    CONSTANTS c_css_url TYPE string VALUE 'css/page_db_entry.css'.

    DATA ms_key TYPE zif_abapgit_persistence=>ty_content.
    DATA mv_edit_mode TYPE abap_bool.

    METHODS register_stylesheet
      RAISING
        zcx_abapgit_exception.

    METHODS render_view
      IMPORTING
        iv_raw_db_value TYPE zif_abapgit_persistence=>ty_content-data_str
        ii_html         TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_edit
      IMPORTING
        iv_raw_db_value TYPE zif_abapgit_persistence=>ty_content-data_str
        ii_html         TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      IMPORTING
        ii_html    TYPE REF TO zif_abapgit_html
        io_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    METHODS build_toolbar
      RETURNING
        VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS render_entry_tag
      IMPORTING
        is_key         TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rv_html) TYPE string.

    CLASS-METHODS dbcontent_decode
      IMPORTING
        io_form_data      TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rs_content) TYPE zif_abapgit_persistence=>ty_content
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS do_update
      IMPORTING
        is_content TYPE zif_abapgit_persistence=>ty_content
      RAISING
        zcx_abapgit_exception .

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DB_ENTRY IMPLEMENTATION.


  METHOD build_toolbar.

    CREATE OBJECT ro_toolbar.

    IF mv_edit_mode = abap_true.
      ro_toolbar->add(
        iv_act = |submitFormById('{ c_edit_form_id }');|
        iv_txt = 'Save'
        iv_typ = zif_abapgit_html=>c_action_type-onclick
        iv_opt = zif_abapgit_html=>c_html_opt-strong ).
    ELSE.
      ro_toolbar->add(
        iv_act = |{ c_action-switch_mode }|
        iv_txt = 'Edit' ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    register_stylesheet( ).
    mv_edit_mode = iv_edit_mode.
    ms_key       = is_key.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_db_entry.

    CREATE OBJECT lo_component
      EXPORTING
        iv_edit_mode = iv_edit_mode
        is_key       = is_key.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_extra_css_url       = c_css_url
      ii_page_title_provider = lo_component
      ii_child_component     = lo_component ).

  ENDMETHOD.


  METHOD dbcontent_decode.

    rs_content-type     = io_form_data->get( 'TYPE' ).
    rs_content-value    = io_form_data->get( 'VALUE' ).
    rs_content-data_str = io_form_data->get( 'XMLDATA' ).

    IF rs_content-data_str(1) <> '<' AND rs_content-data_str+1(1) = '<'. " Hmmm ???
      rs_content-data_str = rs_content-data_str+1.
    ENDIF.

  ENDMETHOD.


  METHOD do_update.

    ASSERT is_content-type IS NOT INITIAL.

    zcl_abapgit_persistence_db=>get_instance( )->update(
      iv_type  = is_content-type
      iv_value = is_content-value
      iv_data  = is_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD register_stylesheet.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    CREATE OBJECT lo_buf.

    " @@abapmerge include zabapgit_css_page_db_entry.w3mi.data.css > lo_buf->add( '$$' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB_ENTRY'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.


  METHOD render_edit.

    DATA lv_formatted TYPE string.

    lv_formatted = escape(
      val    = zcl_abapgit_xml_pretty=>print( iv_raw_db_value )
      format = cl_abap_format=>e_html_attr ).

    " Form
    ii_html->add( |<form id="{ c_edit_form_id }" method="post" action="sapevent:{ c_action-update }">| ).
    ii_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ii_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ii_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_formatted }</textarea>| ).
    ii_html->add( '</form>' ).

  ENDMETHOD.


  METHOD render_entry_tag.

    rv_html =
      |<dl class="entry-tag">| &&
      |<div><dt>Type</dt><dd>{ is_key-type }</dd></div>| &&
      |<div><dt>Key</dt><dd>{ is_key-value }</dd></div>| &&
      |</dl>|.

  ENDMETHOD.


  METHOD render_header.

    ii_html->add( '<div class="toolbar">' ).
    ii_html->add( io_toolbar->render( iv_right = abap_true ) ).
    ii_html->add( render_entry_tag( ms_key ) ).
    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_view.

    DATA lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter.
    DATA lv_formatted   TYPE string.

    " Create syntax highlighter
    lo_highlighter = zcl_abapgit_syntax_factory=>create( '*.xml' ).
    lv_formatted   = lo_highlighter->process_line( zcl_abapgit_xml_pretty=>print( iv_raw_db_value ) ).

    ii_html->add( |<pre class="syntax-hl">{ lv_formatted }</pre>| ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-switch_mode.
        mv_edit_mode = boolc( mv_edit_mode = abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-update.
        do_update( dbcontent_decode( ii_event->form_data( ) ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.

    IF mv_edit_mode = abap_true.
      rv_title = 'Config Edit'.
    ELSE.
      rv_title = 'Config Display'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lv_raw_db_value TYPE zif_abapgit_persistence=>ty_content-data_str.

    register_handlers( ).

    TRY.
        lv_raw_db_value = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH zcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="db-entry">' ).

    render_header(
      ii_html    = ri_html
      io_toolbar = build_toolbar( ) ).

    IF mv_edit_mode = abap_true.
      zcl_abapgit_persistence_db=>get_instance( )->lock(
        iv_type  = ms_key-type
        iv_value = ms_key-value ).
      render_edit(
        iv_raw_db_value = lv_raw_db_value
        ii_html         = ri_html ).
    ELSE.
      render_view(
        iv_raw_db_value = lv_raw_db_value
        ii_html         = ri_html ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
