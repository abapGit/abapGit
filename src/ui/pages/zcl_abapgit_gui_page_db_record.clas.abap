CLASS zcl_abapgit_gui_page_db_record DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      IMPORTING
        is_key TYPE zif_abapgit_persistence=>ty_content
        iv_edit_mode TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .

    METHODS constructor
      IMPORTING
        is_key TYPE zif_abapgit_persistence=>ty_content
        iv_edit_mode TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        update      TYPE string VALUE 'update',
        switch_mode TYPE string VALUE 'switch_mode',
      END OF c_action .

    DATA ms_key TYPE zif_abapgit_persistence=>ty_content.
    DATA mv_edit_mode TYPE abap_bool.

    METHODS render_view
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_edit
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.


    CLASS-METHODS render_record_banner
      IMPORTING
        is_key         TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rv_html) TYPE string.

    CLASS-METHODS dbcontent_decode
      IMPORTING
        io_form_data TYPE REF TO zcl_abapgit_string_map
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



CLASS ZCL_ABAPGIT_GUI_PAGE_DB_RECORD IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_edit_mode = iv_edit_mode.
    ms_key       = is_key.
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_db_record.
    DATA lv_page_title TYPE string.

    IF iv_edit_mode = abap_true.
      lv_page_title = 'Config Edit'.
    ELSE.
      lv_page_title = 'Config Display'.
    ENDIF.


    CREATE OBJECT lo_component
      EXPORTING
        iv_edit_mode = iv_edit_mode
        is_key       = is_key.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = lv_page_title " TODO title provider
      ii_child_component = lo_component ).

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


  METHOD render_edit.

    DATA: lv_data    TYPE zif_abapgit_persistence=>ty_content-data_str,
          lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    TRY.
        lv_data = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH zcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

    zcl_abapgit_persistence_db=>get_instance( )->lock(
      iv_type  = ms_key-type
      iv_value = ms_key-value ).

    lv_data = escape( val    = zcl_abapgit_xml_pretty=>print( lv_data )
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_toolbar.
    lo_toolbar->add( iv_act = 'submitFormById(''db_form'');'
                     iv_txt = 'Save'
                     iv_typ = zif_abapgit_html=>c_action_type-onclick
                     iv_opt = zif_abapgit_html=>c_html_opt-strong ).

    ri_html->add( '<div class="db_entry">' ).

    " Banners & Toolbar
    ri_html->add( '<table class="toolbar"><tr><td>' ).
    ri_html->add( render_record_banner( ms_key ) ).
    ri_html->add( '</td><td>' ).
    ri_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ri_html->add( '</td></tr></table>' ).

    " Form
    ri_html->add( |<form id="db_form" method="post" action="sapevent:{ c_action-update }">| ).
    ri_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ri_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ri_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_data }</textarea>| ).
    ri_html->add( '</form>' ).

    ri_html->add( '</div>' ). "db_entry

  ENDMETHOD.


  METHOD render_record_banner.
    rv_html = |<table class="tag"><tr><td class="label">Type:</td>|
           && | <td>{ is_key-type }</td></tr></table>\n|
           && |<table class="tag"><tr><td class="label">Key:</td>|
           && |  <td>{ is_key-value }</td></tr></table>|.
  ENDMETHOD.


  METHOD render_view.

    DATA:
      lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter,
      lo_toolbar     TYPE REF TO zcl_abapgit_html_toolbar,
      lv_data        TYPE zif_abapgit_persistence=>ty_content-data_str,
      ls_action      TYPE zif_abapgit_persistence=>ty_content.

    TRY.
        lv_data = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH zcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

    " Create syntax highlighter
    lo_highlighter  = zcl_abapgit_syntax_factory=>create( '*.xml' ).
    lv_data         = lo_highlighter->process_line( zcl_abapgit_xml_pretty=>print( lv_data ) ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_toolbar.
    lo_toolbar->add( iv_act = |{ c_action-switch_mode }|
                     iv_txt = 'Edit' ).

    ri_html->add( '<div class="db_entry">' ).
    ri_html->add( '<table class="toolbar"><tr><td>' ).
    ri_html->add( render_record_banner( ms_key ) ).
    ri_html->add( '</td><td>' ).
    ri_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ri_html->add( '</td></tr></table>' ).

    ri_html->add( |<pre class="syntax-hl">{ lv_data }</pre>| ).
    ri_html->add( '</div>' ).

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


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    IF mv_edit_mode = abap_true.
      ri_html = render_edit( ).
    ELSE.
      ri_html = render_view( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
