*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DB
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_db DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_display DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_display IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data TYPE lcl_persistence_db=>ty_content-data_str.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG DISPLAY' ) ).

    ro_html->add( '<div class="db_entry">' ).
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Value:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).
    ro_html->add( |<pre>{ lv_data }</pre>| ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DB ENTRY DISPLAY */'.
    _add 'div.db_entry {'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding: 0.5em;'.
    _add '}'.

    _add 'div.db_entry pre {'.
    _add '  display: block;'.
    _add '  overflow: hidden;'.
    _add '  word-wrap:break-word;'.
    _add '  white-space: pre-wrap;'.
    _add '  background-color: #eaeaea;'.
    _add '  padding: 0.5em;'.
    _add '  width: 50em;'.
    _add '}'.

    _add 'table.tag {'.
    _add '  display: inline-block;'.
    _add '  border: 1px #b3c1cc solid;'.
    _add '  background-color: #eee;'.
    _add '  margin-right: 0.5em; '.
    _add '}'.
    _add 'table.tag td { padding: 0.2em 0.5em; }'.
    _add 'table.tag td.label { background-color: #b3c1cc; }'.

  ENDMETHOD.            "styles

ENDCLASS.

CLASS lcl_gui_page_db_edit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_edit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data    TYPE lcl_persistence_db=>ty_content-data_str,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lcl_app=>db( )->lock(
      iv_type  = ms_key-type
      iv_value = ms_key-value ).

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG EDIT' ) ).

    ro_html->add( '<div class="db_entry">' ).

    " Banners
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Value:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).

    " Form
    ro_html->add( '<form id="db_form" method="post" action="sapevent:db_save">' ).
    ro_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ro_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ro_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_data
                     }</textarea>| ).
    ro_html->add( '</form>' ).

    " Menu
    lo_toolbar->add( iv_act = 'submitDBForm();'
                     iv_txt = 'Save'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-emphas ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( '</div>' ). "db_entry

    ro_html->add( footer( io_include_script = scripts( ) ) ).

  ENDMETHOD.

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DB ENTRY DISPLAY */'.
    _add 'div.db_entry {'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding: 0.5em;'.
    _add '}'.
    _add 'div.db_entry textarea { margin: 0.5em 0em; }'.
    _add 'table.tag {'.
    _add '  display: inline-block;'.
    _add '  border: 1px #b3c1cc solid;'.
    _add '  background-color: #eee;'.
    _add '  margin-right: 0.5em; '.
    _add '}'.
    _add 'table.tag td { padding: 0.2em 0.5em; }'.
    _add 'table.tag td.label { background-color: #b3c1cc; }'.

  ENDMETHOD.            "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    _add 'function submitDBForm() {'.
    _add '  document.getElementById("db_form").submit();'.
    _add '}'.

  ENDMETHOD.    "scripts

ENDCLASS.

CLASS lcl_gui_page_db IMPLEMENTATION.

  METHOD lif_gui_page~render.

    DATA: lt_data    TYPE lcl_persistence_db=>tt_content,
          lv_escaped TYPE string,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = lcl_app=>db( )->list( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DATABASE PERSISTENCY' ) ).

    ro_html->add( '<div class="db_list">' ).
    ro_html->add( '<table width="100%" class="db_tab">' ).

    " Header
    ro_html->add( '<tr>' ).
    ro_html->add( '<th>Type</th>' ).
    ro_html->add( '<th>Value</th>' ).
    ro_html->add( '<th>Data</th>' ).
    ro_html->add( '<th></th>' ).
    ro_html->add( '</tr>' ).

    " Lines
    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR lv_trclass.
      IF sy-tabix = 1.
        lv_trclass = ' class="firstrow"' ##NO_TEXT.
      ENDIF.

      IF strlen( <ls_data>-data_str ) >= 250.
        lv_escaped = escape( val    = <ls_data>-data_str(250)
                             format = cl_abap_format=>e_html_attr ).
      ELSE.
        lv_escaped = escape( val    = <ls_data>-data_str
                             format = cl_abap_format=>e_html_attr ).
      ENDIF.

      lv_action = lcl_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display' iv_act = |db_display?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'    iv_act = |db_edit?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'  iv_act = |db_delete?{ lv_action }| ).

      ro_html->add( |<tr{ lv_trclass }>| ).
      ro_html->add( |<td>{ <ls_data>-type }</td>| ).
      ro_html->add( |<td>{ <ls_data>-value }</td>| ).
      ro_html->add( |<td><pre>{ lv_escaped }</pre></td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_vertical = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.            "lif_gui_page~render

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DB ENTRIES */'.
    _add 'div.db_list {'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding: 0.5em;'.
    _add '}'.
    _add 'table.db_tab pre {'.
    _add '  display: block;'.
    _add '  overflow: hidden;'.
    _add '  word-wrap:break-word;'.
    _add '  white-space: pre-wrap;'.
    _add '  background-color: #eaeaea;'.
    _add '  padding: 3px;'.
    _add '  width: 50em;'.
    _add '}'.
    _add 'table.db_tab tr.firstrow td { padding-top: 0.5em; }'.
    _add 'table.db_tab th {'.
    _add '  text-align: left;'.
    _add '  color: #888;'.
    _add '  padding: 0.2em;'.
    _add '  border-bottom: 1px #ddd solid;'.
    _add '}'.
    _add 'table.db_tab td {'.
    _add '  color: #333;'.
    _add '  padding: 0.2em;'.
    _add '  vertical-align: top;'.
    _add '}'.

  ENDMETHOD.            "styles

ENDCLASS.