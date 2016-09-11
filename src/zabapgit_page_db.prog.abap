*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DB
*&---------------------------------------------------------------------*

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

    DATA: lv_data   TYPE lcl_persistence_db=>ty_content-data_str,
          ls_action TYPE lcl_persistence_db=>ty_content,
          lv_action TYPE string.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    ls_action-type  = ms_key-type.
    ls_action-value = ms_key-value.
    lv_action       = lcl_html_action_utils=>dbkey_encode( ls_action ).

    lv_data         = lcl_xml_pretty=>print( lv_data ).
    lv_data         = escape( val    = lv_data
                              format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG DISPLAY' ) ).

    ro_html->add( '<div class="db_entry">' ).
    ro_html->add( '<table class="toolbar"><tr><td>' ).

    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Key:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).

    ro_html->add( '</td><td class="right">' ).
    ro_html->add_anchor( iv_txt = 'Edit' iv_act = |db_edit?{ lv_action }| ).
    ro_html->add( '</td></tr></table>' ).

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
    _add '  margin: 0.5em 0em;'.
    _add '  width: 50em;'.
    _add '}'.

    _add 'div.db_entry table.toolbar {'.
    _add '  width: 50em;'.
    _add '}'.

    _add 'table.tag {'.
    _add '  display: inline-block;'.
    _add '  border: 1px #b3c1cc solid;'.
    _add '  background-color: #eee;'.
    _add '  border-radius: 3px;'.
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
    ro_html->add( |<table class="tag"><tr><td class="label">Key:</td>| &&
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
    _add '  border-radius: 3px;'.
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

CLASS lcl_gui_page_db DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS explain_content
      IMPORTING is_data TYPE lcl_persistence_db=>ty_content
      RETURNING VALUE(rv_text) TYPE string
      RAISING   lcx_exception.


ENDCLASS.

CLASS lcl_gui_page_db IMPLEMENTATION.

  METHOD lif_gui_page~render.

    DATA: lt_data    TYPE lcl_persistence_db=>tt_content,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = lcl_app=>db( )->list( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DATABASE PERSISTENCY' ) ).

    ro_html->add( '<div class="db_list">' ).
    ro_html->add( '<table class="db_tab">' ).

    " Header
    ro_html->add( '<tr>' ).
    ro_html->add( '<th>Type</th>' ).
    ro_html->add( '<th>Key</th>' ).
    ro_html->add( '<th>Data</th>' ).
    ro_html->add( '<th></th>' ).
    ro_html->add( '</tr>' ).

    " Lines
    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR lv_trclass.
      IF sy-tabix = 1.
        lv_trclass = ' class="firstrow"' ##NO_TEXT.
      ENDIF.

      lv_action  = lcl_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display' iv_act = |db_display?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'    iv_act = |db_edit?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'  iv_act = |db_delete?{ lv_action }| ).

      ro_html->add( |<tr{ lv_trclass }>| ).
      ro_html->add( |<td>{ <ls_data>-type }</td>| ).
      ro_html->add( |<td>{ <ls_data>-value }</td>| ).
      ro_html->add( |<td class="data">{ explain_content( <ls_data> ) }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_vertical = abap_false ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.            "lif_gui_page~render

  METHOD explain_content.
    DATA: lv_result TYPE match_result,
          lv_match  TYPE submatch_result,
          lv_cnt    TYPE i.

    CASE is_data-type.
      WHEN 'REPO'.
        FIND FIRST OCCURRENCE OF REGEX '<url>(.*)</url>'
          IN is_data-data_str IGNORING CASE RESULTS lv_result.
        READ TABLE lv_result-submatches INTO lv_match INDEX 1.
        IF sy-subrc IS INITIAL.
          rv_text = is_data-data_str+lv_match-offset(lv_match-length).
        ENDIF.

        FIND FIRST OCCURRENCE OF REGEX '<OFFLINE/>'
          IN is_data-data_str IGNORING CASE MATCH COUNT lv_cnt.
        IF lv_cnt > 0.
          rv_text = |<b>On-line</b>, Name: <b>{ lcl_url=>name( rv_text ) }</b>|.
        ELSE.
          rv_text = |Off-line, Name: <b>{ rv_text }</b>|.
        ENDIF.

      WHEN 'BACKGROUND'.
        FIND FIRST OCCURRENCE OF REGEX '<method>(.*)</method>'
          IN is_data-data_str IGNORING CASE RESULTS lv_result.
        READ TABLE lv_result-submatches INTO lv_match INDEX 1.
        IF sy-subrc IS NOT INITIAL.
          RETURN.
        ENDIF.
        rv_text = |Method: { is_data-data_str+lv_match-offset(lv_match-length) }, |
               && |Repository: { lcl_app=>repo_srv( )->get( is_data-value )->get_name( ) }|.

      WHEN 'USER'.
        rv_text = '-'. " No additional explanation for user
      WHEN OTHERS.
        IF strlen( is_data-data_str ) >= 250.
          rv_text = is_data-data_str(250).
        ELSE.
          rv_text = is_data-data_str.
        ENDIF.
        rv_text = escape( val    = rv_text
                          format = cl_abap_format=>e_html_attr ).
        rv_text = |<pre>{ rv_text }</pre>|.
    ENDCASE.
  ENDMETHOD.  "explain_content

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DB ENTRIES */'.
    _add 'div.db_list {'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding: 0.5em;'.
    _add '}'.
    _add 'table.db_tab pre {'.
    _add '  display: inline-block;'.
    _add '  overflow: hidden;'.
    _add '  word-wrap:break-word;'.
    _add '  white-space: pre-wrap;'.
    _add '  margin: 0px;'.
    _add '  width: 30em;'.
    _add '}'.
    _add 'table.db_tab tr.firstrow td { padding-top: 0.5em; }'.
    _add 'table.db_tab th {'.
    _add '  text-align: left;'.
    _add '  color: #888;'.
    _add '  padding: 0.5em;'.
    _add '  border-bottom: 1px #ddd solid;'.
    _add '}'.
    _add 'table.db_tab td {'.
    _add '  color: #333;'.
    _add '  padding: 0.5em;'.
    _add '  vertical-align: top;'.
    _add '}'.
    _add 'table.db_tab td.data {'.
    _add '  color: #888;'.
    _add '  font-style: italic;'.
    _add '}'.

  ENDMETHOD.            "styles

ENDCLASS.