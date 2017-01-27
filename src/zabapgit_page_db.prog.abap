*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DB
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_db_display DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

ENDCLASS.

CLASS lcl_gui_page_db_display IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
    ms_control-page_title = 'CONFIG DISPLAY'.
  ENDMETHOD.

  METHOD render_content.

    DATA:
      lo_highlighter  TYPE REF TO lcl_syntax_highlighter,
      lv_data         TYPE lcl_persistence_db=>ty_content-data_str,
      ls_action       TYPE lcl_persistence_db=>ty_content,
      lv_action       TYPE string.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    " Create syntax highlighter
    lo_highlighter  = lcl_syntax_highlighter=>create( '*.xml' ).

    ls_action-type  = ms_key-type.
    ls_action-value = ms_key-value.
    lv_action       = lcl_html_action_utils=>dbkey_encode( ls_action ).
    lv_data         = lcl_xml_pretty=>print( lv_data ).
    lv_data         = lo_highlighter->process_line( lv_data ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="db_entry">' ).
    ro_html->add( '<table class="toolbar"><tr><td>' ).

    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Key:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).

    ro_html->add( '</td><td class="right">' ).
    ro_html->add_a( iv_txt = 'Edit' iv_act = |{ gc_action-db_edit }?{ lv_action }| ).
    ro_html->add( '</td></tr></table>' ).

    ro_html->add( |<pre class="syntax-hl">{ lv_data }</pre>| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

ENDCLASS.

CLASS lcl_gui_page_db_edit DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

ENDCLASS.

CLASS lcl_gui_page_db_edit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
    ms_control-page_title = 'CONFIG EDIT'.
  ENDMETHOD.

  METHOD render_content.

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

    ro_html->add( '<div class="db_entry">' ).

    " Banners
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Key:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).

    " Form
    ro_html->add( |<form id="db_form" method="post" action="sapevent:{ gc_action-db_update }">| ).
    ro_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ro_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ro_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_data
                     }</textarea>| ).
    ro_html->add( '</form>' ).

    " Menu
    lo_toolbar->add( iv_act = 'submitFormById(''db_form'');'
                     iv_txt = 'Save'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-strong ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( '</div>' ). "db_entry

  ENDMETHOD.  "render_content

ENDCLASS.

CLASS lcl_gui_page_db DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    METHODS explain_content
      IMPORTING is_data TYPE lcl_persistence_db=>ty_content
      RETURNING VALUE(rv_text) TYPE string
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_db IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'DATABASE PERSISTENCY'.
  ENDMETHOD.  " constructor.

  METHOD render_content.

    DATA: lt_data    TYPE lcl_persistence_db=>tt_content,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = lcl_app=>db( )->list( ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="db_list">' ).
    ro_html->add( '<table class="db_tab">' ).

    " Header
    ro_html->add( '<thead>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<th>Type</th>' ).
    ro_html->add( '<th>Key</th>' ).
    ro_html->add( '<th>Data</th>' ).
    ro_html->add( '<th></th>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).
    ro_html->add( '<tbody>' ).

    " Lines
    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR lv_trclass.
      IF sy-tabix = 1.
        lv_trclass = ' class="firstrow"' ##NO_TEXT.
      ENDIF.

      lv_action  = lcl_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display' iv_act = |{ gc_action-db_display }?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'    iv_act = |{ gc_action-db_edit }?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'  iv_act = |{ gc_action-db_delete }?{ lv_action }| ).

      ro_html->add( |<tr{ lv_trclass }>| ).
      ro_html->add( |<td>{ <ls_data>-type }</td>| ).
      ro_html->add( |<td>{ <ls_data>-value }</td>| ).
      ro_html->add( |<td class="data">{ explain_content( <ls_data> ) }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_vertical = abap_false ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</tbody>' ).
    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.            "render_content

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
          rv_text = |<strong>On-line</strong>, Name: <strong>{
                    lcl_url=>name( rv_text ) }</strong>|.
        ELSE.
          rv_text = |Off-line, Name: <strong>{ rv_text }</strong>|.
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

ENDCLASS.
