CLASS zcl_abapgit_gui_page_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    METHODS explain_content
      IMPORTING is_data        TYPE zif_abapgit_persistence=>ty_content
      RETURNING VALUE(rv_text) TYPE string
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DB IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'DATABASE PERSISTENCY'.
  ENDMETHOD.  " constructor.


  METHOD explain_content.

    DATA: ls_result TYPE match_result,
          ls_match  TYPE submatch_result,
          lv_cnt    TYPE i.


    CASE is_data-type.
      WHEN 'REPO'.
        FIND FIRST OCCURRENCE OF REGEX '<url>(.*)</url>'
          IN is_data-data_str IGNORING CASE RESULTS ls_result.
        READ TABLE ls_result-submatches INTO ls_match INDEX 1.
        IF sy-subrc IS INITIAL.
          rv_text = is_data-data_str+ls_match-offset(ls_match-length).
        ENDIF.

        FIND FIRST OCCURRENCE OF REGEX '<OFFLINE/>'
          IN is_data-data_str IGNORING CASE MATCH COUNT lv_cnt.
        IF lv_cnt > 0.
          rv_text = |<strong>On-line</strong>, Name: <strong>{
                    zcl_abapgit_url=>name( rv_text ) }</strong>|.
        ELSE.
          rv_text = |Off-line, Name: <strong>{ rv_text }</strong>|.
        ENDIF.

      WHEN 'BACKGROUND'.
        FIND FIRST OCCURRENCE OF REGEX '<method>(.*)</method>'
          IN is_data-data_str IGNORING CASE RESULTS ls_result.
        READ TABLE ls_result-submatches INTO ls_match INDEX 1.
        IF sy-subrc IS NOT INITIAL.
          RETURN.
        ENDIF.
        rv_text = |Method: { is_data-data_str+ls_match-offset(ls_match-length) }, |
               && |Repository: { zcl_abapgit_repo_srv=>get_instance( )->get( is_data-value )->get_name( ) }|.

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


  METHOD render_content.

    DATA: lt_data    TYPE zif_abapgit_persistence=>tt_content,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = zcl_abapgit_persistence_db=>get_instance( )->list( ).

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

      lv_action  = zcl_abapgit_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display' iv_act = |{ zif_abapgit_definitions=>gc_action-db_display }?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'    iv_act = |{ zif_abapgit_definitions=>gc_action-db_edit }?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'  iv_act = |{ zif_abapgit_definitions=>gc_action-db_delete }?{ lv_action }| ).

      ro_html->add( |<tr{ lv_trclass }>| ).
      ro_html->add( |<td>{ <ls_data>-type }</td>| ).
      ro_html->add( |<td>{ <ls_data>-value }</td>| ).
      ro_html->add( |<td class="data">{ explain_content( <ls_data> ) }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</tbody>' ).
    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.            "render_content
ENDCLASS.
