CLASS zcl_abapgit_gui_page_db_dis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING is_key TYPE zif_abapgit_persistence=>ty_content
      RAISING zcx_abapgit_exception.

    CLASS-METHODS: render_record_banner
      IMPORTING is_key         TYPE zif_abapgit_persistence=>ty_content
      RETURNING VALUE(rv_html) TYPE string.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    DATA: ms_key TYPE zif_abapgit_persistence=>ty_content.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DB_DIS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
    ms_control-page_title = 'CONFIG DISPLAY'.
  ENDMETHOD.


  METHOD render_content.

    DATA:
      lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter,
      lo_toolbar     TYPE REF TO zcl_abapgit_html_toolbar,
      lv_data        TYPE zif_abapgit_persistence=>ty_content-data_str,
      ls_action      TYPE zif_abapgit_persistence=>ty_content,
      lv_action      TYPE string.

    TRY.
        lv_data = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH zcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

    " Create syntax highlighter
    lo_highlighter  = zcl_abapgit_syntax_highlighter=>create( '*.xml' ).

    ls_action-type  = ms_key-type.
    ls_action-value = ms_key-value.
    lv_action       = zcl_abapgit_html_action_utils=>dbkey_encode( ls_action ).
    lv_data         = lo_highlighter->process_line( zcl_abapgit_xml_pretty=>print( lv_data ) ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_toolbar.
    lo_toolbar->add( iv_act = |{ zif_abapgit_definitions=>c_action-db_edit }?{ lv_action }|
                     iv_txt = 'Edit' ) ##NO_TEXT.

    ri_html->add( '<div class="db_entry">' ).
    ri_html->add( '<table class="toolbar"><tr><td>' ).
    ri_html->add( render_record_banner( ms_key ) ).
    ri_html->add( '</td><td>' ).
    ri_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ri_html->add( '</td></tr></table>' ).

    ri_html->add( |<pre class="syntax-hl">{ lv_data }</pre>| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_record_banner.
    rv_html = |<table class="tag"><tr><td class="label">Type:</td>|
           && | <td>{ is_key-type }</td></tr></table>|
           && zif_abapgit_definitions=>c_newline
           && |<table class="tag"><tr><td class="label">Key:</td>|
           && |  <td>{ is_key-value }</td></tr></table>|.
  ENDMETHOD.
ENDCLASS.
