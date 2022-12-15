CLASS zcl_abapgit_gui_page_db_edit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        is_key TYPE zif_abapgit_persistence=>ty_content
      RAISING zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    CLASS-METHODS dbcontent_decode
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_content) TYPE zif_abapgit_persistence=>ty_content
      RAISING
        zcx_abapgit_exception .

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        update TYPE string VALUE 'update',
      END OF c_action .
    DATA ms_key TYPE zif_abapgit_persistence=>ty_content .

    CLASS-METHODS update
      IMPORTING
        !is_content TYPE zif_abapgit_persistence=>ty_content
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DB_EDIT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
    ms_control-page_title = 'Config Edit'.
  ENDMETHOD.


  METHOD dbcontent_decode.

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    lo_map = ii_event->form_data( ).
    rs_content-type     = lo_map->get( 'TYPE' ).
    rs_content-value    = lo_map->get( 'VALUE' ).
    rs_content-data_str = lo_map->get( 'XMLDATA' ).

    IF rs_content-data_str(1) <> '<' AND rs_content-data_str+1(1) = '<'. " Hmmm ???
      rs_content-data_str = rs_content-data_str+1.
    ENDIF.

  ENDMETHOD.


  METHOD render_content.

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
    ri_html->add( zcl_abapgit_gui_page_db_dis=>render_record_banner( ms_key ) ).
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


  METHOD update.

    ASSERT is_content-type IS NOT INITIAL.

    zcl_abapgit_persistence_db=>get_instance( )->update(
      iv_type  = is_content-type
      iv_value = is_content-value
      iv_data  = is_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: ls_db TYPE zif_abapgit_persistence=>ty_content.

    CASE ii_event->mv_action.
      WHEN c_action-update.
        ls_db = dbcontent_decode( ii_event ).
        update( ls_db ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
