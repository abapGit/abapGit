CLASS zcl_abapgit_gui_page_db_edit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_key TYPE zif_abapgit_persistence=>ty_content .

    METHODS zif_abapgit_gui_page~on_event
        REDEFINITION .
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF gc_action,
        update TYPE string VALUE 'update',
      END OF gc_action .
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
    ms_control-page_title = 'CONFIG EDIT'.
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

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.
    lo_toolbar->add( iv_act = 'submitFormById(''db_form'');'
                     iv_txt = 'Save'
                     iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-strong ) ##NO_TEXT.

    ro_html->add( '<div class="db_entry">' ).

    " Banners & Toolbar
    ro_html->add( '<table class="toolbar"><tr><td>' ).
    ro_html->add( zcl_abapgit_gui_page_db_dis=>render_record_banner( ms_key ) ).
    ro_html->add( '</td><td>' ).
    ro_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ro_html->add( '</td></tr></table>' ).

    " Form
    ro_html->add( |<form id="db_form" method="post" action="sapevent:| && |{ gc_action-update }">| ).
    ro_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ro_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ro_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_data }</textarea>| ).
    ro_html->add( '</form>' ).

    ro_html->add( '</div>' ). "db_entry

  ENDMETHOD.  "render_content


  METHOD update.

    ASSERT is_content-type IS NOT INITIAL.

    zcl_abapgit_persistence_db=>get_instance( )->update(
      iv_type  = is_content-type
      iv_value = is_content-value
      iv_data  = is_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.

    DATA: ls_db TYPE zif_abapgit_persistence=>ty_content.

    CASE iv_action.
      WHEN gc_action-update.
        ls_db = zcl_abapgit_html_action_utils=>dbcontent_decode( it_postdata ).
        update( ls_db ).
        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
