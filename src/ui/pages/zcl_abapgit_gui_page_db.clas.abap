CLASS zcl_abapgit_gui_page_db DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_menu_provider.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        delete  TYPE string VALUE 'delete',
        backup  TYPE string VALUE 'backup',
        restore TYPE string VALUE 'restore',
      END OF c_action.

    DATA mt_methods TYPE zcl_abapgit_background=>ty_methods.

    CLASS-METHODS backup
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS delete
      IMPORTING
        !is_key TYPE zif_abapgit_persistence=>ty_content
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS restore
      RAISING
        zcx_abapgit_exception.
    METHODS explain_content
      IMPORTING
        !is_data       TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS explain_content_repo
      IMPORTING
        !is_data  TYPE zif_abapgit_persistence=>ty_content
      EXPORTING
        !ev_value TYPE string
        !ev_extra TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS explain_content_repo_cs
      IMPORTING
        !is_data  TYPE zif_abapgit_persistence=>ty_content
      EXPORTING
        !ev_value TYPE string
        !ev_extra TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS explain_content_background
      IMPORTING
        !is_data  TYPE zif_abapgit_persistence=>ty_content
      EXPORTING
        !ev_value TYPE string
        !ev_extra TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DB IMPLEMENTATION.


  METHOD backup.

    DATA:
      lt_data     TYPE zif_abapgit_persistence=>ty_contents,
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO zif_abapgit_frontend_services.

    FIELD-SYMBOLS:
      <ls_data> LIKE LINE OF lt_data.

    lt_data = zcl_abapgit_persistence_db=>get_instance( )->list( ).

    CREATE OBJECT lo_zip.

    LOOP AT lt_data ASSIGNING <ls_data>.
      CONCATENATE <ls_data>-type '_' <ls_data>-value '.xml' INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = zcl_abapgit_convert=>string_to_xstring_utf8( <ls_data>-data_str ) ).
    ENDLOOP.

    lv_zip = lo_zip->save( ).

    CONCATENATE 'abapGit_Backup_' sy-datlo '_' sy-timlo INTO lv_filename.

    li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_save_dialog(
      iv_title            = 'abapGit Backup'
      iv_extension        = 'zip'
      iv_default_filename = lv_filename ).

    li_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = lv_zip ).

    MESSAGE 'abapGit Backup successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_db.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Database Utility'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD delete.

    DATA: lv_answer TYPE c LENGTH 1.

    ASSERT is_key-type IS NOT INITIAL.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = |Are you sure you want to delete entry { is_key-type } { is_key-value }?|
      iv_text_button_1         = 'Yes'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'No'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_persistence_db=>get_instance( )->delete(
      iv_type  = is_key-type
      iv_value = is_key-value ).

    " If deleting repo, also delete corresponding checksums
    " Other way around is ok, since checksums are automatically recreated
    IF is_key-type = zcl_abapgit_persistence_db=>c_type_repo.
      zcl_abapgit_persistence_db=>get_instance( )->delete(
        iv_type  = zcl_abapgit_persistence_db=>c_type_repo_csum
        iv_value = is_key-value ).

      " Initialize repo list
      zcl_abapgit_repo_srv=>get_instance( )->init( ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD explain_content.

    DATA:
          lv_descr  TYPE string,
          lv_value  TYPE string,
          lv_extra  TYPE string.

    CASE is_data-type.
      WHEN zcl_abapgit_persistence_db=>c_type_repo.
        lv_descr = 'Repo Settings'.

        explain_content_repo(
          EXPORTING
            is_data  = is_data
          IMPORTING
            ev_value = lv_value
            ev_extra = lv_extra ).

      WHEN zcl_abapgit_persistence_db=>c_type_background.
        lv_descr = 'Background Settings'.

        explain_content_background(
          EXPORTING
            is_data  = is_data
          IMPORTING
            ev_value = lv_value
            ev_extra = lv_extra ).

      WHEN zcl_abapgit_persistence_db=>c_type_user.
        lv_descr = 'Personal Settings'.
        lv_value = zcl_abapgit_user_record=>get_instance( is_data-value )->get_name( ).
      WHEN zcl_abapgit_persistence_db=>c_type_settings.
        lv_descr = 'Global Settings'.
      WHEN zcl_abapgit_persistence_db=>c_type_packages.
        lv_descr = 'Local Package Details'.
      WHEN zcl_abapgit_persistence_db=>c_type_repo_csum.
        lv_descr = 'Repo Checksums'.

        explain_content_repo_cs(
          EXPORTING
            is_data  = is_data
          IMPORTING
            ev_value = lv_value
            ev_extra = lv_extra ).

      WHEN OTHERS.
        IF strlen( is_data-data_str ) >= 250.
          lv_value = is_data-data_str(250).
        ELSE.
          lv_value = is_data-data_str.
        ENDIF.

        lv_value = escape(
          val    = lv_value
          format = cl_abap_format=>e_html_attr ).

        lv_value = |<pre>{ lv_value }</pre>|.
    ENDCASE.

    IF lv_value IS NOT INITIAL.
      lv_descr = |{ lv_descr }: |.
    ENDIF.

    IF lv_extra IS NOT INITIAL.
      lv_extra = | ({ lv_extra })|.
    ENDIF.

    rv_text = |{ lv_descr }<strong>{ lv_value }</strong>{ lv_extra }|.

    IF strlen( rv_text ) >= 250.
      rv_text = rv_text(250) && '...'.
    ENDIF.

  ENDMETHOD.


  METHOD explain_content_background.

    DATA:
      ls_result TYPE match_result,
      ls_match  TYPE submatch_result,
      lv_class  TYPE string,
      ls_method LIKE LINE OF mt_methods.

    ev_value = |{ zcl_abapgit_repo_srv=>get_instance( )->get( is_data-value )->get_name( ) }|.

    FIND FIRST OCCURRENCE OF REGEX '<METHOD>(.*)</METHOD>'
      IN is_data-data_str IGNORING CASE RESULTS ls_result.
    READ TABLE ls_result-submatches INTO ls_match INDEX 1.
    IF sy-subrc = 0.
      lv_class = is_data-data_str+ls_match-offset(ls_match-length).
    ENDIF.

    IF mt_methods IS INITIAL.
      mt_methods = zcl_abapgit_background=>list_methods( ).
    ENDIF.

    READ TABLE mt_methods INTO ls_method WITH TABLE KEY class = lv_class.
    IF sy-subrc = 0.
      ev_extra = ls_method-description.
    ELSE.
      ev_extra = lv_class.
    ENDIF.

  ENDMETHOD.


  METHOD explain_content_repo.

    DATA:
      ls_result TYPE match_result,
      ls_match  TYPE submatch_result,
      lv_cnt    TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '<OFFLINE/>'
      IN is_data-data_str IGNORING CASE MATCH COUNT lv_cnt.
    IF lv_cnt > 0.
      ev_extra = 'Online'.
    ELSE.
      ev_extra = 'Offline'.
    ENDIF.

    FIND FIRST OCCURRENCE OF REGEX '<DISPLAY_NAME>(.*)</DISPLAY_NAME>'
      IN is_data-data_str IGNORING CASE RESULTS ls_result.
    READ TABLE ls_result-submatches INTO ls_match INDEX 1.
    IF sy-subrc = 0.
      ev_value = is_data-data_str+ls_match-offset(ls_match-length).
    ENDIF.

    IF ev_value IS INITIAL.
      FIND FIRST OCCURRENCE OF REGEX '<URL>(.*)</URL>'
        IN is_data-data_str IGNORING CASE RESULTS ls_result.
      READ TABLE ls_result-submatches INTO ls_match INDEX 1.
      IF sy-subrc = 0.
        ev_value = is_data-data_str+ls_match-offset(ls_match-length).
        IF lv_cnt > 0.
          ev_value = zcl_abapgit_url=>name( ev_value ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD explain_content_repo_cs.

    DATA:
      lt_lines  TYPE string_table.

    IF strlen( is_data-data_str ) > 0.
      SPLIT is_data-data_str AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
      ev_extra = |{ lines( lt_lines ) } lines|.

      READ TABLE lt_lines INDEX 1 INTO ev_value.
      IF sy-subrc = 0.
        REPLACE '#repo_name#' IN ev_value WITH ''.
        ev_value = escape(
          val    = ev_value
          format = cl_abap_format=>e_html_attr ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD restore.

    DATA:
      lv_answer   TYPE c LENGTH 1,
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_filename TYPE string,
      lv_data     TYPE xstring,
      ls_data     TYPE zif_abapgit_persistence=>ty_content,
      lt_data     TYPE zif_abapgit_persistence=>ty_contents,
      lt_data_old TYPE zif_abapgit_persistence=>ty_contents,
      li_fe_serv  TYPE REF TO zif_abapgit_frontend_services.

    FIELD-SYMBOLS:
      <ls_zipfile> LIKE LINE OF lo_zip->files.

    li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_open_dialog(
      iv_title            = 'Restore abapGit Backup'
      iv_extension        = 'zip'
      iv_default_filename = 'abapGit_Backup_*.zip' ).

    lv_zip = li_fe_serv->file_upload( lv_path ).

    CREATE OBJECT lo_zip.

    lo_zip->load(
      EXPORTING
        zip             = lv_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error loading ZIP file' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.
      CLEAR ls_data.
      lv_filename = <ls_zipfile>-name.
      REPLACE '.xml' IN lv_filename WITH ''.
      SPLIT lv_filename AT '_' INTO ls_data-type ls_data-value.

      " Validate DB key
      IF ls_data-type <> zcl_abapgit_persistence_db=>c_type_repo AND
         ls_data-type <> zcl_abapgit_persistence_db=>c_type_user AND
         ls_data-type <> zcl_abapgit_persistence_db=>c_type_settings AND
         ls_data-type <> zcl_abapgit_persistence_db=>c_type_background AND
         ls_data-type <> zcl_abapgit_persistence_db=>c_type_packages.
        zcx_abapgit_exception=>raise( |Invalid DB key. This is not an abapGit Backup| ).
      ENDIF.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error getting file { <ls_zipfile>-name } from ZIP| ).
      ENDIF.

      ls_data-data_str = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).
      INSERT ls_data INTO TABLE lt_data.
    ENDLOOP.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = 'All existing repositories and settings will be deleted and overwritten! Continue?'
      iv_text_button_1         = 'Restore'
      iv_icon_button_1         = 'ICON_IMPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer <> '1'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lt_data_old = zcl_abapgit_persistence_db=>get_instance( )->list( ).
    LOOP AT lt_data_old INTO ls_data.
      zcl_abapgit_persistence_db=>get_instance( )->delete(
        iv_type  = ls_data-type
        iv_value = ls_data-value ).
    ENDLOOP.

    COMMIT WORK AND WAIT.

    LOOP AT lt_data INTO ls_data.
      zcl_abapgit_persistence_db=>get_instance( )->add(
        iv_type  = ls_data-type
        iv_value = ls_data-value
        iv_data  = ls_data-data_str ).
    ENDLOOP.

    COMMIT WORK AND WAIT.

    MESSAGE 'abapGit Backup successfully restored' TYPE 'S'.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA ls_db TYPE zif_abapgit_persistence=>ty_content.
    DATA lo_query TYPE REF TO zcl_abapgit_string_map.

    lo_query = ii_event->query( ).
    CASE ii_event->mv_action.
      WHEN c_action-delete.
        lo_query->to_abap( CHANGING cs_container = ls_db ).
        delete( ls_db ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-backup.
        backup( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-restore.
        restore( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Backup'
      iv_act = c_action-backup ).

    ro_toolbar->add(
      iv_txt = 'Restore'
      iv_act = c_action-restore ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA: lt_data    TYPE zif_abapgit_persistence=>ty_contents,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = zcl_abapgit_persistence_db=>get_instance( )->list( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="db_list">' ).
    ri_html->add( '<table class="db_tab">' ).

    " Header
    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th>Type</th>' ).
    ri_html->add( '<th>Key</th>' ).
    ri_html->add( '<th>Data</th>' ).
    ri_html->add( '<th></th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).
    ri_html->add( '<tbody>' ).

    " Lines
    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR lv_trclass.
      IF sy-tabix = 1.
        lv_trclass = ' class="firstrow"'.
      ENDIF.

      lv_action  = zcl_abapgit_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display'
                       iv_act = |{ zif_abapgit_definitions=>c_action-db_display }?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'
                       iv_act = |{ zif_abapgit_definitions=>c_action-db_edit }?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'
                       iv_act = |{ c_action-delete }?{ lv_action }| ).

      ri_html->add( |<tr{ lv_trclass }>| ).
      ri_html->add( |<td>{ <ls_data>-type }</td>| ).
      ri_html->add( |<td>{ <ls_data>-value }</td>| ).
      ri_html->add( |<td class="data">{ explain_content( <ls_data> ) }</td>| ).
      ri_html->add( '<td>' ).
      ri_html->add( lo_toolbar->render( ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '</tr>' ).
    ENDLOOP.

    ri_html->add( '</tbody>' ).
    ri_html->add( '</table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
