CLASS zcl_abapgit_gui_page_db DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_html_table.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
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

    CONSTANTS c_css_url TYPE string VALUE 'css/page_db.css'.

    TYPES:
      BEGIN OF ty_explanation,
        value TYPE string,
        extra TYPE string,
      END OF ty_explanation.

    DATA mt_methods TYPE zcl_abapgit_background=>ty_methods.

    METHODS register_stylesheet
      RAISING
        zcx_abapgit_exception.

    METHODS render_table
      IMPORTING
        it_db_entries TYPE zif_abapgit_persistence=>ty_contents
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS do_backup_db
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS do_delete_entry
      IMPORTING
        !is_key TYPE zif_abapgit_persistence=>ty_content
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS do_restore_db
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
      RETURNING
        VALUE(rs_expl) TYPE ty_explanation
      RAISING
        zcx_abapgit_exception.
    METHODS explain_content_repo_cs
      IMPORTING
        !is_data  TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rs_expl) TYPE ty_explanation
      RAISING
        zcx_abapgit_exception.
    METHODS explain_content_background
      IMPORTING
        !is_data  TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rs_expl) TYPE ty_explanation
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DB IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    register_stylesheet( ).
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_db.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Database Utility'
      iv_extra_css_url      = c_css_url
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD do_backup_db.

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
      lo_zip->add(
        name    = lv_filename
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


  METHOD do_delete_entry.

    DATA lv_answer TYPE c LENGTH 1.

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
      " TODO: think how to remove this code,
      " maybe implement subscription in persistence_db,
      " so that repo_srv receive a notification on add/delete
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD do_restore_db.

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
      TRY.
          zcl_abapgit_persistence_db=>validate_entry_type( ls_data-type ).
        CATCH zcx_abapgit_exception.
          zcx_abapgit_exception=>raise( |Invalid DB entry type. This is not an abapGit Backup| ).
      ENDTRY.

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


  METHOD explain_content.

    DATA lv_descr TYPE string.
    DATA ls_explanation TYPE ty_explanation.

    CASE is_data-type.
      WHEN zcl_abapgit_persistence_db=>c_type_repo.
        lv_descr       = 'Repo Settings'.
        ls_explanation = explain_content_repo( is_data ).

      WHEN zcl_abapgit_persistence_db=>c_type_background.
        lv_descr       = 'Background Settings'.
        ls_explanation = explain_content_background( is_data ).

      WHEN zcl_abapgit_persistence_db=>c_type_user.
        lv_descr       = 'Personal Settings'.
        ls_explanation-value = zcl_abapgit_user_record=>get_instance( is_data-value )->get_name( ).

      WHEN zcl_abapgit_persistence_db=>c_type_settings.
        lv_descr       = 'Global Settings'.

      WHEN zcl_abapgit_persistence_db=>c_type_packages.
        lv_descr       = 'Local Package Details'.

      WHEN zcl_abapgit_persistence_db=>c_type_repo_csum.
        lv_descr       = 'Repo Checksums'.
        ls_explanation = explain_content_repo_cs( is_data ).

      WHEN OTHERS.
        IF strlen( is_data-data_str ) >= 250.
          ls_explanation-value = is_data-data_str(250).
        ELSE.
          ls_explanation-value = is_data-data_str.
        ENDIF.

        ls_explanation-value = escape(
          val    = ls_explanation-value
          format = cl_abap_format=>e_html_attr ).
        ls_explanation-value = |<pre>{ ls_explanation-value }</pre>|.

    ENDCASE.

    IF ls_explanation-value IS NOT INITIAL.
      lv_descr = |{ lv_descr }: |.
    ENDIF.

    IF ls_explanation-extra IS NOT INITIAL.
      ls_explanation-extra = | ({ ls_explanation-extra })|.
    ENDIF.

    rv_text = |{ lv_descr }<strong>{ ls_explanation-value }</strong>{ ls_explanation-extra }|.

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

    rs_expl-value = |{ zcl_abapgit_repo_srv=>get_instance( )->get( is_data-value )->get_name( ) }|.

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
      rs_expl-extra = ls_method-description.
    ELSE.
      rs_expl-extra = lv_class.
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
      rs_expl-extra = 'Online'.
    ELSE.
      rs_expl-extra = 'Offline'.
    ENDIF.

    FIND FIRST OCCURRENCE OF REGEX '<DISPLAY_NAME>(.*)</DISPLAY_NAME>'
      IN is_data-data_str IGNORING CASE RESULTS ls_result.
    READ TABLE ls_result-submatches INTO ls_match INDEX 1.
    IF sy-subrc = 0.
      rs_expl-value = is_data-data_str+ls_match-offset(ls_match-length).
    ENDIF.

    IF rs_expl-value IS INITIAL.
      FIND FIRST OCCURRENCE OF REGEX '<URL>(.*)</URL>'
        IN is_data-data_str IGNORING CASE RESULTS ls_result.
      READ TABLE ls_result-submatches INTO ls_match INDEX 1.
      IF sy-subrc = 0.
        rs_expl-value = is_data-data_str+ls_match-offset(ls_match-length).
        IF lv_cnt > 0.
          rs_expl-value = zcl_abapgit_url=>name( rs_expl-value ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD explain_content_repo_cs.

    DATA lt_lines TYPE string_table.

    IF strlen( is_data-data_str ) > 0.
      SPLIT is_data-data_str AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
      rs_expl-extra = |{ lines( lt_lines ) } lines|.

      READ TABLE lt_lines INDEX 1 INTO rs_expl-value.
      IF sy-subrc = 0.
        REPLACE '#repo_name#' IN rs_expl-value WITH ''.
        rs_expl-value = escape(
          val    = rs_expl-value
          format = cl_abap_format=>e_html_attr ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD register_stylesheet.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    CREATE OBJECT lo_buf.

    " @@abapmerge include zabapgit_css_page_db.w3mi.data.css > lo_buf->add( '$$' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.


  METHOD render_table.

    ri_html = zcl_abapgit_html_table=>create( ii_renderer = me
      )->define_column(
        iv_column_id = 'type'
        iv_column_title = 'Type'
      )->define_column(
        iv_column_id = 'value'
        iv_column_title = 'Key'
      )->define_column(
        iv_column_id = 'expl'
        iv_column_title = 'Data'
      )->define_column( 'cmd'
      )->render( it_db_entries ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA ls_db TYPE zif_abapgit_persistence=>ty_content.
    DATA lo_query TYPE REF TO zcl_abapgit_string_map.

    lo_query = ii_event->query( ).
    CASE ii_event->mv_action.
      WHEN c_action-delete.
        lo_query->to_abap( CHANGING cs_container = ls_db ).
        do_delete_entry( ls_db ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-backup.
        do_backup_db( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-restore.
        do_restore_db( ).
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

    DATA lt_db_entries TYPE zif_abapgit_persistence=>ty_contents.

    register_handlers( ).

    lt_db_entries = zcl_abapgit_persistence_db=>get_instance( )->list( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="db-list">' ).
    ri_html->add( render_table( lt_db_entries ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.
  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

    DATA lv_action  TYPE string.
    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CASE iv_column_id.
      WHEN 'type' OR 'value'.
        rs_render-content = |{ iv_value }|.
      WHEN 'expl'.
        rs_render-content   = explain_content( is_row ).
        rs_render-css_class = 'data'.
      WHEN 'cmd'.
        lv_action  = zcl_abapgit_html_action_utils=>dbkey_encode( is_row ).
        lo_toolbar = zcl_abapgit_html_toolbar=>create(
          )->add(
            iv_txt = 'Display'
            iv_act = |{ zif_abapgit_definitions=>c_action-db_display }?{ lv_action }|
          )->add(
            iv_txt = 'Edit'
            iv_act = |{ zif_abapgit_definitions=>c_action-db_edit }?{ lv_action }|
          )->add(
            iv_txt = 'Delete'
            iv_act = |{ c_action-delete }?{ lv_action }| ).
        rs_render-html = lo_toolbar->render( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
