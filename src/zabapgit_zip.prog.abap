*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_ZIP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_zip DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS import
      IMPORTING iv_key TYPE lcl_persistence_db=>ty_value
      RAISING   lcx_exception.

    CLASS-METHODS export
      IMPORTING io_repo   TYPE REF TO lcl_repo
                it_filter TYPE scts_tadir OPTIONAL
      RAISING   lcx_exception.

    CLASS-METHODS export_package
      RAISING lcx_exception lcx_cancel.

    CLASS-METHODS export_object
      RAISING lcx_exception lcx_cancel.

  PRIVATE SECTION.
    CLASS-METHODS file_upload
      RETURNING value(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS unzip_file
      IMPORTING iv_xstr         TYPE xstring
      RETURNING value(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS normalize_path
      CHANGING ct_files TYPE ty_files_tt
      RAISING  lcx_exception.

    CLASS-METHODS filename
      IMPORTING iv_str      TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS file_download
      IMPORTING iv_package TYPE devclass
                iv_xstr    TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE ty_files_item_tt
      RETURNING value(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS get_message
      RETURNING value(rv_message) TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_zip DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zip IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip IMPLEMENTATION.

  METHOD get_message.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Commit message'.                "#EC NOTEXT
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Enter commit message'            "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.
    IF lv_returncode = 'A'.
      lcx_exception=>raise( 'cancelled' ).
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rv_message = <ls_field>-value.

  ENDMETHOD.                    "get_message

  METHOD file_download.

    DATA: lt_rawdata  TYPE solix_tab,
          lv_action   TYPE i,
          lv_filename TYPE string,
          lv_default  TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string,
          lv_package  TYPE devclass.


    lv_package = iv_package.
    TRANSLATE lv_package USING '/#'.
    CONCATENATE lv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = 'Export ZIP'
        default_extension    = 'zip'
        default_file_name    = lv_default
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_fullpath
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).                         "#EC NOTEXT
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from file_save_dialog' ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      lcx_exception=>raise( 'cancelled' ).
    ENDIF.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstr ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_xstr )
        filename                  = lv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from gui_download' ).
    ENDIF.

  ENDMETHOD.                    "file_download

  METHOD encode_files.

    DATA: lo_zip      TYPE REF TO cl_abap_zip,
          lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    CREATE OBJECT lo_zip.

    LOOP AT it_files ASSIGNING <ls_file>.
      CONCATENATE <ls_file>-file-path+1 <ls_file>-file-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-file-data ).
    ENDLOOP.

    rv_xstr = lo_zip->save( ).

  ENDMETHOD.                    "encode_files

  METHOD filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'Malformed path' ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.
    ELSE.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
    TRANSLATE ev_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

  METHOD file_upload.

    DATA: lt_data       TYPE TABLE OF x255,
          lt_file_table TYPE filetable,
          ls_file_table LIKE LINE OF lt_file_table,
          lv_action     TYPE i,
          lv_string     TYPE string,
          lv_rc         TYPE i,
          lv_length     TYPE i.


    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Import ZIP'
        default_filename        = '*.zip'
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).                      "#EC NOTEXT
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from file_open_dialog' ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      lcx_exception=>raise( 'cancelled' ).
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    lv_string = ls_file_table-filename.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_string
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from gui_upload' ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.                    "file_upload

  METHOD normalize_path.
* removes first folder from path if needed

    DATA: lt_split  TYPE TABLE OF string,
          lv_needed TYPE abap_bool,
          lv_length TYPE i,
          lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.


    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF NOT <ls_file>-path CP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "normalize_path

  METHOD unzip_file.

    DATA: lo_zip    TYPE REF TO cl_abap_zip,
          lv_xstr   TYPE xstring,
          lt_splice TYPE cl_abap_zip=>t_splice_entries.

    FIELD-SYMBOLS: <ls_splice> LIKE LINE OF lt_splice,
                   <ls_file>   LIKE LINE OF rt_files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from zip' ).
    ENDIF.

    lt_splice = cl_abap_zip=>splice( iv_xstr ).

    LOOP AT lt_splice ASSIGNING <ls_splice>.
      lo_zip->get(
        EXPORTING
          name                    = <ls_splice>-name
        IMPORTING
          content                 = lv_xstr
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from zip get' ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      filename(
        EXPORTING
          iv_str      = <ls_splice>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_xstr.

      <ls_file>-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob
                                       iv_data = <ls_file>-data ).

    ENDLOOP.

    normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.                    "decode_files

  METHOD export.

    DATA: lo_log   TYPE REF TO lcl_log,
          lv_index TYPE i,
          lt_zip   TYPE ty_files_item_tt.

    FIELD-SYMBOLS: <ls_zip> LIKE LINE OF lt_zip.


    CREATE OBJECT lo_log.

    lt_zip = io_repo->get_files_local( lo_log ).

    IF lo_log->count( ) > 0.
      lo_log->show( ).
    ENDIF.

    IF lines( it_filter ) > 0.
      LOOP AT lt_zip ASSIGNING <ls_zip>.
        lv_index = sy-tabix.
        READ TABLE it_filter WITH KEY
          object = <ls_zip>-item-obj_type
          obj_name = <ls_zip>-item-obj_name
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_zip INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDIF.

    file_download( iv_package = io_repo->get_package( )
                   iv_xstr    = encode_files( lt_zip ) ).

  ENDMETHOD.                    "export_key

  METHOD import.

    DATA: lo_repo TYPE REF TO lcl_repo_offline.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
    lo_repo->set_files_remote( unzip_file( file_upload( ) ) ).
    lo_repo->deserialize( ).

  ENDMETHOD.                    "import

  METHOD export_package.

    DATA: lo_repo TYPE REF TO lcl_repo_offline,
          ls_data TYPE lcl_persistence_repo=>ty_repo.

    ls_data-package = lcl_popups=>popup_package_export( ).
    IF ls_data-package IS INITIAL.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    ls_data-key             = 'DUMMY'.
    ls_data-master_language = sy-langu.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data = ls_data.

    lcl_zip=>export( lo_repo ).

  ENDMETHOD.  "export_package

  METHOD export_object.

    DATA: ls_tadir    TYPE tadir,
          ls_item     TYPE ty_item,
          lv_folder   TYPE string,
          lv_fullpath TYPE string,
          lt_rawdata  TYPE solix_tab,
          lv_sep      TYPE c LENGTH 1,
          lt_files    TYPE ty_files_tt.

    STATICS: lv_prev TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    ls_tadir = lcl_popups=>popup_object( ).
    IF ls_tadir IS INITIAL.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.

    lt_files = lcl_objects=>serialize(
      is_item     = ls_item
      iv_language = sy-langu ).

    IF lines( lt_files ) = 0.
      MESSAGE 'Empty' TYPE 'S'.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        initial_folder  = lv_prev
      CHANGING
        selected_folder = lv_folder ).
    IF lv_folder IS INITIAL.
      RETURN.
    ENDIF.

    lv_prev = lv_folder.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator = lv_sep ).

    LOOP AT lt_files ASSIGNING <ls_file>.
      CONCATENATE lv_folder lv_sep <ls_file>-filename INTO lv_fullpath.

      lt_rawdata = cl_bcs_convert=>xstring_to_solix( <ls_file>-data ).

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize              = xstrlen( <ls_file>-data )
          filename                  = lv_fullpath
          filetype                  = 'BIN'
        CHANGING
          data_tab                  = lt_rawdata
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          OTHERS                    = 24 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from gui_download' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.  "export_package

ENDCLASS.                    "lcl_zip IMPLEMENTATION