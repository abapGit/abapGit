CLASS zcl_abapgit_zip DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS import
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export
      IMPORTING
        !io_repo   TYPE REF TO zcl_abapgit_repo
        !it_filter TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export_package
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS export_object
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
  PRIVATE SECTION.
    CLASS-METHODS file_upload
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS unzip_file
      IMPORTING iv_xstr         TYPE xstring
      RETURNING VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS normalize_path
      CHANGING ct_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING  zcx_abapgit_exception.

    CLASS-METHODS filename
      IMPORTING iv_str      TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS file_download
      IMPORTING iv_package TYPE devclass
                iv_xstr    TYPE xstring
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_ZIP IMPLEMENTATION.


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


  METHOD export.

    DATA: lo_log     TYPE REF TO zcl_abapgit_log,
          lt_zip     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lv_package TYPE devclass.


    CREATE OBJECT lo_log.

    lv_package = io_repo->get_package( ).

    IF zcl_abapgit_factory=>get_sap_package( lv_package )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { lv_package } doesn't exist| ).
    ENDIF.

    lt_zip = io_repo->get_files_local( io_log    = lo_log
                                       it_filter = it_filter ).

    IF lo_log->count( ) > 0.
      lo_log->show( ).
    ENDIF.

    file_download( iv_package = io_repo->get_package( )
                   iv_xstr    = encode_files( lt_zip ) ).

  ENDMETHOD.                    "export_key


  METHOD export_object.

    DATA: ls_tadir    TYPE zif_abapgit_definitions=>ty_tadir,
          ls_item     TYPE zif_abapgit_definitions=>ty_item,
          lv_folder   TYPE string,
          lv_fullpath TYPE string,
          lt_rawdata  TYPE solix_tab,
          lv_sep      TYPE c LENGTH 1,
          lt_files    TYPE zif_abapgit_definitions=>ty_files_tt.

    STATICS: sv_prev TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
    IF ls_tadir IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.

    lt_files = zcl_abapgit_objects=>serialize(
      is_item     = ls_item
      iv_language = sy-langu ).

    IF lines( lt_files ) = 0.
      MESSAGE 'Empty' TYPE 'S'.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        initial_folder  = sv_prev
      CHANGING
        selected_folder = lv_folder ).
    IF lv_folder IS INITIAL.
      RETURN.
    ENDIF.

    sv_prev = lv_folder.

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
        zcx_abapgit_exception=>raise( 'error from gui_download' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.  "export_package


  METHOD export_package.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_offline,
          ls_data   TYPE zif_abapgit_persistence=>ty_repo,
          li_popups TYPE REF TO zif_abapgit_popups.


    ls_data-key = 'DUMMY'.
    ls_data-dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ).

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_package_export(
      IMPORTING
        ev_package      = ls_data-package
        ev_folder_logic = ls_data-dot_abapgit-folder_logic ).
    IF ls_data-package IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data = ls_data.

    export( lo_repo ).

  ENDMETHOD.  "export_package


  METHOD filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Malformed path' ).
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
      zcx_abapgit_exception=>raise( 'error from file_save_dialog' ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      zcx_abapgit_exception=>raise( 'cancelled' ).
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
      zcx_abapgit_exception=>raise( 'error from gui_download' ).
    ENDIF.

  ENDMETHOD.                    "file_download


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
      zcx_abapgit_exception=>raise( 'error from file_open_dialog' ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      zcx_abapgit_exception=>raise( 'cancelled' ).
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
      zcx_abapgit_exception=>raise( 'error from gui_upload' ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.                    "file_upload


  METHOD import.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_offline.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lo_repo->set_files_remote( unzip_file( file_upload( ) ) ).

    zcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

  ENDMETHOD.                    "import


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

    DATA: lo_zip  TYPE REF TO cl_abap_zip,
          lv_data TYPE xstring.

    FIELD-SYMBOLS: <ls_zipfile> TYPE cl_abap_zip=>t_file,
                   <ls_file>    LIKE LINE OF rt_files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from zip' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

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
        zcx_abapgit_exception=>raise( 'error from zip get' ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      filename(
        EXPORTING
          iv_str      = <ls_zipfile>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.

      <ls_file>-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob
                                               iv_data = <ls_file>-data ).

    ENDLOOP.

    normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.                    "decode_files
ENDCLASS.
