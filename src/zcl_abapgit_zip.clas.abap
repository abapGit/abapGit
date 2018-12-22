CLASS zcl_abapgit_zip DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
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

  PROTECTED SECTION.
  PRIVATE SECTION.
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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD export_object.

    DATA: ls_tadir    TYPE zif_abapgit_definitions=>ty_tadir,
          lv_folder   TYPE string,
          lv_fullpath TYPE string,
          lt_rawdata  TYPE solix_tab,
          lv_sep      TYPE c LENGTH 1,
          ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    STATICS: sv_prev TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files_item-files.


    ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
    IF ls_tadir IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    ls_files_item-item-obj_type = ls_tadir-object.
    ls_files_item-item-obj_name = ls_tadir-obj_name.

    ls_files_item = zcl_abapgit_objects=>serialize( is_item = ls_files_item-item
                                                    iv_language = sy-langu ).

    IF lines( ls_files_item-files ) = 0.
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

    LOOP AT ls_files_item-files ASSIGNING <ls_file>.
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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD file_download.

    DATA:
      lv_path     TYPE string,
      lv_default  TYPE string,
      lo_fe_serv  TYPE REF TO zif_abapgit_frontend_services,
      lv_package  TYPE devclass.

    lv_package = iv_package.
    TRANSLATE lv_package USING '/#'.
    CONCATENATE lv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    lo_fe_serv = zcl_abapgit_factory=>get_frontend_services( ).

    lv_path = lo_fe_serv->show_file_save_dialog(
      iv_title            = 'Export ZIP'
      iv_extension        = 'zip'
      iv_default_filename = lv_default ).

    lo_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = iv_xstr ).

  ENDMETHOD.
ENDCLASS.
