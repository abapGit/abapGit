CLASS zcl_abapgit_zip DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS export
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
        !iv_show_log   TYPE abap_bool DEFAULT abap_true
        !it_filter     TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export_object
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export_package
      EXPORTING
        !ev_xstr    TYPE xstring
        !ev_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS load
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS save_binstring_to_localfile
      IMPORTING iv_filename  TYPE string
                iv_binstring TYPE xstring
      RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.

    CLASS-DATA gv_prev TYPE string .
  PRIVATE SECTION.

    CLASS-METHODS encode_files
      IMPORTING
        !it_files      TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS filename
      IMPORTING
        !iv_str      TYPE string
      EXPORTING
        !ev_path     TYPE string
        !ev_filename TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS normalize_path
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS unzip_file
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
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

    DATA: li_log     TYPE REF TO zif_abapgit_log,
          lt_zip     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lv_package TYPE devclass.


    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    lv_package = io_repo->get_package( ).

    IF zcl_abapgit_factory=>get_sap_package( lv_package )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { lv_package } doesn't exist| ).
    ENDIF.

    lt_zip = io_repo->get_files_local( ii_log    = li_log
                                       it_filter = it_filter ).

    IF li_log->count( ) > 0 AND iv_show_log = abap_true.
      zcl_abapgit_log_viewer=>show_log( iv_header_text = 'Zip Export Log'
                                        ii_log         = li_log ).
    ENDIF.

    rv_xstr = encode_files( lt_zip ).

  ENDMETHOD.


  METHOD export_object.

    DATA: ls_tadir      TYPE zif_abapgit_definitions=>ty_tadir,
          lv_folder     TYPE string,
          lv_fullpath   TYPE string,
          lv_sep        TYPE c LENGTH 1,
          ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files_item-files.

    WHILE ls_tadir IS INITIAL.

      ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
      IF ls_tadir IS INITIAL.
        MESSAGE 'Object couldn''t be found' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ENDWHILE.

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
        initial_folder  = gv_prev
      CHANGING
        selected_folder = lv_folder ).
    IF lv_folder IS INITIAL.
      RETURN.
    ENDIF.

    gv_prev = lv_folder.

    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = lv_sep ).

    LOOP AT ls_files_item-files ASSIGNING <ls_file>.
      CONCATENATE lv_folder lv_sep <ls_file>-filename INTO lv_fullpath.

      save_binstring_to_localfile( iv_filename = lv_fullpath
                                   iv_binstring = <ls_file>-data ).

    ENDLOOP.

  ENDMETHOD.


  METHOD export_package.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_offline,
          ls_data   TYPE zif_abapgit_persistence=>ty_repo,
          li_popups TYPE REF TO zif_abapgit_popups.

    DATA lv_serialize_master_lang_only TYPE abap_bool.

    ls_data-key = 'DUMMY'.
    ls_data-dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ).

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_package_export(
      IMPORTING
        ev_package      = ls_data-package
        ev_folder_logic = ls_data-dot_abapgit-folder_logic
        ev_serialize_master_lang_only = lv_serialize_master_lang_only ).
    IF ls_data-package IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    ls_data-local_settings-serialize_master_lang_only = lv_serialize_master_lang_only.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data = ls_data.

    ev_xstr = export( lo_repo ).
    ev_package = ls_data-package.

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD load.

    rt_files = unzip_file( iv_xstr ).

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD save_binstring_to_localfile.

    DATA lt_rawdata TYPE solix_tab.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_binstring ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_binstring )
        filename                  = iv_filename
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

  ENDMETHOD.


  METHOD unzip_file.

    DATA: lo_zip  TYPE REF TO cl_abap_zip,
          lv_data TYPE xstring.

    FIELD-SYMBOLS: <ls_zipfile> LIKE LINE OF lo_zip->files,
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

      <ls_file>-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                               iv_data = <ls_file>-data ).

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.
ENDCLASS.
