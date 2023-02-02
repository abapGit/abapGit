CLASS zcl_abapgit_zip DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS encode_files
      IMPORTING
        !it_files      TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export
      IMPORTING
        !is_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings
        !iv_package        TYPE devclass
        !io_dot_abapgit    TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_show_log       TYPE abap_bool DEFAULT abap_true
        !it_filter         TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      RETURNING
        VALUE(rv_xstr)     TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export_object
      IMPORTING
        !iv_object_type        TYPE trobjtype
        !iv_object_name        TYPE sobj_name
        !iv_main_language_only TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS export_package
      IMPORTING
        !iv_package        TYPE devclass
        !iv_folder_logic   TYPE string
        !iv_main_lang_only TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS load
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS save_binstring_to_localfile
      IMPORTING
        !iv_filename  TYPE string
        !iv_binstring TYPE xstring
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    CLASS-DATA gv_prev TYPE string .
  PRIVATE SECTION.

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
        !ct_files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS unzip_file
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
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

    DATA li_log       TYPE REF TO zif_abapgit_log.
    DATA lt_zip       TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lo_serialize TYPE REF TO zcl_abapgit_serialize.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.
    li_log->set_title( 'Zip Export Log' ).

    IF zcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { iv_package } doesn't exist| ).
    ENDIF.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = io_dot_abapgit
        is_local_settings = is_local_settings.

    lt_zip = lo_serialize->files_local(
      iv_package = iv_package
      ii_log     = li_log
      it_filter  = it_filter ).

    FREE lo_serialize.

    IF li_log->count( ) > 0 AND iv_show_log = abap_true.
      zcl_abapgit_log_viewer=>show_log( li_log ).
    ENDIF.

    rv_xstr = encode_files( lt_zip ).

  ENDMETHOD.


  METHOD export_object.

    DATA: ls_tadir         TYPE zif_abapgit_definitions=>ty_tadir,
          lv_folder        TYPE string,
          lv_fullpath      TYPE string,
          lv_sep           TYPE c LENGTH 1,
          ls_files_item    TYPE zcl_abapgit_objects=>ty_serialization,
          lo_frontend_serv TYPE REF TO zif_abapgit_frontend_services.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files_item-files.

    ls_tadir = zcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = iv_object_type
        iv_obj_name = iv_object_name ).

    IF ls_tadir IS INITIAL.
      zcx_abapgit_exception=>raise( 'Object could not be found' ).
    ENDIF.

    ls_files_item-item-obj_type = ls_tadir-object.
    ls_files_item-item-obj_name = ls_tadir-obj_name.

    ls_files_item = zcl_abapgit_objects=>serialize(
      iv_main_language_only = iv_main_language_only
      is_item               = ls_files_item-item
      iv_language           = sy-langu ).

    IF lines( ls_files_item-files ) = 0.
      zcx_abapgit_exception=>raise( 'Empty' ).
    ENDIF.

    lo_frontend_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).
    lo_frontend_serv->directory_browse(
      EXPORTING
        iv_initial_folder  = gv_prev
      CHANGING
        cv_selected_folder = lv_folder ).
    IF lv_folder IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    gv_prev = lv_folder.
    lo_frontend_serv->get_file_separator( CHANGING cv_file_separator = lv_sep ).

    LOOP AT ls_files_item-files ASSIGNING <ls_file>.
      lv_fullpath = |{ lv_folder }{ lv_sep }{ <ls_file>-filename }|.
      save_binstring_to_localfile( iv_filename  = lv_fullpath
                                   iv_binstring = <ls_file>-data ).

    ENDLOOP.

  ENDMETHOD.


  METHOD export_package.

    DATA: ls_local_settings  TYPE zif_abapgit_persistence=>ty_repo-local_settings,
          lo_dot_abapgit     TYPE REF TO zcl_abapgit_dot_abapgit,
          lo_frontend_serv   TYPE REF TO zif_abapgit_frontend_services,
          lv_default         TYPE string,
          lv_package_escaped TYPE string,
          lv_path            TYPE string,
          lv_zip_xstring     TYPE xstring.

    ls_local_settings-main_language_only = iv_main_lang_only.

    lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).

    lo_frontend_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_package_escaped = iv_package.
    REPLACE ALL OCCURRENCES OF '/' IN lv_package_escaped WITH '#'.
    lv_default = |{ lv_package_escaped }_{ sy-datlo }_{ sy-timlo }|.

    lv_zip_xstring = export(
     is_local_settings = ls_local_settings
     iv_package        = iv_package
     io_dot_abapgit    = lo_dot_abapgit ).

    lv_path = lo_frontend_serv->show_file_save_dialog(
        iv_title            = 'Package Export'
        iv_extension        = 'zip'
        iv_default_filename = lv_default ).

    lo_frontend_serv->file_download(
        iv_path = lv_path
        iv_xstr = lv_zip_xstring ).
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

    zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
      iv_path = iv_filename
      iv_xstr = iv_binstring ).

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

      <ls_file>-sha1 = zcl_abapgit_hash=>sha1_blob( <ls_file>-data ).

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.
ENDCLASS.
