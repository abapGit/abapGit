*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_DOT_ABAPGIT
*&---------------------------------------------------------------------*

CLASS ltcl_dot_abapgit DEFINITION DEFERRED.

CLASS lcl_dot_abapgit DEFINITION FINAL FRIENDS ltcl_dot_abapgit.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_dot_abapgit,
             master_language TYPE spras,
             starting_folder TYPE string,
             ignore          TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           END OF ty_dot_abapgit.

    CLASS-METHODS:
      build_default
        RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_dot_abapgit,
      deserialize
        IMPORTING iv_xstr               TYPE xstring
        RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_dot_abapgit
        RAISING   lcx_exception.

    METHODS:
      constructor
        IMPORTING is_data TYPE ty_dot_abapgit.

    METHODS:
      serialize
        RETURNING VALUE(rv_xstr) TYPE xstring
        RAISING   lcx_exception,
      get_data
        RETURNING VALUE(rs_data) TYPE ty_dot_abapgit,
      add_ignore
        IMPORTING iv_path     TYPE string
                  iv_filename TYPE string,
      is_ignored
        IMPORTING iv_path           TYPE string
                  iv_filename       TYPE string
        RETURNING VALUE(rv_ignored) TYPE abap_bool,
      remove_ignore
        IMPORTING iv_path     TYPE string
                  iv_filename TYPE string,
      get_starting_folder
        RETURNING VALUE(rv_path) TYPE string,
      set_starting_folder
        IMPORTING iv_path TYPE string,
      get_master_language
        RETURNING VALUE(rv_language) TYPE spras,
      set_master_language
        IMPORTING iv_language TYPE spras,
      get_signature
        RETURNING VALUE(rs_signature) TYPE ty_file_signature
        RAISING   lcx_exception.

  PRIVATE SECTION.
    DATA: ms_data TYPE ty_dot_abapgit.

    CLASS-METHODS:
      to_xml
        IMPORTING is_data       TYPE ty_dot_abapgit
        RETURNING VALUE(rv_xml) TYPE string
        RAISING   lcx_exception,
      from_xml
        IMPORTING iv_xml         TYPE string
        RETURNING VALUE(rs_data) TYPE ty_dot_abapgit.

ENDCLASS.

CLASS lcl_dot_abapgit IMPLEMENTATION.

  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.

  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE ty_dot_abapgit.


    lv_xml = lcl_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD get_data.
    rs_data = ms_data.
  ENDMETHOD.

  METHOD serialize.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( ms_data ).

    rv_xstr = lcl_convert=>string_to_xstring_utf8( lv_xml ).

  ENDMETHOD.

  METHOD build_default.

    DATA: ls_data TYPE ty_dot_abapgit.


    ls_data-master_language = sy-langu.
    ls_data-starting_folder = '/'.
    APPEND '/.gitignore' TO ls_data-ignore.
    APPEND '/LICENSE' TO ls_data-ignore.
    APPEND '/README.md' TO ls_data-ignore.
    APPEND '/package.json' TO ls_data-ignore.
    APPEND '/.travis.yml' TO ls_data-ignore.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE data = is_data
      RESULT XML rv_xml.

    rv_xml = lcl_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_DATA_--29>' IN lv_xml WITH '<DATA>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_DATA_--29>' IN lv_xml WITH '</DATA>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data ##NO_TEXT.
  ENDMETHOD.

  METHOD add_ignore.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <lv_ignore> LIKE LINE OF ms_data-ignore.


    lv_name = iv_path && iv_filename.

    READ TABLE ms_data-ignore FROM lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO ms_data-ignore ASSIGNING <lv_ignore>.
    <lv_ignore> = lv_name.

  ENDMETHOD.

  METHOD is_ignored.

    DATA: lv_name   TYPE string,
          lv_ignore TYPE string.


    lv_name = iv_path && iv_filename.

    LOOP AT ms_data-ignore INTO lv_ignore.
      IF lv_name CP lv_ignore.
        rv_ignored = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD remove_ignore.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    DELETE TABLE ms_data-ignore FROM lv_name.

  ENDMETHOD.

  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.

  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.

  METHOD get_master_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.

  METHOD set_master_language.
    ms_data-master_language = iv_language.
  ENDMETHOD.

  METHOD get_signature.

    rs_signature-path     = gc_root_dir.
    rs_signature-filename = gc_dot_abapgit.
    rs_signature-sha1     = lcl_hash=>sha1( iv_type = gc_type-blob
                                            iv_data = serialize( ) ).

  ENDMETHOD. "get_signature

ENDCLASS.
