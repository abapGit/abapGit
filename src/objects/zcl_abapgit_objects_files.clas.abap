CLASS zcl_abapgit_objects_files DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
                  iv_path TYPE string OPTIONAL,
      add_string
        IMPORTING iv_extra  TYPE clike OPTIONAL
                  iv_ext    TYPE string
                  iv_string TYPE string
        RAISING   zcx_abapgit_exception,
      read_string
        IMPORTING iv_extra         TYPE clike OPTIONAL
                  iv_ext           TYPE string
        RETURNING VALUE(rv_string) TYPE string
        RAISING   zcx_abapgit_exception,
      add_xml
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO zcl_abapgit_xml_output
                  iv_normalize TYPE sap_bool DEFAULT abap_true
                  is_metadata  TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
        RAISING   zcx_abapgit_exception,
* needed since type-check during dynamic call fails even if the object is compatible
      add_xml_from_plugin
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO object
                  iv_normalize TYPE sap_bool DEFAULT abap_true
        RAISING   zcx_abapgit_exception ##called,
      read_xml
        IMPORTING iv_extra      TYPE clike OPTIONAL
        RETURNING VALUE(ro_xml) TYPE REF TO zcl_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      read_abap
        IMPORTING iv_extra       TYPE clike OPTIONAL
                  iv_error       TYPE sap_bool DEFAULT abap_true
        RETURNING VALUE(rt_abap) TYPE abaptxt255_tab
        RAISING   zcx_abapgit_exception,
      add_abap
        IMPORTING iv_extra TYPE clike OPTIONAL
                  it_abap  TYPE STANDARD TABLE
        RAISING   zcx_abapgit_exception,
      add
        IMPORTING is_file TYPE zif_abapgit_definitions=>ty_file,
      add_raw
        IMPORTING iv_extra TYPE clike OPTIONAL
                  iv_ext   TYPE string
                  iv_data  TYPE xstring
        RAISING   zcx_abapgit_exception,
      read_raw
        IMPORTING iv_extra       TYPE clike OPTIONAL
                  iv_ext         TYPE string
        RETURNING VALUE(rv_data) TYPE xstring
        RAISING   zcx_abapgit_exception,
      get_files
        RETURNING VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt,
      set_files
        IMPORTING it_files TYPE zif_abapgit_definitions=>ty_files_tt,
      get_accessed_files
        RETURNING VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt.

  PRIVATE SECTION.
    DATA: ms_item           TYPE zif_abapgit_definitions=>ty_item,
          mt_accessed_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          mt_files          TYPE zif_abapgit_definitions=>ty_files_tt,
          mv_path           TYPE string.

    METHODS:
      read_file
        IMPORTING iv_filename TYPE string
                  iv_error    TYPE abap_bool DEFAULT abap_true
        EXPORTING ev_data     TYPE xstring
        RAISING   zcx_abapgit_exception,
      filename
        IMPORTING iv_extra           TYPE clike OPTIONAL
                  iv_ext             TYPE string
        RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_FILES IMPLEMENTATION.


  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.                    "add


  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE zif_abapgit_definitions=>ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY zif_abapgit_definitions=>gc_newline.
* when editing files via eg. GitHub web interface it adds a newline at end of file
    lv_source = lv_source && zif_abapgit_definitions=>gc_newline.

    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'abap' ).       "#EC NOTEXT
    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "abap_to_file


  METHOD add_raw.

    DATA: ls_file TYPE zif_abapgit_definitions=>ty_file.

    ls_file-path     = '/'.
    ls_file-data     = iv_data.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "add_raw


  METHOD add_string.

    DATA: ls_file TYPE zif_abapgit_definitions=>ty_file.


    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).       "#EC NOTEXT
    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "add_string


  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE zif_abapgit_definitions=>ty_file.


    lv_xml = io_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
    ls_file-path = '/'.

    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'xml' ).        "#EC NOTEXT

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "do


  METHOD add_xml_from_plugin.
*    this method wraps add_xml as in the plugin. This is necessary as the wrapped
*    xml-object in the plugin can only be typed to object.
*    ABAP does not perform implicit type casts (also if compatible) in signatures,
*    therefore this method's signature is typed ref to object
    DATA lo_xml TYPE REF TO zcl_abapgit_xml_output.

    lo_xml ?= io_xml.

    me->add_xml(
      iv_extra     = iv_extra
      io_xml       = lo_xml
      iv_normalize = iv_normalize ).

  ENDMETHOD.                    "add_xml_from_plugin


  METHOD constructor.
    ms_item = is_item.
    mv_path = iv_path.
  ENDMETHOD.                    "constructor


  METHOD filename.

    DATA: lv_obj_name TYPE string.


    lv_obj_name = ms_item-obj_name.

    IF ms_item-obj_type = 'DEVC'.
      " Packages have a fixed filename so that the repository can be installed to a different
      " package(-hierarchy) on the client and not show up as a different package in the repo.
      lv_obj_name = 'package'.
    ENDIF.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ELSE.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_extra '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ENDIF.

* handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename


  METHOD get_accessed_files.
    rt_files = mt_accessed_files.
  ENDMETHOD.  " get_accessed_files.


  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.                    "get_files


  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_abap     TYPE string.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'abap' ).            "#EC NOTEXT

    read_file( EXPORTING iv_filename = lv_filename
                         iv_error    = iv_error
               IMPORTING ev_data     = lv_data ).

    IF lv_data IS INITIAL. " Post-handling of iv_error = false
      RETURN.
    ENDIF.

    lv_abap = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    SPLIT lv_abap AT zif_abapgit_definitions=>gc_newline INTO TABLE rt_abap.

  ENDMETHOD.                    "read_abap


  METHOD read_file.

    FIELD-SYMBOLS: <ls_file>     LIKE LINE OF mt_files,
                   <ls_accessed> LIKE LINE OF mt_accessed_files.

    CLEAR ev_data.

    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files ASSIGNING <ls_file> WITH KEY path     = mv_path
                                                       filename = iv_filename.
    ELSE.
      READ TABLE mt_files ASSIGNING <ls_file> WITH KEY filename = iv_filename.
    ENDIF.

    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        zcx_abapgit_exception=>raise( |File not found: { iv_filename }| ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    " Update access table
    READ TABLE mt_accessed_files TRANSPORTING NO FIELDS
      WITH KEY path = <ls_file>-path filename = <ls_file>-filename.
    IF sy-subrc > 0. " Not found ? -> Add
      APPEND INITIAL LINE TO mt_accessed_files ASSIGNING <ls_accessed>.
      MOVE-CORRESPONDING <ls_file> TO <ls_accessed>.
    ENDIF.

    ev_data = <ls_file>-data.

  ENDMETHOD.  " read_file.


  METHOD read_raw.

    DATA: lv_filename TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).

    read_file( EXPORTING iv_filename = lv_filename
               IMPORTING ev_data     = rv_data ).

  ENDMETHOD.                    "read_raw


  METHOD read_string.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).            "#EC NOTEXT

    read_file( EXPORTING iv_filename = lv_filename
               IMPORTING ev_data     = lv_data ).

    rv_string = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

  ENDMETHOD.                    "read_string


  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_xml      TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'xml' ).             "#EC NOTEXT

    read_file( EXPORTING iv_filename = lv_filename
               IMPORTING ev_data     = lv_data ).

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    CREATE OBJECT ro_xml
      EXPORTING
        iv_xml = lv_xml.

  ENDMETHOD.                    "read_xml


  METHOD set_files.
    mt_files = it_files.
  ENDMETHOD.                    "set_files
ENDCLASS.
