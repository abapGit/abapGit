class ZCL_ABAPGIT_DOT_ABAPGIT definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_requirement,
             component   TYPE dlvunit,
             min_release TYPE saprelease,
             min_patch   TYPE sappatchlv,
           END OF ty_requirement .
  types:
    ty_requirement_tt TYPE STANDARD TABLE OF ty_requirement WITH DEFAULT KEY .
  types:
    BEGIN OF ty_dot_abapgit,
             master_language TYPE spras,
             starting_folder TYPE string,
             folder_logic    TYPE string,
             ignore          TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             requirements    TYPE ty_requirement_tt,
           END OF ty_dot_abapgit .

  constants:
    BEGIN OF c_folder_logic,
                 prefix TYPE string VALUE 'PREFIX',
                 full   TYPE string VALUE 'FULL',
               END OF c_folder_logic .

  class-methods BUILD_DEFAULT
    returning
      value(RO_DOT_ABAPGIT) type ref to ZCL_ABAPGIT_DOT_ABAPGIT .
  class-methods DESERIALIZE
    importing
      !IV_XSTR type XSTRING
    returning
      value(RO_DOT_ABAPGIT) type ref to ZCL_ABAPGIT_DOT_ABAPGIT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IS_DATA type TY_DOT_ABAPGIT .
  methods SERIALIZE
    returning
      value(RV_XSTR) type XSTRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_DATA
    returning
      value(RS_DATA) type TY_DOT_ABAPGIT .
  methods ADD_IGNORE
    importing
      !IV_PATH type STRING
      !IV_FILENAME type STRING .
  methods IS_IGNORED
    importing
      !IV_PATH type STRING
      !IV_FILENAME type STRING
    returning
      value(RV_IGNORED) type ABAP_BOOL .
  methods REMOVE_IGNORE
    importing
      !IV_PATH type STRING
      !IV_FILENAME type STRING .
  methods GET_STARTING_FOLDER
    returning
      value(RV_PATH) type STRING .
  methods GET_FOLDER_LOGIC
    returning
      value(RV_LOGIC) type STRING .
  methods SET_FOLDER_LOGIC
    importing
      !IV_LOGIC type STRING .
  methods SET_STARTING_FOLDER
    importing
      !IV_PATH type STRING .
  methods GET_MASTER_LANGUAGE
    returning
      value(RV_LANGUAGE) type SPRAS .
*      set_master_language
*        IMPORTING iv_language TYPE spras,
  methods GET_SIGNATURE
    returning
      value(RS_SIGNATURE) type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE_SIGNATURE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  PRIVATE SECTION.
    DATA: ms_data TYPE ty_dot_abapgit.

    CLASS-METHODS:
      to_xml
        IMPORTING is_data       TYPE ty_dot_abapgit
        RETURNING VALUE(rv_xml) TYPE string
        RAISING   zcx_abapgit_exception,
      from_xml
        IMPORTING iv_xml         TYPE string
        RETURNING VALUE(rs_data) TYPE ty_dot_abapgit.

ENDCLASS.



CLASS ZCL_ABAPGIT_DOT_ABAPGIT IMPLEMENTATION.


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


  METHOD build_default.

    DATA: ls_data TYPE ty_dot_abapgit.


    ls_data-master_language = sy-langu.
    ls_data-starting_folder = '/'.
    ls_data-folder_logic    = c_folder_logic-prefix.

    APPEND '/.gitignore' TO ls_data-ignore.
    APPEND '/LICENSE' TO ls_data-ignore.
    APPEND '/README.md' TO ls_data-ignore.
    APPEND '/package.json' TO ls_data-ignore.
    APPEND '/.travis.yml' TO ls_data-ignore.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.


  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE ty_dot_abapgit.


    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

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

* downward compatibility
    IF rs_data-folder_logic IS INITIAL.
      rs_data-folder_logic = c_folder_logic-prefix.
    ENDIF.

  ENDMETHOD.


  METHOD get_data.
    rs_data = ms_data.
  ENDMETHOD.


  METHOD get_folder_logic.
    rv_logic = ms_data-folder_logic.
  ENDMETHOD.


  METHOD get_master_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.


  METHOD get_signature.

    rs_signature-path     = zif_abapgit_definitions=>gc_root_dir.
    rs_signature-filename = zif_abapgit_definitions=>gc_dot_abapgit.
    rs_signature-sha1     = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob
                                                    iv_data = serialize( ) ).

  ENDMETHOD. "get_signature


  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.


  METHOD is_ignored.

    DATA: lv_name     TYPE string,
          lv_starting TYPE string,
          lv_dot      TYPE string,
          lv_count    TYPE i,
          lv_ignore   TYPE string.


    lv_name = iv_path && iv_filename.

    CONCATENATE ms_data-starting_folder '*' INTO lv_starting.
    CONCATENATE '/' zif_abapgit_definitions=>gc_dot_abapgit INTO lv_dot.

    LOOP AT ms_data-ignore INTO lv_ignore.
      FIND ALL OCCURRENCES OF '/' IN lv_name MATCH COUNT lv_count.

      IF lv_name CP lv_ignore
          OR ( ms_data-starting_folder <> '/'
          AND lv_count > 1
          AND NOT lv_name CP lv_starting
          AND NOT lv_name = lv_dot ).
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


  METHOD serialize.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( ms_data ).

    rv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml ).

  ENDMETHOD.


  METHOD set_folder_logic.
    ms_data-folder_logic = iv_logic.
  ENDMETHOD.


  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.


  METHOD to_xml.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = is_data
      RESULT XML rv_xml.

    rv_xml = zcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
