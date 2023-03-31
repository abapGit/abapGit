CLASS zcl_abapgit_dot_abapgit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS build_default
      RETURNING
        VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit .
    CLASS-METHODS deserialize
      IMPORTING
        !iv_xstr              TYPE xstring
      RETURNING
        VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !is_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit .
    METHODS serialize
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS to_file
      RETURNING
        VALUE(rs_file) TYPE zif_abapgit_git_definitions=>ty_file
      RAISING
        zcx_abapgit_exception.
    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit .
    METHODS add_ignore
      IMPORTING
        !iv_path     TYPE string
        !iv_filename TYPE string .
    METHODS is_ignored
      IMPORTING
        !iv_path          TYPE string
        !iv_filename      TYPE string
      RETURNING
        VALUE(rv_ignored) TYPE abap_bool .
    METHODS remove_ignore
      IMPORTING
        !iv_path     TYPE string
        !iv_filename TYPE string .
    METHODS get_starting_folder
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS get_folder_logic
      RETURNING
        VALUE(rv_logic) TYPE string .
    METHODS set_folder_logic
      IMPORTING
        !iv_logic TYPE string .
    METHODS set_starting_folder
      IMPORTING
        !iv_path TYPE string .
    METHODS get_main_language
      RETURNING
        VALUE(rv_language) TYPE spras .
    METHODS get_i18n_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    METHODS set_i18n_languages
      IMPORTING
        VALUE(it_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    METHODS get_signature
      RETURNING
        VALUE(rs_signature) TYPE zif_abapgit_git_definitions=>ty_file_signature
      RAISING
        zcx_abapgit_exception .
    METHODS use_lxe
      IMPORTING
        iv_yes TYPE abap_bool DEFAULT abap_undefined
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
    METHODS get_requirements
      RETURNING
        VALUE(rt_requirements) TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt .
    METHODS set_requirements
      IMPORTING
        !it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt .
    METHODS get_version_constant
      RETURNING VALUE(rv_version_constant) TYPE string.
    METHODS set_version_constant
      IMPORTING iv_version_constant TYPE csequence.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ms_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

    CLASS-METHODS:
      to_xml
        IMPORTING is_data       TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit
        RETURNING VALUE(rv_xml) TYPE string
        RAISING   zcx_abapgit_exception,
      from_xml
        IMPORTING iv_xml         TYPE string
        RETURNING VALUE(rs_data) TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

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

    DATA: ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.


    ls_data-master_language = sy-langu.
    ls_data-starting_folder = '/src/'.
    ls_data-folder_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.


  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.


    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.


  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data.

* downward compatibility
    IF rs_data-folder_logic IS INITIAL.
      rs_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
    ENDIF.

  ENDMETHOD.


  METHOD get_data.
    rs_data = ms_data.
  ENDMETHOD.


  METHOD get_folder_logic.
    rv_logic = ms_data-folder_logic.
  ENDMETHOD.


  METHOD get_i18n_languages.
    rt_languages = ms_data-i18n_languages.
  ENDMETHOD.


  METHOD get_main_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.


  METHOD get_requirements.
    rt_requirements = ms_data-requirements.
  ENDMETHOD.


  METHOD get_signature.

    rs_signature-path     = zif_abapgit_definitions=>c_root_dir.
    rs_signature-filename = zif_abapgit_definitions=>c_dot_abapgit.
    rs_signature-sha1     = zcl_abapgit_hash=>sha1_blob( serialize( ) ).

  ENDMETHOD.


  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.


  METHOD get_version_constant.
    rv_version_constant = ms_data-version_constant.
  ENDMETHOD.


  METHOD is_ignored.

    DATA: lv_name     TYPE string,
          lv_starting TYPE string,
          lv_dot      TYPE string,
          lv_ignore   TYPE string.


    lv_name = iv_path && iv_filename.

    CONCATENATE ms_data-starting_folder '*' INTO lv_starting.

    " Always allow .abapgit.xml and .apack-manifest.xml
    CONCATENATE '/' zif_abapgit_definitions=>c_dot_abapgit INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.
    CONCATENATE '/' zif_abapgit_apack_definitions=>c_dot_apack_manifest INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.

    " Ignore all files matching pattern in ignore list
    LOOP AT ms_data-ignore INTO lv_ignore.
      IF lv_name CP lv_ignore.
        rv_ignored = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Ignore all files outside of starting folder tree
    IF ms_data-starting_folder <> '/' AND NOT lv_name CP lv_starting.
      rv_ignored = abap_true.
    ENDIF.

    IF iv_path = zif_abapgit_data_config=>c_default_path.
      rv_ignored = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD remove_ignore.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    DELETE TABLE ms_data-ignore FROM lv_name.

  ENDMETHOD.


  METHOD serialize.

    DATA lv_xml TYPE string.

    lv_xml = to_xml( ms_data ).

    rv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

  ENDMETHOD.


  METHOD set_folder_logic.
    ms_data-folder_logic = iv_logic.
  ENDMETHOD.


  METHOD set_i18n_languages.
    ms_data-i18n_languages = it_languages.
  ENDMETHOD.


  METHOD set_requirements.
    ms_data-requirements = it_requirements.
  ENDMETHOD.


  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.


  METHOD set_version_constant.
    ms_data-version_constant = iv_version_constant.
  ENDMETHOD.


  METHOD to_file.
    rs_file-path     = zif_abapgit_definitions=>c_root_dir.
    rs_file-filename = zif_abapgit_definitions=>c_dot_abapgit.
    rs_file-data     = serialize( ).
    rs_file-sha1     = zcl_abapgit_hash=>sha1_blob( rs_file-data ).
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


  METHOD use_lxe.

    IF iv_yes <> abap_undefined.
      ms_data-use_lxe = iv_yes.
    ENDIF.

    rv_yes = ms_data-use_lxe.

  ENDMETHOD.
ENDCLASS.
