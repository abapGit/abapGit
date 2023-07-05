class ZCL_ABAPGIT_DOT_ABAPGIT definition
  public
  create public .

public section.

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
      !IS_DATA type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT .
  methods SERIALIZE
    returning
      value(RV_XSTR) type XSTRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods TO_FILE
    returning
      value(RS_FILE) type ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_FILE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_DATA
    returning
      value(RS_DATA) type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT .
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
  methods GET_MAIN_LANGUAGE
    returning
      value(RV_LANGUAGE) type SPRAS .
  methods GET_I18N_LANGUAGES
    returning
      value(RT_LANGUAGES) type ZIF_ABAPGIT_DEFINITIONS=>TY_LANGUAGES
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SET_I18N_LANGUAGES
    importing
      value(IT_LANGUAGES) type ZIF_ABAPGIT_DEFINITIONS=>TY_LANGUAGES
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_SIGNATURE
    returning
      value(RS_SIGNATURE) type ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_FILE_SIGNATURE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods USE_LXE
    importing
      !IV_YES type ABAP_BOOL default ABAP_UNDEFINED
    returning
      value(RV_YES) type ABAP_BOOL .
  methods GET_REQUIREMENTS
    returning
      value(RT_REQUIREMENTS) type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_REQUIREMENT_TT .
  methods SET_REQUIREMENTS
    importing
      !IT_REQUIREMENTS type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_REQUIREMENT_TT .
  methods GET_VERSION_CONSTANT
    returning
      value(RV_VERSION_CONSTANT) type STRING .
  methods SET_VERSION_CONSTANT
    importing
      !IV_VERSION_CONSTANT type CSEQUENCE .
  methods GET_ABAP_LANGUAGE_VERSION
    returning
      value(RV_ABAP_LANGUAGE_VERSION) type STRING .
  methods SET_ABAP_LANGUAGE_VERSION
    importing
      !IV_ABAP_LANGUAGE_VERSION type STRING .
  PROTECTED SECTION.
private section.

  data MS_DATA type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT .

  class-methods TO_XML
    importing
      !IS_DATA type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT
    returning
      value(RV_XML) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods FROM_XML
    importing
      !IV_XML type STRING
    returning
      value(RS_DATA) type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT .
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
    ls_data-abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.

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


  method GET_ABAP_LANGUAGE_VERSION.
   rv_abap_language_version = ms_data-abap_language_version.
  endmethod.


  METHOD set_abap_language_version.
    ms_data-abap_language_version = iv_abap_language_version.
  ENDMETHOD.
ENDCLASS.
