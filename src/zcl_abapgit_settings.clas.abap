class ZCL_ABAPGIT_SETTINGS definition
  public
  create public .

public section.

  constants C_COMMITMSG_COMMENT_LENGTH_DFT type I value 50 ##NO_TEXT.
  constants C_COMMITMSG_BODY_SIZE_DFT type I value 72 ##NO_TEXT.

  methods SET_PROXY_URL
    importing
      !IV_URL type STRING .
  methods SET_PROXY_PORT
    importing
      !IV_PORT type STRING .
  methods SET_PROXY_AUTHENTICATION
    importing
      !IV_AUTH type ABAP_BOOL .
  methods GET_PROXY_URL
    returning
      value(RV_PROXY_URL) type STRING .
  methods GET_PROXY_PORT
    returning
      value(RV_PORT) type STRING .
  methods GET_PROXY_AUTHENTICATION
    returning
      value(RV_AUTH) type ABAP_BOOL .
  methods SET_RUN_CRITICAL_TESTS
    importing
      !IV_RUN type ABAP_BOOL .
  methods GET_RUN_CRITICAL_TESTS
    returning
      value(RV_RUN) type ABAP_BOOL .
  methods SET_EXPERIMENTAL_FEATURES
    importing
      !IV_RUN type ABAP_BOOL .
  methods GET_EXPERIMENTAL_FEATURES
    returning
      value(RV_RUN) type ABAP_BOOL .
  methods SET_MAX_LINES
    importing
      !IV_LINES type I .
  methods GET_MAX_LINES
    returning
      value(RV_LINES) type I .
  methods SET_ADT_JUMP_ENANBLED
    importing
      !IV_ADT_JUMP_ENABLED type ABAP_BOOL .
  methods GET_ADT_JUMP_ENABLED
    returning
      value(RV_ADT_JUMP_ENABLED) type ABAP_BOOL .
  methods SET_COMMITMSG_COMMENT_LENGTH
    importing
      !IV_LENGTH type I .
  methods GET_COMMITMSG_COMMENT_LENGTH
    returning
      value(RV_LENGTH) type I .
  methods SET_COMMITMSG_BODY_SIZE
    importing
      !IV_LENGTH type I .
  methods GET_COMMITMSG_BODY_SIZE
    returning
      value(RV_LENGTH) type I .
  methods GET_SETTINGS_XML
    returning
      value(RV_SETTINGS_XML) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_USER_SETTINGS
    returning
      value(RS_SETTINGS) type ZIF_ABAPGIT_DEFINITIONS=>TY_S_USER_SETTINGS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SET_XML_SETTINGS
    importing
      !IV_SETTINGS_XML type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SET_DEFAULTS .
  methods SET_USER_SETTINGS
    importing
      !IS_USER_SETTINGS type ZIF_ABAPGIT_DEFINITIONS=>TY_S_USER_SETTINGS .
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_settings,
             proxy_url                TYPE string,
             proxy_port               TYPE string,
             proxy_auth               TYPE string,
             run_critical_tests       TYPE abap_bool,
             experimental_features    TYPE abap_bool,
             commitmsg_comment_length TYPE i,
             commitmsg_body_size      TYPE i,
           END OF ty_s_settings.
    DATA: ms_settings      TYPE ty_s_settings,
          ms_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.

ENDCLASS.



CLASS ZCL_ABAPGIT_SETTINGS IMPLEMENTATION.


  METHOD get_adt_jump_enabled.
    rv_adt_jump_enabled = ms_user_settings-adt_jump_enabled.
  ENDMETHOD.


  METHOD get_commitmsg_body_size.
    rv_length = ms_settings-commitmsg_body_size.
  ENDMETHOD.


  METHOD get_commitmsg_comment_length.
    rv_length = ms_settings-commitmsg_comment_length.
  ENDMETHOD.


  METHOD get_experimental_features.
    rv_run = ms_settings-experimental_features.
  ENDMETHOD.


  METHOD get_max_lines.
    rv_lines = ms_user_settings-max_lines.
  ENDMETHOD.


  METHOD get_proxy_authentication.
    rv_auth = ms_settings-proxy_auth.
  ENDMETHOD.


  METHOD get_proxy_port.
    rv_port = ms_settings-proxy_port.
  ENDMETHOD.


  METHOD get_proxy_url.
    rv_proxy_url = ms_settings-proxy_url.
  ENDMETHOD.


  METHOD get_run_critical_tests.
    rv_run = ms_settings-run_critical_tests.
  ENDMETHOD.


  METHOD get_settings_xml.

    DATA: lo_output TYPE REF TO zcl_abapgit_xml_output.


    CREATE OBJECT lo_output.

    lo_output->add( iv_name = zcl_abapgit_persistence_db=>c_type_settings
                    ig_data = ms_settings ).

    rv_settings_xml = lo_output->render( ).

  ENDMETHOD.


  METHOD get_user_settings.
    rs_settings = ms_user_settings.
  ENDMETHOD.


  METHOD set_adt_jump_enanbled.
    ms_user_settings-adt_jump_enabled = iv_adt_jump_enabled.
  ENDMETHOD.


  METHOD set_commitmsg_body_size.
    ms_settings-commitmsg_body_size = iv_length.
  ENDMETHOD.


  METHOD set_commitmsg_comment_length.
    ms_settings-commitmsg_comment_length = iv_length.
  ENDMETHOD.


  METHOD set_defaults.

    CLEAR ms_settings.

    set_proxy_authentication( abap_false ).
    set_run_critical_tests( abap_false ).
    set_experimental_features( abap_false ).
    set_max_lines( 500 ).
    set_adt_jump_enanbled( abap_false ).
    set_commitmsg_comment_length( c_commitmsg_comment_length_dft ).
    set_commitmsg_body_size( c_commitmsg_body_size_dft ).

  ENDMETHOD.


  METHOD set_experimental_features.
    ms_settings-experimental_features = iv_run.
  ENDMETHOD.


  METHOD set_max_lines.
    ms_user_settings-max_lines = iv_lines.
  ENDMETHOD.


  METHOD set_proxy_authentication.
    ms_settings-proxy_auth = iv_auth.
  ENDMETHOD.


  METHOD set_proxy_port.
    ms_settings-proxy_port = iv_port.
  ENDMETHOD.


  METHOD set_proxy_url.
    ms_settings-proxy_url = iv_url.
  ENDMETHOD.


  METHOD set_run_critical_tests.
    ms_settings-run_critical_tests = iv_run.
  ENDMETHOD.


  METHOD set_user_settings.
    ms_user_settings = is_user_settings.
  ENDMETHOD.


  METHOD set_xml_settings.

    DATA: lo_input TYPE REF TO zcl_abapgit_xml_input.


    CREATE OBJECT lo_input EXPORTING iv_xml = iv_settings_xml.

    CLEAR ms_settings.

    lo_input->read(
      EXPORTING
        iv_name = zcl_abapgit_persistence_db=>c_type_settings
      CHANGING
        cg_data = ms_settings ).

  ENDMETHOD.
ENDCLASS.
