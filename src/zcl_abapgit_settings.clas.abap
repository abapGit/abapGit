CLASS zcl_abapgit_settings DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: c_commitmsg_comment_length_dft TYPE i VALUE 50.
    CONSTANTS: c_commitmsg_body_size_dft      TYPE i VALUE 72.

    METHODS:
      set_proxy_url
        IMPORTING
          iv_url TYPE string,
      set_proxy_port
        IMPORTING
          iv_port TYPE string,
      set_proxy_authentication
        IMPORTING
          iv_auth TYPE abap_bool,
      get_proxy_url
        RETURNING
          VALUE(rv_proxy_url) TYPE string,
      get_proxy_port
        RETURNING
          VALUE(rv_port) TYPE string,
      get_proxy_authentication
        RETURNING
          VALUE(rv_auth) TYPE abap_bool,
      set_run_critical_tests
        IMPORTING
          iv_run TYPE abap_bool,
      get_run_critical_tests
        RETURNING
          VALUE(rv_run) TYPE abap_bool,
      set_experimental_features
        IMPORTING
          iv_run TYPE abap_bool,
      get_experimental_features
        RETURNING
          VALUE(rv_run) TYPE abap_bool,
      set_max_lines
        IMPORTING iv_lines TYPE i,
      get_max_lines
        RETURNING
          VALUE(rv_lines) TYPE i,
      set_adt_jump_enanbled
        IMPORTING
          iv_adt_jump_enabled TYPE abap_bool,
      get_adt_jump_enabled
        RETURNING
          VALUE(rv_adt_jump_enabled) TYPE abap_bool,
      set_commitmsg_comment_length
        IMPORTING
          iv_length TYPE i,
      get_commitmsg_comment_length
        RETURNING
          VALUE(rv_length) TYPE i,
      set_commitmsg_body_size
        IMPORTING
          iv_length TYPE i,
      get_commitmsg_body_size
        RETURNING
          VALUE(rv_length) TYPE i,
      get_settings_xml
        RETURNING
          VALUE(rv_settings_xml) TYPE string
        RAISING
          zcx_abapgit_exception,
      get_user_settings
        RETURNING
          VALUE(rs_settings) TYPE zif_abapgit_definitions=>ty_s_user_settings
        RAISING
          zcx_abapgit_exception,
      set_xml_settings
        IMPORTING
          iv_settings_xml TYPE string
        RAISING
          zcx_abapgit_exception,
      set_defaults,
      set_user_settings
        IMPORTING
          is_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.

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



CLASS zcl_abapgit_settings IMPLEMENTATION.


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

  METHOD get_user_settings.
    rs_settings = ms_user_settings.
  ENDMETHOD.

ENDCLASS.
