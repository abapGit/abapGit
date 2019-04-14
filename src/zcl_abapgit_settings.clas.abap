CLASS zcl_abapgit_settings DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: c_commitmsg_comment_length_dft TYPE i VALUE 50.
    CONSTANTS: c_commitmsg_body_size_dft      TYPE i VALUE 72.

    CONSTANTS:
      BEGIN OF c_icon_scaling,
        large TYPE c VALUE 'L',
        small TYPE c VALUE 'S',
      END OF c_icon_scaling.

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
          is_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings,
      get_show_default_repo
        RETURNING
          VALUE(rv_show_default_repo) TYPE abap_bool,
      set_show_default_repo
        IMPORTING
          iv_show_default_repo TYPE abap_bool,
      set_link_hints_enabled
        IMPORTING
          iv_link_hints_enabled TYPE abap_bool,
      get_link_hints_enabled
        RETURNING
          VALUE(rv_link_hints_enabled) TYPE abap_bool
        RAISING
          zcx_abapgit_exception,
      set_link_hint_key
        IMPORTING
          iv_link_hint_key TYPE char01,
      get_link_hint_key
        RETURNING
          VALUE(rv_link_hint_key) TYPE char01,
      get_link_hint_background_color
        RETURNING
          VALUE(rv_background_color) TYPE string,
      set_link_hint_background_color
        IMPORTING
          iv_background_color TYPE string,
      set_hotkeys
        IMPORTING
          it_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey,
      get_hotkeys
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_definitions=>tty_hotkey
        RAISING
          zcx_abapgit_exception,
      set_parallel_proc_disabled
        IMPORTING
          iv_disable_parallel_proc TYPE abap_bool,
      get_parallel_proc_disabled
        RETURNING
          VALUE(rv_disable_parallel_proc) TYPE abap_bool,
      get_icon_scaling
        RETURNING
          VALUE(rv_scaling) TYPE zif_abapgit_definitions=>ty_s_user_settings-icon_scaling,
      set_icon_scaling
        IMPORTING
          iv_scaling TYPE zif_abapgit_definitions=>ty_s_user_settings-icon_scaling.
  PROTECTED SECTION.
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

    METHODS:
      set_default_link_hint_key,
      set_default_link_hint_bg_color.

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


  METHOD get_hotkeys.

    DATA: lt_default_hotkeys TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action,
          ls_hotkey          LIKE LINE OF rt_hotkeys.

    FIELD-SYMBOLS: <ls_default_hotkey> LIKE LINE OF lt_default_hotkeys.

    IF lines( ms_user_settings-hotkeys ) > 0.

      rt_hotkeys = ms_user_settings-hotkeys.

    ELSE.

      " provide default hotkeys
      lt_default_hotkeys = zcl_abapgit_hotkeys=>get_default_hotkeys_from_pages( ).

      LOOP AT lt_default_hotkeys ASSIGNING <ls_default_hotkey>.

        ls_hotkey-action   = <ls_default_hotkey>-action.
        ls_hotkey-sequence = <ls_default_hotkey>-default_hotkey.
        INSERT ls_hotkey INTO TABLE rt_hotkeys.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_icon_scaling.
    rv_scaling = ms_user_settings-icon_scaling.
  ENDMETHOD.


  METHOD get_link_hints_enabled.
    rv_link_hints_enabled = ms_user_settings-link_hints_enabled.
  ENDMETHOD.


  METHOD get_link_hint_background_color.
    rv_background_color = ms_user_settings-link_hint_background_color.
  ENDMETHOD.


  METHOD get_link_hint_key.
    rv_link_hint_key = ms_user_settings-link_hint_key.
  ENDMETHOD.


  METHOD get_max_lines.
    rv_lines = ms_user_settings-max_lines.
  ENDMETHOD.


  METHOD get_parallel_proc_disabled.
    rv_disable_parallel_proc = ms_user_settings-parallel_proc_disabled.
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


  METHOD get_show_default_repo.
    rv_show_default_repo = ms_user_settings-show_default_repo.
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
    set_adt_jump_enanbled( abap_true ).
    set_show_default_repo( abap_false ).
    set_commitmsg_comment_length( c_commitmsg_comment_length_dft ).
    set_commitmsg_body_size( c_commitmsg_body_size_dft ).
    set_default_link_hint_key( ).
    set_default_link_hint_bg_color( ).
    set_icon_scaling( '' ).

  ENDMETHOD.


  METHOD set_default_link_hint_bg_color.
    set_link_hint_background_color( |lightgreen| ).
  ENDMETHOD.


  METHOD set_default_link_hint_key.
    set_link_hint_key( |f| ).
  ENDMETHOD.


  METHOD set_experimental_features.
    ms_settings-experimental_features = iv_run.
  ENDMETHOD.


  METHOD set_hotkeys.
    ms_user_settings-hotkeys = it_hotkeys.
  ENDMETHOD.


  METHOD set_icon_scaling.
    ms_user_settings-icon_scaling = iv_scaling.
    IF ms_user_settings-icon_scaling NA c_icon_scaling.
      ms_user_settings-icon_scaling = ''. " Reset to default
    ENDIF.
  ENDMETHOD.


  METHOD set_link_hints_enabled.
    ms_user_settings-link_hints_enabled = iv_link_hints_enabled.
  ENDMETHOD.


  METHOD set_link_hint_background_color.
    ms_user_settings-link_hint_background_color = iv_background_color.
  ENDMETHOD.


  METHOD set_link_hint_key.
    ms_user_settings-link_hint_key = iv_link_hint_key.
  ENDMETHOD.


  METHOD set_max_lines.
    ms_user_settings-max_lines = iv_lines.
  ENDMETHOD.


  METHOD set_parallel_proc_disabled.
    ms_user_settings-parallel_proc_disabled = iv_disable_parallel_proc.
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


  METHOD set_show_default_repo.
    ms_user_settings-show_default_repo = iv_show_default_repo.
  ENDMETHOD.


  METHOD set_user_settings.
    ms_user_settings = is_user_settings.

    IF ms_user_settings-link_hint_key IS INITIAL.
      set_default_link_hint_key( ).
    ENDIF.

    IF ms_user_settings-link_hint_background_color IS INITIAL.
      set_default_link_hint_bg_color( ).
    ENDIF.
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
