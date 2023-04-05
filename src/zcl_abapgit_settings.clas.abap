CLASS zcl_abapgit_settings DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_commitmsg_comment_length_dft TYPE i VALUE 50 ##NO_TEXT.
    CONSTANTS c_commitmsg_body_size_dft TYPE i VALUE 72 ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_icon_scaling,
        large TYPE c VALUE 'L',
        small TYPE c VALUE 'S',
      END OF c_icon_scaling .
    CONSTANTS:
      BEGIN OF c_ui_theme,
        default         TYPE string VALUE 'default',
        dark            TYPE string VALUE 'dark',
        belize          TYPE string VALUE 'belize',
        synced_with_gui TYPE string VALUE 'synced_with_gui',
      END OF c_ui_theme .

    METHODS set_proxy_url
      IMPORTING
        !iv_url TYPE string .
    METHODS set_proxy_port
      IMPORTING
        !iv_port TYPE string .
    METHODS set_proxy_authentication
      IMPORTING
        !iv_auth TYPE abap_bool .
    METHODS set_proxy_bypass
      IMPORTING
        !it_bypass TYPE zif_abapgit_definitions=>ty_range_proxy_bypass_url OPTIONAL .
    METHODS get_proxy_url
      RETURNING
        VALUE(rv_proxy_url) TYPE string .
    METHODS get_proxy_port
      RETURNING
        VALUE(rv_port) TYPE string .
    METHODS get_proxy_authentication
      RETURNING
        VALUE(rv_auth) TYPE abap_bool .
    METHODS get_proxy_bypass
      RETURNING
        VALUE(rt_bypass) TYPE zif_abapgit_definitions=>ty_range_proxy_bypass_url .
    METHODS set_run_critical_tests
      IMPORTING
        !iv_run TYPE abap_bool .
    METHODS get_run_critical_tests
      RETURNING
        VALUE(rv_run) TYPE abap_bool .
    METHODS set_experimental_features
      IMPORTING
        !iv_run TYPE abap_bool .
    METHODS get_experimental_features
      RETURNING
        VALUE(rv_run) TYPE abap_bool .
    METHODS set_max_lines
      IMPORTING
        !iv_lines TYPE i .
    METHODS get_max_lines
      RETURNING
        VALUE(rv_lines) TYPE i .
    METHODS set_adt_jump_enanbled
      IMPORTING
        !iv_adt_jump_enabled TYPE abap_bool .
    METHODS get_adt_jump_enabled
      RETURNING
        VALUE(rv_adt_jump_enabled) TYPE abap_bool .
    METHODS set_commitmsg_comment_length
      IMPORTING
        !iv_length TYPE i .
    METHODS get_commitmsg_comment_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS set_commitmsg_comment_default
      IMPORTING
        !iv_default TYPE string .
    METHODS get_commitmsg_comment_default
      RETURNING
        VALUE(rv_default) TYPE string .
    METHODS set_commitmsg_body_size
      IMPORTING
        !iv_length TYPE i .
    METHODS get_commitmsg_body_size
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS set_commitmsg_hide_author
      IMPORTING
        !iv_hide_author TYPE abap_bool.
    METHODS get_commitmsg_hide_author
      RETURNING
        VALUE(rv_hide_author) TYPE abap_bool.
    METHODS get_settings_xml
      RETURNING
        VALUE(rv_settings_xml) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS get_user_settings
      RETURNING
        VALUE(rs_settings) TYPE zif_abapgit_definitions=>ty_s_user_settings
      RAISING
        zcx_abapgit_exception .
    METHODS set_xml_settings
      IMPORTING
        !iv_settings_xml TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS set_defaults .
    METHODS set_user_settings
      IMPORTING
        !is_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings .
    METHODS get_show_default_repo
      RETURNING
        VALUE(rv_show_default_repo) TYPE abap_bool .
    METHODS set_show_default_repo
      IMPORTING
        !iv_show_default_repo TYPE abap_bool .
    METHODS set_link_hints_enabled
      IMPORTING
        !iv_link_hints_enabled TYPE abap_bool .
    METHODS get_link_hints_enabled
      RETURNING
        VALUE(rv_link_hints_enabled) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS set_link_hint_key
      IMPORTING
        !iv_link_hint_key TYPE string .
    METHODS get_link_hint_key
      RETURNING
        VALUE(rv_link_hint_key) TYPE string .
    METHODS set_parallel_proc_disabled
      IMPORTING
        !iv_disable_parallel_proc TYPE abap_bool .
    METHODS get_parallel_proc_disabled
      RETURNING
        VALUE(rv_disable_parallel_proc) TYPE abap_bool .
    METHODS get_icon_scaling
      RETURNING
        VALUE(rv_scaling) TYPE zif_abapgit_definitions=>ty_s_user_settings-icon_scaling .
    METHODS set_icon_scaling
      IMPORTING
        !iv_scaling TYPE zif_abapgit_definitions=>ty_s_user_settings-icon_scaling .
    METHODS get_ui_theme
      IMPORTING
        !iv_resolve_synced TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_ui_theme) TYPE zif_abapgit_definitions=>ty_s_user_settings-ui_theme .
    METHODS set_ui_theme
      IMPORTING
        !iv_ui_theme TYPE zif_abapgit_definitions=>ty_s_user_settings-ui_theme .
    METHODS get_activate_wo_popup
      RETURNING
        VALUE(rv_act_wo_popup) TYPE zif_abapgit_definitions=>ty_s_user_settings-activate_wo_popup .
    METHODS set_activate_wo_popup
      IMPORTING
        !iv_act_wo_popup TYPE zif_abapgit_definitions=>ty_s_user_settings-activate_wo_popup .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_settings,
             proxy_url                TYPE string,
             proxy_port               TYPE string,
             proxy_auth               TYPE string,
             proxy_bypass             TYPE zif_abapgit_definitions=>ty_range_proxy_bypass_url,
             run_critical_tests       TYPE abap_bool,
             experimental_features    TYPE abap_bool,
             commitmsg_comment_length TYPE i,
             commitmsg_comment_deflt  TYPE string,
             commitmsg_body_size      TYPE i,
             commitmsg_hide_author    TYPE abap_bool,
           END OF ty_s_settings.

    DATA: ms_settings      TYPE ty_s_settings,
          ms_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.

    METHODS:
      set_default_link_hint_key.

ENDCLASS.



CLASS zcl_abapgit_settings IMPLEMENTATION.


  METHOD get_activate_wo_popup.
    rv_act_wo_popup = ms_user_settings-activate_wo_popup.
  ENDMETHOD.


  METHOD get_adt_jump_enabled.
    rv_adt_jump_enabled = ms_user_settings-adt_jump_enabled.
  ENDMETHOD.


  METHOD get_commitmsg_body_size.
    rv_length = ms_settings-commitmsg_body_size.
  ENDMETHOD.


  METHOD get_commitmsg_comment_default.
    rv_default = ms_settings-commitmsg_comment_deflt.
  ENDMETHOD.


  METHOD get_commitmsg_comment_length.
    rv_length = ms_settings-commitmsg_comment_length.
  ENDMETHOD.


  METHOD get_commitmsg_hide_author.
    rv_hide_author = ms_settings-commitmsg_hide_author.
  ENDMETHOD.


  METHOD get_experimental_features.
    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      rv_run = ms_settings-experimental_features.
    ENDIF.
  ENDMETHOD.


  METHOD get_icon_scaling.
    rv_scaling = ms_user_settings-icon_scaling.
  ENDMETHOD.


  METHOD get_link_hints_enabled.
    rv_link_hints_enabled = ms_user_settings-link_hints_enabled.
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


  METHOD get_proxy_bypass.
    rt_bypass = ms_settings-proxy_bypass.
  ENDMETHOD.


  METHOD get_proxy_port.
    rv_port = ms_settings-proxy_port.
  ENDMETHOD.


  METHOD get_proxy_url.
    rv_proxy_url = ms_settings-proxy_url.
  ENDMETHOD.


  METHOD get_run_critical_tests.
    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      rv_run = ms_settings-run_critical_tests.
    ENDIF.
  ENDMETHOD.


  METHOD get_settings_xml.

    DATA: li_output TYPE REF TO zif_abapgit_xml_output.


    CREATE OBJECT li_output TYPE zcl_abapgit_xml_output.

    li_output->add( iv_name = zcl_abapgit_persistence_db=>c_type_settings
                    ig_data = ms_settings ).

    rv_settings_xml = li_output->render( ).

  ENDMETHOD.


  METHOD get_show_default_repo.
    rv_show_default_repo = ms_user_settings-show_default_repo.
  ENDMETHOD.


  METHOD get_ui_theme.
    DATA lv_frontend_theme TYPE string.
    DATA lv_cl_gui TYPE string.

    lv_cl_gui = 'CL_GUI_RESOURCES'.

    rv_ui_theme = ms_user_settings-ui_theme.

    IF rv_ui_theme = c_ui_theme-synced_with_gui AND iv_resolve_synced = abap_true.
      TRY.
          CALL METHOD (lv_cl_gui)=>get_themename
            IMPORTING
              themename              = lv_frontend_theme
            EXCEPTIONS
              get_std_resource_error = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
            rv_ui_theme = c_ui_theme-default.
            RETURN.
          ENDIF.
        CATCH cx_sy_dyn_call_error.
          rv_ui_theme = c_ui_theme-default.
          RETURN.
      ENDTRY.

      CASE lv_frontend_theme.
        WHEN 'Belize'.
          rv_ui_theme = c_ui_theme-belize.
        WHEN OTHERS.
          rv_ui_theme = c_ui_theme-default.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_user_settings.
    rs_settings = ms_user_settings.
  ENDMETHOD.


  METHOD set_activate_wo_popup.
    ms_user_settings-activate_wo_popup = iv_act_wo_popup.
  ENDMETHOD.


  METHOD set_adt_jump_enanbled.
    ms_user_settings-adt_jump_enabled = iv_adt_jump_enabled.
  ENDMETHOD.


  METHOD set_commitmsg_body_size.
    ms_settings-commitmsg_body_size = iv_length.
  ENDMETHOD.


  METHOD set_commitmsg_comment_default.
    ms_settings-commitmsg_comment_deflt = iv_default.
  ENDMETHOD.


  METHOD set_commitmsg_comment_length.
    ms_settings-commitmsg_comment_length = iv_length.
  ENDMETHOD.


  METHOD set_commitmsg_hide_author.
    ms_settings-commitmsg_hide_author = iv_hide_author.
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
    set_icon_scaling( '' ).

  ENDMETHOD.


  METHOD set_default_link_hint_key.
    " Since #5859 'f' is used for "focus filter", we use 't' as the new default
    set_link_hint_key( |t| ).
  ENDMETHOD.


  METHOD set_experimental_features.
    ms_settings-experimental_features = iv_run.
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


  METHOD set_proxy_bypass.
    ms_settings-proxy_bypass = it_bypass.
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


  METHOD set_ui_theme.
    ms_user_settings-ui_theme = iv_ui_theme.
    IF ms_user_settings-ui_theme <> c_ui_theme-default
        AND ms_user_settings-ui_theme <> c_ui_theme-dark
        AND ms_user_settings-ui_theme <> c_ui_theme-belize
        AND ms_user_settings-ui_theme <> c_ui_theme-synced_with_gui.
      ms_user_settings-ui_theme = c_ui_theme-default. " Reset to default
    ENDIF.
  ENDMETHOD.


  METHOD set_user_settings.
    ms_user_settings = is_user_settings.

    IF ms_user_settings-link_hint_key IS INITIAL.
      set_default_link_hint_key( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_xml_settings.

    DATA: lo_input TYPE REF TO zif_abapgit_xml_input.


    CREATE OBJECT lo_input TYPE zcl_abapgit_xml_input EXPORTING iv_xml = iv_settings_xml.

    CLEAR ms_settings.

    lo_input->read(
      EXPORTING
        iv_name = zcl_abapgit_persistence_db=>c_type_settings
      CHANGING
        cg_data = ms_settings ).

  ENDMETHOD.
ENDCLASS.
