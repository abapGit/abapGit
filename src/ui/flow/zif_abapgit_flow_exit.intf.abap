INTERFACE zif_abapgit_flow_exit
  PUBLIC .

  TYPES: BEGIN OF ty_settings,
           allow_not_up_to_date TYPE abap_bool,
         END OF ty_settings.

  METHODS get_settings
    IMPORTING
      !iv_repo_key       TYPE zif_abapgit_persistence=>ty_repo-key
    RETURNING
      VALUE(rs_settings) TYPE ty_settings
    RAISING
      zcx_abapgit_exception.

  METHODS toolbar_extras
    IMPORTING
      !io_toolbar TYPE REF TO zcl_abapgit_html_toolbar
      !is_feature TYPE zif_abapgit_flow_logic=>ty_feature
      !iv_index   TYPE i
    RAISING
      zcx_abapgit_exception.

  METHODS info_extras
    IMPORTING
      !ii_html    TYPE REF TO zif_abapgit_html
      !is_feature TYPE zif_abapgit_flow_logic=>ty_feature
    RAISING
      zcx_abapgit_exception.

  TYPES: BEGIN OF ty_event_result,
           handled TYPE zif_abapgit_gui_event_handler=>ty_handling_result,
           refresh TYPE abap_bool,
         END OF ty_event_result.

  METHODS on_event
    IMPORTING
      ii_event         TYPE REF TO zif_abapgit_gui_event
      it_features      TYPE zif_abapgit_flow_logic=>ty_features
    RETURNING
      VALUE(rs_result) TYPE ty_event_result
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
