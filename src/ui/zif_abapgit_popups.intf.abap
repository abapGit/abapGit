INTERFACE zif_abapgit_popups
  PUBLIC .


  TYPES:
    ty_sval_tt TYPE STANDARD TABLE OF sval WITH DEFAULT KEY .

  CONSTANTS c_new_branch_label TYPE string VALUE '+ create new ...' ##NO_TEXT.

  METHODS popup_search_help
    IMPORTING
      !iv_tab_field   TYPE string
    RETURNING
      VALUE(rv_value) TYPE ddshretval-fieldval
    RAISING
      zcx_abapgit_exception .
  METHODS popup_folder_logic
    RETURNING
      VALUE(rv_folder_logic) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS create_branch_popup
    IMPORTING
      !iv_source_branch_name TYPE string
    EXPORTING
      !ev_name               TYPE string
      !ev_cancel             TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS branch_list_popup
    IMPORTING
      !iv_url             TYPE string
      !iv_default_branch  TYPE string OPTIONAL
      !iv_show_new_option TYPE abap_bool OPTIONAL
      !iv_hide_branch     TYPE zif_abapgit_persistence=>ty_repo-branch_name OPTIONAL
      !iv_hide_head       TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(rs_branch)    TYPE zif_abapgit_definitions=>ty_git_branch
    RAISING
      zcx_abapgit_exception .
  METHODS popup_to_confirm
    IMPORTING
      !iv_titlebar              TYPE clike
      !iv_text_question         TYPE clike
      !iv_text_button_1         TYPE clike DEFAULT 'Yes'
      !iv_icon_button_1         TYPE icon-name DEFAULT space
      !iv_text_button_2         TYPE clike DEFAULT 'No'
      !iv_icon_button_2         TYPE icon-name DEFAULT space
      !iv_default_button        TYPE char1 DEFAULT '1'
      !iv_display_cancel_button TYPE char1 DEFAULT abap_true
    RETURNING
      VALUE(rv_answer)          TYPE char1
    RAISING
      zcx_abapgit_exception .
  METHODS popup_to_create_package
    EXPORTING
      !es_package_data TYPE scompkdtln
      !ev_create       TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS popup_to_create_transp_branch
    IMPORTING
      !it_transport_headers      TYPE trwbo_request_headers
    RETURNING
      VALUE(rs_transport_branch) TYPE zif_abapgit_definitions=>ty_transport_to_branch
    RAISING
      zcx_abapgit_exception .
  METHODS popup_to_select_transports
    RETURNING
      VALUE(rt_trkorr) TYPE trwbo_request_headers .
  METHODS popup_to_select_from_list
    IMPORTING
      !it_list               TYPE STANDARD TABLE
      !iv_title              TYPE lvc_title DEFAULT space
      !iv_header_text        TYPE csequence DEFAULT space
      !iv_start_column       TYPE i DEFAULT 10
      !iv_end_column         TYPE i DEFAULT 90
      !iv_start_line         TYPE i DEFAULT 8
      !iv_end_line           TYPE i DEFAULT 25
      !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
      !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
      !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
      !iv_select_column_text TYPE csequence DEFAULT space
      !it_columns_to_display TYPE zif_abapgit_definitions=>ty_alv_column_tt
    EXPORTING
      VALUE(et_list)         TYPE STANDARD TABLE
    RAISING
      zcx_abapgit_exception .
  METHODS branch_popup_callback
    IMPORTING
      !iv_code       TYPE clike
    CHANGING
      !ct_fields     TYPE ty_sval_tt
      !cs_error      TYPE svale
      !cv_show_popup TYPE char01
    RAISING
      zcx_abapgit_exception .
  METHODS popup_transport_request
    IMPORTING
      !is_transport_type  TYPE zif_abapgit_definitions=>ty_transport_type
    RETURNING
      VALUE(rv_transport) TYPE trkorr
    RAISING
      zcx_abapgit_exception .
  METHODS choose_pr_popup
    IMPORTING
      !it_pulls      TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests
    RETURNING
      VALUE(rs_pull) TYPE zif_abapgit_pr_enum_provider=>ty_pull_request
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
