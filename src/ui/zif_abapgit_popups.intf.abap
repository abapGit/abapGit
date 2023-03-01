INTERFACE zif_abapgit_popups
  PUBLIC .


  TYPES:
    ty_sval_tt TYPE STANDARD TABLE OF sval WITH DEFAULT KEY,
    ty_rows    TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

  TYPES:
    BEGIN OF ty_alv_column,
      name      TYPE string,
      text      TYPE string,
      length    TYPE lvc_outlen,
      show_icon TYPE abap_bool,
      center    TYPE abap_bool,
    END OF ty_alv_column,
    ty_alv_column_tt TYPE TABLE OF ty_alv_column WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_popup_position,
      start_column LIKE  sy-cucol,
      start_row    LIKE  sy-curow,
      end_column   LIKE  sy-cucol,
      end_row      LIKE  sy-curow,
    END OF ty_popup_position.

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
      VALUE(rs_branch)    TYPE zif_abapgit_git_definitions=>ty_git_branch
    RAISING
      zcx_abapgit_exception .
  METHODS tag_list_popup
    IMPORTING
      !iv_url       TYPE string
    RETURNING
      VALUE(rs_tag) TYPE zif_abapgit_git_definitions=>ty_git_tag
    RAISING
      zcx_abapgit_exception .
  METHODS commit_list_popup
    IMPORTING
      !iv_repo_url     TYPE string
      !iv_branch_name  TYPE string OPTIONAL
    RETURNING
      VALUE(rs_commit) TYPE zif_abapgit_definitions=>ty_commit
    RAISING
      zcx_abapgit_exception .
  TYPES ty_char1 TYPE c LENGTH 1.
  TYPES ty_icon TYPE c LENGTH 30.
  METHODS popup_to_confirm
    IMPORTING
      !iv_titlebar              TYPE clike
      !iv_text_question         TYPE clike
      !iv_text_button_1         TYPE clike DEFAULT 'Yes'
      !iv_icon_button_1         TYPE ty_icon DEFAULT space
      !iv_text_button_2         TYPE clike DEFAULT 'No'
      !iv_icon_button_2         TYPE ty_icon DEFAULT space
      !iv_default_button        TYPE ty_char1 DEFAULT '1'
      !iv_display_cancel_button TYPE ty_char1 DEFAULT abap_true
    RETURNING
      VALUE(rv_answer)          TYPE ty_char1
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
      !iv_end_column         TYPE i DEFAULT 125
      !iv_start_line         TYPE i DEFAULT 10
      !iv_end_line           TYPE i DEFAULT 30
      !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
      !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
      !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
      !iv_select_column_text TYPE csequence DEFAULT space
      !it_columns_to_display TYPE ty_alv_column_tt
      !it_preselected_rows   TYPE ty_rows OPTIONAL
    EXPORTING
      VALUE(et_list)         TYPE STANDARD TABLE
    RAISING
      zcx_abapgit_exception .
  METHODS popup_transport_request
    IMPORTING
      !is_transport_type        TYPE zif_abapgit_definitions=>ty_transport_type OPTIONAL
      !iv_use_default_transport TYPE abap_bool DEFAULT abap_false
      PREFERRED PARAMETER is_transport_type
    RETURNING
      VALUE(rv_transport)       TYPE trkorr
    RAISING
      zcx_abapgit_exception .
  METHODS choose_pr_popup
    IMPORTING
      !it_pulls      TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests
    RETURNING
      VALUE(rs_pull) TYPE zif_abapgit_pr_enum_provider=>ty_pull_request
    RAISING
      zcx_abapgit_exception .
  METHODS popup_select_tr_requests
    IMPORTING is_selection        TYPE trwbo_selection
              iv_title            TYPE trwbo_title
              iv_username_pattern TYPE any DEFAULT sy-uname
    RETURNING VALUE(rt_r_trkorr)  TYPE zif_abapgit_definitions=>ty_trrngtrkor_tt
    RAISING   zcx_abapgit_exception.
  METHODS popup_select_wb_tc_tr_and_tsk
    RETURNING VALUE(rt_r_trkorr) TYPE zif_abapgit_definitions=>ty_trrngtrkor_tt
    RAISING   zcx_abapgit_exception.
  METHODS popup_to_select_labels
    IMPORTING iv_labels        TYPE string OPTIONAL
    RETURNING VALUE(rv_labels) TYPE string
    RAISING   zcx_abapgit_exception.
  METHODS choose_code_insp_check_variant
    RETURNING VALUE(rv_check_variant) TYPE sci_chkv
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
