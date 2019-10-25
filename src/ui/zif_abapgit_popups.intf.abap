INTERFACE zif_abapgit_popups
  PUBLIC .


  TYPES:
    BEGIN OF ty_popup,
      url          TYPE string,
      package      TYPE devclass,
      branch_name  TYPE string,
      display_name TYPE string,
      folder_logic TYPE string,
      ign_subpkg   TYPE abap_bool,
      cancel       TYPE abap_bool,
    END OF ty_popup .

  CONSTANTS c_new_branch_label TYPE string VALUE '+ create new ...' ##NO_TEXT.

  METHODS popup_package_export
    EXPORTING
      !ev_package      TYPE devclass
      !ev_folder_logic TYPE string
      !ev_serialize_master_lang_only TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS popup_folder_logic
    RETURNING
      VALUE(rv_folder_logic) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS popup_object
    RETURNING
      VALUE(rs_tadir) TYPE zif_abapgit_definitions=>ty_tadir
    RAISING
      zcx_abapgit_exception .
  METHODS create_branch_popup
    EXPORTING
      !ev_name   TYPE string
      !ev_cancel TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS run_page_class_popup
    EXPORTING
      !ev_name   TYPE string
      !ev_cancel TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS repo_new_offline
    RETURNING
      VALUE(rs_popup) TYPE zif_abapgit_popups=>ty_popup
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
  METHODS repo_popup
    IMPORTING
      !iv_url            TYPE string
      !iv_package        TYPE devclass OPTIONAL
      !iv_branch         TYPE string DEFAULT 'refs/heads/master'
      !iv_freeze_package TYPE abap_bool OPTIONAL
      !iv_freeze_url     TYPE abap_bool OPTIONAL
      !iv_title          TYPE clike DEFAULT 'New Online Project'
      !iv_display_name   TYPE string OPTIONAL
    RETURNING
      VALUE(rs_popup)    TYPE zif_abapgit_popups=>ty_popup
    RAISING
      zcx_abapgit_exception ##NO_TEXT.
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
  METHODS popup_to_inform
    IMPORTING
      !iv_titlebar     TYPE clike
      !iv_text_message TYPE clike
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
      zcx_abapgit_exception.
  METHODS popup_to_select_transports
    RETURNING
      VALUE(rt_trkorr) TYPE trwbo_request_headers .
  METHODS popup_to_select_from_list
    IMPORTING
      !it_list               TYPE STANDARD TABLE
      !iv_header_text        TYPE csequence
      !iv_select_column_text TYPE csequence
      !it_columns_to_display TYPE string_table
    EXPORTING
      VALUE(et_list)         TYPE STANDARD TABLE
    RAISING
      zcx_abapgit_exception .
  METHODS branch_popup_callback
    IMPORTING
      !iv_code       TYPE clike
    CHANGING
      !ct_fields     TYPE zif_abapgit_definitions=>ty_sval_tt
      !cs_error      TYPE svale
      !cv_show_popup TYPE char01
    RAISING
      zcx_abapgit_exception .
  METHODS package_popup_callback
    IMPORTING
      !iv_code       TYPE clike
    CHANGING
      !ct_fields     TYPE zif_abapgit_definitions=>ty_sval_tt
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
      zcx_abapgit_exception.
ENDINTERFACE.
