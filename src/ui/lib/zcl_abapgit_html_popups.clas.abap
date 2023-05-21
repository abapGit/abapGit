CLASS zcl_abapgit_html_popups DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS branch_list
      IMPORTING
        !iv_url             TYPE string
        !iv_default_branch  TYPE string OPTIONAL
        !iv_show_new_option TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_popup)     TYPE REF TO zif_abapgit_html_popup.

    CLASS-METHODS pull_request_list
      IMPORTING
        iv_url          TYPE string
      RETURNING
        VALUE(ri_popup) TYPE REF TO zif_abapgit_html_popup.

    CLASS-METHODS tag_list
      IMPORTING
        iv_url          TYPE string
      RETURNING
        VALUE(ri_popup) TYPE REF TO zif_abapgit_html_popup.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_html_popups IMPLEMENTATION.


  METHOD branch_list.
    CREATE OBJECT ri_popup TYPE lcl_branch_popup
      EXPORTING
        iv_url             = iv_url
        iv_default_branch  = iv_default_branch
        iv_show_new_option = iv_show_new_option.
  ENDMETHOD.


  METHOD pull_request_list.
    CREATE OBJECT ri_popup TYPE lcl_pr_popup
      EXPORTING
        iv_url = iv_url.
  ENDMETHOD.


  METHOD tag_list.
    CREATE OBJECT ri_popup TYPE lcl_tag_popup
      EXPORTING
        iv_url = iv_url.
  ENDMETHOD.
ENDCLASS.
