*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_callback_pr DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_form_data TYPE REF TO zcl_abapgit_string_map
        iv_key       TYPE string.
    INTERFACES zif_abapgit_gui_page_callback.
  PRIVATE SECTION.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mv_key       TYPE string.
ENDCLASS.

CLASS lcl_callback_pr IMPLEMENTATION.
  METHOD constructor.
    mo_form_data = io_form_data.
    mv_key = iv_key.
  ENDMETHOD.

  METHOD zif_abapgit_gui_page_callback~row_selected.
    DATA lt_pulls TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests.
    DATA ls_pull LIKE LINE OF lt_pulls.
    DATA lv_pull_request TYPE string.

    lt_pulls = it_table.

    READ TABLE lt_pulls INDEX iv_index INTO ls_pull.
    ASSERT sy-subrc = 0.

    IF ls_pull IS NOT INITIAL.
      lv_pull_request = ls_pull-head_url && '@' && ls_pull-head_branch.
    ENDIF.

    mo_form_data->set(
      iv_key = mv_key
      iv_val = lv_pull_request ).
  ENDMETHOD.
ENDCLASS.
