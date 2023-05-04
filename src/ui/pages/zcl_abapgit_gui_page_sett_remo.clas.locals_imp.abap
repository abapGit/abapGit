*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_callback_pr DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_page_callback.
ENDCLASS.

CLASS lcl_callback_pr IMPLEMENTATION.
  METHOD zif_abapgit_gui_page_callback~row_selected.
    BREAK-POINT.

*    IF ls_pull IS NOT INITIAL.
*      rv_pull_request = ls_pull-head_url && '@' && ls_pull-head_branch.
*    ENDIF.

*          mo_form_data->set(
*            iv_key = c_id-pull_request
*            iv_val = lv_pull_request ).
  ENDMETHOD.
ENDCLASS.
