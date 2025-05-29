INTERFACE zif_abapgit_gui_diff_extra PUBLIC.
* only diff rendering, not page related stuff

  METHODS insert_nav
    RETURNING
      VALUE(rv_insert_nav) TYPE abap_bool .

* this overrides the default
  METHODS render_beacon_begin_of_row
    IMPORTING
      !ii_html TYPE REF TO zif_abapgit_html
      !is_diff TYPE zcl_abapgit_gui_page_diff_base=>ty_file_diff .

* this overrides the default
  METHODS render_diff_head_after_state
    IMPORTING
      !ii_html TYPE REF TO zif_abapgit_html
      !is_diff TYPE zcl_abapgit_gui_page_diff_base=>ty_file_diff .

ENDINTERFACE.
