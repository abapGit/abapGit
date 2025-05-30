INTERFACE zif_abapgit_gui_diff_extra PUBLIC.
* only diff rendering, not page related stuff

  METHODS insert_nav
    RETURNING
      VALUE(rv_insert_nav) TYPE abap_bool .

* this overrides the default
  METHODS render_beacon_begin_of_row
    IMPORTING
      !ii_html TYPE REF TO zif_abapgit_html
      !is_diff TYPE zif_abapgit_gui_diff=>ty_file_diff .

* this overrides the default
  METHODS render_diff_head_after_state
    IMPORTING
      !ii_html TYPE REF TO zif_abapgit_html
      !is_diff TYPE zif_abapgit_gui_diff=>ty_file_diff .

* extra rendering before the default
  METHODS render_line_split_row
    IMPORTING
      !ii_html      TYPE REF TO zif_abapgit_html
      !iv_filename  TYPE string
      !is_diff_line TYPE zif_abapgit_definitions=>ty_diff
      !iv_index     TYPE sy-tabix
      !iv_fstate    TYPE char1
      !iv_new       TYPE string
      !iv_old       TYPE string
    RAISING
      zcx_abapgit_exception .

* extra rendering after the default
  METHODS render_table_head_non_unified
    IMPORTING
      !ii_html TYPE REF TO zif_abapgit_html
      !is_diff TYPE zif_abapgit_gui_diff=>ty_file_diff .

ENDINTERFACE.
