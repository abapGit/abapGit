INTERFACE zif_abapgit_html_table
  PUBLIC .

  TYPES:
    BEGIN OF ty_row_attrs,
      css_class TYPE string,
      data      TYPE zif_abapgit_html=>ty_data_attr,
    END OF ty_row_attrs.

  TYPES:
    BEGIN OF ty_cell_render,
      css_class TYPE string,
      content   TYPE string,
      html      TYPE REF TO zif_abapgit_html,
    END OF ty_cell_render.

  TYPES:
    BEGIN OF ty_sorting_state,
      column_id  TYPE string,
      descending TYPE abap_bool,
    END OF ty_sorting_state.

  METHODS get_row_attrs
    IMPORTING
      iv_table_id     TYPE string
      iv_row_index    TYPE i
      is_row          TYPE any
    RETURNING
      VALUE(rs_attrs) TYPE ty_row_attrs
    RAISING
      zcx_abapgit_exception.

  METHODS render_cell
    IMPORTING
      iv_table_id      TYPE string
      iv_row_index     TYPE i
      is_row           TYPE any
      iv_column_id     TYPE string
      iv_value         TYPE any
    RETURNING
      VALUE(rs_render) TYPE ty_cell_render
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
