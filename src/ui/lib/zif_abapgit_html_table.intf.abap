INTERFACE zif_abapgit_html_table
  PUBLIC .

  TYPES:
    BEGIN OF ty_row_attrs,
      css_class TYPE string,
    END OF ty_row_attrs.

  TYPES:
    BEGIN OF ty_cell_render,
      css_class TYPE string,
      content TYPE string,
      html TYPE REF TO zif_abapgit_html,
    END OF ty_cell_render.

  METHODS get_row_attrs
    IMPORTING
      iv_row_index TYPE i
      is_row TYPE any
    RETURNING
      VALUE(rs_attrs) TYPE ty_row_attrs
    RAISING
      zcx_abapgit_exception.

  METHODS render_cell
    IMPORTING
      iv_row_index TYPE i
      is_row TYPE any
      iv_column_id TYPE string
      iv_value TYPE any
    RETURNING
      VALUE(rs_render) TYPE ty_cell_render
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
