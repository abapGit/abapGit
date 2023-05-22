INTERFACE zif_abapgit_gui_render_item
  PUBLIC .

  METHODS render
    IMPORTING
      iv_item        TYPE any
      iv_index       TYPE i
    RETURNING
      VALUE(ri_html) TYPE REF TO zif_abapgit_html
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
