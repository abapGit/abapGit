INTERFACE zif_abapgit_gui_renderable
  PUBLIC .

  METHODS render
    RETURNING
      VALUE(ri_html) TYPE REF TO zif_abapgit_html
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
