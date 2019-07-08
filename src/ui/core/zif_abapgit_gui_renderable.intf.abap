INTERFACE zif_abapgit_gui_renderable
  PUBLIC .

  METHODS render
    RETURNING
      VALUE(ro_html) TYPE REF TO zif_abapgit_html
    RAISING
      zcx_abapgit_exception.

  METHODS show_error
    IMPORTING
      ix_error          TYPE REF TO zcx_abapgit_exception
    RETURNING
      VALUE(rv_handled) TYPE abap_bool.

ENDINTERFACE.
