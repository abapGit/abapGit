INTERFACE zif_abapgit_gui_error_handler
  PUBLIC .

  METHODS handle_error
    IMPORTING
      ix_error          TYPE REF TO zcx_abapgit_exception
    RETURNING
      VALUE(rv_handled) TYPE abap_bool.

ENDINTERFACE.
