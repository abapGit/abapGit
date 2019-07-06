INTERFACE zif_abapgit_gui_error_handler
  PUBLIC .

  METHODS handle_error
    IMPORTING
      ii_page  TYPE REF TO zif_abapgit_gui_renderable
      ix_error TYPE REF TO zcx_abapgit_exception.

ENDINTERFACE.
