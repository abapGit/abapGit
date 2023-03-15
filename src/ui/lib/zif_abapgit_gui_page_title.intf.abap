INTERFACE zif_abapgit_gui_page_title
  PUBLIC .

  METHODS get_page_title
    RETURNING
      VALUE(rv_title) TYPE string.

ENDINTERFACE.
