INTERFACE zif_abapgit_gui_functions
  PUBLIC .

  METHODS:
    gui_is_available
      RETURNING
        VALUE(rv_gui_is_available) TYPE abap_bool.

ENDINTERFACE.
