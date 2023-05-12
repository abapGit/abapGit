INTERFACE zif_abapgit_gui_modal
  PUBLIC .

  METHODS is_modal
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

ENDINTERFACE.
