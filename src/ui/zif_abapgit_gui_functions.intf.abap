INTERFACE zif_abapgit_gui_functions
  PUBLIC.

  METHODS:
    gui_is_available
      RETURNING
        VALUE(rv_gui_is_available) TYPE abap_bool,

    is_sapgui_for_java
      RETURNING
        VALUE(rv_result) TYPE abap_bool,

    is_sapgui_for_windows
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDINTERFACE.
