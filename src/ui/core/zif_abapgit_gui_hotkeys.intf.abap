INTERFACE zif_abapgit_gui_hotkeys
  PUBLIC .

  TYPES:
    BEGIN OF ty_hotkey_with_descr,
      ui_component TYPE string,
      action       TYPE string,
      hotkey       TYPE string,
      description  TYPE string,
    END OF ty_hotkey_with_descr .
  TYPES:
    ty_hotkeys_with_descr TYPE STANDARD TABLE OF ty_hotkey_with_descr
      WITH DEFAULT KEY
      WITH UNIQUE SORTED KEY action COMPONENTS ui_component action .

  METHODS get_hotkey_actions
    RETURNING
      VALUE(rt_hotkey_actions) TYPE ty_hotkeys_with_descr .

ENDINTERFACE.
