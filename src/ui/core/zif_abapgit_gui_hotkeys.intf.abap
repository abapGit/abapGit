INTERFACE zif_abapgit_gui_hotkeys
  PUBLIC .

  TYPES:
    BEGIN OF ty_hotkey_with_descr.
      INCLUDE TYPE zif_abapgit_definitions=>ty_hotkey.
  TYPES:
    description TYPE string,
  END OF ty_hotkey_with_descr .
  TYPES:
    tty_hotkey_with_descr TYPE STANDARD TABLE OF ty_hotkey_with_descr
      WITH DEFAULT KEY
      WITH UNIQUE SORTED KEY action COMPONENTS ui_component action .

  CLASS-METHODS get_hotkey_actions " TODO: try to refactor class-method
    RETURNING
      VALUE(rt_hotkey_actions) TYPE tty_hotkey_with_descr .

ENDINTERFACE.
