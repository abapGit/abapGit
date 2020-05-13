INTERFACE zif_abapgit_gui_hotkey_ctl
  PUBLIC .

  METHODS register_hotkeys
    IMPORTING
      ii_hotkeys TYPE REF TO zif_abapgit_gui_hotkeys.

  METHODS reset.

  METHODS get_registered_hotkeys
    RETURNING
      VALUE(rt_registered_hotkeys) TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
