INTERFACE zif_abapgit_gui_page_hotkey
  PUBLIC.

  TYPES:
    BEGIN OF ty_hotkey_with_name.
      INCLUDE TYPE zif_abapgit_definitions=>ty_hotkey.
  TYPES:
    name TYPE string,
    END OF ty_hotkey_with_name,
    tty_hotkey_with_name TYPE STANDARD TABLE OF ty_hotkey_with_name
                              WITH NON-UNIQUE DEFAULT KEY
                              WITH NON-UNIQUE SORTED KEY action
                                   COMPONENTS action.

  CLASS-METHODS:
    get_hotkey_actions
      RETURNING
        VALUE(rt_hotkey_actions) TYPE tty_hotkey_with_name.

ENDINTERFACE.
