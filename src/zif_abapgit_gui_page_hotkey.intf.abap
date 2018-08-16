INTERFACE zif_abapgit_gui_page_hotkey
  PUBLIC.

  TYPES:
    BEGIN OF ty_hotkey_action,
      name           TYPE string,
      action         TYPE string,
      default_hotkey TYPE string,
    END OF ty_hotkey_action,
    tty_hotkey_action TYPE STANDARD TABLE OF ty_hotkey_action
                           WITH NON-UNIQUE DEFAULT KEY
                           WITH NON-UNIQUE SORTED KEY action
                                COMPONENTS action.

  CLASS-METHODS
    get_hotkey_actions
      RETURNING
        VALUE(rt_hotkey_actions) TYPE tty_hotkey_action.

ENDINTERFACE.
