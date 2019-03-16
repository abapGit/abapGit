INTERFACE zif_abapgit_gui_page PUBLIC.

  INTERFACES zif_abapgit_gui_event_handler.
  INTERFACES zif_abapgit_gui_renderable.
  ALIASES on_event FOR zif_abapgit_gui_event_handler~on_event.
  ALIASES render FOR zif_abapgit_gui_renderable~render.

ENDINTERFACE.
