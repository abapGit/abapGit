INTERFACE zif_abapgit_gui_menu_provider
  PUBLIC .

  METHODS get_menu
    RETURNING
      VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
