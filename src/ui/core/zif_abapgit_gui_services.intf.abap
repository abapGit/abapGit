INTERFACE zif_abapgit_gui_services
  PUBLIC .

  METHODS cache_asset
    IMPORTING
      iv_text       TYPE string OPTIONAL
      iv_xdata      TYPE xstring OPTIONAL
      iv_url        TYPE w3url OPTIONAL
      iv_type       TYPE c
      iv_subtype    TYPE c
    RETURNING
      VALUE(rv_url) TYPE w3url.

  METHODS register_event_handler
    IMPORTING
      ii_event_handler TYPE REF TO zif_abapgit_gui_event_handler.

  METHODS get_current_page_name
    RETURNING
      VALUE(rv_page_name) TYPE string.


  " For future refactoring
  " Potentially also: back, go_home, go_page, +some access to page stack

ENDINTERFACE.
