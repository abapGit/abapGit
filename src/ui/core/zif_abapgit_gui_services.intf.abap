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

  " For future refactoring
  " Potentially also: back, go_home, go_page, +some access to page stack

ENDINTERFACE.
