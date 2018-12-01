INTERFACE zif_abapgit_gui_asset_manager
  PUBLIC .

  TYPES:
    BEGIN OF ty_web_asset,
      url     TYPE w3url,
      type    TYPE char50,
      subtype TYPE char50,
      content TYPE xstring,
    END OF ty_web_asset .
  TYPES:
    tt_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY .

  METHODS get_all_assets
    RETURNING
      VALUE(rt_assets) TYPE tt_web_assets
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
