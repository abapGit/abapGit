INTERFACE zif_abapgit_gui_asset_manager
  PUBLIC .

  TYPES:
    BEGIN OF ty_web_asset,
      url          TYPE w3url,
      type         TYPE char50,
      subtype      TYPE char50,
      content      TYPE xstring,
      is_cacheable TYPE abap_bool,
    END OF ty_web_asset .
  TYPES:
    tt_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY .

  METHODS get_all_assets
    RETURNING
      VALUE(rt_assets) TYPE tt_web_assets
    RAISING
      zcx_abapgit_exception.

  METHODS get_asset
    IMPORTING
      iv_url          TYPE string
    RETURNING
      VALUE(rs_asset) TYPE ty_web_asset
    RAISING
      zcx_abapgit_exception.

  METHODS get_text_asset
    IMPORTING
      iv_url            TYPE string
      iv_assert_subtype TYPE string OPTIONAL
    RETURNING
      VALUE(rv_asset)   TYPE string
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
