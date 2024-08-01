CLASS zcl_abapgit_web DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS handle
      IMPORTING
        ii_request  TYPE REF TO zif_abapgit_web_request
        ii_response TYPE REF TO zif_abapgit_web_response
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    CONSTANTS c_base      TYPE string VALUE '/sap/zabapgit/' ##NO_TEXT.

    CLASS-DATA go_viewer   TYPE REF TO zcl_abapgit_html_viewer_web .
    CLASS-DATA go_gui      TYPE REF TO zcl_abapgit_gui .
    CLASS-DATA gi_request  TYPE REF TO zif_abapgit_web_request.
    CLASS-DATA gi_response TYPE REF TO zif_abapgit_web_response.

    CLASS-METHODS initialize
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS sapevent.
    CLASS-METHODS redirect.
    CLASS-METHODS search_asset
      RETURNING
        VALUE(rv_found) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_web IMPLEMENTATION.

  METHOD handle.

    DATA lv_found TYPE abap_bool.
    DATA lv_path  TYPE string.

    gi_request = ii_request.
    gi_response = ii_response.

    IF go_viewer IS INITIAL.
      initialize( ).
    ENDIF.

    lv_found = search_asset( ).
    IF lv_found = abap_true.
      RETURN.
    ENDIF.

    lv_path = cl_http_utility=>unescape_url( ii_request->get_header_field( '~path' ) ).
    IF lv_path = '/sap/zabapgit'.
      redirect( ).
    ELSEIF lv_path = c_base.
      go_gui->go_home( zif_abapgit_definitions=>c_action-go_home ).
    ELSEIF lv_path = |{ c_base }css/bundle.css|.
      go_viewer->zif_abapgit_html_viewer~show_url( |css/bundle.css| ).
    ELSEIF lv_path CP |{ c_base }sapevent:+*|.
      sapevent( ).
    ELSE.
      ii_response->set_content_type( 'text/html' ).
      ii_response->set_cdata( 'handle_request, unknown path ' && lv_path ).
    ENDIF.

  ENDMETHOD.


  METHOD initialize.

    CREATE OBJECT go_viewer
      EXPORTING
        ii_request  = gi_request
        ii_response = gi_response.

    zcl_abapgit_ui_injector=>set_html_viewer( go_viewer ).

    go_gui = zcl_abapgit_ui_factory=>get_gui( ).

  ENDMETHOD.


  METHOD redirect.

    DATA lv_html TYPE string.

    lv_html =
      |<!DOCTYPE html>\n| &&
      |<html>\n| &&
      |   <head>\n| &&
      |      <title>HTML Meta Tag</title>\n| &&
      |      <meta http-equiv = "refresh" content = "0; url = { c_base }" />\n| &&
      |   </head>\n| &&
      |   <body>\n| &&
      |      <p>Redirecting</p>\n| &&
      |   </body>\n| &&
      |</html>|.

    gi_response->set_cdata( lv_html ).

  ENDMETHOD.


  METHOD sapevent.

* todo, parse and pass data
* todo, respect GET and POST

    DATA: lv_action   TYPE c LENGTH 100,
          lv_getdata  TYPE c LENGTH 100,
          lv_method   TYPE string,
          lv_body     TYPE string,
          lv_value    TYPE string,
          lt_postdata TYPE zif_abapgit_html_viewer=>ty_post_data.

    lv_value = gi_request->get_header_field( '~request_uri' ).

    REPLACE FIRST OCCURRENCE OF c_base IN lv_value WITH ''.

    FIND REGEX '^sapevent:([\w-]+)' IN lv_value SUBMATCHES lv_action.

    FIND REGEX '\?([\w=&%.]+)' IN lv_value SUBMATCHES lv_getdata.

    lv_method = gi_request->get_method( ).
    IF lv_method = 'POST'.
      lv_body = gi_request->get_cdata( ).

      zcl_abapgit_convert=>string_to_tab(
        EXPORTING
          iv_str = lv_body
        IMPORTING
          et_tab = lt_postdata ).
    ENDIF.

    go_gui->on_event(
      action   = lv_action
      getdata  = lv_getdata
      postdata = lt_postdata ).

* sdf     getdata     = iv_getdata
* sdf     postdata    = VALUE #( )
* sdf     query_table = VALUE #( ).

  ENDMETHOD.


  METHOD search_asset.

    DATA ls_asset  TYPE zif_abapgit_gui_asset_manager=>ty_web_asset.
    DATA lv_search TYPE string.
    DATA lv_path   TYPE string.
    DATA li_assets TYPE REF TO zif_abapgit_gui_asset_manager.


    lv_path = cl_http_utility=>unescape_url( gi_request->get_header_field( '~path' ) ).

    li_assets = zcl_abapgit_ui_factory=>get_asset_manager( ).

    IF lv_path CP |{ c_base }+*|.
      lv_search = lv_path.
      REPLACE FIRST OCCURRENCE OF c_base IN lv_search WITH ''.
      TRY.
          ls_asset = li_assets->get_asset( lv_search ).
          gi_response->set_content_type( |{ ls_asset-type }/{ ls_asset-subtype }| ).
          gi_response->set_xdata( ls_asset-content ).
          rv_found = abap_true.
        CATCH zcx_abapgit_exception.
          rv_found = abap_false.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
