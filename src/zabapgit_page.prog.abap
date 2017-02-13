*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       INTERFACE lif_gui_page DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_gui_page.

  METHODS on_event
    IMPORTING iv_action    TYPE clike
              iv_prev_page TYPE clike
              iv_getdata   TYPE clike OPTIONAL
              it_postdata  TYPE cnht_post_data_tab OPTIONAL
    EXPORTING ei_page      TYPE REF TO lif_gui_page
              ev_state     TYPE i
    RAISING   lcx_exception lcx_cancel.

  METHODS render
    RETURNING VALUE(ro_html) TYPE REF TO lcl_html
    RAISING   lcx_exception.

ENDINTERFACE.

CLASS lcl_gui_page DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_gui_page.

  PROTECTED SECTION.

    TYPES: BEGIN OF ty_control,
             redirect_url TYPE string,
             page_title   TYPE string,
             page_menu    TYPE REF TO lcl_html_toolbar,
           END OF  ty_control.

    DATA: ms_control TYPE ty_control.

    METHODS render_content ABSTRACT
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html
      RAISING   lcx_exception.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html
      RAISING   lcx_exception.

  PRIVATE SECTION.

    METHODS html_head
      RETURNING VALUE(ro_html)   TYPE REF TO lcl_html.

    METHODS title
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.


    METHODS footer
      RETURNING VALUE(ro_html)    TYPE REF TO lcl_html.

    METHODS redirect
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.

ENDCLASS. "lcl_gui_page

CLASS lcl_gui_page IMPLEMENTATION.

  METHOD html_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<head>' ).                               "#EC NOTEXT

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT

    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ro_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT

    ro_html->add( lcl_gui_asset_manager=>get_webfont_link( ) ). " Web fonts

    ro_html->add( '</head>' ).                              "#EC NOTEXT

  ENDMETHOD.                    "html_head

  METHOD title.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="header">' ).                    "#EC NOTEXT
    ro_html->add( '<table class="w100"><tr>' ).             "#EC NOTEXT

    ro_html->add( |<td class="logo">{
                  lcl_html=>a( iv_txt = '<img src="img/logo" alt="logo">'
                               iv_act = gc_action-abapgit_home )
                  }</td>| ).                                "#EC NOTEXT

    ro_html->add( |<td class="headpad"><span class="page_title"> &#x25BA; {
                  ms_control-page_title
                  }</span></td>| ).                         "#EC NOTEXT

    IF ms_control-page_menu IS BOUND.
      ro_html->add( '<td class="headpad right">' ).         "#EC NOTEXT
      ro_html->add( ms_control-page_menu->render( ) ).
      ro_html->add( '</td>' ).                              "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render page title

  METHOD footer.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="footer">' ).                    "#EC NOTEXT

    ro_html->add( '<img src="img/logo" alt="logo">' ).      "#EC NOTEXT
    ro_html->add( '<table class="w100"><tr>' ).             "#EC NOTEXT

    ro_html->add( '<td class="w40"></td>' ).                "#EC NOTEXT
    ro_html->add( |<td><span class="version">{ gc_abap_version }</span></td>| ). "#EC NOTEXT
    ro_html->add( '<td id="debug-output" class="w40"></td>' ). "#EC NOTEXT

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD. "footer

  METHOD redirect.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                "#EC NOTEXT
    ro_html->add( '<html>' ).                         "#EC NOTEXT
    ro_html->add( '<head>' ).                         "#EC NOTEXT
    ro_html->add( |<meta http-equiv="refresh" content="0; url={
                  ms_control-redirect_url }">| ). "#EC NOTEXT
    ro_html->add( '</head>').                         "#EC NOTEXT
    ro_html->add( '</html>').                         "#EC NOTEXT

  ENDMETHOD.

  METHOD scripts.
    ASSERT 1 = 1. " Dummy
  ENDMETHOD. "scripts

  METHOD lif_gui_page~on_event.
    ev_state = gc_event_state-not_handled.
  ENDMETHOD. "lif_gui_page~on_event

  METHOD lif_gui_page~render.

    DATA lo_script TYPE REF TO lcl_html.

    " Redirect
    IF ms_control-redirect_url IS NOT INITIAL.
      ro_html = redirect( ).
      RETURN.
    ENDIF.

    " Real page
    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( html_head( ) ).
    ro_html->add( '<body>' ).                               "#EC NOTEXT
    ro_html->add( title( ) ).
    ro_html->add( render_content( ) ).
    ro_html->add( footer( ) ).
    ro_html->add( '</body>' ).                              "#EC NOTEXT

    lo_script = scripts( ).

    IF lo_script IS BOUND AND lo_script->is_empty( ) = abap_false.
      ro_html->add( '<script type="text/javascript">' ).
      ro_html->add( lo_script ).
      ro_html->add( 'confirmInitialized();' ).
      ro_html->add( '</script>' ).
    ENDIF.

    ro_html->add( '</html>').                               "#EC NOTEXT

  ENDMETHOD.  " lif_gui_page~render.

ENDCLASS. "lcl_gui_page
