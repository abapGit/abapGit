*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       INTERFACE lif_gui_page DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_gui_page.

  METHODS on_event
    IMPORTING iv_action   TYPE clike
              iv_getdata  TYPE clike OPTIONAL
              it_postdata TYPE cnht_post_data_tab OPTIONAL
    EXPORTING ei_page     TYPE REF TO lif_gui_page
              ev_state    TYPE i
    RAISING   lcx_exception.

  METHODS render
    RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
    RAISING   lcx_exception.

  METHODS get_assets
    RETURNING VALUE(rt_assets) TYPE tt_web_assets.

ENDINTERFACE.

CLASS lcl_gui_page_super DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_gui_page ABSTRACT METHODS render.

  PROTECTED SECTION.
    METHODS render_repo_top
      IMPORTING io_repo         TYPE REF TO lcl_repo
                iv_show_package TYPE abap_bool DEFAULT abap_true
                iv_show_branch  TYPE abap_bool DEFAULT abap_true
                iv_branch       TYPE string OPTIONAL
      RETURNING VALUE(ro_html)  TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS header
      IMPORTING io_include_style TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html)   TYPE REF TO lcl_html_helper.

    METHODS footer
      IMPORTING io_include_script TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html)    TYPE REF TO lcl_html_helper.

    METHODS title
      IMPORTING iv_title       TYPE string
                io_menu        TYPE REF TO lcl_html_toolbar OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS redirect
      IMPORTING iv_url         TYPE string
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

  PRIVATE SECTION.
    METHODS styles RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_super IMPLEMENTATION.

  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          lv_icon        TYPE string.


    CREATE OBJECT ro_html.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'img/repo_offline' ##NO_TEXT.
    ELSE.
      lv_icon = 'img/repo_online' ##NO_TEXT.
    ENDIF.

    ro_html->add( |<a id="repo{ io_repo->get_key( ) }"></a>| ).
    ro_html->add( '<table width="100%"><tr>' ).

    ro_html->add( '<td class="repo_name">' ).
    ro_html->add( |<img src="{ lv_icon }">| ).
    ro_html->add( |<span>{ io_repo->get_name( ) }</span>| ).
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="repo_attr right">' ).
    IF iv_show_package = abap_true.
      ro_html->add( '<img src="img/pkg">' ).
      ro_html->add( |<span>{ io_repo->get_package( ) }</span>| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      IF iv_show_branch = abap_true.
        ro_html->add( '<img src="img/branch">' ).
        IF iv_branch IS INITIAL.
          ro_html->add( |<span>{ lo_repo_online->get_branch_name( ) }</span>| ).
        ELSE.
          ro_html->add( |<span>{ iv_branch }</span>| ).
        ENDIF.
      ENDIF.
      ro_html->add( '<img src="img/link">' ).
      ro_html->add( |<input type="text" value="{ lo_repo_online->get_url( ) }" readonly>| ).
    ENDIF.

    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

  ENDMETHOD.

  METHOD header.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( '<head>' ).                               "#EC NOTEXT
    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( styles( ) ).

    IF io_include_style IS BOUND.
      ro_html->add( '<style type="text/css">' ).            "#EC NOTEXT
      ro_html->add( io_include_style ).
      ro_html->add( '</style>' ).                           "#EC NOTEXT
    ENDIF.

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '</head>' ).                              "#EC NOTEXT
    ro_html->add( '<body>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render html header

  METHOD title.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="header">' ).                    "#EC NOTEXT
    ro_html->add( '<table width="100%"><tr>' ).             "#EC NOTEXT

    ro_html->add( '<td class="logo">' ).                    "#EC NOTEXT
    ro_html->add( '<a href="sapevent:abapgithome">' ).      "#EC NOTEXT
    ro_html->add( '<img src="img/logo">' ).                 "#EC NOTEXT
    ro_html->add( '</a>' ).                                 "#EC NOTEXT
    ro_html->add( '</td>' ).                                "#EC NOTEXT

    ro_html->add( '<td class="headpad"><span class="page_title">' ). "#EC NOTEXT
    ro_html->add( |&#x25BA; { iv_title }| ).                "#EC NOTEXT
    ro_html->add( '</span></td>' ).                         "#EC NOTEXT

    IF io_menu IS BOUND.
      ro_html->add( '<td class="headpad right">' ).         "#EC NOTEXT
      ro_html->add( io_menu->render( ) ).
      ro_html->add( '</td>' ).                              "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render page title

  METHOD footer.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="footer">' ).                    "#EC NOTEXT
    ro_html->add( '<img src="img/logo" >' ).                "#EC NOTEXT
    ro_html->add( |<span class="version">{ gc_abap_version }</span>| ). "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT
    ro_html->add( '</body>' ).                              "#EC NOTEXT

    IF io_include_script IS BOUND.
      ro_html->add( '<script type="text/javascript">' ).
      ro_html->add( io_include_script ).
      ro_html->add( '</script>' ).
    ENDIF.

    ro_html->add( '</html>').                               "#EC NOTEXT

  ENDMETHOD.                    "render html footer & logo

  METHOD redirect.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html><head>' ).                         "#EC NOTEXT
    ro_html->add( |<meta http-equiv="refresh" content="0; url={ iv_url }">| ). "#EC NOTEXT
    ro_html->add( '</head></html>').                        "#EC NOTEXT

  ENDMETHOD.

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '<style type="text/css">'.

    " Global styles
    _add '/* GLOBALS */'.
    _add 'body {'.
    _add '  font-family: Arial,Helvetica,sans-serif;'.
    _add '  font-size:    12pt;'.
    _add '  background: #E8E8E8;'.
    _add '}'.
    _add 'a, a:visited {'.
    _add '  color:            #4078c0;'.
    _add '  text-decoration:  none;'.
    _add '}'.
    _add 'a:hover, a:active {'.
    _add '  cursor: pointer;'.
    _add '  text-decoration: underline;'.
    _add '}'.
    _add 'img               { border: 0px; vertical-align: middle; }'.
    _add 'table             { border-collapse: collapse; }'.
    _add 'pre               { display: inline; }'.

    _add 'form input, textarea, select {'.
    _add '  border: 1px solid #DDD;'.
    _add '  padding: 3px 6px;'.
    _add '}'.
    _add 'form input:focus, textarea:focus {'.
    _add '  border: 1px solid #8cadd9;'.
    _add '}'.

    " Modifiers
    _add '/* MODIFIERS */'.
    _add '.grey             { color: lightgrey  !important; }'.
    _add '.emphasis         { font-weight: bold !important; }'.
    _add '.attention        { color: red        !important; }'.
    _add '.right            { text-align:right; }'.
    _add '.paddings         { padding: 0.5em 0.5em 0.5em 0.5em; }'.

    " Structure div styles: header, footer, toc
    _add '/* STRUCTURE DIVS, HEADER & FOOTER */'.
    _add 'td.headpad { padding-top: 11px; }'.
    _add 'td.logo    { width: 164px; }'.
    _add 'div#header {'.
    _add '  padding:          0.5em 0.5em 0.5em 0.5em;'.
    _add '  border-bottom:    3px double lightgrey;'.
    _add '}'.
    _add 'div#toc {'.
    _add '  padding:          0.5em 1em 0.5em 1em;'.
    _add '  background-color: #f2f2f2;'.
    _add '}'.
    _add 'div#footer {'.
    _add '  padding:          0.5em 1em 0.5em 1em;'.
    _add '  border-top:       3px double lightgrey;'.
    _add '  text-align:       center;'.
    _add '}'.
    _add 'div.dummydiv {'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding:          0.5em 1em 0.5em 1em;'.
    _add '  text-align:       center;'.
    _add '}'.
    _add 'span.version {'.
    _add '  display: block;'.
    _add '  color: grey;'.
    _add '  margin-top: 0.3em;'.
    _add '}'.
    _add 'span.page_title {'.
    _add '  font-weight: normal;'.
    _add '  font-size: 18pt;'.
    _add '  color: #bbb;'.
    _add '  padding-left: 0.4em;'.
    _add '}'.

    " Menu styles
    _add '/* MENU */'.
    _add 'div.menu           { display: inline; }'.
    _add 'div.menu .menu_end { border-right: 0px !important; }'.
    _add 'div.menu a {'.
    _add '  padding-left: 0.5em;'.
    _add '  padding-right: 0.5em;'.
    _add '  border-right: 1px solid lightgrey;'.
    _add '  font-size: 12pt;'.
    _add '}'.
    _add 'div.menu_vertical   { display: inline; }'.
    _add 'div.menu_vertical a {'.
    _add '  display: block; '.
    _add '  font-size: 12pt;'.
    _add '}'.

    " Drop down styles
    _add '/*DROP DOWN*/'.
    _add '.dropdown {'.
    _add '    position: relative;'.
    _add '    display: inline;'.
    _add '}'.
    _add '.dropdown_content {'.
    _add '    display: none;'.
    _add '    z-index: 1;'.
    _add '    position: absolute;'.
    _add '    right: 0;'.
    _add '    top: 1.1em; /*IE7 woraround*/'.
    _add '    background-color: #f9f9f9;'.
    _add '    white-space: nowrap;'.
*    _add '    min-width: 10em;'.
    _add '    border-bottom: 1px solid lightgrey;'.
    _add '}'.
    _add '.dropdown_content a {'.
    _add '    padding: 0.2em;'.
    _add '    text-decoration: none;'.
    _add '    display: block;'.
    _add '}'.
    _add '.dropdown_content a:hover { background-color: #f1f1f1 }'.
    _add '.dropdown:hover .dropdown_content { display: block; }'.
    _add '.dropdown:hover .dropbtn  { color: #79a0d2; }'.

    _add '/* REPOSITORY */'.
    _add 'div.repo {'.
    _add '  margin-top:       3px;'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding: 0.5em 1em 0.5em 1em;'.
    _add '}'.
    _add '.repo_name span {'.
    _add '  font-weight: bold;'.
    _add '  color: #333;'.
    _add '  font-size: 14pt;'.
    _add '}'.
    _add '.repo_name img {'.
    _add '  vertical-align: baseline;'.
    _add '  margin: 0 5px 0 5px;'.
    _add '}'.
    _add '.repo_attr {'.
    _add '  color: grey;'.
    _add '  font-size: 12pt;'.
    _add '}'.
    _add '.repo_attr span {'.
    _add '  margin-left: 0.2em;'.
    _add '  margin-right: 0.5em;'.
    _add '}'.
    _add '.repo_attr input {'.
    _add '  color: grey;'.     " Input wants it personaly
    _add '  font-size: 12pt;'. " Input wants it personaly
    _add '  margin-left: 0.5em;'.
    _add '  margin-right: 0.5em;'.
    _add '  background-color: transparent;'.
    _add '  border-style: none;'.
    _add '  text-overflow: ellipsis;'.
    _add '}'.

    " Other and outdated (?) styles
    _add '/* MISC AND REFACTOR */'.
    _add 'a.grey:link {color: grey; font-size: smaller;}'.
    _add 'a.grey:visited {color: grey; font-size: smaller;}'.
    _add 'a.plain:link {color: black; text-decoration: none;}'.
    _add 'a.plain:visited {color: black; text-decoration: none;}'.
    _add 'a.bkg:link {color: #E8E8E8;}'.
    _add 'a.bkg:visited {color: #E8E8E8;}'.
    _add 'h1 {display: inline;}'.
    _add 'h2 {display: inline;}'.
    _add 'h3 {'.
    _add '  display: inline;'.
    _add '  color: grey;'.
    _add '  font-weight:normal;'.
    _add '  font-size: smaller;'.
    _add '}'.

    _add '.hidden-submit {'.
    _add '  border: 0 none;'.
    _add '  height: 0;'.
    _add '  width: 0;'.
    _add '  padding: 0;'.
    _add '  margin: 0;'.
    _add '  overflow: hidden;'.
    _add '}'.

    _add '</style>'.

  ENDMETHOD.                    "common styles

  METHOD lif_gui_page~get_assets. " Common images here

    DATA ls_image TYPE ty_web_asset.

* see https://github.com/larshp/abapGit/issues/201 for source SVG
    ls_image-url     = 'img/logo' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAKMAAAAoCAYAAACSG0qbAAAABHNCSVQICAgIfAhkiAAA'
      && 'AAlwSFlzAAAEJQAABCUBprHeCQAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9y'
      && 'Z5vuPBoAAA8VSURBVHic7Zx7cJzVeYef31nJAtvYko1JjM3FYHlXimwZkLWyLEMcwIGQ'
      && 'cEkDJWmTltLStGkoDCkzwBAuCemUlksDNCkhJTTTljJpZhIuBQxxAWPvyuYiW7UkG8Il'
      && 'UByIsS1sLEu75+0fu5JXu9/etAJz0TOzM/rOec85765+37m+3yczY8w0NU3qrwv9npfa'
      && 'Hfx02pPPd469sgk+7misYnyjpWXy5IOG7kd8ZjjNjEtr13TdOm7eTfCxwo2lUJAQASRu'
      && '2dnRfMn4uDbBx42yxZhPiMNMCHKCsVK2GGuqqqoQUwrZTAhygrFQshjfaGmZ/M7yxQtm'
      && 'xGL9/qDqzwLxQvYTgpygXEoS4/DQ7LE1O05atLBu1YZdE4KcYLwpupoOmCO+5Z2dXPfE'
      && 'xk07Tm2ZroGhBwX1wAygKqiOiVX2Rw9Jam/gyH0wuGGzvTEudRYSY4HFyogghxN2n7Sw'
      && 'IendvcCioLoOtCCXNeqohOf0oDwPq9f3Wt/77dOHlWhYzUj/BRybTnrGEnZO5wv2m0rq'
      && 'DezJoOiqeZbzegzpk6TVPPWJTT39y5svMogF1ZcesjlQgkwYp4F+EJQXwv4E+MiLUZJa'
      && 'F7AIcRq4hWZ2mMRhQD/oZcErXv7FScaja3rt/wpU9E/sFyLACQq57wB/XIl/gWIstn2T'
      && 'xpHVre7ZW71p8sFDeQscSEHKu3pTBadNH2Lq61VT57iwNazLgaNSqYaUaWXLDZCJIbBo'
      && 'g3tK2A2xHns0oMrm3CRrqdTPnAVMiUIEmLlz2XGLMxNmH7YrifFcoUIHalHj8f8p6UfA'
      && 'O+932weStno1zghps6Q7GBFiUYRxopkeaZ2vIwLyfxtQ4vV8lbWHNScacf+T/vwqn90o'
      && 'MZYhRADJ+bv725vmj6Q8tHWffPKUD6IgO/tsfawneRHYd97Pdg8kSyJaZiGtBY4pYPYO'
      && 'kH84C0Cyv8tKSiK7OZ99EpYAJ2V8AhkRY5lCHGaxhaq+BLCzY/EXd5y0aOG0td1vf1AF'
      && 'CWCw7/1u80DQEtahQvcB03MyjQfM7Hwnmxfv9dPivX5SssqOwuzPSqk71mN3ymw5ZtdK'
      && 'dmVIdly8xx7JZ29yy0qptwrGLMRRCA6T1w93nLTo5Lq13Zv625tOMRd6DLF4v0lWmQO8'
      && 'qPko45y7TWaHZyUnwa6M99mN2fYbuu1V4K5oxF1B4Z4UgFifrQHWFLNbvkh1QheV5DNN'
      && 'TZMqFWIGs5zX48M95PTqGa3TZ4erzbvj8/WUErf0L2++uNyGJLn2Js1oDeuYlkbNbmlR'
      && 'deXup2hq0qS2es2VlHMDFaOlRdXL5uuwlnodG23QTEljCkbJV3d7WHOK+dXWqHqZnZeb'
      && 'Y1fGe3OFOArRU5GTGbSHNWdwUL8Epo1qIQ9V/bXu3HES4jCznNfjb7e1zZ8Ri/UD1MLz'
      && 'u05s/huMx4IKGNy4+8Tj/2Pqk8++Vaji86TQqxEuNNM5rWGtSCaokSDkgd0QjbidoPvN'
      && '+5s7t9jz5TgdbdBMvLsG2cop6FgLUdUaZk804jYKuyrWa6vzlT2+XrOqQnxd6KwQOj5R'
      && 'hULpL9Yaxkcj7g3QT6zK397ZbdtGtbtAZ+B0U3adkt0c67E7OyI6fFDuSpktC6HGpJjU'
      && 'GmZ3NOI2mdnVnX32eHZZ7903hGXfBG8mp3J7sd/B0DPCTgUmBf9O7lmMybk56or3Jn8f'
      && 'oLVB7Q5dZ9Iy4OBsw2jYbUUk96fwQrzHf955iBZzsDA+aL9k1owZ20fNzaY/tfFXwK48'
      && 'ldQkSZ5YqJXmZk15JaJfmOmfgdOAmgCzWrCvyum5aIO+Uor3AIbOx7QV2TeBMPu3vKYA'
      && 'Sw091hbWt4PKRhu0oDqkmND1wAnk3vkOmAN2lRLa2hrWMVm5Tek2R3286YzWiK4eQltk'
      && '9g1gMfsFMhVYKunR1obQddk+SXZqwLe8acMGe7fYb9HZk7wm3utrBmpsqiXsyClHMHK6'
      && '0hLWoRjHBfmLbP9K3bPYjFPIFWLaQeZnlZ8H4JyFflrMwcK4wG63v3/ycZnXOzqalxE0'
      && 'mU7x9rvvVv93oVZqBtzNGGeU7Jbp9pZGzS7ReiVQVyDfmXRda4PaA9p5mBLmWGmmSron'
      && 'M0FytUGGgjPTAi8UIeVk9u1og5YOJ0QbNBOjIac+Y22JPgLQ1WV7Ol+w36xebYnhtGpj'
      && 'FjBYTj3l4KY9/dx6My4d74pN/Ki/Y9HpSG5HR/Nyh/1DHtO9OM6dvWFDwbtWslOykt6U'
      && 's5VWZbOFnQtsyMqvc56Ty3T7NeBhLGAfDZDpe5nX6V5uXpbZ43K2NGQ2V9glwLas/I62'
      && 'hfrE8EWsJ3mFsGYs+OQqze+A1cBLgbmma4f/9AmOJGBe5vKVLYN1W6wnOWSHmdkVhexM'
      && 'PG6yC0x2AbmjoQ3njdh4uwrSw1Htmq5bd3Y0I3FLpQ5n0GTSQ7s6Fva70RPYTPbi+Pz0'
      && 'J7ryboRC+m5PnRfsJjVEAfp5bLNflTb52dKIBj36RWY5ZyX2WCLukvbX67ZYHFLHZtGw'
      && '+1fD/jDL8qQljWpav9m6Uw3wKYzXgUNJTxsk+0Fssw0L6x+j4dCx6eF/BEtwDBkbx7Fe'
      && '29gWCa0yrC2rvXXO26WZfrWG3V2kji8zWbm0QUev67GX5ZgZ8A0H121hXIIZNrxou9oW'
      && '6m4b4m/z2aTP+fsAohF3PaNHROvssZ8ElRs5DnyPBAkovxDFF4oJESDeY9tJD4Ur5umg'
      && 'PSFm1Uy23Zk2SaM7e43p5Y4uxUMzu2f4H56+tuZmff2gfTqHrGEy5DkW6Abo7LH7gfsB'
      && '2uo1LQGzBmoYFSwg57vNcjqqo4F1JXh2S7Zfx83TZZNqdD6MXkQkU369jONgcmfxe83M'
      && 'B7XQEdEhg1B0HzDk2ZHpy3vBqLPpMQhyi/f2AIA3WyPZG6KkeVpKiE925awEi7H6JRsA'
      && 'cqJDfIi9oayfW8ZB5dY/TFeX7YlGQg+RmgJkcnSQfWyr9QP92enmGcgeNCvx67mXbGdb'
      && 'xD1hjI5AklJ+ydgTUGz6iiZNXd09+gYGGIRlQgXn6wDesZYSRFsJOYES5QjSw7fqnu7q'
      && 'Bqh7uqu7f3nzdw3uKFJszEIcpqVRs12SRuAYiTrJ1YXMzSGgS6iQnHmWyQWe70pySz/F'
      && 'MZagMWnMlaiTuTqTTih7s7IIHm1T1ncVI37l3BAAA4McAYF7iAvG17uxExi1U6Igd9XN'
      && 'Dj+UmZA8qPrf3MDQbeSPIN8Ldub0JzeWLcT2I3Swn8JFhr4VQnMze5uKnv0ugOHfUXa3'
      && 'ZhySedkR0eGDuMtbw/rTZCI1pA9PF0yWf4e3MnJ7YKXm0pOr6H03QRIIZeYnUj1njhid'
      && '8aaRscKX/VGWSRLsCjnK2rcdC3njGUsQ5PSdv92yqJaMk5WBoRMpJsSnNgZufBdCkmsN'
      && '60FgRbllK8PNzOlttT/qpz2sOUnpeWGHvq9ewcyc28/7XQCru213NOL+l6wgZ0kXAjnD'
      && 'cazP7gXuTdu41rCyxbgr3mt/P16+F6LgUVXtmq5bC237yNsNu5YtPBZgx4kLFznZ1XlM'
      && 'BzB/1liECBAN801yhfiq0HflbKXz1ojZ4qCylSBsbm6q/93wX0n0Q1Ir6UzWYXaZyZaF'
      && 'qqxeZn813n4ZlhPWJWXMo00P5OTDF5c0qmm8fRlPip6bFhHk6Ti3ddfy5i3OXBemJQE2'
      && 'A5g/c/qaTasC8krC0KdzE+3qWG/y6thmW7Vui/UkQ7w51vqDaGnRZFInPdlshNQ2C8oJ'
      && 'h0oqaefF++zmzh5bu7bbXrBxjp88bp5qgZzNdyfWD/9t+B+TO4GW8/p+R0SHcGBxLWEF'
      && 'jiQlHeIXEaRIPZAVRMVCTDcQCUh8LfOyaqjgCcr+YpY7NRFa2VY/egsqtNtdw8ie5gjJ'
      && 'oUTqicjofOYA2f/YgcR03s5MMBF4wlIa7rMr5mnUyru6xl0LZAeFvDG3l83DF5199muk'
      && 'oJO1FUMoviSi8Nh9Kg+Ru7qvUvCqPO+cMZsxbPsM4HXW9KcrEyKApTa7s9BVSyLaF3Ik'
      && 'SbLSQros18RyInkkV2u5q+6zLaS+aCT0oJl/QVI78IWcsvDos1vtLYCE551QKNuCKW63'
      && '+157g36cMOYI9yWhC3K+j4KDEHKxC9+t0altDaFHwL/kvVZIBJw761/uM5/MTJlU7S/Z'
      && 'N6hTBNlhZA0OPReNuGdM6nL4jR4G5ZnRusAtKmVHwg1Slcxe11nODZJKh1fJ6kwM3dQa'
      && 'VgOw3omjkGuL9/o/L/vFTzs7mi8pQZBpIT4f9PxE2bRFQncY9pdjKDoExDH7ebzPbgFo'
      && 'bQjdng48KBfvzZau77ORN61FI66PsW2N7ARiZnZTZ589BtAWCV1v5J1zF+JNVdui2CbL'
      && 'OcJsq1ejD2lVgCDL4e14r58J0N6k+cmEu0HYIssdrbxgnaGeeG9yJEg32hC6GbOix81y'
      && 'trTsWLtiixpgQNLZ4yVEgCT++xSP0H7C0N1ZadVAh6SR3kRm2WfJO0H/XqTuQcn+IlOI'
      && 'AFjRVaZhus3g2az0WuA0wcIi5QP3DDNIIPtakBABYltts7AO4OEi9eTFYGCksSRzwM4L'
      && 'ECKAM1gG9tVR5UP+RkqZN5s7a0yBnwUEOSDp7GlPPp83BH0srO+1PmQrDIIen9wOdnln'
      && 'n31G5n9ZtDLL6ck2x3uTf6DUee8rASX6vNnyWI/dmZ0R77O7LNXLBkWy9CE7Pd6XvNih'
      && 'QkEQeZHZl9PBFtsDstebtyWFwv0B4r32UrzXn+6xDtBdwIslNL0N+JnMvravxiraFO/s'
      && 'tm0y+xzQlcfkddCNCe/vGfP7GQH6lzdfbHAjqSCBHZK+PN5CzESSlixgnhMLzXAeXp+3'
      && 'hWfuM0sWL10abQv1CdtHixzvmtiYPhcvSFOTJk1NEPEQkWdPUry4oc96y2o3YJiWs5Wx'
      && 'zbYq83THHHu9Y1N2kG45tDRqdsgzxxuznKPOGbsTsN2M7d6zfXhePJ5Ici1h6mUcAcw0'
      && '8Zo5fp35NoqKxAjwTrRhZmLSpPY9ySmPzV27dm+lTn9cKSTGA+XT+03Jq+l8HBLv2Q7c'
      && 'X9K+ygQTFGDcHhaaoGJyouDNV7JH+eGj4mF6gspoC+tzJt1ObsT4MDsF2zxs886+Ml5v'
      && '/PogUvEwPUGFiE+SX4gAtQa1gkhV7onQR4oJMR5oxC6stDeghd7Dh6E+CPw/HL4vVO2f'
      && 'cpUAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.                    "lif_gui_page~get_assets

  METHOD lif_gui_page~on_event.
    ev_state = gc_event_state-not_handled.
  ENDMETHOD.                    "lif_gui_page~on_event

ENDCLASS.