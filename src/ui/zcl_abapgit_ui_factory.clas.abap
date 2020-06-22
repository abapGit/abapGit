CLASS zcl_abapgit_ui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_popups
      RETURNING
        VALUE(ri_popups) TYPE REF TO zif_abapgit_popups .
    CLASS-METHODS get_tag_popups
      RETURNING
        VALUE(ri_tag_popups) TYPE REF TO zif_abapgit_tag_popups .
    CLASS-METHODS get_gui_functions
      RETURNING
        VALUE(ri_gui_functions) TYPE REF TO zif_abapgit_gui_functions .
    CLASS-METHODS get_gui
      RETURNING
        VALUE(ro_gui) TYPE REF TO zcl_abapgit_gui
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO zif_abapgit_gui_services
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_frontend_services
      RETURNING
        VALUE(ri_fe_serv) TYPE REF TO zif_abapgit_frontend_services .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_popups TYPE REF TO zif_abapgit_popups .
    CLASS-DATA gi_tag_popups TYPE REF TO zif_abapgit_tag_popups .
    CLASS-DATA gi_gui_functions TYPE REF TO zif_abapgit_gui_functions .
    CLASS-DATA go_gui TYPE REF TO zcl_abapgit_gui .
    CLASS-DATA gi_fe_services TYPE REF TO zif_abapgit_frontend_services .
    CLASS-DATA gi_gui_services TYPE REF TO zif_abapgit_gui_services .

    CLASS-METHODS init_asset_manager
      RETURNING
        VALUE(ro_asset_man) TYPE REF TO zcl_abapgit_gui_asset_manager
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_UI_FACTORY IMPLEMENTATION.


  METHOD get_frontend_services.

    IF gi_fe_services IS INITIAL.
      CREATE OBJECT gi_fe_services TYPE zcl_abapgit_frontend_services.
    ENDIF.

    ri_fe_serv = gi_fe_services.

  ENDMETHOD.


  METHOD get_gui.

    DATA:
      li_hotkey_ctl TYPE REF TO zif_abapgit_gui_hotkey_ctl,
      li_router     TYPE REF TO zif_abapgit_gui_event_handler,
      li_asset_man  TYPE REF TO zif_abapgit_gui_asset_manager.

    DATA lo_html_preprocessor TYPE REF TO zcl_abapgit_gui_html_processor.

    IF go_gui IS INITIAL.
      li_asset_man ?= init_asset_manager( ).

      CREATE OBJECT lo_html_preprocessor EXPORTING ii_asset_man = li_asset_man.
      lo_html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      lo_html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT li_router TYPE zcl_abapgit_gui_router.
      CREATE OBJECT li_hotkey_ctl TYPE zcl_abapgit_hotkeys.

      CREATE OBJECT go_gui
        EXPORTING
          io_component      = li_router
          ii_hotkey_ctl     = li_hotkey_ctl
          ii_html_processor = lo_html_preprocessor
          ii_asset_man      = li_asset_man.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.


  METHOD get_gui_functions.

    IF gi_gui_functions IS INITIAL.
      CREATE OBJECT gi_gui_functions TYPE zcl_abapgit_gui_functions.
    ENDIF.

    ri_gui_functions = gi_gui_functions.

  ENDMETHOD.


  METHOD get_gui_services.
    IF gi_gui_services IS NOT BOUND.
      gi_gui_services ?= get_gui( ).
    ENDIF.
    ri_gui_services = gi_gui_services.
  ENDMETHOD.


  METHOD get_popups.

    IF gi_popups IS INITIAL.
      CREATE OBJECT gi_popups TYPE zcl_abapgit_popups.
    ENDIF.

    ri_popups = gi_popups.

  ENDMETHOD.


  METHOD get_tag_popups.

    IF gi_tag_popups IS INITIAL.
      CREATE OBJECT gi_tag_popups TYPE zcl_abapgit_tag_popups.
    ENDIF.

    ri_tag_popups = gi_tag_popups.

  ENDMETHOD.


  METHOD init_asset_manager.

    DATA lo_buf TYPE REF TO lcl_string_buffer.

    CREATE OBJECT lo_buf.
    CREATE OBJECT ro_asset_man.

    " @@abapmerge include zabapgit_css_common.w3mi.data.css > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_default.w3mi.data.css > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'css/theme-default.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DEFAULT'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_dark.w3mi.data.css > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'css/theme-dark.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DARK'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_belize_blue.w3mi.data.css > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'css/theme-belize-blue.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_BELIZE_BLUE'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_js_common.w3mi.data.js > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'js/common.js'
      iv_type      = 'text/javascript'
      iv_mime_name = 'ZABAPGIT_JS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_icon_font_css.w3mi.data.css > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'css/ag-icons.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_ICON_FONT_CSS'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include-base64 zabapgit_icon_font.w3mi.data.woff > lo_buf->add( '$$' ).
    ro_asset_man->register_asset(
      iv_url       = 'font/ag-icons.woff'
      iv_type      = 'font/woff'
      iv_mime_name = 'ZABAPGIT_ICON_FONT'
      iv_base64    = lo_buf->join_and_flush( ) ).

    " see https://github.com/larshp/abapGit/issues/201 for source SVG
    ro_asset_man->register_asset(
      iv_url       = 'img/logo.png'
      iv_type      = 'image/png'
      iv_base64    =
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
        && 'cpUAAAAASUVORK5CYII=' ).

   " logo for dark theme
    ro_asset_man->register_asset(
      iv_url       = 'img/logo_dark.png'
      iv_type      = 'image/png'
      iv_base64    =
           'iVBORw0KGgoAAAANSUhEUgAAAKMAAAAoCAYAAACSG0qbAAAAB3RJTUUH5AUMDiEumeD'
        && '22gAAAAlwSFlzAAALEgAACxIB0t1+/AAAAARnQU1BAACxjwv8YQUAABBeSURBVHja7V'
        && 'wLcFzVef7PuXdXWsmW1qvHSjY2T1u7slaS7eAYv4ixE8iEABOgLZlm8qB5lGZoOkNm2'
        && 'lAmbWlpIWnSTiHTtGCapEyAJhMabCBgkwBOMWZsS1rJu2tZ2PgBWj139dj3Paf/2dXK'
        && 'ex/7XlsY9M2s7t5zz/nvf875zn/+/5yzIlAGeHu7OWiTbudANhNCfmZ9o/dQOfIW8dE'
        && 'GKbXguQ0bamqrY88DITek0zjnf7Hs9+5/WehKLeLSRElkNCJiGouEXESpKJqMuYiYxi'
        && 'IhF1EKaLEFqiRJ5gC1ufKg//jDyS2uby105RZxaaFgMgqLGNzauabx0KEpbjHfiNbvr'
        && 'Vz5Fwm5iGJREBnTUzPj/MD4dpfLtu9wcJGQi6g08vqMWh8RCTjKJNjZ8LrbPbFrQz0J'
        && 'x/ailNXAwYbkk41kLPqQHz5gn9Kjx0Y2mSQacjmaeiohMycZswUrmYRMp01tX+tkivQ'
        && '0SuzMovyCEvLIgH+zSWLfNNQNpJ92OppfWijdLjX0+vx3ymz6KQpxk7jnpCqkSEtvcK'
        && '1ueqscuXK2B7miZrSATZTxfRjItBDRl4i61wc86FN+DW8OGsmbm7JhoQgpU+UemQfvM'
        && 'noWg/owXj70ZBTWrO/4yO2UK18mkHASYEuBKzXYOzEckOOcSO8BSL44p4+td9qPZpMj'
        && '89ATaSIKEB6tIYr8FH69phz9DMlYyPINUpCmiZjGTNjkrrXEsxZZSEISwtrU2n50MDQ'
        && '0WReKh//D4xu63czjRn1uwU89ts9VeN2KGe7u95wMKmB5uMvZ8o/azEjEpfq0xGXl6q'
        && 'kLYAoiogAhDZObu7ozk1qbUH/Oj+YutjBBDQfTowmo9QKRPlKUREv42UhsfAxnhT8kx'
        && 'kQ0hASRejNMPuQeHP249hnjVGdx0DbNZJPV4xvZMOA9OZn6nJrIlk9FxoKJCElnkxDK'
        && 'ngxsap83zfSlE1FQlF0fREJ2Ouw/cTlXOeO89oWL+d6FhNvn/4asTP5v5pSqAw5O7Mi'
        && 'i5Ca45RltmgLVP8iWn3J2B+URa+oTXpY1X/pLMUTMqEk3SPId4tvkls7PiWUf68FjEx'
        && '9UQs69N3qx37kQcPtG18h86lH0C3VBKiM1/gSx/msUGrY5HWuow9GGTqPtOkz7aSGyu'
        && '9pXfCFGbH+WIHW/i0Pdy1Gw3d7paHkoW36cwncWIjdptksjohqEQzNV4N+RkDutGGWj'
        && 'xdwFkrQPh11yKicGkftCBzUfZhAWegVtmKRNT5D6F1yO5Z/Rpne120XgeXDAc+KunJY'
        && '0nd9h/xFefpQv39t9w1dSHvxYITpTcQysXCLON4CIshXYn7aQ9bJto7W1zYRDcX02Sy'
        && 'kIiVH4vcW+CyND+ciA/2q3b+R6t3e0++jR4Zpy9U/D4xlb0nt89FoRfRZbVujRMzja3'
        && 'n98dEevZ2S10LNSeiVlo/+VT6+eY/5PShBapU3PRkRVu4I8WSl9ha41plCPkXU2ghy0'
        && '0jsrQcQ0UoTk+8c2bryGvPbaVFqvya2ub6NG+4zKMA6PTG3t/nndgZ7RXLIPTk2ZLWe'
        && 'nn5BI/Cav19dooRiLsLmKVAMc8/g4g+qTjFq+09nW/AwUAUEciUT/h0LMSWDcbFYYeH'
        && '2TgE73NPpI38Wo8ofZyg4MjjmYEn1MgugmwidrIJFKF2bJ6x1HvQYVhVgOKsx8T3d7c'
        && '19m2T7f8N/IfPavtDLjrPpZMR0e8fjXmSD2E9TrGokELZwJvQIc9ZrBaPfvcHr8vq5T'
        && 'afwR7coBB4klwvSL+doByehFH7KZc2ZcV89QiBDQWVyFy36X8/LkAOjz+r9OeeghiU3'
        && 'YwGAJ45h3KAYZDxiXAh3OK+wUp9GPQxnggkoaCELabBMq38x6wP0qmrOgkQysXJVC2P'
        && 'Zc7+nvH21ecm7ijJlM/bEE4UajSooRiBbhKhMbf9rtff/RQusgk9itZhLwSXymi/CYe'
        && 'b4juIJTR2QpRpU/cHvPvWpUttfj/xQkAn3on90g1tsM64fTpcynt1TRQI+w5KpnnG0U'
        && '79R+ZJrYhe/cbyHBIzLMuJCMlgy9iNDLxCe/5/acfU33Ph51atMUsuSldetaxvK1hct'
        && '52fXCh1SURntCbtwUGI70pp8JP1ToYaQvBWU+MCGg7MBo3JCIKf1iJlV5UOpFOiVM+l'
        && 'mhnWbc0NTQBE9PW9Zl3mOAswVZV2+UF1WeCIVML+Z8kRz5HZKwuVC9ZJi+R0zfheTFC'
        && 'K9BdHBOeXxqhxjxBu/ZW4iPlaookojN/FqVxJlhdImWpUUQXAyIPPXc3usZ/mr6Xlh4'
        && 'JEyVNh8DaW+hbSfQ0dE0InZUtm1bGTkvg68uNvIuBtT6Zs/bYquu0oI5gf8MbnF9Gol'
        && 'GJza7thHg38+SM4yMvnPF4cOhXPJwtKtW98W0E+XWv4zyhq4YWP+ZkapZtVhBrsTXoQ'
        && 'jgFKXg1DcGRDaco9Dq/lPmvbBySESVT6hA9UQMbF+N0oaPYaT5K60sCtG6I/3+jvR9p'
        && '/Oy62Kk4U5GqrOu04m6MmKZZGBKGD2XSWR+YZpKxDBYwBmwrK06ga625r0JuWEV+p6/'
        && 'ydOO/4WR+RPiw4HqzGP6WfqjkNqkC5RsSBHNiqhWBBPlKnwepAMJ+UJwq2uKAqnLonY'
        && 'YG+nmZW/0vZpLkhjtAFMkuWA9Z8Gwc7zdztaH57Lc1+t5P2KG6P3qkqytEE0ZVM0kaO'
        && '2fd7XZdye1wgCh33v6hAyhKzPziXWyPo//tk6n/bmU+myXIDBOM1J6SkIy7u5y2h+fK'
        && '/K5fu+ZHglmujLlSBK/Di/9853saP5Fn+fsvRQi21R6kerpBKn5VqZeA76zHnQn1qj1'
        && 'Cjf0eEa2djubD6BOK3U9gdasa01TL1QAHdc0nsHBdJ8swY3Z8szt8ye3V9Ff/hLWROV'
        && 'juhytf2JUbn5UXxhCJpsiOxEBPpuPiAJdzuZBvCSnwsNDY1Yz422RsDKSmYdS6RVklY'
        && 'qMSJKCtqiw0/ekOzxZjhCGPuomkKN+3TRJ+Eb8+1yqUVsewIv4AEbfK7DF15gV8nZmd'
        && 'oVRt0SgSy2EGx4m0SLBqx/vcqj18pyc+BREQ6dAE2Cgv7UJL0hGrtuqQ68exzwxtKpH'
        && 'PCOtMmF359MFBbyX2UYXAqopplhC4kg9jvPHHvFd4XCAcv4+lm0toGSKiAfc+4tVeMP'
        && 'VjQG86KccToyCI3OpDSN8Jrfn3bNoHdWE5orDKD9annN4OadNJxINgW7S5yUvQzmvtL'
        && '3b7zk1iv5zk0ZmcurnQAI6HXC4Heoftm/saPFrnwkiYiD0YL73MqiexsvFI6NAMYTEi'
        && 't/X8GZfcroR18BW19/j18fylCqZiGmIwISDsosQtDCcN2AQYCM8VHBwUzioIJeKjNix'
        && 'lxvWinO555j/ekr5Dko4+re8AT9WqvF1KwJCA9j4ajJy5aq5byNGRapMVPiSuiAG67M'
        && 'OPiAwXJAtiJCcB5ctd7wAcH7ZjFWbnyKR+L+RrCfIuTiqdUspROw7NWGDcPhxtFS3ED'
        && '6W8kEu9JEHQmf07+Cq3/+IdUAzie/2+ga7q+nclH7Bj2LQWW0KmdMrkuAvmySDEpytB'
        && 'wMy4vR7ToHaofS9xGev1ubBQGwvY/SNC12rrLsDeQlJSH3A7xO+z/mdlXB8OyG5iWg9'
        && '4N4HRUJYnWO+d09QyL7JfkGAFteg4vNEEDsMZhY4RLhy4dY7DNtD0S+REZrcYBBT8YB'
        && 'nKEIhVp35GKN4sbKgm44721rF7te9Kblc9nq96hM5ROadjhU3X4x65WxEQcicyz4M/i'
        && 'GweW1yJE1s7ejEafMB48aDCP65tRQiCri97z2rPe3BiSmRIEsPiOWdGG24OUZtf1Dxx'
        && 'iGsSZvGgY6nv5t4eC8GSao2TEXA9b+OE9s3ImzZrjipf/YC6KUblJyTk+d1qDqiK8ND'
        && 'K8RKQKV1qWi98mXIRUicjj/NqTSIFvQ4BdpLgFyrbySIoF9yi/X3/a+UrmR8kzYtQWp'
        && '3uxyXbetub31YrH9RTgcr2TBHjw43ojVZYaDNm6l6cYrPdX5qgi7Z6XIsv7XTYf/xur'
        && 'Ut+3EGCeR/W+HAaPpyCWJWbToDac/8DTF/x6isRMK7RfRcSX1KgdjJMUovaHrJQ0icy'
        && 'clqo2eVIGLqHYkGvXB6XH3LVxYoTueaEB67XizNqDJVx/cY7cpIsunn4to3OLZB95xI'
        && 'wGaVk2rZvLHUeuMg/IxYyspMU6Kh53T7xkRmkRWWeTK62ppfU6BmSCcPZ5cqmDrZ5x2'
        && '5qVSdigUGubotJHQzvmSUt+ATJcUv+yARObnV+n/lETEJQuNYK9UyDU4797t9/lnG0B'
        && 'iRxOclZfITumLAda680fabBOFWicVPuz2nT2DHnkQSdFM+Y9fmU8Ay2rG60ZssQ8kwa'
        && 'JuZK2CyJF7p8fgfxIi6HUn+BZkHdVYAGazbrjOCDLNrpHhsVOjFQTotkfgGyqd1AzMB'
        && 'ljc31dXFMtPC8apPLjHHBoGrj5Fh3aoon3hxwPPOFAPTu4zTXkppFJVvGvCdWSvlU0r'
        && 'IMJEq7XJV1liByNPA4yqdJT5zH7pe69Hl2Yd+eTsam+s6HCvXFv3vTcSB2HyEPE/E3p'
        && 'eLlW8E7Aw3dkxHseXEzoPCTVPox/0Wp87bUrLO9ImDB6XokaANX0Gr82T6/ph3KCo2+'
        && 'ouVI7bIGFSNc2r5nqvN/ohI6/Ocfd0E09uKlyWxiFLftb7D3q99Jn5yYGLBX2kJWVwj'
        && 'ytzpWJ0k2tGB4Z0yjT6JhLbjR1dvBaqmOMjvu5yr5tdisb0PY3uvz/casYVadBSYL6h'
        && 'JEpHCbZUiYlImqf52vp94I+ECCqlV+Y1iOhN7wUiYzfNtC0qLKg8SA8udyiVbkDrG6/'
        && '47k4gCOBU+nU93hSzpFT/lVMlLni4KN6Il3ZyrrNG+rlavBKl7wIiIAp1rmp+PQf0Ny'
        && 'f320jFvAyUKa2UIrTQiYvI5tjU+V23BKrzqy4X87ohA/O6SliSyEXKeiG/0/aYUudkg'
        && '9jpjYP0ajlL9ERYcuWIdrL3tiiYG5l/mk0WJovLBODHPdDhWXYmdtltE6Lo6EXMsTpZ'
        && '9RZwt1D5zOVq/iJHzM0YnWebK3Y/TT7fC5XdKqXec1v/t3GELXWeiZZ2NkWV/lOu4v0'
        && 'CXo/n1DucVTQlq+9ME1Ppwao5DPqAPykjN2Tix/lhJWJeXonsa4vwmtu03sW/CRs/F3'
        && 'n4ClrxFqPnBkv8/o4A4oS0OxorziEjESeyTuypNRJXi4gDD8dFtaPFuTPko1DfbWv2U'
        && '1l8qB+JEjWzid6BRDaL8PXP74jkhTobHCLsJLd4WHOJBVO0QWtGifvhlNE2LZSuxWpC'
        && '5u4PtPI026EXtId1ikNrBYi70qa/AVrVjHwbQbzyLLDyN13fWzvnFlYSoA0bRc7tmbD'
        && 'nGAV6MOg9m/jeKssgoELzW0aCYzZtnlJp9qw4eDJcr76OKXGRcaN0uFsr+fUb9216xC'
        && 'Pz8QldkEZc+Luo21iKyA92OOoM0eymyLlWUPU0vojz0+oa/K7OZv9aeGE9DRNQcpDgG'
        && 'FSEGtTsq9R+/Poio2M8oF1EaCGOXZyNi8jkwNJDMDDxujnNLUzGyLzUsknGBwYGMi2W'
        && 'gQvIyBrOF5LtU8f+/s5bKAnfqhQAAAABJRU5ErkJggg==' ).

  ENDMETHOD.
ENDCLASS.
