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
    CLASS-DATA gi_gui_services TYPE REF TO zif_abapgit_gui_services.

    CLASS-METHODS init_asset_manager
      RETURNING
        VALUE(ro_asset_man) TYPE REF TO zcl_abapgit_gui_asset_manager
      RAISING
        zcx_abapgit_exception.

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
      li_router    TYPE REF TO zif_abapgit_gui_event_handler,
      li_asset_man TYPE REF TO zif_abapgit_gui_asset_manager.

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
      iv_url       = 'img/logo'
      iv_type      = 'image/png'
      iv_base64    =
           'iVBORw0KGgoAAAANSUhEUgAAAKMAAAAoCAYAAACSG0qbAAAABHNCSVQICAgI'
        && 'fAhkiAAAAAlwSFlzAAAEbAAABGwBr11x4QAAABl0RVh0U29mdHdhcmUAd3d3'
        && 'Lmlua3NjYXBlLm9yZ5vuPBoAAA9SSURBVHic7Zx7cNRVlsc/53YHCAJ5yFvw'
        && 'hToqJAwGXyTMooIj0gmilXHW3dKdrVnH1dUhncjOWLNlZqd8IXRAZnSF2nXK'
        && 'dWdnTLnrkA4sispKwHKFFQLsijrqOhhUFDoJEJLuvmf/6O4k3f3rRx6KSr5V'
        && 'XQX33nPu+f1+3/u755x7fhEGAJ139ojW0Jhq4GIVfbZg657fD0TfEE5tSH8F'
        && 'W0pKRo7MDa4HruluVO7L39b80GAYNoRTD/0ioyMRYxgi5BD6iT6TMS0RYxgi'
        && '5BD6AdNXgdzR4WHA6LSDhAePlM2s7q9RQzg1kTUZW0pKRrZdWXRhwZZdAXWb'
        && '7wL/lW68oCuGCDmEviArMsa2ZuuS7YHSGbMLtuwK2BE51wKvp5MTdEWgtPin'
        && 'g2LpEL7xyOgztpSUjBw5ItiAcHW06TMlPL+gad/uw/NL8kxHcD3CROAcIMdJ'
        && 'hyI1BU27Vw6i3UM4ydC7F47DLSWICRIMvyFrNrYNVGdaMjoQMSb1uYbN/ILt'
        && 'u3bFmg6VzpqcI+HngCscjT/JhFRvxSxU73DulKdl1fptX7JJX1to9aLvolJP'
        && 'T+zwCcZcKyvWNw9ErztVR0oiAiinG2MbFaYIKMC4bW+2tJXNvM2i+530RX1I'
        && 'Th4hdS6itzt2iX0d+MaTUWuWjCccmo/R+SgXAZOAcUAIaEXYD7yJyH8wqv1V'
        && 'qd0SclYkjxAfxE7A2lrgxoHY50jGtESM2QMdMSLGMLpp9zutZcVdwDAnmagP'
        && 'KfnbmlcMxOh+QfXc/qf4v97QKs8ZGL0PG/whwrD4p9aNMShTgfmo3kvbqI+1'
        && '2vMw7Qcfl7U7gwljpyWLy3kDtTMpgIkGK/50RIzi7M+/M2NqXEtJiRvYk1ZK'
        && 'eDRQWlzTV0MHjByzHBEv8H9f+twnEVpdsQCRfajcSYqXRApMRFnFmMlTkpXy'
        && 'nsNMf0hpw72e89XrOdz9S+EexpGxm4hwVRbGulzWPB+YM6N7lcjOnUHQBcDO'
        && 'tJIngZCyfH2LrGyoQ/nVlznvyYR6y29CtRE0z6HbAu8Bb4D+D3A0a8XCT4GO'
        && 'Xi2HsfKLlOPDXAIU9Po5opuM3VtzdkSM4RLELAE4MrfohsPfKSrKb9pzRN1m'
        && 'PhnykAiPBuYW39eHuQYHQvhLn/MkQKsqzgP9J9DEDMdxVO7HBqeIzz9NfP7L'
        && 'xNc4nT/m5oOWEvEf00J8/g24wxcA30f0JjpzLpBVDf+dRqQsG5vd0Oc3orOB'
        && 'ME6srD38naJrCrbs2hMoK7oO5EXgAlKd2CgPBEqLu06KD/mNh10HjEloPILo'
        && 'AvH5k3Yuqa8PA9vV63kHuCiTdlm+8QDwu0zjdFnFZEL21mwsdkfKwLoaQeZl'
        && 'I5AB44yVlw7PKb46v6l5r1ZWXi719eHAnBnTMOYZnNI+kS2bvhJSq8svxNpx'
        && 'YAoxCKIBgvKerG74cCAXEI04L0JsHtb1OeHgDlmzsTMr2drKYQSOXYzbVYil'
        && 'AGNPYDmCht6Suk2HB2TX3QvH4XZfjLGnYzkEI3dIXX2H41hv+RWg85J75K9k'
        && 'ZTIR44XZh2QmY1Y2ez0zCdlnSFwUt5e4SQ6KcLeFRtcMEhFjGGcML7eUlJwt'
        && '9fXHAfK37/3D4bLpNxtc7+N06iM8Ephb/HL+1uZ0r3p02cIphNzLQBejeiYi'
        && 'gEZiegVcino976Ks5kDuE9HVnhXUu+gqMLXY4FwEAQFjwbja1Vu+iqMtv3CI'
        && 'KmOyc0Duoe3EdRiTh42Gqzbqp0uOVe+i11Bqpa5xc5xsVfktiP6Lg9qf4/P/'
        && 'nGrPTaguBbkS1KASdf87jmmVZw15ufdLbX1XvKj9UXKMoNvE538u440QXgDO'
        && 'BwqB0+M0gOD1HANyHe7CDvE1XgrRPKSV5UCx4xyjJnWp19O7Za/4/EVuRQZl'
        && 'FSRON3nnzuO9Gwqb9n0YKCv+CJjqMN6oyreBlGTU6sUXE7LbUzjjvXEewhqm'
        && 'dlwI/E125pqloDNAnaK80aB/x2mTZmtlZXkiwdXruRX4NZCY6YqbAKQUYZN6'
        && 'y78nvoYeUoidliK4vAbvojkoC1L0n4bwE9pOfFsrKz3xdkmyu6XmqVTG9Yb4'
        && '/OuAdY6dVZ7JOBIxAVbOQVIQMQ2Mhd+S5i72E7ltVxZd2Lvh6LzpE4HJKcYf'
        && 'w5qX0mrUsM+BiF0gB0Gc3lh36tJFWS40LSLT0aiwkCkd3jip28tHgjzmIHss'
        && 'YlfSfTWgPq2sdPXolTbiI9MYykAWZGH7dUw9fnf3/6o8ZwBnJdsf3ppZVwa4'
        && 'GckXmBozhU3NDQJLGWRCWpc8E0v7HCqdNTkUcv0GcDkM7UDtkoLtb6a8SK25'
        && '9jQg4cHoNnJOjBdfw2SOtpyG6OMJYoKRhX0w+RjwO1QeRGjA6X4IS7V2Xs9B'
        && 'wRh7VdICUa1jzNF88TVMJhieALyZoOVMpnbM6Fa50r8at5kAmuqTjROAH/AB'
        && 'zzsPkXt6cnd2ksOAEL4N76TQnzXkUf87+PznAN9POzAn7Ed1AaoLED5P6ld7'
        && 'KaKze36umyEaTec1NT/WWlaMwioG8ClCAkow5t0jZUUHhfAEnCuEOlC7OH/b'
        && '3hfTajIj8gjbTQjjUSYgjAXzrDyyuRVA1u4M6tLr12HkznjBrE8Fnids7pDV'
        && '6z+JNWh1eRWqvoRxkwmMuZzY0WHYBEFfRmQc6DhgLOhTsWM0WbPxkHoXPQ0y'
        && 'y8Gu3d3/W76+Xb2eXcDiuGEq/wZ6j9T5P+puqvL8DCExp3cONRVFrFjfDK5C'
        && 'h3UUSOdD9AUCqi77n4RTF3xFI+0DABrxMeN8T+o27HSyp3uV5zU1PxYondmJ'
        && '6BMMHiERxGmlAnSKmMq8pub0RCSSsAau792mtbUGGno1uA4nX58mpjacofJi'
        && 'byIC0N7yS0ZNug8YG9fuCl9ClIxS1/AC8EKyXRt6W+8URacvTu4W1WfF10PE'
        && 'CHJXQoeXxORxODwLaMZlc7oDp15mpZpCa5aMx3b9ILMtpkVWNvxzVnb3E3Fn'
        && '0/nbdj/ZWlY8vA9vyP1hZCOADctrxnAASD4+SkYHahdnQ8RUkNpaG9fgMkpS'
        && 'Plv7XMnerX/tzqB6PS8At8Sr5Mw+2SWiaAIX1NFdyc6uuvoOrSrfjuii+A4T'
        && 'ue+q7Q6PLvWitKHLQB7OOLHldeDLIyP0acu2Jqw35L/W/BZA4fbmvYG5xX+B'
        && 'sjmNDGS7NaeA1iwZj3aVoeZc0EmghUABaid+AYUQB5KbpNDRLu/1F4BcBpwD'
        && 'MjYyTgtRHXABQbIJ9uOkRyNR31XcAdQmSgzXZQunRLfPRFzo0HZS4Fi1kyUh'
        && '/zjmtT1vxclN/NaW1oP7Q6n0MgAiqnfxVAjXYYM3gLh6dp5YLm8ALpHYVMIO'
        && 'UW78a06XVpRiwqtAZieM6789GSHHks2KTtgZ3E+OqxMYHtcfds/GaXGp3YXw'
        && 'k6heAzyYMKILozcSNo6lgYOJlPWMeU3Nj7XOnamquhpnQk48PL8kr3DzztZY'
        && 'Q/vBt6el0dl/ItYsGY8NbsM5RwmRQ/9W0hzC9w8yPolUYgI9dnnmYe0mkFTV'
        && 'MJ1AF9n6iNljrENbAEDWbOxUr6cZuDS+W2/GIRqPJuE3A2hlpYupHYlkPC4r'
        && 'GhsHweaMSOtT5W3dvUZEfozzMh9uOoL/+Om86aMA2uaVjLXYJ1Oo6gCt6O/W'
        && 'jAZrSCKibgO9mrCZKD6/C6WoX7rTT5xct2ftwZ5/s5zksqx1WDuLYDhPfP4R'
        && 'qN7N4ONch7ZeZV2afNKi3Jh93vXkIKODn5aQwk3DQq4DgbLiHTYU/CDFsWKE'
        && 'iE17MvmSqaHqUMAhd4mv8ZWkKHiQoHctOd3xJMO4mgF0WcVoIGFr5hA+/49k'
        && '1YZdg/FNiKNdyyomA5cldbh6nV6ZYU8ReSP3xjAMT+vfzs90gvXF487K05ya'
        && 's4o287buXgN6B85vyDygBHCaYOBEBMAkb7+SsDW65FtZqUqUizTeGim5ikBr'
        && 'aw3Dgw87lF+dwH08WhoXzCPZfXFRWdl9TyOJaHNBVnY5QbktSr4eu8L6IInP'
        && 'TXmHR/3vdl/Nin//FBWHwhOZTdeIV3Wp50rH+ab3P8pPDYfTsdxjlziNTOkz'
        && 'JiK/ac/aQFkRIP9AdmmfQSIigB4isdRd9Qn1lj8E1oCpwOotyXKS7Ksp+Q4T'
        && 'XIHYPer1bAb9iNYdVzqerYqujyXaGdnxGW2jlPh7UcjUE79Wr+dfgTNAb4vW'
        && 'CMbD6KjU19p7PhYSsm9HU0wttO0oA2Ymj5O1SUnkUOjvyXEtBqYn6CxG2K5e'
        && 'z/+CbgfTAuoGyaetI0tXRx1ePJKrIEl2iH6KJj478xv1LnoSIQDmLJRLGdN+'
        && 'TZ+TIYGyotuzIOQgEhG0yvNQT8TXZxxC9Rmpa/QCqNezDvhhP/ScADtTfBve'
        && '7rar2rMdxfktkx4KfIDqfVLX+NuoXbXA/X3WJOynI3e2PF6fVKmtPy4/E6Ob'
        && 'Ec7vh40xBMTnL4jYWDEL7FNEqnpGOoz9DGWv1Pm73Rv1eh4BlmWeRq/uc1I4'
        && 'v2nP2jRbNgwyEQHICf8KaE87RqUR1TqHnnGIXNwzLtH5l4PALzNY0AXy572J'
        && 'CICVRzLIKegDKFsS2oVIPjJVdiAm/kH6fmlFzPeciAggqxs+RN2Xga7AuRgj'
        && 'G/T6tsUWEnkzOxERYCzC5XEt7vAaIItaTvH064QiDSEHn4jEzjr1OiKfMvSe'
        && 'U1F5HZU/o66hHMMmIlFl/E/k4x5l4o7v11fF578b5UbgjYSpQygbsfbyuLKv'
        && 'mKq6ht+j/CXJlSzR4gaZI77GnwEvOdtFgHRQ/WtEbiVSWtf7ujuB53DppZm+'
        && 'VZZVzwfE13gvbnNe9IM0P+kX9hGQV0AfwNgrxOfvFaRph+N1xN/P9+PmX77x'
        && 'AEb/JHoPErPxJyLt8gPcpnZAZxatZUV3KbKaSDVOO8KS/K3N6UvBBgitLh8L'
        && '9iyETtx8JA83Hhl0/apnoNYyrOvDbh8xk1xkS5yIkTZGt38gtVtO9Glep21a'
        && '9DpZ2bgJotF9TudUxBXkGO/L2objDmqyn29ZxWjUTiRk8zCihFxHGU6LLF+f'
        && 'fgca6JxdoWkIboQ2jn7yfu+C5QEfoH1eOnO62zArGOKVsa/t/iizxBCckImM'
        && 'pwKyjqZT4fRtu/cB+wbBliGc4uh3VcsQBg9aVZmbHFgBykVas2R8pCztm49T'
        && '9A9+fHWg1eV/iuozpH8xWOAQrnCpPLox5V9u+LrjlFhxX2kow8n8HAwwgaDp'
        && 'y58n+dphwD7jEAYIox9gpT6rscIXct79VcH/A/nqFrWQqumlAAAAAElFTkSu'
        && 'QmCC' ).

  ENDMETHOD.
ENDCLASS.
