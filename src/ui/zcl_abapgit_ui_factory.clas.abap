CLASS zcl_abapgit_ui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_asset_manager
      RETURNING
        VALUE(ri_asset_man) TYPE REF TO zif_abapgit_gui_asset_manager
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_popups
      RETURNING
        VALUE(ri_popups) TYPE REF TO zif_abapgit_popups .
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
    CLASS-METHODS get_html_viewer
      IMPORTING
        !io_container           TYPE REF TO cl_gui_container DEFAULT cl_gui_container=>screen0
        !iv_disable_query_table TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ri_viewer)        TYPE REF TO zif_abapgit_html_viewer .
    CLASS-METHODS get_gui_jumper
      RETURNING
        VALUE(ri_gui_jumper) TYPE REF TO zif_abapgit_gui_jumper .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_popups TYPE REF TO zif_abapgit_popups .
    CLASS-DATA gi_html_viewer TYPE REF TO zif_abapgit_html_viewer .
    CLASS-DATA go_gui TYPE REF TO zcl_abapgit_gui .
    CLASS-DATA gi_fe_services TYPE REF TO zif_abapgit_frontend_services .
    CLASS-DATA gi_gui_services TYPE REF TO zif_abapgit_gui_services .
    CLASS-DATA gi_gui_jumper TYPE REF TO zif_abapgit_gui_jumper .
ENDCLASS.



CLASS zcl_abapgit_ui_factory IMPLEMENTATION.


  METHOD get_asset_manager.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.
    DATA li_asset_man TYPE REF TO zif_abapgit_gui_asset_manager.

    CREATE OBJECT lo_buf.

    li_asset_man = zcl_abapgit_gui_asset_manager=>create( ).

    " @@abapmerge include zabapgit_css_common.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_default.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-default.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DEFAULT'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_dark.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-dark.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DARK'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_belize_blue.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-belize-blue.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_BELIZE_BLUE'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_js_common.w3mi.data.js > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'js/common.js'
      iv_type      = 'text/javascript'
      iv_mime_name = 'ZABAPGIT_JS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_icon_font_css.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/ag-icons.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_ICON_FONT_CSS'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include-base64 zabapgit_icon_font.w3mi.data.woff > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'font/ag-icons.woff'
      iv_type      = 'font/woff'
      iv_mime_name = 'ZABAPGIT_ICON_FONT'
      iv_base64    = lo_buf->join_and_flush( ) ).

    ri_asset_man = li_asset_man.

  ENDMETHOD.


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
      li_asset_man = get_asset_manager( ).

      CREATE OBJECT lo_html_preprocessor EXPORTING ii_asset_man = li_asset_man.
      lo_html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      lo_html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT li_router TYPE zcl_abapgit_gui_router.
      CREATE OBJECT li_hotkey_ctl TYPE zcl_abapgit_gui_hotkey_ctl.

      CREATE OBJECT go_gui
        EXPORTING
          io_component      = li_router
          ii_hotkey_ctl     = li_hotkey_ctl
          ii_html_processor = lo_html_preprocessor
          ii_asset_man      = li_asset_man.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.


  METHOD get_gui_jumper.

    IF gi_gui_jumper IS INITIAL.
      CREATE OBJECT gi_gui_jumper TYPE zcl_abapgit_gui_jumper.
    ENDIF.

    ri_gui_jumper = gi_gui_jumper.

  ENDMETHOD.


  METHOD get_gui_services.
    IF gi_gui_services IS NOT BOUND.
      gi_gui_services ?= get_gui( ).
    ENDIF.
    ri_gui_services = gi_gui_services.
  ENDMETHOD.


  METHOD get_html_viewer.

    IF gi_html_viewer IS NOT BOUND.
      CREATE OBJECT gi_html_viewer TYPE zcl_abapgit_html_viewer_gui
        EXPORTING
          io_container           = io_container
          iv_disable_query_table = iv_disable_query_table.
    ENDIF.

    ri_viewer = gi_html_viewer.

  ENDMETHOD.


  METHOD get_popups.

    IF gi_popups IS INITIAL.
      CREATE OBJECT gi_popups TYPE zcl_abapgit_popups.
    ENDIF.

    ri_popups = gi_popups.

  ENDMETHOD.
ENDCLASS.
