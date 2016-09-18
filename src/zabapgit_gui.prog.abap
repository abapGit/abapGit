*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GUI
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_app.

  PUBLIC SECTION.

    METHODS go_home
      RAISING lcx_exception.

    METHODS back
      IMPORTING iv_to_bookmark TYPE abap_bool DEFAULT abap_false
      RETURNING value(rv_exit) TYPE xfeld
      RAISING   lcx_exception.

    METHODS on_event FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_page_stack,
             page     TYPE REF TO lif_gui_page,
             bookmark TYPE abap_bool,
           END OF ty_page_stack.

    DATA: mi_cur_page    TYPE REF TO lif_gui_page,
          mt_stack       TYPE TABLE OF ty_page_stack,
          mt_assets      TYPE tt_w3urls,
          mo_router      TYPE REF TO lcl_gui_router,
          mo_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS constructor
      RAISING lcx_exception.

    METHODS startup
      RAISING lcx_exception.

    METHODS cache_image
      IMPORTING iv_url    TYPE w3url
                iv_base64 TYPE string.

    METHODS cache_html
      IMPORTING iv_html       TYPE string
      RETURNING value(rv_url) TYPE w3url.

    METHODS render
      RAISING lcx_exception.

    METHODS get_current_page_name
      RETURNING value(rv_page_name) TYPE string.

    METHODS call_page
      IMPORTING ii_page          TYPE REF TO lif_gui_page
                iv_with_bookmark TYPE abap_bool DEFAULT abap_false
                iv_replacing     TYPE abap_bool DEFAULT abap_false
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD constructor.

    startup( ).

  ENDMETHOD.            "constructor

  METHOD on_event.

    DATA: lx_exception TYPE REF TO lcx_exception,
          li_page      TYPE REF TO lif_gui_page,
          lv_state     TYPE i.

    TRY.
        IF mi_cur_page IS BOUND.
          mi_cur_page->on_event(
            EXPORTING
              iv_action    = action
              iv_prev_page = get_current_page_name( )
              iv_getdata   = getdata
              it_postdata  = postdata
            IMPORTING
              ei_page      = li_page
              ev_state     = lv_state ).
        ENDIF.

        IF lv_state IS INITIAL.
          mo_router->on_event(
            EXPORTING
              iv_action    = action
              iv_prev_page = get_current_page_name( )
              iv_getdata   = getdata
              it_postdata  = postdata
            IMPORTING
              ei_page      = li_page
              ev_state     = lv_state ).
        ENDIF.

        CASE lv_state.
          WHEN gc_event_state-re_render.
            render( ).
          WHEN gc_event_state-new_page.
            call_page( li_page ).
          WHEN gc_event_state-new_page_w_bookmark.
            call_page( ii_page = li_page iv_with_bookmark = abap_true ).
          WHEN gc_event_state-new_page_replacing.
            call_page( ii_page = li_page iv_replacing = abap_true ).
          WHEN gc_event_state-go_back.
            back( ).
          WHEN gc_event_state-go_back_to_bookmark.
            back( iv_to_bookmark = abap_true ).
          WHEN gc_event_state-no_more_act.
            " Do nothing, handling completed
          WHEN OTHERS.
            lcx_exception=>raise( 'Unknown action' ).
        ENDCASE.

      CATCH lcx_exception INTO lx_exception.
        ROLLBACK WORK.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
      CATCH lcx_cancel ##NO_HANDLER.
        " Do nothing = gc_event_state-no_more_act
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD back.

    DATA: lv_index TYPE i,
          ls_stack LIKE LINE OF mt_stack.

    lv_index = lines( mt_stack ).

    IF lv_index = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    DO lv_index TIMES.
      READ TABLE mt_stack INDEX lv_index INTO ls_stack.
      ASSERT sy-subrc = 0.

      DELETE mt_stack INDEX lv_index.
      ASSERT sy-subrc = 0.

      lv_index = lv_index - 1.

      IF iv_to_bookmark = abap_false OR ls_stack-bookmark = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    mi_cur_page = ls_stack-page. " last page always stays
    render( ).

  ENDMETHOD.                "back

  METHOD call_page.

    DATA: lt_assets TYPE tt_web_assets,
          ls_stack  TYPE ty_page_stack.
    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.

    IF iv_replacing = abap_false AND NOT mi_cur_page IS INITIAL.
      ls_stack-page     = mi_cur_page.
      ls_stack-bookmark = iv_with_bookmark.
      APPEND ls_stack TO mt_stack.
    ENDIF.

    lt_assets = ii_page->get_assets( ).
    IF lines( lt_assets ) > 0.
      LOOP AT lt_assets ASSIGNING <ls_asset>.
        READ TABLE mt_assets TRANSPORTING NO FIELDS WITH KEY table_line = <ls_asset>-url.
        CHECK sy-subrc IS NOT INITIAL.
        APPEND <ls_asset>-url TO mt_assets.
        cache_image( iv_url = <ls_asset>-url iv_base64 = <ls_asset>-content ).
      ENDLOOP.
    ENDIF.

    mi_cur_page = ii_page.
    render( ).

  ENDMETHOD.                "call_page

  METHOD go_home.

    on_event( action = |{ gc_action-go_main }| ). " doesn't accept strings directly

  ENDMETHOD.                "go_home

  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_router.
    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = mo_html_viewer->m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.
    mo_html_viewer->set_registered_events( lt_events ).

    SET HANDLER me->on_event FOR mo_html_viewer.

  ENDMETHOD.                    "startup

  METHOD render.

    DATA lv_url TYPE w3url.

    lv_url = cache_html( mi_cur_page->render( )->mv_html ).

    mo_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "render

  METHOD cache_html.

    DATA: lt_data TYPE TABLE OF text200.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_html
      TABLES
        ftext_tab = lt_data.

    mo_html_viewer->load_data(
      IMPORTING
        assigned_url = rv_url
      CHANGING
        data_table   = lt_data ).

  ENDMETHOD.                    "cache_html

  METHOD cache_image.

    DATA lv_xtmp  TYPE xstring.
    DATA lv_size  TYPE int4.
    DATA lt_xdata TYPE TABLE OF w3_mime. " RAW255

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = lv_xtmp
      EXCEPTIONS
        OTHERS  = 1.

    ASSERT sy-subrc = 0. " Image data error

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xtmp
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_xdata.

    mo_html_viewer->load_data(
      EXPORTING  type         = 'image'
                 subtype      = 'png'
                 size         = lv_size
                 url          = iv_url
      CHANGING   data_table   = lt_xdata
      EXCEPTIONS OTHERS       = 1 ) ##NO_TEXT.

    ASSERT sy-subrc = 0. " Image data error

  ENDMETHOD.                  "cache_image

  METHOD get_current_page_name.
    IF mi_cur_page IS BOUND.
      rv_page_name =
        cl_abap_classdescr=>describe_by_object_ref( mi_cur_page
          )->get_relative_name( ).
      SHIFT rv_page_name LEFT DELETING LEADING 'LCL_GUI_'.
    ENDIF." ELSE - return is empty => initial page

  ENDMETHOD.  "get_current_page_name

ENDCLASS.                     "lcl_gui IMPLEMENTATION