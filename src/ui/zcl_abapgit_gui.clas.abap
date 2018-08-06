CLASS zcl_abapgit_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS: get_instance
      RETURNING VALUE(ro_gui) TYPE REF TO zcl_abapgit_gui
      RAISING   zcx_abapgit_exception.

    METHODS go_home
      RAISING zcx_abapgit_exception.

    METHODS back
      IMPORTING iv_to_bookmark TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_exit) TYPE xfeld
      RAISING   zcx_abapgit_exception.

    METHODS on_event FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

    METHODS focus.

  PRIVATE SECTION.

    CLASS-DATA: go_gui TYPE REF TO zcl_abapgit_gui.

    TYPES: BEGIN OF ty_page_stack,
             page     TYPE REF TO zif_abapgit_gui_page,
             bookmark TYPE abap_bool,
           END OF ty_page_stack.

    DATA: mi_cur_page    TYPE REF TO zif_abapgit_gui_page,
          mt_stack       TYPE STANDARD TABLE OF ty_page_stack,
          mo_router      TYPE REF TO zcl_abapgit_gui_router,
          mo_asset_man   TYPE REF TO zcl_abapgit_gui_asset_manager,
          mo_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS constructor
      RAISING zcx_abapgit_exception.

    METHODS startup
      RAISING zcx_abapgit_exception.

    METHODS cache_html
      IMPORTING iv_text       TYPE string
      RETURNING VALUE(rv_url) TYPE w3url.

    METHODS cache_asset
      IMPORTING iv_text       TYPE string OPTIONAL
                iv_xdata      TYPE xstring OPTIONAL
                iv_url        TYPE w3url OPTIONAL
                iv_type       TYPE c
                iv_subtype    TYPE c
      RETURNING VALUE(rv_url) TYPE w3url.

    METHODS render
      RAISING zcx_abapgit_exception.

    METHODS get_current_page_name
      RETURNING VALUE(rv_page_name) TYPE string.

    METHODS call_page
      IMPORTING ii_page          TYPE REF TO zif_abapgit_gui_page
                iv_with_bookmark TYPE abap_bool DEFAULT abap_false
                iv_replacing     TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_abapgit_exception.

    METHODS handle_action
      IMPORTING action      TYPE c
                frame       TYPE c OPTIONAL
                getdata     TYPE c OPTIONAL
                postdata    TYPE cnht_post_data_tab OPTIONAL
                query_table TYPE cnht_query_table OPTIONAL.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI IMPLEMENTATION.


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


  METHOD cache_asset.

    DATA: lv_xstr  TYPE xstring,
          lt_xdata TYPE TABLE OF w3_mime, " RAW255
          lv_size  TYPE int4.

    ASSERT iv_text IS SUPPLIED OR iv_xdata IS SUPPLIED.

    IF iv_text IS SUPPLIED. " String input

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = iv_text
        IMPORTING
          buffer = lv_xstr
        EXCEPTIONS
          OTHERS = 1.
      ASSERT sy-subrc = 0.

    ELSE. " Raw input
      lv_xstr = iv_xdata.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstr
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_xdata.

    mo_html_viewer->load_data(
      EXPORTING
        type         = iv_type
        subtype      = iv_subtype
        size         = lv_size
        url          = iv_url
      IMPORTING
        assigned_url = rv_url
      CHANGING
        data_table   = lt_xdata
      EXCEPTIONS
        OTHERS       = 1 ) ##NO_TEXT.

    ASSERT sy-subrc = 0. " Image data error

  ENDMETHOD.  " cache_asset.


  METHOD cache_html.

    rv_url = cache_asset( iv_text    = iv_text
                          iv_type    = 'text'
                          iv_subtype = 'html' ).

  ENDMETHOD.                    "cache_html


  METHOD call_page.

    DATA: ls_stack TYPE ty_page_stack.

    IF iv_replacing = abap_false AND NOT mi_cur_page IS INITIAL.
      ls_stack-page     = mi_cur_page.
      ls_stack-bookmark = iv_with_bookmark.
      APPEND ls_stack TO mt_stack.
    ENDIF.

    mi_cur_page = ii_page.
    render( ).

  ENDMETHOD.                "call_page


  METHOD constructor.

    startup( ).

  ENDMETHOD.            "constructor


  METHOD focus.

    cl_gui_control=>set_focus( mo_html_viewer ).

  ENDMETHOD.


  METHOD get_current_page_name.
    IF mi_cur_page IS BOUND.
      rv_page_name =
        cl_abap_classdescr=>describe_by_object_ref( mi_cur_page
          )->get_relative_name( ).
      SHIFT rv_page_name LEFT DELETING LEADING 'LCL_GUI_'.
    ENDIF." ELSE - return is empty => initial page

  ENDMETHOD.  "get_current_page_name


  METHOD get_instance.
    IF go_gui IS INITIAL.
      CREATE OBJECT go_gui.
    ENDIF.
    ro_gui = go_gui.
  ENDMETHOD.


  METHOD go_home.

    on_event( action = |{ zif_abapgit_definitions=>gc_action-go_main }| ). " doesn't accept strings directly

  ENDMETHOD.                "go_home


  METHOD handle_action.

    DATA: lx_exception TYPE REF TO zcx_abapgit_exception,
          li_page      TYPE REF TO zif_abapgit_gui_page,
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
          WHEN zif_abapgit_definitions=>gc_event_state-re_render.
            render( ).
          WHEN zif_abapgit_definitions=>gc_event_state-new_page.
            call_page( li_page ).
          WHEN zif_abapgit_definitions=>gc_event_state-new_page_w_bookmark.
            call_page( ii_page = li_page iv_with_bookmark = abap_true ).
          WHEN zif_abapgit_definitions=>gc_event_state-new_page_replacing.
            call_page( ii_page = li_page iv_replacing = abap_true ).
          WHEN zif_abapgit_definitions=>gc_event_state-go_back.
            back( ).
          WHEN zif_abapgit_definitions=>gc_event_state-go_back_to_bookmark.
            back( abap_true ).
          WHEN zif_abapgit_definitions=>gc_event_state-no_more_act.
            " Do nothing, handling completed
          WHEN OTHERS.
            zcx_abapgit_exception=>raise( |Unknown action: { action }| ).
        ENDCASE.

      CATCH zcx_abapgit_exception INTO lx_exception.
        ROLLBACK WORK.
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
      CATCH zcx_abapgit_cancel ##NO_HANDLER.
        " Do nothing = gc_event_state-no_more_act
    ENDTRY.

  ENDMETHOD.  "handle_action


  METHOD on_event.

    handle_action(
      action      = action
      frame       = frame
      getdata     = getdata
      postdata    = postdata
      query_table = query_table ).

  ENDMETHOD.                    "on_event


  METHOD render.

    DATA: lv_url  TYPE w3url,
          lo_html TYPE REF TO zcl_abapgit_html.

    lo_html = mi_cur_page->render( ).
    lv_url  = cache_html( lo_html->render( iv_no_indent_jscss = abap_true ) ).

    mo_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "render


  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events,
          lt_assets TYPE zif_abapgit_definitions=>tt_web_assets.

    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.

    CREATE OBJECT mo_router.
    CREATE OBJECT mo_asset_man.
    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

    cache_asset( iv_xdata   = mo_asset_man->get_asset( 'css_common' )
                 iv_url     = 'css/common.css'
                 iv_type    = 'text'
                 iv_subtype = 'css' ).

    cache_asset( iv_xdata   = mo_asset_man->get_asset( 'js_common' )
                 iv_url     = 'js/common.js'
                 iv_type    = 'text'
                 iv_subtype = 'javascript' ).

    lt_assets = mo_asset_man->get_images( ).
    IF lines( lt_assets ) > 0.
      LOOP AT lt_assets ASSIGNING <ls_asset>.
        cache_asset( iv_xdata   = <ls_asset>-content
                     iv_url     = <ls_asset>-url
                     iv_type    = 'image'
                     iv_subtype = 'png' ).
      ENDLOOP.
    ENDIF.

    ls_event-eventid    = mo_html_viewer->m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_html_viewer->set_registered_events( lt_events ).
    SET HANDLER me->on_event FOR mo_html_viewer.

  ENDMETHOD.                    "startup
ENDCLASS.
