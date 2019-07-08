CLASS zcl_abapgit_gui_page_repo_over DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    METHODS constructor .

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_overview,
        favorite        TYPE string,
        type            TYPE string,
        key             TYPE string,
        name            TYPE string,
        url             TYPE string,
        package         TYPE string,
        branch          TYPE string,
        created_by      TYPE string,
        created_at      TYPE string,
        deserialized_by TYPE string,
        deserialized_at TYPE string,
      END OF ty_overview,
      tty_overview TYPE STANDARD TABLE OF ty_overview
                   WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS:
      BEGIN OF c_action,
        delete          TYPE string VALUE 'delete',
        select          TYPE string VALUE 'select',
        change_order_by TYPE string VALUE 'change_order_by',
        direction       TYPE string VALUE 'direction',
        apply_filter    TYPE string VALUE 'apply_filter',
      END OF c_action .

    DATA:
      mv_order_by         TYPE string,
      mv_order_descending TYPE char01,
      mv_filter           TYPE string,
      mv_time_zone        TYPE timezone.

    METHODS:
      render_text_input
        IMPORTING iv_name        TYPE string
                  iv_label       TYPE string
                  iv_value       TYPE string OPTIONAL
                  iv_max_length  TYPE string OPTIONAL
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      parse_change_order_by
        IMPORTING
          it_postdata TYPE cnht_post_data_tab,

      parse_direction
        IMPORTING
          it_postdata TYPE cnht_post_data_tab,

      parse_filter
        IMPORTING
          it_postdata TYPE cnht_post_data_tab,

      add_order_by_option
        IMPORTING
          iv_option TYPE string
          io_html   TYPE REF TO zcl_abapgit_html,

      add_direction_option
        IMPORTING
          iv_option   TYPE string
          io_html     TYPE REF TO zcl_abapgit_html
          iv_selected TYPE abap_bool,

      apply_order_by
        CHANGING
          ct_overview TYPE zcl_abapgit_gui_page_repo_over=>tty_overview,

      apply_filter
        CHANGING
          ct_overview TYPE zcl_abapgit_gui_page_repo_over=>tty_overview,

      map_repo_list_to_overview
        IMPORTING
          it_repo_list       TYPE zif_abapgit_persistence=>tt_repo
        RETURNING
          VALUE(rt_overview) TYPE zcl_abapgit_gui_page_repo_over=>tty_overview
        RAISING
          zcx_abapgit_exception,

      render_table_header
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html,

      render_table
        IMPORTING
          io_html     TYPE REF TO zcl_abapgit_html
          it_overview TYPE zcl_abapgit_gui_page_repo_over=>tty_overview,

      render_table_body
        IMPORTING
          io_html     TYPE REF TO zcl_abapgit_html
          it_overview TYPE zcl_abapgit_gui_page_repo_over=>tty_overview,

      render_order_by
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html,

      render_order_by_direction
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html,

      render_header_bar
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_REPO_OVER IMPLEMENTATION.


  METHOD add_direction_option.

    DATA: lv_selected TYPE string.

    IF iv_selected = abap_true.
      lv_selected = 'selected'.
    ENDIF.

    io_html->add( |<option value="{ iv_option }" { lv_selected }>|
               && |{ to_mixed( iv_option ) }</option>| ).

  ENDMETHOD.


  METHOD add_order_by_option.

    DATA: lv_selected TYPE string.

    IF mv_order_by = iv_option.
      lv_selected = 'selected'.
    ENDIF.

    io_html->add( |<option value="{ iv_option }" { lv_selected }>|
               && |{ to_mixed( iv_option ) }</option>| ).

  ENDMETHOD.


  METHOD apply_filter.

    IF mv_filter IS NOT INITIAL.

      DELETE ct_overview WHERE key             NS mv_filter
                           AND name            NS mv_filter
                           AND url             NS mv_filter
                           AND package         NS mv_filter
                           AND branch          NS mv_filter
                           AND created_by      NS mv_filter
                           AND created_at      NS mv_filter
                           AND deserialized_by NS mv_filter
                           AND deserialized_at NS mv_filter.

    ENDIF.

  ENDMETHOD.


  METHOD apply_order_by.

    DATA:
      lt_sort TYPE abap_sortorder_tab,
      ls_sort LIKE LINE OF lt_sort.

    IF mv_order_by IS NOT INITIAL.

      ls_sort-name       = mv_order_by.
      ls_sort-descending = mv_order_descending.
      ls_sort-astext     = abap_true.
      INSERT ls_sort INTO TABLE lt_sort.
      SORT ct_overview BY (lt_sort).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    ms_control-page_title = |Repository Overview|.
    mv_order_by = |NAME|.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = mv_time_zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD map_repo_list_to_overview.

    DATA: ls_overview LIKE LINE OF rt_overview,
          lo_repo_srv TYPE REF TO zcl_abapgit_repo,
          lv_date     TYPE d,
          lv_time     TYPE t.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repo_list.


    LOOP AT it_repo_list ASSIGNING <ls_repo>.

      CLEAR: ls_overview.
      lo_repo_srv = zcl_abapgit_repo_srv=>get_instance( )->get( <ls_repo>-key ).

      ls_overview-favorite   = zcl_abapgit_persistence_user=>get_instance(
        )->is_favorite_repo( <ls_repo>-key ).
      ls_overview-type       = <ls_repo>-offline.
      ls_overview-key        = <ls_repo>-key.
      ls_overview-name       = lo_repo_srv->get_name( ).
      ls_overview-url        = <ls_repo>-url.
      ls_overview-package    = <ls_repo>-package.
      ls_overview-branch     = zcl_abapgit_git_branch_list=>get_display_name( <ls_repo>-branch_name ).
      ls_overview-created_by = <ls_repo>-created_by.

      IF <ls_repo>-created_at IS NOT INITIAL.
        CONVERT TIME STAMP <ls_repo>-created_at
                TIME ZONE mv_time_zone
                INTO DATE lv_date
                     TIME lv_time.

        ls_overview-created_at = |{ lv_date DATE = USER } { lv_time TIME = USER }|.
      ENDIF.

      ls_overview-deserialized_by = <ls_repo>-deserialized_by.

      IF <ls_repo>-deserialized_at IS NOT INITIAL.
        CONVERT TIME STAMP <ls_repo>-deserialized_at
                TIME ZONE mv_time_zone
                INTO DATE lv_date
                     TIME lv_time.

        ls_overview-deserialized_at = |{ lv_date DATE = USER } { lv_time TIME = USER }|.
      ENDIF.

      INSERT ls_overview INTO TABLE rt_overview.

    ENDLOOP.

  ENDMETHOD.


  METHOD parse_change_order_by.

    FIELD-SYMBOLS: <lv_postdata> TYPE cnht_post_data_line.

    READ TABLE it_postdata ASSIGNING <lv_postdata>
                           INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `orderBy=(.*)`
           IN <lv_postdata>
           SUBMATCHES mv_order_by.
    ENDIF.

    mv_order_by = condense( mv_order_by ).

  ENDMETHOD.


  METHOD parse_direction.

    DATA: lv_direction TYPE string.

    FIELD-SYMBOLS: <lv_postdata> TYPE cnht_post_data_line.

    CLEAR: mv_order_descending.

    READ TABLE it_postdata ASSIGNING <lv_postdata>
                           INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `direction=(.*)`
           IN <lv_postdata>
           SUBMATCHES lv_direction.
    ENDIF.

    IF condense( lv_direction ) = 'DESCENDING'.
      mv_order_descending = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD parse_filter.

    FIELD-SYMBOLS: <lv_postdata> LIKE LINE OF it_postdata.

    READ TABLE it_postdata ASSIGNING <lv_postdata>
                           INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `filter=(.*)`
           IN <lv_postdata>
           SUBMATCHES mv_filter.
    ENDIF.

    mv_filter = condense( mv_filter ).

  ENDMETHOD.


  METHOD render_content.

    DATA: lt_overview TYPE tty_overview.


    lt_overview = map_repo_list_to_overview(
      zcl_abapgit_persist_factory=>get_repo( )->list( ) ).

    apply_order_by( CHANGING ct_overview = lt_overview ).

    apply_filter( CHANGING ct_overview = lt_overview ).

    CREATE OBJECT ro_html.

    render_header_bar( ro_html ).
    render_table( io_html     = ro_html
                  it_overview = lt_overview ).

  ENDMETHOD.


  METHOD render_header_bar.

    io_html->add( |<div class="form-container">| ).

    io_html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).

    render_order_by( io_html ).
    render_order_by_direction( io_html ).

    io_html->add( render_text_input(
      iv_name  = |filter|
      iv_label = |Filter: |
      iv_value = mv_filter ) ).
    io_html->add( |<input type="submit" class="hidden-submit">| ).
    io_html->add( |</form>| ).

    io_html->add( zcl_abapgit_html=>a(
      iv_txt = 'Toggle detail'
      iv_act = |toggleRepoListDetail()|
      iv_typ = zif_abapgit_html=>c_action_type-onclick ) ).

    io_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_order_by.

    io_html->add( |Order by: <select name="order_by" onchange="onOrderByChange(this)">| ).

    add_order_by_option( iv_option = |TYPE|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |KEY|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |NAME|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |URL|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |PACKAGE|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |BRANCH|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |CREATED_BY|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |CREATED_AT|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |DESERIALIZED_BY|
                         io_html   = io_html ).

    add_order_by_option( iv_option = |DESERIALIZED_AT|
                         io_html   = io_html ).

    io_html->add( |</select>| ).

  ENDMETHOD.


  METHOD render_order_by_direction.

    io_html->add( |<select name="direction" onchange="onDirectionChange(this)">| ).

    add_direction_option( iv_option   = |ASCENDING|
                          iv_selected = mv_order_descending
                          io_html     = io_html ).

    add_direction_option( iv_option   = |DESCENDING|
                          iv_selected = mv_order_descending
                          io_html     = io_html ).

    io_html->add( |</select>| ).

  ENDMETHOD.


  METHOD render_table.

    io_html->add( |<div class="db_list repo-overview">| ).
    io_html->add( |<table class="db_tab w100">| ).

    render_table_header( io_html ).
    render_table_body( io_html     = io_html
                       it_overview = it_overview ).

    io_html->add( |</table>| ).
    io_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_table_body.

    DATA:
          lv_type_icon     TYPE string,
          lv_favorite_icon TYPE string.

    FIELD-SYMBOLS: <ls_overview> LIKE LINE OF it_overview.

    io_html->add( '<tbody>' ).

    LOOP AT it_overview ASSIGNING <ls_overview>.

      IF <ls_overview>-type = abap_true.
        lv_type_icon = 'plug/darkgrey'.
      ELSE.
        lv_type_icon = 'cloud-upload-alt/darkgrey'.
      ENDIF.

      IF <ls_overview>-favorite = abap_true.
        lv_favorite_icon = 'star/blue'.
      ELSE.
        lv_favorite_icon = 'star/grey'.
      ENDIF.

      io_html->add( |<tr>| ).
      io_html->add( |<td class="wmin">| ).
      io_html->add_a( iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?{ <ls_overview>-key }|
                      iv_txt = zcl_abapgit_html=>icon( iv_name  = lv_favorite_icon
                                                       iv_class = 'pad-sides'
                                                       iv_hint  = 'Click to toggle favorite' ) ).
      io_html->add( |</td>| ).
      io_html->add( |<td class="wmin">{ zcl_abapgit_html=>icon( lv_type_icon )  }</td>| ).

      io_html->add( |<td>{ zcl_abapgit_html=>a( iv_txt = <ls_overview>-name
                                                iv_act = |{ c_action-select }?{ <ls_overview>-key }| ) }</td>| ).

      IF <ls_overview>-type = abap_false.
        io_html->add( |<td>{ io_html->a( iv_txt = <ls_overview>-url
                                         iv_act = |{ zif_abapgit_definitions=>c_action-url }?|
                                               && |{ <ls_overview>-url }| ) }</td>| ).
      ELSE.
        io_html->add( |<td></td>| ).
      ENDIF.

      io_html->add( |<td>{ <ls_overview>-package }</td>| ).
      io_html->add( |<td>{ <ls_overview>-branch }</td>| ).
      io_html->add( |<td class="ro-detail">{ <ls_overview>-deserialized_by }</td>| ).
      io_html->add( |<td class="ro-detail">{ <ls_overview>-deserialized_at }</td>| ).
      io_html->add( |<td class="ro-detail">{ <ls_overview>-created_by }</td>| ).
      io_html->add( |<td class="ro-detail">{ <ls_overview>-created_at }</td>| ).
      io_html->add( |<td class="ro-detail">{ <ls_overview>-key }</td>| ).
      io_html->add( |</tr>| ).

    ENDLOOP.

    io_html->add( |</tbody>| ).

  ENDMETHOD.


  METHOD render_table_header.

    io_html->add( |<thead>| ).
    io_html->add( |<tr>| ).
    io_html->add( |<th class="wmin"></th>| ). " Fav icon
    io_html->add( |<th class="wmin"></th>| ). " Repo type
    io_html->add( |<th>Name</th>| ).
    io_html->add( |<th>Url</th>| ).
    io_html->add( |<th>Package</th>| ).
    io_html->add( |<th>Branch name</th>| ).
    io_html->add( |<th class="ro-detail">Deserialized by</th>| ).
    io_html->add( |<th class="ro-detail">Deserialized at [{ mv_time_zone }]</th>| ).
    io_html->add( |<th class="ro-detail">Creator</th>| ).
    io_html->add( |<th class="ro-detail">Created at [{ mv_time_zone }]</th>| ).
    io_html->add( |<th class="ro-detail">Key</th>| ).
    io_html->add( '</tr>' ).
    io_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ro_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = | maxlength="{ iv_max_length }"|.
    ENDIF.

    ro_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ro_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.


  METHOD scripts.

    ro_html = super->scripts( ).

    ro_html->add( 'setInitialFocus("filter");' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lv_key  TYPE zif_abapgit_persistence=>ty_value.

    CASE iv_action.
      WHEN c_action-select.

        lv_key = iv_getdata.

        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_key ).

        TRY.
            zcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->refresh( ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_action-change_order_by.

        parse_change_order_by( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-direction.

        parse_direction( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        parse_filter( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.

        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_prev_page = iv_prev_page
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
