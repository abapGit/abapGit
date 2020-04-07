CLASS zcl_abapgit_gui_page_repo_over DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      RAISING zcx_abapgit_exception.
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
        select       TYPE string VALUE 'select',
        apply_filter TYPE string VALUE 'apply_filter',
      END OF c_action .

    DATA:
      mv_order_by         TYPE string,
      mv_order_descending TYPE abap_bool,
      mv_filter           TYPE string,
      mv_time_zone        TYPE timezone,
      mt_col_spec         TYPE zif_abapgit_definitions=>tty_col_spec.

    METHODS:
      render_text_input
        IMPORTING iv_name        TYPE string
                  iv_label       TYPE string
                  iv_value       TYPE string OPTIONAL
                  iv_max_length  TYPE string OPTIONAL
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      parse_filter
        IMPORTING
          it_postdata TYPE cnht_post_data_tab,

      apply_filter
        CHANGING
          ct_overview TYPE tty_overview,

      map_repo_list_to_overview
        IMPORTING
          it_repo_list       TYPE zif_abapgit_persistence=>tt_repo
        RETURNING
          VALUE(rt_overview) TYPE tty_overview
        RAISING
          zcx_abapgit_exception,

      render_table_header
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html,

      render_table
        IMPORTING
          io_html     TYPE REF TO zcl_abapgit_html
          it_overview TYPE tty_overview,

      render_table_body
        IMPORTING
          io_html     TYPE REF TO zcl_abapgit_html
          it_overview TYPE tty_overview,

      render_header_bar
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html,

      apply_order_by
        CHANGING ct_overview TYPE tty_overview,

      _add_col
        IMPORTING
          iv_descriptor TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_REPO_OVER IMPLEMENTATION.


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

    lt_overview = map_repo_list_to_overview( zcl_abapgit_persist_factory=>get_repo( )->list( ) ).

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

    io_html->add( render_text_input(
      iv_name  = |filter|
      iv_label = |Filter: |
      iv_value = mv_filter ) ).
    io_html->add( |<input type="submit" class="hidden-submit">| ).
    io_html->add( |</form>| ).

    io_html->add( zcl_abapgit_html=>a(
      iv_txt = 'Toggle detail'
      iv_act = |gHelper.toggleRepoListDetail()|
      iv_typ = zif_abapgit_html=>c_action_type-onclick ) ).

    io_html->add( |</div>| ).

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

    CLEAR mt_col_spec.
    "          technical name  /display name    /css class /add timezone
    _add_col( 'FAVORITE        /                /wmin      / ' ).
    _add_col( 'TYPE            /                /wmin      / ' ).
    _add_col( 'NAME            /Name            /          / ' ).
    _add_col( 'URL             /Url             /          / ' ).
    _add_col( 'PACKAGE         /Package         /          / ' ).
    _add_col( 'BRANCH          /Branch          /          / ' ).
    _add_col( 'DESERIALIZED_BY /Deserialized by /ro-detail / ' ).
    _add_col( 'DESERIALIZED_AT /Deserialized at /ro-detail /X' ).
    _add_col( 'CREATED_BY      /Created by      /ro-detail / ' ).
    _add_col( 'CREATED_AT      /Created at      /ro-detail /X' ).
    _add_col( 'KEY             /Key             /ro-detail / ' ).

    io_html->add( |<thead>| ).
    io_html->add( |<tr>| ).

    io_html->add( zcl_abapgit_gui_chunk_lib=>render_order_by_header_cells(
                      it_col_spec         = mt_col_spec
                      iv_order_by         = mv_order_by
                      iv_order_descending = mv_order_descending ) ).

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
    ro_html->add( 'var gHelper = new RepoOverViewHelper();' ).

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

      WHEN zif_abapgit_definitions=>c_action-change_order_by.

        mv_order_by = zcl_abapgit_gui_chunk_lib=>parse_change_order_by( iv_getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-direction.

        mv_order_descending = zcl_abapgit_gui_chunk_lib=>parse_direction( iv_getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        parse_filter( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.

        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).

    ENDCASE.

  ENDMETHOD.


  METHOD _add_col.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_col_spec.
    APPEND INITIAL LINE TO mt_col_spec ASSIGNING <ls_col>.
    SPLIT iv_descriptor AT '/' INTO
      <ls_col>-tech_name
      <ls_col>-display_name
      <ls_col>-css_class
      <ls_col>-add_tz.
    CONDENSE <ls_col>-tech_name.
    CONDENSE <ls_col>-display_name.
    CONDENSE <ls_col>-css_class.
    CONDENSE <ls_col>-add_tz.

  ENDMETHOD.
ENDCLASS.
