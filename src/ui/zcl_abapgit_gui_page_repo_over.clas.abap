CLASS zcl_abapgit_gui_page_repo_over DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable .

    DATA mv_order_by TYPE string READ-ONLY .
    DATA mv_only_favorites   TYPE abap_bool READ-ONLY.

    METHODS constructor
      IMPORTING iv_only_favorites TYPE abap_bool
      RAISING
                zcx_abapgit_exception .
    METHODS set_order_by
      IMPORTING
        !iv_order_by TYPE string .
    METHODS set_order_direction
      IMPORTING
        !iv_order_descending TYPE abap_bool .
    METHODS set_filter
      IMPORTING
        it_postdata TYPE zif_abapgit_html_viewer=>ty_post_data .
    METHODS set_only_favorites
      IMPORTING
        iv_only_favorites TYPE abap_bool.
    METHODS
      get_only_favorites RETURNING VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.


  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_overview,
        favorite        TYPE string,
        "! True for offline, false for online repo
        type            TYPE string,
        key             TYPE zif_abapgit_persistence=>ty_value,
        name            TYPE string,
        url             TYPE string,
        package         TYPE devclass,
        branch          TYPE string,
        created_by      TYPE syuname,
        created_at      TYPE string,
        deserialized_by TYPE syuname,
        deserialized_at TYPE string,
        write_protected TYPE abap_bool,
      END OF ty_overview,
      ty_overviews TYPE STANDARD TABLE OF ty_overview
                   WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS:
      BEGIN OF c_action,
        select       TYPE string VALUE 'select',
        apply_filter TYPE string VALUE 'apply_filter',
      END OF c_action .

    DATA: mv_order_descending TYPE abap_bool,
          mv_filter           TYPE string,
          mv_time_zone        TYPE timezone,
          mt_col_spec         TYPE zif_abapgit_definitions=>ty_col_spec_tt,
          mt_overview         TYPE ty_overviews.

    METHODS: render_text_input
      IMPORTING iv_name        TYPE string
                iv_label       TYPE string
                iv_value       TYPE string OPTIONAL
                iv_max_length  TYPE string OPTIONAL
                !iv_autofocus  TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ri_html) TYPE REF TO zif_abapgit_html,

      apply_filter
        CHANGING
          ct_overview TYPE ty_overviews,

      map_repo_list_to_overview
        RETURNING
          VALUE(rt_overview) TYPE ty_overviews
        RAISING
          zcx_abapgit_exception,

      render_table_header
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      render_table
        IMPORTING
          ii_html     TYPE REF TO zif_abapgit_html
          it_overview TYPE ty_overviews
        RAISING
          zcx_abapgit_exception,

      render_table_body
        IMPORTING
          ii_html      TYPE REF TO zif_abapgit_html
          it_repo_list TYPE ty_overviews
        RAISING
          zcx_abapgit_exception,

      render_header_bar
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      apply_order_by
        CHANGING ct_overview TYPE ty_overviews,

      _add_column
        IMPORTING
          iv_tech_name      TYPE string OPTIONAL
          iv_display_name   TYPE string OPTIONAL
          iv_css_class      TYPE string OPTIONAL
          iv_add_tz         TYPE abap_bool OPTIONAL
          iv_title          TYPE string OPTIONAL
          iv_allow_order_by TYPE any OPTIONAL.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS shorten_repo_url
      IMPORTING iv_full_url         TYPE string
                iv_max_length       TYPE i DEFAULT 60
      RETURNING VALUE(rv_shortened) TYPE string.

    METHODS render_actions
      IMPORTING ii_html TYPE REF TO zif_abapgit_html.

    METHODS column
      IMPORTING iv_content     TYPE string OPTIONAL
                iv_css_class   TYPE string OPTIONAL
      RETURNING VALUE(rv_html) TYPE string.

    METHODS action_link
      IMPORTING iv_content     TYPE string
      RETURNING VALUE(rv_html) TYPE string.
ENDCLASS.



CLASS zcl_abapgit_gui_page_repo_over IMPLEMENTATION.


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

    ls_sort-name = 'FAVORITE'.
    ls_sort-descending = abap_true.
    ls_sort-astext = abap_true.
    INSERT ls_sort INTO TABLE lt_sort.

    IF mv_order_by IS NOT INITIAL.

      ls_sort-name       = mv_order_by.
      ls_sort-descending = mv_order_descending.
      ls_sort-astext     = abap_true.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    SORT ct_overview BY (lt_sort).


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mv_order_by = |NAME|.
    mv_only_favorites = iv_only_favorites.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = mv_time_zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD map_repo_list_to_overview.

    DATA: ls_overview      LIKE LINE OF rt_overview,
          lv_date          TYPE d,
          lv_time          TYPE t,
          lt_repo_obj_list TYPE zif_abapgit_repo_srv=>ty_repo_list.

    FIELD-SYMBOLS <ls_repo> LIKE LINE OF lt_repo_obj_list.

    IF mv_only_favorites = abap_true.
      lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list_favorites( ).
    ELSE.
      lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    ENDIF.

    LOOP AT lt_repo_obj_list ASSIGNING <ls_repo>.

      CLEAR: ls_overview.

      ls_overview-favorite   = zcl_abapgit_persistence_user=>get_instance(
        )->is_favorite_repo( <ls_repo>->ms_data-key ).
      ls_overview-type       = <ls_repo>->ms_data-offline.
      ls_overview-key        = <ls_repo>->ms_data-key.
      ls_overview-name       = <ls_repo>->get_name( ).
      ls_overview-url        = <ls_repo>->ms_data-url.
      ls_overview-package    = <ls_repo>->ms_data-package.
      ls_overview-branch     = <ls_repo>->ms_data-branch_name.
      ls_overview-created_by = <ls_repo>->ms_data-created_by.
      ls_overview-write_protected = <ls_repo>->ms_data-local_settings-write_protected.

      IF <ls_repo>->ms_data-created_at IS NOT INITIAL.
        CONVERT TIME STAMP <ls_repo>->ms_data-created_at
                TIME ZONE mv_time_zone
                INTO DATE lv_date
                     TIME lv_time.

        ls_overview-created_at = |{ lv_date DATE = USER } { lv_time TIME = USER }|.
      ENDIF.

      ls_overview-deserialized_by = <ls_repo>->ms_data-deserialized_by.

      IF <ls_repo>->ms_data-deserialized_at IS NOT INITIAL.
        CONVERT TIME STAMP <ls_repo>->ms_data-deserialized_at
                TIME ZONE mv_time_zone
                INTO DATE lv_date
                     TIME lv_time.

        ls_overview-deserialized_at = |{ lv_date DATE = USER } { lv_time TIME = USER }|.
      ENDIF.

      INSERT ls_overview INTO TABLE rt_overview.

    ENDLOOP.

  ENDMETHOD.


  METHOD render_header_bar.

    DATA: lv_new_toggle_favorites TYPE abap_bool,
          lv_icon_class           TYPE string.

    ii_html->add( |<div class="form-container">| ).

    ii_html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).

    ii_html->add( render_text_input(
      iv_name      = |filter|
      iv_label     = |Filter: |
      iv_value     = mv_filter
      iv_autofocus = abap_true ) ).
    ii_html->add( |<input type="submit" class="hidden-submit">| ).
    ii_html->add( |</form>| ).

    lv_new_toggle_favorites = boolc( NOT mv_only_favorites = abap_true ).
    " render icon for current state but filter value for new state
    IF mv_only_favorites = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.

    ii_html->add( ii_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Only Favorites|
      iv_act   = |{ zif_abapgit_definitions=>c_action-toggle_favorites }?favorites={ lv_new_toggle_favorites }| ) ).

    ii_html->add( `<span class="separator">|</span>` ).

    ii_html->add( ii_html->a(
      iv_txt = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act = |gHelper.toggleRepoListDetail()|
      iv_typ = zif_abapgit_html=>c_action_type-onclick ) ).

    render_actions( ii_html = ii_html ).

    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'setInitialFocus("filter");' ).
    ri_html->add( 'var gHelper = new RepoOverViewHelper();' ).

  ENDMETHOD.


  METHOD render_table.

    ii_html->add( |<div class="db_list repo-overview">| ).
    ii_html->add( |<table class="db_tab">| ).

    render_table_header( ii_html ).
    render_table_body( ii_html      = ii_html
                       it_repo_list = it_overview ).

    ii_html->add( |</table>| ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_table_body.

    DATA:
      lv_type_icon            TYPE string,
      lv_favorite_icon        TYPE string,
      lv_favorite_class       TYPE string,
      lv_text                 TYPE string,
      lv_lock                 TYPE string,
      lv_toggle_favorite_link TYPE string,
      lv_repo_go_link         TYPE string,
      lv_remote_icon_link     TYPE string.

    FIELD-SYMBOLS: <ls_repo>     LIKE LINE OF it_repo_list.

    ii_html->add( '<tbody>' ).

    LOOP AT it_repo_list ASSIGNING <ls_repo>.

      IF <ls_repo>-type = abap_true.
        lv_type_icon = 'plug/darkgrey'.
      ELSE.
        lv_type_icon = 'cloud-upload-alt/darkgrey'.
      ENDIF.

      IF <ls_repo>-favorite = abap_true.
        lv_favorite_icon = 'star/blue'.
        lv_favorite_class = 'favorite'.
      ELSE.
        lv_favorite_icon = 'star/grey'.
        lv_favorite_class = ''.
      ENDIF.

      ii_html->add(
        |<tr class="repo { lv_favorite_class }" data-key="{ <ls_repo>-key }" data-offline="{ <ls_repo>-type }">| ).

      lv_toggle_favorite_link = ii_html->a(
        iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ <ls_repo>-key }|
        iv_txt = ii_html->icon( iv_name  = lv_favorite_icon
        iv_class = 'pad-sides'
        iv_hint  = 'Click to toggle favorite' ) ).

      ii_html->add(
        column( iv_content = lv_toggle_favorite_link
                iv_css_class = 'wmin' ) ).

      CLEAR lv_lock.
      IF <ls_repo>-write_protected = abap_true.
        lv_lock = ii_html->icon( iv_name  = 'lock/grey70'
                                 iv_class = 'm-em5-sides'
                                 iv_hint  = 'Locked from pulls' ).
      ENDIF.

      ii_html->add(
        column( iv_content = ii_html->icon( lv_type_icon )
                iv_css_class = 'wmin' ) ).

      ii_html->add(
        column( iv_content = ii_html->a( iv_txt = <ls_repo>-name
                                         iv_act = |{ c_action-select }?key={ <ls_repo>-key }| ) && lv_lock ) ).

      ii_html->add(
        column( iv_content = zcl_abapgit_gui_chunk_lib=>render_package_name(
                            iv_package = <ls_repo>-package
                            iv_suppress_title = boolc( NOT mv_only_favorites = abap_true ) )->render( ) ) ).


      IF <ls_repo>-type = abap_false.
        lv_remote_icon_link = ii_html->a(
          iv_txt   = ii_html->icon( iv_name  = 'edit-solid'
                                    iv_class = 'pad-sides'
                                    iv_hint  = 'Change remote' )
          iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remote_settings }?| &&
                     |key={ <ls_repo>-key }|
          iv_class = |remote_repo| ).

        lv_text = shorten_repo_url( <ls_repo>-url ) && lv_remote_icon_link.

        ii_html->add( column( iv_content = |{ ii_html->a(
          iv_txt   = lv_text
          iv_title = <ls_repo>-url
          iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ <ls_repo>-url }| ) }| ) ).
      ELSE.
        ii_html->add( column( ) ).
      ENDIF.

      IF <ls_repo>-branch IS INITIAL.
        ii_html->add( column( iv_content = |&nbsp;| ) ).
      ELSE.
        ii_html->add(
          column( iv_content = zcl_abapgit_gui_chunk_lib=>render_branch_name(
                              iv_branch   = <ls_repo>-branch
                              iv_repo_key = <ls_repo>-key )->render( ) ) ).
      ENDIF.

      ii_html->add(
        column( iv_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
                            iv_username = <ls_repo>-deserialized_by
                            iv_suppress_title = boolc( NOT mv_only_favorites = abap_true ) )->render( )
                iv_css_class = 'ro-detail' ) ).

      ii_html->add(
        column( iv_content = <ls_repo>-deserialized_at
                iv_css_class = 'ro-detail' ) ).

      ii_html->add(
        column( iv_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
                    iv_username = <ls_repo>-created_by
                    iv_suppress_title = boolc( NOT mv_only_favorites = abap_true ) )->render( )
                iv_css_class = 'ro-detail' ) ).

      ii_html->add(
        column( iv_content = <ls_repo>-created_at
                iv_css_class = 'ro-detail' ) ).

      ii_html->add(
        column( iv_content = |{ <ls_repo>-key }|
                iv_css_class = 'ro-detail' ) ).

      " the link is clicked in javascript
      lv_repo_go_link = ii_html->a(
        iv_txt   = ``
        iv_act   = |{ c_action-select }?key={ <ls_repo>-key }|
        iv_class = 'hidden' ).

      ii_html->add( column(
        iv_content = |<span class="link" title="Open">&rsaquo;{ lv_repo_go_link }</span>|
        iv_css_class = 'ro-go' ) ).
      ii_html->add( `</tr>` ).
    ENDLOOP.

    ii_html->add( |</tbody>| ).

    IF mv_only_favorites = abap_true.
      ii_html->add( `<tfoot><tr><td colspan="5">` ).
      ii_html->add( `(Only favorites are shown. ` ).
      ii_html->add( ii_html->a(
        iv_txt   = |Show All|
        iv_act   = |{ zif_abapgit_definitions=>c_action-toggle_favorites }?favorites={ abap_false }| ) ).
      ii_html->add( `)</td></tr></tfoot>` ).
    ENDIF.

  ENDMETHOD.

  METHOD render_actions.

    CONSTANTS:
      lc_separator     TYPE string VALUE `<span class="separator">|</span>`,
      lc_dummy_key     TYPE string VALUE `?key=#`,
      lc_offline_class TYPE string VALUE `action_offline_repo`,
      lc_online_class  TYPE string VALUE `action_online_repo`,
      lc_action_class  TYPE string VALUE `action_link`.

    DATA:
      lv_settings_link TYPE string,
      lv_check_link    TYPE string,
      lv_stage_link    TYPE string,
      lv_patch_link    TYPE string,
      lv_diff_link     TYPE string,
      lv_pull_link     TYPE string.

    DATA:
      lv_zip_import_link TYPE string,
      lv_zip_export_link TYPE string.

    ii_html->add( |<div class="float-right">| ).

    lv_pull_link = ii_html->a(
      iv_txt   = |Pull|
      iv_act   = |{ zif_abapgit_definitions=>c_action-git_reset }{ lc_dummy_key }|
      iv_class = |{ lc_action_class } { lc_online_class }| ).

    ii_html->add( action_link( lv_pull_link && lc_separator ) ).

    lv_stage_link = ii_html->a(
      iv_txt   = |Stage|
      iv_act   = |{ zif_abapgit_definitions=>c_action-go_stage }{ lc_dummy_key }|
      iv_class = |{ lc_action_class } { lc_online_class } | ).

    ii_html->add( action_link( lv_stage_link && lc_separator ) ).

    lv_patch_link = ii_html->a(
      iv_txt   = |Patch|
      iv_act   = |{ zif_abapgit_definitions=>c_action-go_patch }{ lc_dummy_key }|
      iv_class = |{ lc_action_class } { lc_online_class } | ).

    ii_html->add( action_link( lv_patch_link && lc_separator ) ).

    lv_diff_link = ii_html->a(
      iv_txt   = |Diff|
      iv_act   = |{ zif_abapgit_definitions=>c_action-go_repo_diff }{ lc_dummy_key }|
      iv_class = |{ lc_action_class } { lc_online_class }| ).

    ii_html->add( action_link( lv_diff_link && lc_separator ) ).

    lv_check_link = ii_html->a(
      iv_txt   = |Check|
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_code_inspector }{ lc_dummy_key }|
      iv_class = |{ lc_action_class }| ).

    ii_html->add( action_link( lv_check_link && lc_separator ) ).

    lv_zip_import_link = ii_html->a(
      iv_txt   = |Import|
      iv_act   = |{ zif_abapgit_definitions=>c_action-zip_import }{ lc_dummy_key }|
      iv_class = |{ lc_action_class } { lc_offline_class }| ).

    ii_html->add( action_link( lv_zip_import_link && lc_separator ) ).

    lv_zip_export_link = ii_html->a(
      iv_txt   = |Export|
      iv_act   = |{ zif_abapgit_definitions=>c_action-zip_export }{ lc_dummy_key }|
      iv_class = |{ lc_action_class } { lc_offline_class }| ).

    ii_html->add( action_link( lv_zip_export_link && lc_separator ) ).

    lv_settings_link = ii_html->a(
      iv_txt   = |Settings|
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_settings }{ lc_dummy_key }|
      iv_class = |{ lc_action_class }| ).

    ii_html->add( action_link( lv_settings_link ) ).

    ii_html->add( |</div>| ).
  ENDMETHOD.

  METHOD shorten_repo_url.
    DATA lv_new_length TYPE i.
    DATA lv_length_to_truncate_to TYPE i.

    rv_shortened = iv_full_url.

    REPLACE FIRST OCCURRENCE OF 'https://' IN rv_shortened WITH ''.
    REPLACE FIRST OCCURRENCE OF 'http://' IN rv_shortened WITH ''.
    IF rv_shortened CP '*.git'.
      lv_new_length = strlen( rv_shortened ) - 4.
      rv_shortened  = rv_shortened(lv_new_length).
    ENDIF.

    IF strlen( rv_shortened ) > iv_max_length.
      lv_length_to_truncate_to = iv_max_length - 3.
      rv_shortened = rv_shortened(lv_length_to_truncate_to) && `...`.
    ENDIF.
  ENDMETHOD.

  METHOD render_table_header.

    CLEAR mt_col_spec.

    _add_column(
      iv_tech_name      = 'FAVORITE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false ).

    _add_column(
      iv_tech_name      = 'TYPE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false ).

    _add_column(
      iv_tech_name      = 'NAME'
      iv_display_name   = 'Name'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'PACKAGE'
      iv_display_name   = 'Package'
      iv_css_class      = 'package'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'URL'
      iv_display_name   = 'Remote'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'BRANCH'
      iv_display_name   = 'Branch'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'DESERIALIZED_BY'
      iv_display_name   = 'Deserialized by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'DESERIALIZED_AT'
      iv_display_name   = 'Deserialized at'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'CREATED_BY'
      iv_display_name   = 'Created by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'CREATED_AT'
      iv_display_name   = 'Created at'
      iv_css_class      = 'ro-detail'
      iv_add_tz         = abap_true
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'KEY'
      iv_display_name   = 'Key'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true ).

    _add_column(
      iv_tech_name      = 'GO'
      iv_css_class      = 'ro-go'
      iv_allow_order_by = abap_false ).

    ii_html->add( |<thead>| ).
    ii_html->add( |<tr>| ).

    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_order_by_header_cells(
                      it_col_spec         = mt_col_spec
                      iv_order_by         = mv_order_by
                      iv_order_descending = mv_order_descending ) ).

    ii_html->add( '</tr>' ).
    ii_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = lv_attrs && | maxlength="{ iv_max_length }"|.
    ENDIF.

    IF iv_autofocus = abap_true.
      lv_attrs = lv_attrs && | autofocus|.
    ENDIF.

    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.


  METHOD set_filter.

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


  METHOD set_order_by.
    mv_order_by = iv_order_by.
  ENDMETHOD.


  METHOD set_order_direction.
    mv_order_descending = iv_order_descending.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    mt_overview = map_repo_list_to_overview( ).
    apply_order_by( CHANGING ct_overview = mt_overview ).
    apply_filter( CHANGING ct_overview = mt_overview ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    render_header_bar( ri_html ).

    zcl_abapgit_exit=>get_instance( )->wall_message_list( ri_html ).

    render_table( ii_html     = ri_html
                  it_overview = mt_overview ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.


  METHOD _add_column.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_col_spec.
    APPEND INITIAL LINE TO mt_col_spec ASSIGNING <ls_col>.
    <ls_col>-display_name = iv_display_name.
    <ls_col>-tech_name = iv_tech_name.
    <ls_col>-title = iv_title.
    <ls_col>-css_class = iv_css_class.
    <ls_col>-add_tz = iv_add_tz.
    <ls_col>-allow_order_by = iv_allow_order_by.
  ENDMETHOD.

  METHOD column.
    IF iv_css_class IS NOT INITIAL.
      rv_html = |<td class="{ iv_css_class }">| && iv_content && |</td>|.
    ELSE.
      rv_html = |<td>| && iv_content && |</td>|.
    ENDIF.
  ENDMETHOD.

  METHOD action_link.
    rv_html = |<span class="action_link">| && iv_content && |</span>|.
  ENDMETHOD.


  METHOD set_only_favorites.
    mv_only_favorites = iv_only_favorites.
  ENDMETHOD.

  METHOD get_only_favorites.
    rv_result = mv_only_favorites.
  ENDMETHOD.

ENDCLASS.
