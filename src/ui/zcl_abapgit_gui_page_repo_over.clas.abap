CLASS zcl_abapgit_gui_page_repo_over DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_hotkeys.

    DATA mv_order_by TYPE string READ-ONLY .
    DATA mv_only_favorites TYPE abap_bool READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_only_favorites TYPE abap_bool
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

  PROTECTED SECTION.


  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_overview,
        favorite            TYPE string,
        "! True for offline, false for online repo
        type                TYPE string,
        key                 TYPE zif_abapgit_persistence=>ty_value,
        name                TYPE string,
        url                 TYPE string,
        package             TYPE devclass,
        branch              TYPE string,
        created_by          TYPE syuname,
        created_at          TYPE string,
        created_at_raw      TYPE timestampl,
        deserialized_by     TYPE syuname,
        deserialized_at     TYPE string,
        deserialized_at_raw TYPE timestampl,
        write_protected     TYPE abap_bool,
      END OF ty_overview,
      ty_overviews TYPE STANDARD TABLE OF ty_overview
                   WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS:
      BEGIN OF c_action,
        select       TYPE string VALUE 'select',
        apply_filter TYPE string VALUE 'apply_filter',
      END OF c_action,
      c_raw_field_suffix TYPE string VALUE `_RAW` ##NO_TEXT.

    DATA: mv_order_descending TYPE abap_bool,
          mv_filter           TYPE string,
          mt_col_spec         TYPE zif_abapgit_definitions=>ty_col_spec_tt.

    METHODS:

      apply_filter
        CHANGING
          ct_overview TYPE ty_overviews,

      map_repo_list_to_overview
        IMPORTING
          it_repo_obj_list TYPE zif_abapgit_repo_srv=>ty_repo_list
        RETURNING
          VALUE(rt_overview) TYPE ty_overviews
        RAISING
          zcx_abapgit_exception,

      render_repo_list
        IMPORTING
          ii_html     TYPE REF TO zif_abapgit_html
          it_overview TYPE ty_overviews
        RAISING
          zcx_abapgit_exception,

      render_table_header
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      render_table_footer
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      render_table_body
        IMPORTING
          ii_html      TYPE REF TO zif_abapgit_html
          it_repo_list TYPE ty_overviews
        RAISING
          zcx_abapgit_exception,

      render_table_item
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html
          is_repo TYPE ty_overview
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

    METHODS prepare_overviews
      RETURNING
        VALUE(rt_overviews) TYPE ty_overviews
      RAISING
        zcx_abapgit_exception.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_action_toolbar
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS render_filter_bar
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS column
      IMPORTING
        iv_content     TYPE string OPTIONAL
        iv_css_class   TYPE string OPTIONAL
        PREFERRED PARAMETER iv_content
      RETURNING
        VALUE(rv_html) TYPE string.

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

    ls_sort-name = 'FAVORITE'.
    ls_sort-descending = abap_true.
    ls_sort-astext = abap_true.
    INSERT ls_sort INTO TABLE lt_sort.

    IF mv_order_by IS NOT INITIAL.

      CLEAR ls_sort.

      IF mv_order_by = 'CREATED_AT' OR mv_order_by = 'DESERIALIZED_AT'.
        ls_sort-name = mv_order_by && c_raw_field_suffix.
      ELSE.
        ls_sort-name   = mv_order_by.
        ls_sort-astext = abap_true.
      ENDIF.

      ls_sort-descending = mv_order_descending.
      INSERT ls_sort INTO TABLE lt_sort.

    ENDIF.

    SORT ct_overview BY (lt_sort).

  ENDMETHOD.


  METHOD column.
    IF iv_css_class IS NOT INITIAL.
      rv_html = |<td class="{ iv_css_class }">| && iv_content && |</td>|.
    ELSE.
      rv_html = |<td>| && iv_content && |</td>|.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mv_order_by = |NAME|.
    mv_only_favorites = iv_only_favorites.

  ENDMETHOD.


  METHOD map_repo_list_to_overview.

    DATA ls_overview      LIKE LINE OF rt_overview.
    FIELD-SYMBOLS <ls_repo> LIKE LINE OF it_repo_obj_list.

    LOOP AT it_repo_obj_list ASSIGNING <ls_repo>.

      CLEAR ls_overview.

      ls_overview-favorite        = zcl_abapgit_persistence_user=>get_instance(
        )->is_favorite_repo( <ls_repo>->ms_data-key ).
      ls_overview-type            = <ls_repo>->ms_data-offline.
      ls_overview-key             = <ls_repo>->ms_data-key.
      ls_overview-name            = <ls_repo>->get_name( ).
      ls_overview-url             = <ls_repo>->ms_data-url.
      ls_overview-package         = <ls_repo>->ms_data-package.
      ls_overview-branch          = <ls_repo>->ms_data-branch_name.
      ls_overview-created_by      = <ls_repo>->ms_data-created_by.
      ls_overview-write_protected = <ls_repo>->ms_data-local_settings-write_protected.
      ls_overview-created_at_raw  = <ls_repo>->ms_data-created_at.

      IF <ls_repo>->ms_data-created_at IS NOT INITIAL.
        ls_overview-created_at = zcl_abapgit_gui_chunk_lib=>render_timestamp( <ls_repo>->ms_data-created_at ).
      ENDIF.

      ls_overview-deserialized_by     = <ls_repo>->ms_data-deserialized_by.
      ls_overview-deserialized_at_raw = <ls_repo>->ms_data-deserialized_at.

      IF <ls_repo>->ms_data-deserialized_at IS NOT INITIAL.
        ls_overview-deserialized_at = zcl_abapgit_gui_chunk_lib=>render_timestamp( <ls_repo>->ms_data-deserialized_at ).
      ENDIF.

      INSERT ls_overview INTO TABLE rt_overview.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_overviews.

    DATA lt_repo_obj_list TYPE zif_abapgit_repo_srv=>ty_repo_list.

    IF mv_only_favorites = abap_true.
      lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list_favorites( ).
    ELSE.
      lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    ENDIF.

    rt_overviews = map_repo_list_to_overview( lt_repo_obj_list ).
    apply_order_by( CHANGING ct_overview = rt_overviews ).
    apply_filter( CHANGING ct_overview = rt_overviews ).

  ENDMETHOD.


  METHOD render_action_toolbar.

    CONSTANTS:
      lc_dummy_key     TYPE string VALUE `?key=#`,
      lc_offline_class TYPE string VALUE `action_offline_repo`,
      lc_online_class  TYPE string VALUE `action_online_repo`,
      lc_action_class  TYPE string VALUE `action_link`.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.
    DATA lo_toolbar_more_sub TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT lo_toolbar EXPORTING iv_id = 'toolbar-ovp'.

    lo_toolbar->add(
      iv_txt      = |Pull|
      iv_act      = |{ zif_abapgit_definitions=>c_action-git_reset }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Stage|
      iv_act      = |{ zif_abapgit_definitions=>c_action-go_stage }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Patch|
      iv_act      = |{ zif_abapgit_definitions=>c_action-go_patch }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Diff|
      iv_act      = |{ zif_abapgit_definitions=>c_action-go_repo_diff }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Check|
      iv_act      = |{ zif_abapgit_definitions=>c_action-repo_code_inspector }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Import|
      iv_act      = |{ zif_abapgit_definitions=>c_action-zip_import }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_offline_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Export|
      iv_act      = |{ zif_abapgit_definitions=>c_action-zip_export }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_offline_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Settings|
      iv_act      = |{ zif_abapgit_definitions=>c_action-repo_settings }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    CREATE OBJECT lo_toolbar_more_sub EXPORTING iv_id = 'toolbar-ovp-more_sub'.

    lo_toolbar_more_sub->add(
      iv_txt      = |Stage by Transport|
      iv_act      = |{ zif_abapgit_definitions=>c_action-go_stage_transport }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt      = |Export by Transport|
      iv_act      = |{ zif_abapgit_definitions=>c_action-zip_export_transport }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_offline_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt = 'Danger'
      iv_typ = zif_abapgit_html=>c_action_type-separator ).

    lo_toolbar_more_sub->add(
      iv_txt   = |Remove|
      iv_title = |Remove abapGit's records of the repository (the system's |
              && |development objects will remain unaffected)|
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remove }{ lc_dummy_key }| ).

    lo_toolbar_more_sub->add(
      iv_txt      = |Uninstall|
      iv_title    = |Delete all development objects belonging to this package |
                 && |(and subpackages) from the system|
      iv_act      = |{ zif_abapgit_definitions=>c_action-repo_purge }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |More|
      io_sub      = lo_toolbar_more_sub
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    ri_html = lo_toolbar->render( iv_right = abap_true ).

  ENDMETHOD.


  METHOD render_filter_bar.

    DATA lv_icon_class TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_text_input(
      iv_name      = |filter|
      iv_label     = |Filter: |
      iv_value     = mv_filter
      iv_autofocus = abap_true ) ).
    ri_html->add( |<input type="submit" class="hidden-submit">| ).
    ri_html->add( |</form>| ).

    IF mv_only_favorites = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.

    ri_html->add( '<span class="toolbar-light pad-sides">' ).
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Only Favorites|
      iv_act   = |{ zif_abapgit_definitions=>c_action-toggle_favorites }| ) ).
    ri_html->add( ri_html->a(
      iv_txt = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act = |gHelper.toggleRepoListDetail()|
      iv_typ = zif_abapgit_html=>c_action_type-onclick ) ).
    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_header_bar.

    ii_html->add( |<div class="pad-1em" id="repo-overview-toolbar">| ).
    ii_html->add( render_filter_bar( ) ).
    ii_html->add( render_action_toolbar( ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_repo_list.

    ii_html->add( |<div class="db_list repo-overview">| ).
    ii_html->add( |<table class="db_tab">| ).

    render_table_header( ii_html ).
    render_table_body(
      ii_html      = ii_html
      it_repo_list = it_overview ).
    render_table_footer( ii_html ).

    ii_html->add( |</table>| ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'setInitialFocus("filter");' ).
    ri_html->add( 'var gHelper = new RepoOverViewHelper();' ).

  ENDMETHOD.


  METHOD render_table_body.

    FIELD-SYMBOLS <ls_repo> LIKE LINE OF it_repo_list.

    ii_html->add( '<tbody>' ).

    LOOP AT it_repo_list ASSIGNING <ls_repo>.
      render_table_item(
        ii_html = ii_html
        is_repo = <ls_repo> ).
    ENDLOOP.

    ii_html->add( |</tbody>| ).

  ENDMETHOD.


  METHOD render_table_footer.

    IF mv_only_favorites = abap_true.
      ii_html->add( `<tfoot><tr><td colspan="5">` ).
      ii_html->add( `(Only favorites are shown. ` ).
      ii_html->add( ii_html->a(
        iv_txt   = |Show All|
        iv_act   = |{ zif_abapgit_definitions=>c_action-toggle_favorites }?favorites={ abap_false }| ) ).
      ii_html->add( `)</td></tr></tfoot>` ).
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


  METHOD render_table_item.

    DATA:
      lv_type_icon             TYPE string,
      lv_favorite_icon         TYPE string,
      lv_favorite_class        TYPE string,
      lv_text                  TYPE string,
      lv_lock                  TYPE string,
      lv_repo_go_link          TYPE string,
      lv_edit_remote_icon_link TYPE string.

    " Precalculations
    IF is_repo-type = abap_true. " False means online
      lv_type_icon = 'plug/darkgrey'.
    ELSE.
      lv_type_icon = 'cloud-upload-alt/darkgrey'.
    ENDIF.

    IF is_repo-favorite = abap_true.
      lv_favorite_icon = 'star/blue'.
      lv_favorite_class = 'favorite'.
    ELSE.
      lv_favorite_icon = 'star/grey'.
      lv_favorite_class = ''.
    ENDIF.

    lv_favorite_icon = ii_html->icon(
      iv_name  = lv_favorite_icon
      iv_class = 'pad-sides'
      iv_hint  = 'Click to toggle favorite' ).

    IF is_repo-write_protected = abap_true.
      lv_lock = ii_html->icon(
        iv_name  = 'lock/grey70'
        iv_class = 'm-em5-sides'
        iv_hint  = 'Locked from pulls' ).
    ENDIF.

    " Start of row
    ii_html->add(
      |<tr class="repo { lv_favorite_class }" data-key="{ is_repo-key }" data-offline="{ is_repo-type }">| ).

    " Favorite
    ii_html->add(
      column(
        iv_content = ii_html->a(
          iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ is_repo-key }|
          iv_txt = lv_favorite_icon )
        iv_css_class = 'wmin' ) ).

    " Online/Offline
    ii_html->add(
      column(
        iv_content = ii_html->icon( lv_type_icon )
        iv_css_class = 'wmin' ) ).

    " Repo name
    ii_html->add(
      column( ii_html->a(
        iv_txt = is_repo-name
        iv_act = |{ c_action-select }?key={ is_repo-key }| ) && lv_lock ) ).

    " Package
    ii_html->add(
      column( zcl_abapgit_gui_chunk_lib=>render_package_name(
        iv_package = is_repo-package
        iv_suppress_title = boolc( NOT mv_only_favorites = abap_true ) )->render( ) ) ).

    " Repo URL
    IF is_repo-type = abap_false.
      lv_edit_remote_icon_link = ii_html->a(
        iv_txt   = ii_html->icon(
          iv_name  = 'edit-solid'
          iv_class = 'pad-sides'
          iv_hint  = 'Change remote' )
        iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remote_settings }?key={ is_repo-key }|
        iv_class = |remote_repo| ).

      ii_html->add( column( ii_html->a(
        iv_txt   = zcl_abapgit_gui_chunk_lib=>shorten_repo_url( is_repo-url ) && lv_edit_remote_icon_link
        iv_title = is_repo-url
        iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_repo-url }| ) ) ).
    ELSE.
      ii_html->add( column( ) ).
    ENDIF.

    " Branch
    IF is_repo-branch IS INITIAL.
      ii_html->add( column( |&nbsp;| ) ).
    ELSE.
      ii_html->add( column( zcl_abapgit_gui_chunk_lib=>render_branch_name(
        iv_branch   = is_repo-branch
        iv_repo_key = is_repo-key )->render( ) ) ).
    ENDIF.

    " Details: deserialized by
    ii_html->add( column(
      iv_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username       = is_repo-deserialized_by
        iv_suppress_title = boolc( NOT mv_only_favorites = abap_true ) )->render( )
      iv_css_class = 'ro-detail' ) ).

    " Details: deserialized at
    ii_html->add( column(
      iv_content = is_repo-deserialized_at
      iv_css_class = 'ro-detail' ) ).

    " Details: created by
    ii_html->add( column(
      iv_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username = is_repo-created_by
        iv_suppress_title = boolc( NOT mv_only_favorites = abap_true ) )->render( )
      iv_css_class = 'ro-detail' ) ).

    " Details: created at
    ii_html->add( column(
      iv_content = is_repo-created_at
      iv_css_class = 'ro-detail' ) ).

    " Details: repo key
    ii_html->add( column(
      iv_content = |{ is_repo-key }|
      iv_css_class = 'ro-detail' ) ).

    " the link is clicked in javascript
    lv_repo_go_link = ii_html->a(
      iv_txt   = ``
      iv_act   = |{ c_action-select }?key={ is_repo-key }|
      iv_class = 'hidden' ).

    ii_html->add( column(
      iv_content   = |<span class="link" title="Open">&rsaquo;{ lv_repo_go_link }</span>|
      iv_css_class = 'ro-go' ) ).

    ii_html->add( `</tr>` ).

  ENDMETHOD.


  METHOD set_filter.

    FIELD-SYMBOLS <lv_postdata> LIKE LINE OF it_postdata.

    READ TABLE it_postdata ASSIGNING <lv_postdata> INDEX 1.
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


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Repo overview'.

    ls_hotkey_action-description = |Stage|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Diff|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_repo_diff.
    ls_hotkey_action-hotkey = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Check|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_code_inspector.
    ls_hotkey_action-hotkey = |c|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Pull|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-git_reset.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Patch|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_patch.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    " registered/handled in js
    ls_hotkey_action-description = |Previous Repository|.
    ls_hotkey_action-action = `#`.
    ls_hotkey_action-hotkey = |4|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Next Repository|.
    ls_hotkey_action-action = `##`.
    ls_hotkey_action-hotkey = |6|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Open Repository|.
    ls_hotkey_action-action = `###`.
    ls_hotkey_action-hotkey = |Enter|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lt_overview TYPE ty_overviews.

    lt_overview = prepare_overviews( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    render_header_bar( ri_html ).
    zcl_abapgit_exit=>get_instance( )->wall_message_list( ri_html ).
    render_repo_list(
      ii_html     = ri_html
      it_overview = lt_overview ).

    register_deferred_script( render_scripts( ) ).
    register_hotkeys( ).

  ENDMETHOD.


  METHOD _add_column.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_col_spec.
    APPEND INITIAL LINE TO mt_col_spec ASSIGNING <ls_col>.
    <ls_col>-display_name   = iv_display_name.
    <ls_col>-tech_name      = iv_tech_name.
    <ls_col>-title          = iv_title.
    <ls_col>-css_class      = iv_css_class.
    <ls_col>-add_tz         = iv_add_tz.
    <ls_col>-allow_order_by = iv_allow_order_by.

  ENDMETHOD.
ENDCLASS.
