CLASS zcl_abapgit_gui_page_repo_over DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !iv_only_favorites TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ri_page)     TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_only_favorites TYPE abap_bool OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_overview,
        favorite            TYPE string,
        "! True for offline, false for online repo
        type                TYPE string,
        key                 TYPE zif_abapgit_persistence=>ty_value,
        name                TYPE string,
        labels              TYPE string_table,
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
        label_filter TYPE string VALUE 'label_filter',
      END OF c_action,
      c_label_filter_prefix TYPE string VALUE `label:`,
      c_raw_field_suffix    TYPE string VALUE `_RAW` ##NO_TEXT.

    DATA: mt_all_labels   TYPE string_table,
          mo_label_colors TYPE REF TO zcl_abapgit_string_map.
    DATA ms_list_settings TYPE zif_abapgit_definitions=>ty_list_settings.

    METHODS set_order_by
      IMPORTING
        !iv_order_by TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS set_order_direction
      IMPORTING
        !iv_order_descending TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS set_filter
      IMPORTING
        it_postdata TYPE zif_abapgit_html_viewer=>ty_post_data
      RAISING
        zcx_abapgit_exception.

    METHODS:
      apply_filter
        CHANGING
          ct_overview TYPE ty_overviews,

      map_repo_list_to_overview
        IMPORTING
          it_repo_obj_list   TYPE zif_abapgit_repo_srv=>ty_repo_list
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

      render_header_label_list
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      apply_order_by
        CHANGING ct_overview TYPE ty_overviews.

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

    METHODS build_table_scheme
      RETURNING
        VALUE(rt_tab_scheme) TYPE zif_abapgit_definitions=>ty_col_spec_tt.

    METHODS collect_all_labels
      IMPORTING
        it_overview    TYPE ty_overviews
      RETURNING
        VALUE(rt_list) TYPE string_table.

    METHODS render_filter_help_hint
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS save_settings
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_gui_page_repo_over IMPLEMENTATION.


  METHOD apply_filter.

    DATA lv_pfxl TYPE i.
    DATA lv_idx TYPE i.
    DATA lv_filter_label TYPE string.
    FIELD-SYMBOLS <ls_r> LIKE LINE OF ct_overview.

    IF ms_list_settings-filter IS INITIAL.
      RETURN.
    ENDIF.

    lv_pfxl = strlen( c_label_filter_prefix ).

    IF strlen( ms_list_settings-filter ) > lv_pfxl AND ms_list_settings-filter+0(lv_pfxl) = c_label_filter_prefix.
      lv_filter_label = ms_list_settings-filter+lv_pfxl.
      IF lv_filter_label = 'all'.
        DELETE ct_overview WHERE labels IS INITIAL.
      ELSEIF lv_filter_label = 'none'.
        DELETE ct_overview WHERE labels IS NOT INITIAL.
      ELSE.
        LOOP AT ct_overview ASSIGNING <ls_r>.
          lv_idx = sy-tabix.
          READ TABLE <ls_r>-labels TRANSPORTING NO FIELDS WITH KEY table_line = lv_filter_label.
          IF sy-subrc <> 0.
            DELETE ct_overview INDEX lv_idx.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE. " Regular filter
      DELETE ct_overview WHERE
            key             NS ms_list_settings-filter
        AND name            NS ms_list_settings-filter
        AND url             NS ms_list_settings-filter
        AND package         NS ms_list_settings-filter
        AND branch          NS ms_list_settings-filter
        AND created_by      NS ms_list_settings-filter
        AND created_at      NS ms_list_settings-filter
        AND deserialized_by NS ms_list_settings-filter
        AND deserialized_at NS ms_list_settings-filter.
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

    IF ms_list_settings-order_by IS NOT INITIAL.

      CLEAR ls_sort.

      IF ms_list_settings-order_by = 'CREATED_AT' OR ms_list_settings-order_by = 'DESERIALIZED_AT'.
        ls_sort-name = ms_list_settings-order_by && c_raw_field_suffix.
      ELSE.
        ls_sort-name   = ms_list_settings-order_by.
        ls_sort-astext = abap_true.
      ENDIF.

      ls_sort-descending = ms_list_settings-order_descending.
      INSERT ls_sort INTO TABLE lt_sort.

    ENDIF.

    SORT ct_overview BY (lt_sort).

  ENDMETHOD.


  METHOD build_table_scheme.

    DATA lo_tab_scheme TYPE REF TO lcl_table_scheme.

    CREATE OBJECT lo_tab_scheme.

    lo_tab_scheme->add_column(
      iv_tech_name      = 'FAVORITE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false
    )->add_column(
      iv_tech_name      = 'TYPE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false
    )->add_column(
      iv_tech_name      = 'NAME'
      iv_display_name   = 'Name'
      iv_allow_order_by = abap_true ).

    IF mt_all_labels IS NOT INITIAL.
      lo_tab_scheme->add_column(
        iv_tech_name      = 'LABELS'
        iv_display_name   = 'Labels'
        iv_allow_order_by = abap_false ).
    ENDIF.

    lo_tab_scheme->add_column(
      iv_tech_name      = 'PACKAGE'
      iv_display_name   = 'Package'
      iv_css_class      = 'package'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'URL'
      iv_display_name   = 'Remote'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'BRANCH'
      iv_display_name   = 'Branch/Tag'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'DESERIALIZED_BY'
      iv_display_name   = 'Deserialized by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'DESERIALIZED_AT'
      iv_display_name   = 'Deserialized at'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'CREATED_BY'
      iv_display_name   = 'Created by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'CREATED_AT'
      iv_display_name   = 'Created at'
      iv_css_class      = 'ro-detail'
      iv_add_tz         = abap_true
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'KEY'
      iv_display_name   = 'Key'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'GO'
      iv_css_class      = 'ro-go wmin'
      iv_allow_order_by = abap_false ).

    rt_tab_scheme = lo_tab_scheme->mt_col_spec.

  ENDMETHOD.


  METHOD collect_all_labels.

    FIELD-SYMBOLS <ls_r> LIKE LINE OF it_overview.

    LOOP AT it_overview ASSIGNING <ls_r>.
      APPEND LINES OF <ls_r>-labels TO rt_list.
    ENDLOOP.

    SORT rt_list.
    DELETE rt_list WHERE table_line IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM rt_list.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    ms_list_settings = zcl_abapgit_persistence_user=>get_instance( )->get_list_settings( ).

    " Overwrite setting
    IF iv_only_favorites = abap_true.
      ms_list_settings-only_favorites = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_repo_over.

    CREATE OBJECT lo_component
      EXPORTING
        iv_only_favorites = iv_only_favorites.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Repository List'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

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
      ls_overview-labels          = zcl_abapgit_repo_labels=>split( <ls_repo>->ms_data-local_settings-labels ).
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

    IF ms_list_settings-only_favorites = abap_true.
      lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list_favorites( ).
    ELSE.
      lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    ENDIF.

    rt_overviews = map_repo_list_to_overview( lt_repo_obj_list ).

    " Hmmm, side effect, not ideal, but we need label list before filter applied
    mt_all_labels = collect_all_labels( rt_overviews ).

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
      iv_act      = |{ zif_abapgit_definitions=>c_action-git_pull }{ lc_dummy_key }|
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
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remove }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt      = |Uninstall|
      iv_title    = |Delete all development objects belonging to this package |
                 && |(and subpackages) from the system|
      iv_act      = |{ zif_abapgit_definitions=>c_action-repo_purge }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
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
      iv_label     = |Filter: { render_filter_help_hint( ) }|
      iv_value     = ms_list_settings-filter ) ).
    ri_html->add( |<input type="submit" class="hidden-submit">| ).
    ri_html->add( |</form>| ).

    IF ms_list_settings-only_favorites = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.

    ri_html->add( '<span class="toolbar-light pad-sides">' ).
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Only Favorites|
      iv_class = 'command'
      iv_act   = |{ zif_abapgit_definitions=>c_action-toggle_favorites }| ) ).
    ri_html->add( ri_html->a(
      iv_txt   = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act   = |gHelper.toggleRepoListDetail()|
      iv_class = 'command'
      iv_typ   = zif_abapgit_html=>c_action_type-onclick ) ).
    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_filter_help_hint.

    DATA lt_fragments TYPE string_table.

    APPEND `Filter is applied to all text fields in the below table.` TO lt_fragments.
    APPEND ` Search works for any portion of the text (so can be a mid part as well).` TO lt_fragments.
    APPEND `<br>Starting query from <code>label:xxx</code> will filter appropriate label.` TO lt_fragments.
    APPEND `Two "special" label queries are available:` TO lt_fragments.
    APPEND ` <code>all</code> (to select all repos that has at least one label)` TO lt_fragments.
    APPEND ` and <code>none</code> (to select unlabeled repos).` TO lt_fragments.

    rv_html = zcl_abapgit_gui_chunk_lib=>render_help_hint( concat_lines_of( table = lt_fragments ) ).

  ENDMETHOD.


  METHOD render_header_bar.

    ii_html->add( |<div class="repo-overview-toolbar">| ).
    ii_html->add( render_filter_bar( ) ).
    ii_html->add( render_action_toolbar( ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_header_label_list.

    IF mt_all_labels IS INITIAL.
      RETURN.
    ENDIF.

    ii_html->add( |<div class="repo-label-catalog">| ).
    ii_html->add( '<label>Filter by label:</label>' ).
    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_label_list(
      it_labels           = mt_all_labels
      io_label_colors     = mo_label_colors
      iv_clickable_action = c_action-label_filter ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_repo_list.

    ii_html->add( |<table>| ).

    render_table_header( ii_html ).
    render_table_body(
      ii_html      = ii_html
      it_repo_list = it_overview ).
    render_table_footer( ii_html ).

    ii_html->add( |</table>| ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'var gHelper = new RepoOverViewHelper({ focusFilterKey: "f" });' ).

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

    IF ms_list_settings-only_favorites = abap_true.
      ii_html->add( `<tfoot>` ).
      ii_html->add( `<tr><td colspan="100%">` ).
      ii_html->add( |(Only favorites are shown. {
        ii_html->a(
          iv_txt   = |Show All|
          iv_act   = |{ zif_abapgit_definitions=>c_action-toggle_favorites }?force_state={ abap_false }| )
      })| ).
      ii_html->add( `</td></tr>` ).
      ii_html->add( `</tfoot>` ).
    ENDIF.

  ENDMETHOD.


  METHOD render_table_header.

    ii_html->add( |<thead>| ).
    ii_html->add( |<tr>| ).

    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_order_by_header_cells(
      it_col_spec         = build_table_scheme( )
      iv_order_by         = ms_list_settings-order_by
      iv_order_descending = ms_list_settings-order_descending ) ).

    ii_html->add( '</tr>' ).
    ii_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_table_item.

    DATA:
      lv_is_online_repo TYPE abap_bool,
      lv_repo_type_icon TYPE string,
      lv_favorite_icon  TYPE string,
      lv_fav_tr_class   TYPE string,
      lv_lock           TYPE string.

    lv_is_online_repo = boolc( is_repo-type = abap_false ).

    " Start of row
    IF is_repo-favorite = abap_true.
      lv_fav_tr_class = ' class="favorite"'.
    ELSE.
      lv_fav_tr_class = ''.
    ENDIF.

    ii_html->add( |<tr{ lv_fav_tr_class } data-key="{ is_repo-key }" data-offline="{ is_repo-type }">| ).

    " Favorite
    lv_favorite_icon = ii_html->icon(
      iv_name  = 'star/grey' " blue is added in css, based on TR style
      iv_class = 'pad-sides'
      iv_hint  = 'Click to toggle favorite' ).

    ii_html->td(
      iv_class   = 'wmin'
      iv_content = ii_html->a(
        iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ is_repo-key }|
        iv_txt = lv_favorite_icon ) ).

    " Online/Offline
    IF lv_is_online_repo = abap_true.
      lv_repo_type_icon = 'cloud-upload-alt/darkgrey'.
    ELSE.
      lv_repo_type_icon = 'plug/darkgrey'.
    ENDIF.

    ii_html->td(
      iv_class   = 'wmin'
      iv_content = ii_html->icon( lv_repo_type_icon ) ).

    " Repo name
    IF is_repo-write_protected = abap_true.
      lv_lock = ii_html->icon(
        iv_name  = 'lock/grey70'
        iv_class = 'm-em5-sides'
        iv_hint  = 'Locked from pulls' ).
    ENDIF.

    ii_html->td(
      ii_html->a(
        iv_txt = is_repo-name
        iv_act = |{ c_action-select }?key={ is_repo-key }| ) && lv_lock ).

    " Labels
    IF mt_all_labels IS NOT INITIAL.
      ii_html->td(
        iv_content = zcl_abapgit_gui_chunk_lib=>render_label_list(
          it_labels = is_repo-labels
          io_label_colors = mo_label_colors )
        iv_class   = 'labels' ).
    ENDIF.

    " Package
    ii_html->td( ii_content = zcl_abapgit_gui_chunk_lib=>render_package_name(
      iv_package        = is_repo-package
      iv_suppress_title = boolc( NOT ms_list_settings-only_favorites = abap_true ) ) ).

    " Repo URL
    IF lv_is_online_repo = abap_true.
      ii_html->td( ii_content = zcl_abapgit_gui_chunk_lib=>render_repo_url(
        iv_url = is_repo-url
        iv_render_remote_edit_for_key = is_repo-key ) ).
    ELSE.
      ii_html->td( ).
    ENDIF.

    " Branch
    IF is_repo-branch IS INITIAL.
      ii_html->td( ).
    ELSE.
      ii_html->td( ii_content = zcl_abapgit_gui_chunk_lib=>render_branch_name(
        iv_branch   = is_repo-branch
        iv_repo_key = is_repo-key ) ).
    ENDIF.

    " Details: deserialized by
    ii_html->td(
      iv_class   = 'ro-detail'
      ii_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username       = is_repo-deserialized_by
        iv_suppress_title = boolc( NOT ms_list_settings-only_favorites = abap_true ) ) ).

    " Details: deserialized at
    ii_html->td(
      iv_class = 'ro-detail'
      iv_content = is_repo-deserialized_at ).

    " Details: created by
    ii_html->td(
      iv_class   = 'ro-detail'
      ii_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username = is_repo-created_by
        iv_suppress_title = boolc( NOT ms_list_settings-only_favorites = abap_true ) ) ).

    " Details: created at
    ii_html->td(
      iv_class = 'ro-detail'
      iv_content = is_repo-created_at ).

    " Details: repo key
    ii_html->td(
      iv_class = 'ro-detail'
      iv_content = |{ is_repo-key }| ).

    " Go-to action
    ii_html->td(
      iv_class = 'ro-go wmin'
      iv_content = ii_html->a(
        iv_title = 'Open'
        iv_txt   = '&rtrif;'
        iv_act   = |{ c_action-select }?key={ is_repo-key }| ) ).

    ii_html->add( `</tr>` ).

  ENDMETHOD.


  METHOD save_settings.
    zcl_abapgit_persistence_user=>get_instance( )->set_list_settings( ms_list_settings ).
  ENDMETHOD.


  METHOD set_filter.

    FIELD-SYMBOLS <lv_postdata> LIKE LINE OF it_postdata.

    READ TABLE it_postdata ASSIGNING <lv_postdata> INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `filter=(.*)`
        IN <lv_postdata>
        SUBMATCHES ms_list_settings-filter.
    ENDIF.

    ms_list_settings-filter = condense( ms_list_settings-filter ).
    save_settings( ).

  ENDMETHOD.


  METHOD set_order_by.
    IF ms_list_settings-order_by <> iv_order_by.
      set_order_direction( abap_false ). " Reset ordering
    ENDIF.
    ms_list_settings-order_by = iv_order_by.
    save_settings( ).
  ENDMETHOD.


  METHOD set_order_direction.
    ms_list_settings-order_descending = iv_order_descending.
    save_settings( ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_key TYPE zif_abapgit_persistence=>ty_value.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN c_action-select.

        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_key ).

        TRY.
            zcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->refresh( ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        rs_handled-page  = zcl_abapgit_gui_page_repo_view=>create( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN zif_abapgit_definitions=>c_action-change_order_by.

        set_order_by( ii_event->query( )->get( 'ORDERBY' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-toggle_favorites.

        IF ii_event->query( )->has( 'FORCE_STATE' ) = abap_true.
          ms_list_settings-only_favorites = ii_event->query( )->get( 'FORCE_STATE' ).
        ELSE.
          ms_list_settings-only_favorites = boolc( ms_list_settings-only_favorites = abap_false ).
        ENDIF.
        save_settings( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-direction.

        set_order_direction( boolc( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        set_filter( ii_event->mt_postdata ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-label_filter.

        IF ii_event->mv_getdata IS NOT INITIAL.
          ms_list_settings-filter = c_label_filter_prefix && ii_event->mv_getdata.
        ELSE.
          CLEAR ms_list_settings-filter. " Unexpected request
        ENDIF.
        save_settings( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-go_patch.

        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_patch
          EXPORTING
            iv_key = lv_key.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Repo overview'.

    ls_hotkey_action-description   = |New Online Repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newonline.
    ls_hotkey_action-hotkey = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |New Offline Repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newoffline.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |abapGit Settings|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

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
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-git_pull.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Patch|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_patch.
    ls_hotkey_action-hotkey = |a|.
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

    ls_hotkey_action-description = |Focus Filter|.
    ls_hotkey_action-action = `####`.
    ls_hotkey_action-hotkey = |f|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = zcl_abapgit_gui_buttons=>new_online( )
      iv_act = zif_abapgit_definitions=>c_action-repo_newonline
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>new_offline( )
      iv_act = zif_abapgit_definitions=>c_action-repo_newoffline
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>settings( )
      iv_act = zif_abapgit_definitions=>c_action-go_settings
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>advanced( )
      iv_title = 'Utilities'
      io_sub = zcl_abapgit_gui_chunk_lib=>advanced_submenu( )
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>help( )
      iv_title = 'Help'
      io_sub = zcl_abapgit_gui_chunk_lib=>help_submenu( ) ).

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
      ro_toolbar->add(
        iv_txt   = zcl_abapgit_gui_buttons=>experimental( )
        iv_title = 'Experimental Features are Enabled'
        iv_act   = zif_abapgit_definitions=>c_action-go_settings ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lt_overview TYPE ty_overviews.
    DATA ls_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.

    ls_settings = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_user_settings( ).
    mo_label_colors = zcl_abapgit_repo_labels=>split_colors_into_map( ls_settings-label_colors ).

    lt_overview = prepare_overviews( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    zcl_abapgit_exit=>get_instance( )->wall_message_list( ri_html ).

    ri_html->add( |<div class="repo-overview">| ).
    render_header_bar( ri_html ).
    render_header_label_list( ri_html ).
    render_repo_list(
      ii_html     = ri_html
      it_overview = lt_overview ).
    ri_html->add( |</div>| ).

    register_deferred_script( render_scripts( ) ).
    register_deferred_script( zcl_abapgit_gui_chunk_lib=>render_repo_palette( c_action-select ) ).
    register_handlers( ).

  ENDMETHOD.
ENDCLASS.
