CLASS zcl_abapgit_gui_page_stats DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_page_title.
    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_html_table.

    CLASS-METHODS create
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING zcx_abapgit_exception.
    METHODS constructor RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA ms_snapshot TYPE zcl_abapgit_statistics=>ty_snapshot.
    DATA mo_table TYPE REF TO zcl_abapgit_html_table.
    DATA ms_sorting TYPE zif_abapgit_html_table=>ty_sorting_state.
    METHODS create_table
      RETURNING VALUE(ro_table) TYPE REF TO zcl_abapgit_html_table.
    CLASS-METHODS format_size
      IMPORTING iv_size        TYPE zcl_abapgit_statistics=>ty_size
      RETURNING VALUE(rv_size) TYPE string.
    CLASS-METHODS sort_repositories
      IMPORTING it_repos        TYPE zcl_abapgit_statistics=>ty_repos
                is_sorting      TYPE zif_abapgit_html_table=>ty_sorting_state
      RETURNING VALUE(rt_repos) TYPE zcl_abapgit_statistics=>ty_repos.
ENDCLASS.



CLASS zcl_abapgit_gui_page_stats IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_snapshot = zcl_abapgit_statistics=>read( ).
    mo_table = create_table( ).
  ENDMETHOD.


  METHOD create.
    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_stats.
    CREATE OBJECT lo_component.
    ri_page = zcl_abapgit_gui_page_hoc=>create( lo_component ).
  ENDMETHOD.


  METHOD create_table.

    ms_sorting-column_id = 'favorite'.
    ms_sorting-descending = abap_true.
    ro_table = zcl_abapgit_html_table=>create(
      ii_renderer = me
      is_initial_sorting_state = ms_sorting ).
    ro_table->define_column( iv_column_id = 'favorite'
                             iv_column_title = 'Fav'
    )->define_column( iv_column_id = 'offline'
                      iv_column_title = 'Type'
    )->define_column( iv_column_id = 'name'
                      iv_column_title = 'Repository'
    )->define_column( iv_column_id = 'package'
                      iv_column_title = 'Package'
    )->define_column( iv_column_id = 'host'
                      iv_column_title = 'Host'
    )->define_column( iv_column_id = 'write_protected'
                      iv_column_title = 'Write protected'
    )->define_column( iv_column_id = 'objects'
                      iv_column_title = 'TADIR objects'
    )->define_column( iv_column_id = 'files'
                      iv_column_title = 'Local files'
    )->define_column( iv_column_id = 'size'
                      iv_column_title = 'Size'
    )->define_column( iv_column_id = 'error'
                      iv_column_title = 'Status' ).

  ENDMETHOD.


  METHOD format_size.

    DATA lv_size TYPE p LENGTH 16 DECIMALS 2.

    IF iv_size >= 1024 * 1024 * 1024.
      lv_size = iv_size / 1024 / 1024 / 1024.
      rv_size = |{ lv_size } GB|.
    ELSEIF iv_size >= 1024 * 1024.
      lv_size = iv_size / 1024 / 1024.
      rv_size = |{ lv_size } MB|.
    ELSEIF iv_size >= 1024.
      lv_size = iv_size / 1024.
      rv_size = |{ lv_size } KB|.
    ELSE.
      rv_size = |{ iv_size } Bytes|.
    ENDIF.

  ENDMETHOD.


  METHOD sort_repositories.

    DATA lv_field TYPE abap_compname.

    rt_repos = it_repos.
    SORT rt_repos BY name ASCENDING package ASCENDING.
    CASE is_sorting-column_id.
      WHEN 'favorite' OR 'name' OR 'package' OR 'offline' OR 'host'
        OR 'write_protected' OR 'objects' OR 'files' OR 'size' OR 'error'.
        lv_field = to_upper( is_sorting-column_id ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    " Keep description order within equal values, especially the favorite groups.
    IF is_sorting-descending = abap_true.
      SORT rt_repos STABLE BY (lv_field) DESCENDING.
    ELSE.
      SORT rt_repos STABLE BY (lv_field) ASCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_key TYPE zif_abapgit_persistence=>ty_repo-key.

    IF ii_event->mv_action = 'refresh'.
      ms_snapshot = zcl_abapgit_statistics=>refresh( ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ELSEIF ii_event->mv_action = zif_abapgit_definitions=>c_action-go_repo.
      lv_key = ii_event->query( )->get( 'KEY' ).
      rs_handled-page = zcl_abapgit_gui_page_repo_view=>create( lv_key ).
      zcl_abapgit_persist_factory=>get_user( )->set_repo_show( lv_key ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ELSEIF mo_table->process_sorting_request( ii_event->mv_action ) = abap_true.
      ms_sorting = zcl_abapgit_html_table=>detect_sorting_request( ii_event->mv_action ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.
    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-statistics' )->add(
      iv_txt = 'Refresh'
      iv_act = 'refresh'
    )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = 'Statistics'.
  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.
    " No additional row attributes required.
  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

    DATA ls_repo TYPE zcl_abapgit_statistics=>ty_repo.
    DATA lv_content TYPE string.
    DATA lv_icon TYPE string.
    DATA lv_hint TYPE string.

    ls_repo = is_row.
    lv_content = iv_value.
    CASE iv_column_id.
      WHEN 'name'.
        IF ls_repo-key IS NOT INITIAL.
          rs_render-html = zcl_abapgit_html=>create( ).
          rs_render-html->add_a(
            iv_txt = escape( val = ls_repo-name
                             format = cl_abap_format=>e_html_text )
            iv_act = |{ zif_abapgit_definitions=>c_action-go_repo }?key={ ls_repo-key }| ).
          RETURN.
        ENDIF.
      WHEN 'host'.
        IF ls_repo-offline = abap_false AND ls_repo-url IS NOT INITIAL.
          rs_render-html = zcl_abapgit_html=>create( ).
          rs_render-html->add_a(
            iv_txt = escape( val = ls_repo-host
                             format = cl_abap_format=>e_html_text )
            iv_act = escape( val = ls_repo-url
                             format = cl_abap_format=>e_html_attr )
            iv_typ = zif_abapgit_html=>c_action_type-url ).
          RETURN.
        ENDIF.
      WHEN 'package'.
        rs_render-html = zcl_abapgit_gui_chunk_lib=>render_package_name( ls_repo-package ).
        RETURN.
      WHEN 'offline'.
        lv_icon = 'cloud-upload-alt/darkgrey'.
        lv_hint = 'Online'.
        IF ls_repo-offline = abap_true.
          lv_icon = 'plug/darkgrey'.
          lv_hint = 'Offline'.
        ENDIF.
        rs_render-html = zcl_abapgit_html=>create( ).
        rs_render-html->add_icon( iv_name = lv_icon
                                  iv_hint = lv_hint ).
        RETURN.
      WHEN 'favorite'.
        lv_icon = 'star/grey'.
        lv_hint = 'Not a favorite'.
        IF ls_repo-favorite = abap_true.
          lv_icon = 'star/blue'.
          lv_hint = 'Favorite'.
        ENDIF.
        rs_render-html = zcl_abapgit_html=>create( ).
        rs_render-html->add_icon( iv_name = lv_icon
                                  iv_hint = lv_hint ).
        RETURN.
      WHEN 'write_protected'.
        IF ls_repo-write_protected = abap_true.
          rs_render-html = zcl_abapgit_html=>create( ).
          rs_render-html->add_icon( iv_name = 'lock/grey70'
                                    iv_hint = 'Locked from Pulls' ).
        ENDIF.
        RETURN.
      WHEN 'size'.
        lv_content = format_size( ls_repo-size ).
      WHEN 'error'.
        IF ls_repo-error IS INITIAL.
          lv_content = 'OK'.
        ENDIF.
    ENDCASE.
    rs_render-content = escape( val = lv_content
                                format = cl_abap_format=>e_html_text ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA ls_repo TYPE zcl_abapgit_statistics=>ty_repo.
    DATA lv_online TYPE i.
    DATA lv_offline TYPE i.
    DATA lv_favorites TYPE i.
    DATA lv_protected TYPE i.
    DATA lv_github TYPE i.
    DATA lv_gitlab TYPE i.
    DATA lv_others TYPE i.
    DATA lv_objects TYPE i.
    DATA lv_files TYPE i.
    DATA lv_objects_min TYPE i.
    DATA lv_objects_avg TYPE p LENGTH 16 DECIMALS 2.
    DATA lv_objects_max TYPE i.
    DATA lv_files_min TYPE i.
    DATA lv_files_avg TYPE p LENGTH 16 DECIMALS 2.
    DATA lv_files_max TYPE i.
    DATA lv_size TYPE zcl_abapgit_statistics=>ty_size.
    DATA lv_errors TYPE i.

    register_handlers( ).
    ri_html = zcl_abapgit_html=>create( ).
    ri_html->add( '<div class="repo-overview">' ).
    IF ms_snapshot-refreshed_at IS INITIAL.
      ri_html->add( '<p>No saved statistics. Choose Refresh to serialize all repositories locally.</p>' ).
      ri_html->add( '</div>' ).
      RETURN.
    ENDIF.

    LOOP AT ms_snapshot-repos INTO ls_repo.
      IF sy-tabix = 1 OR ls_repo-objects < lv_objects_min.
        lv_objects_min = ls_repo-objects.
      ENDIF.
      IF ls_repo-objects > lv_objects_max.
        lv_objects_max = ls_repo-objects.
      ENDIF.
      IF sy-tabix = 1 OR ls_repo-files < lv_files_min.
        lv_files_min = ls_repo-files.
      ENDIF.
      IF ls_repo-files > lv_files_max.
        lv_files_max = ls_repo-files.
      ENDIF.
      IF ls_repo-offline = abap_true.
        lv_offline = lv_offline + 1.
      ELSE.
        lv_online = lv_online + 1.
        CASE ls_repo-host.
          WHEN 'GitHub'.
            lv_github = lv_github + 1.
          WHEN 'GitLab'.
            lv_gitlab = lv_gitlab + 1.
          WHEN OTHERS.
            lv_others = lv_others + 1.
        ENDCASE.
      ENDIF.
      IF ls_repo-favorite = abap_true.
        lv_favorites = lv_favorites + 1.
      ENDIF.
      IF ls_repo-write_protected = abap_true.
        lv_protected = lv_protected + 1.
      ENDIF.
      IF ls_repo-error IS NOT INITIAL.
        lv_errors = lv_errors + 1.
      ENDIF.
      lv_objects = lv_objects + ls_repo-objects.
      lv_files = lv_files + ls_repo-files.
      lv_size = lv_size + ls_repo-size.
    ENDLOOP.
    IF ms_snapshot-repos IS NOT INITIAL.
      lv_objects_avg = lv_objects / lines( ms_snapshot-repos ).
      lv_files_avg = lv_files / lines( ms_snapshot-repos ).
    ENDIF.

    ri_html->add( |<p>Last refresh: { ms_snapshot-refreshed_at TIMESTAMP = ISO } UTC</p>| ).
    ri_html->add( '<p>Saved statistics for the current user. Refresh serializes local files without fetching remotes.</p>' ).
    ri_html->add( '<table class="default-table"><tbody>' ).
    ri_html->add( |<tr><th>Total repositories</th><td>{ lines( ms_snapshot-repos ) }</td></tr>| ).
    ri_html->add( |<tr><th>Online repositories</th><td>{ lv_online }</td></tr>| ).
    ri_html->add( |<tr><th>Offline repositories</th><td>{ lv_offline }</td></tr>| ).
    ri_html->add( |<tr><th>Favorites</th><td>{ lv_favorites }</td></tr>| ).
    ri_html->add( |<tr><th>Write protected repositories</th><td>{ lv_protected }</td></tr>| ).
    ri_html->add( |<tr><th>GitHub / GitLab / Others (online)</th>| &&
      |<td>{ lv_github } / { lv_gitlab } / { lv_others }</td></tr>| ).
    ri_html->add( |<tr><th>TADIR objects</th><td>{ lv_objects }</td></tr>| ).
    ri_html->add( |<tr><th>TADIR objects per repo (min / avg / max)</th>| &&
      |<td>{ lv_objects_min } / { lv_objects_avg } / { lv_objects_max }</td></tr>| ).
    ri_html->add( |<tr><th>Local files</th><td>{ lv_files }</td></tr>| ).
    ri_html->add( |<tr><th>Local files per repo (min / avg / max)</th>| &&
      |<td>{ lv_files_min } / { lv_files_avg } / { lv_files_max }</td></tr>| ).
    ri_html->add( |<tr><th>Total file size</th><td>{ format_size( lv_size ) }</td></tr>| ).
    ri_html->add( '</tbody></table>' ).
    IF lv_errors > 0.
      ri_html->add( |<p class="attention">{ lv_errors } repositories have errors. | &&
        |Counts may be incomplete; see details below.</p>| ).
    ENDIF.
    ri_html->add( '<p>Hosts classify github.com and gitlab.com; other hosts, '
      && 'including self-hosted services, count as Others.</p>' ).
    ri_html->add( mo_table->render(
      it_data = sort_repositories( it_repos = ms_snapshot-repos
                                  is_sorting = ms_sorting )
      is_sorting_state = ms_sorting
      iv_id = 'statistics-repositories'
      iv_css_class = 'default-table' ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
