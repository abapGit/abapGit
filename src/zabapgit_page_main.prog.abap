*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_MAIN
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_main DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        RAISING lcx_exception,
      lif_gui_page~render     REDEFINITION,
      lif_gui_page~on_event   REDEFINITION,
      lif_gui_page~get_assets REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_actions,
                 newoffline    TYPE string VALUE 'newoffline' ##NO_TEXT,
                 switch_branch TYPE string VALUE 'switch_branch' ##NO_TEXT,
                 delete_branch TYPE string VALUE 'delete_branch' ##NO_TEXT,
                 install       TYPE string VALUE 'install' ##NO_TEXT,
                 show          TYPE string VALUE 'show' ##NO_TEXT,
               END OF c_actions.

    CONSTANTS: c_default_sortkey TYPE i VALUE 9999.

    TYPES: BEGIN OF ty_repo_item,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             is_first TYPE abap_bool,
             files    TYPE tt_repo_files,
             sortkey  TYPE i,
           END OF ty_repo_item.
    TYPES tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY.

    DATA: mv_show TYPE lcl_persistence_db=>ty_value.

    METHODS:
      retrieve_active_repo
        RAISING lcx_exception,
      styles
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_error
        IMPORTING ix_error       TYPE REF TO lcx_exception
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_toc
        IMPORTING it_list        TYPE lcl_repo_srv=>ty_repo_tt
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_toc_line
        IMPORTING io_toolbar     TYPE REF TO lcl_html_toolbar
                  iv_image_url   TYPE string
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      build_main_menu
        RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar,
      render_repo_menu
        IMPORTING io_repo        TYPE REF TO lcl_repo
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_repo
        IMPORTING io_repo        TYPE REF TO lcl_repo
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      extract_repo_content
        IMPORTING io_repo       TYPE REF TO lcl_repo
        EXPORTING et_repo_items TYPE tt_repo_items
                  eo_log        TYPE REF TO lcl_log
        RAISING   lcx_exception,
      render_repo_item
        IMPORTING io_repo        TYPE REF TO lcl_repo
                  is_item        TYPE ty_repo_item
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_obj_jump_link
        IMPORTING iv_obj_type    TYPE tadir-object
                  iv_obj_name    TYPE tadir-obj_name
        RETURNING VALUE(rv_html) TYPE string,
      render_explore
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_main IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.

  METHOD retrieve_active_repo.

    DATA: lt_repos TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo  LIKE LINE OF lt_repos.

    TRY.
        lt_repos = lcl_app=>repo_srv( )->list( ).
      CATCH lcx_exception.
        RETURN.
    ENDTRY.

    mv_show = lcl_app=>user( )->get_repo_show( ). " Get default repo from user cfg

    IF mv_show IS NOT INITIAL.
      TRY. " verify the key exists
          lo_repo = lcl_app=>repo_srv( )->get( mv_show ).
        CATCH lcx_exception.
          clear mv_show.
      ENDTRY.
    ENDIF.

    IF mv_show IS INITIAL. " Fall back to first available repo
      READ TABLE lt_repos INTO lo_repo INDEX 1.
      IF sy-subrc = 0.
        mv_show = lo_repo->get_key( ).
        lcl_app=>user( )->set_repo_show( mv_show ).
      ENDIF.
    ENDIF.

  ENDMETHOD.  "retrieve_active_repo

  METHOD render_obj_jump_link.

    DATA: lv_encode TYPE string,
          lo_html   TYPE REF TO lcl_html_helper.

    lv_encode = lcl_html_action_utils=>jump_encode( iv_obj_type = iv_obj_type
                                                    iv_obj_name = iv_obj_name ).

    CREATE OBJECT lo_html.
    lo_html->add_anchor( iv_txt = |{ iv_obj_name }| iv_act = |jump?{ lv_encode }| ).
    rv_html = lo_html->mv_html.

  ENDMETHOD.

  METHOD build_main_menu.

    DATA lo_betasub TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_menu.
    CREATE OBJECT lo_betasub.

    lo_betasub->add( iv_txt = 'Database util'    iv_act = 'db' ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Package to zip'   iv_act = gc_action-zip_package ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Transport to zip' iv_act = gc_action-zip_transport ) ##NO_TEXT.

    ro_menu->add( iv_txt = 'Clone'            iv_act = gc_action-repo_clone ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'Explore'          iv_act = 'explore' ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'New offline repo' iv_act = c_actions-newoffline ) ##NO_TEXT.
    IF lcl_services_abapgit=>needs_installation( ) = abap_true.
      ro_menu->add( iv_txt = 'Get abapGit'    iv_act = gc_action-abapgit_install ) ##NO_TEXT.
    ENDIF.
    ro_menu->add( iv_txt = 'Advanced'         io_sub = lo_betasub ) ##NO_TEXT.

  ENDMETHOD.                    "build main_menu

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* REPOSITORY TABLE*/'.
    _add '.repo_tab {'.
    _add '  border: 1px solid #DDD;'.
    _add '  border-radius: 3px;'.
    _add '  background: #fff;'.
    _add '  margin-top: 0.5em;'.
    _add '}'.
    _add '.repo_tab td {'.
    _add '  border-top: 1px solid #eee;'.
    _add '  vertical-align: middle;'.
    _add '  color: #333;'.
    _add '  padding-top: 2px;'.
    _add '  padding-bottom: 2px;'.
    _add '}'.
    _add '.repo_tab td.icon {'.
    _add '  width: 32px;'.
    _add '  text-align: center;'.
    _add '}'.
    _add '.repo_tab td.type {'.
    _add '  width: 3em;'.
    _add '}'.
    _add '.repo_tab td.object {'.
    _add '  padding-left: 0.5em;'.
    _add '}'.
    _add '.repo_tab td.files {'.
    _add '  padding-left: 0.5em;'.
    _add '}'.
    _add '.repo_tab td.cmd {'.
    _add '  text-align: right;'.
    _add '  padding-left: 0.5em;'.
    _add '  padding-right: 1em;'.
    _add '}'.
    _add '.repo_tab tr.unsupported { color: lightgrey; }'.
    _add '.repo_tab tr.modified    { background: #fbf7e9; }'.
    _add '.repo_tab tr.firstrow td { border-top: 0px; }'.
    _add '.repo_tab td.files span  { display: block; }'.
    _add '.repo_tab td.cmd span    { display: block; }'.
    _add '.repo_tab td.cmd a       { display: block; }'.

  ENDMETHOD.

  METHOD render_repo_menu.

    DATA: lo_toolbar     TYPE REF TO lcl_html_toolbar,
          lo_tb_advanced TYPE REF TO lcl_html_toolbar,
          lo_tb_branch   TYPE REF TO lcl_html_toolbar,
          lv_key         TYPE lcl_persistence_db=>ty_value,
          lv_wp_opt      LIKE gc_html_opt-crossout,
          lv_pull_opt    LIKE gc_html_opt-crossout,
          lo_repo_online TYPE REF TO lcl_repo_online.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.
    CREATE OBJECT lo_tb_branch.
    CREATE OBJECT lo_tb_advanced.

    lv_key = io_repo->get_key( ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
    ENDIF.

    IF io_repo->is_write_protected( ) = abap_true.
      lv_wp_opt   = gc_html_opt-crossout.
      lv_pull_opt = gc_html_opt-crossout.
    ELSE.
      lv_pull_opt = gc_html_opt-emphas.
    ENDIF.

    " Build branch drop-down ========================
    IF io_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_branch->add( iv_txt = 'Overview'
                         iv_act = |branch_overview?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Switch'
                         iv_act = |{ c_actions-switch_branch }?{ lv_key }|
                         iv_opt = lv_wp_opt ).
      lo_tb_branch->add( iv_txt = 'Create'
                         iv_act = |create_branch?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Delete'
                         iv_act = |{ c_actions-delete_branch }?{ lv_key }| ).
    ENDIF.

    " Build advanced drop-down ========================
    IF io_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_advanced->add( iv_txt = 'Reset local'
                           iv_act = |reset?{ lv_key }|
                           iv_opt = lv_wp_opt ).
      lo_tb_advanced->add( iv_txt = 'Background mode'
                           iv_act = |background?{ lv_key }| ).
    ENDIF.
    lo_tb_advanced->add( iv_txt = 'Remove'
                         iv_act = |{ gc_action-repo_remove }?{ lv_key }| ).
    lo_tb_advanced->add( iv_txt = 'Uninstall'
                         iv_act = |{ gc_action-repo_purge }?{ lv_key }|
                         iv_opt = lv_wp_opt ).

    " Build main toolbar ==============================
    IF io_repo->is_offline( ) = abap_false. " Online ?
      TRY.
          IF lo_repo_online->get_sha1_remote( ) <> lo_repo_online->get_sha1_local( ).
            lo_toolbar->add( iv_txt = 'Pull'
                             iv_act = |pull?{ lv_key }|
                             iv_opt = lv_pull_opt ).
          ELSEIF lcl_stage_logic=>count( lo_repo_online ) > 0.
            lo_toolbar->add( iv_txt = 'Stage'
                             iv_act = |stage?{ lv_key }|
                             iv_opt = gc_html_opt-emphas ).
          ENDIF.
        CATCH lcx_exception ##NO_HANDLER.
          " authorization error or repository does not exist
          " ignore error
      ENDTRY.
      lo_toolbar->add( iv_txt = 'Branch'
                       io_sub = lo_tb_branch ) ##NO_TEXT.
    ELSE.
      lo_toolbar->add( iv_txt = 'Import ZIP'
                       iv_act = |{ gc_action-zip_import }?{ lv_key }|
                       iv_opt = gc_html_opt-emphas ).
      lo_toolbar->add( iv_txt = 'Export ZIP'
                       iv_act = |{ gc_action-zip_export }?{ lv_key }|
                       iv_opt = gc_html_opt-emphas ).
    ENDIF.

    lo_toolbar->add( iv_txt = 'Advanced'
                     io_sub = lo_tb_advanced ) ##NO_TEXT.
    lo_toolbar->add( iv_txt = 'Refresh'
                     iv_act = |{ gc_action-repo_refresh }?{ lv_key }| ).

    " Render ==========================================
    ro_html->add( '<div class="paddings right">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_repo.

    DATA: lt_repo_items TYPE tt_repo_items,
          lx_error      TYPE REF TO lcx_exception,
          lo_log        TYPE REF TO lcl_log.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.


    CREATE OBJECT ro_html.

    ro_html->add( |<div class="repo" id="repo{ io_repo->get_key( ) }">| ).
    ro_html->add( render_repo_top( io_repo = io_repo iv_interactive_branch = abap_true ) ).

    TRY.
        extract_repo_content( EXPORTING io_repo       = io_repo
                              IMPORTING et_repo_items = lt_repo_items
                                        eo_log        = lo_log ).

* extract_repo_content must be called before rendering the menu
* so that lo_log is filled with errors from the serialization
        ro_html->add( render_repo_menu( io_repo ) ).

        IF io_repo->is_offline( ) = abap_false.
          ro_html->add( '<div class="log">' ).
* shows eg. list of unsupported objects
          ro_html->add( lo_log->to_html( ) ).
          ro_html->add( '</div>' ).
        ENDIF.

        ro_html->add( '<table width="100%" class="repo_tab">' ).
        IF lines( lt_repo_items ) = 0.
          ro_html->add( '<tr class="unsupported firstrow"><td class="paddings">'
                       && '<center>Empty package</center>'
                       && '</td></tr>' ) ##NO_TEXT.
        ELSE.
          LOOP AT lt_repo_items ASSIGNING <ls_item>.
            ro_html->add( render_repo_item( io_repo = io_repo is_item = <ls_item> ) ).
          ENDLOOP.
        ENDIF.
        ro_html->add( '</table>' ).

      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_repo_menu( io_repo ) ).
        ro_html->add( render_error( lx_error ) ).
    ENDTRY.

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD extract_repo_content.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          lt_tadir       TYPE ty_tadir_tt,
          ls_file        TYPE ty_repo_file,
          lt_results     TYPE ty_results_tt.

    FIELD-SYMBOLS: <ls_result>    LIKE LINE OF lt_results,
                   <ls_repo_item> LIKE LINE OF et_repo_items,
                   <ls_tadir>     LIKE LINE OF lt_tadir.


    CLEAR et_repo_items.

    IF io_repo->is_offline( ) = abap_true.
      lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        APPEND INITIAL LINE TO et_repo_items ASSIGNING <ls_repo_item>.
        IF sy-tabix = 1.
          <ls_repo_item>-is_first = abap_true.
        ENDIF.
        <ls_repo_item>-obj_type = <ls_tadir>-object.
        <ls_repo_item>-obj_name = <ls_tadir>-obj_name.
      ENDLOOP.

    ELSE.
      CREATE OBJECT eo_log.
      lo_repo_online ?= io_repo.
      lt_results      = lo_repo_online->status( eo_log ).
      LOOP AT lt_results ASSIGNING <ls_result>.
        AT NEW obj_name. "obj_type + obj_name
          APPEND INITIAL LINE TO et_repo_items ASSIGNING <ls_repo_item>.
          <ls_repo_item>-obj_type = <ls_result>-obj_type.
          <ls_repo_item>-obj_name = <ls_result>-obj_name.
          <ls_repo_item>-sortkey  = c_default_sortkey. " Default sort key
        ENDAT.

        IF <ls_result>-filename IS NOT INITIAL.
          ls_file-path        = <ls_result>-path.
          ls_file-filename    = <ls_result>-filename.
          ls_file-is_changed  = boolc( NOT <ls_result>-match = abap_true ).
          ls_file-remote_only = <ls_result>-remote_only.
          APPEND ls_file TO <ls_repo_item>-files.

          IF ls_file-is_changed = abap_true.
            <ls_repo_item>-sortkey = 2. " Changed files
          ENDIF.
        ENDIF.

        AT END OF obj_name. "obj_type + obj_name
          IF <ls_repo_item>-obj_type IS INITIAL.
            <ls_repo_item>-sortkey = 0. "Virtual objects
          ELSEIF lines( <ls_repo_item>-files ) = 0.
            <ls_repo_item>-sortkey = 1. "New object to commit
          ENDIF.
        ENDAT.
      ENDLOOP.

      SORT et_repo_items BY sortkey obj_type obj_name ASCENDING.
      READ TABLE et_repo_items ASSIGNING <ls_repo_item> INDEX 1.
      IF sy-subrc IS INITIAL.
        <ls_repo_item>-is_first = abap_true.
      ENDIF.
    ENDIF.


  ENDMETHOD.

  METHOD render_repo_item.
    DATA:
      lv_link     TYPE string,
      lv_icon     TYPE string,
      lv_difflink TYPE string,
      ls_file     LIKE LINE OF is_item-files,
      lv_trclass  TYPE string.

    CREATE OBJECT ro_html.

    IF is_item-is_first = abap_true. " TR class
      lv_trclass = 'firstrow' ##NO_TEXT.
    ENDIF.
    IF is_item-obj_name IS INITIAL.
      lv_trclass = lv_trclass && ' unsupported' ##NO_TEXT.
    ENDIF.
    IF is_item-sortkey > 0 AND is_item-sortkey < c_default_sortkey.
      lv_trclass = lv_trclass && ' modified' ##NO_TEXT.
    ENDIF.
    IF lv_trclass IS NOT INITIAL.
      SHIFT lv_trclass LEFT DELETING LEADING space.
      lv_trclass = | class="{ lv_trclass }"|.
    ENDIF.

    ro_html->add( |<tr{ lv_trclass }>| ).

    IF is_item-obj_name IS INITIAL.
      ro_html->add( '<td colspan="3"></td>' ).
    ELSE.
      CASE is_item-obj_type. "TODO ??
        WHEN 'PROG' OR 'CLAS' OR 'FUGR'.
          lv_icon = |<img src="img/code">|.
        WHEN 'W3MI' OR 'W3HT'.
          lv_icon = |<img src="img/bin">|.
        WHEN ''.
          lv_icon = space. " no icon
        WHEN OTHERS.
          lv_icon = |<img src="img/obj">|.
      ENDCASE.

      lv_link = render_obj_jump_link( iv_obj_name = is_item-obj_name
                                      iv_obj_type = is_item-obj_type ).
      ro_html->add( |<td class="icon">{ lv_icon }</td>| ).
      ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
      ro_html->add( |<td class="object">{ lv_link }</td>| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false. " Files for online repos only

      ro_html->add( '<td class="files">' ).
      LOOP AT is_item-files INTO ls_file.
        ro_html->add( |<span>{ ls_file-path && ls_file-filename }</span>| ).
      ENDLOOP.
      ro_html->add( '</td>' ).

      ro_html->add( '<td class="cmd">' ).
      IF lines( is_item-files ) = 0.
        ro_html->add( '<span class="grey">Only Local</span>' ).
      ELSE.
        LOOP AT is_item-files INTO ls_file.
          IF ls_file-remote_only = abap_true.
            ro_html->add( '<span class="grey">Only Remote</span>' ).
          ELSEIF ls_file-is_changed = abap_true.
            lv_difflink = lcl_html_action_utils=>file_encode(
              iv_key  = io_repo->get_key( )
              ig_file = ls_file ).
            ro_html->add_anchor(
              iv_txt = 'diff'
              iv_act = |diff?{ lv_difflink }| ).
          ELSE.
            ro_html->add( |<span>&nbsp;</span>| ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      ro_html->add( '</td>' ).

    ENDIF.

    ro_html->add( '</tr>' ).

  ENDMETHOD.

  METHOD render_toc.

    DATA: lo_pback      TYPE REF TO lcl_persistence_background,
          lt_repo_bkg   TYPE lcl_persistence_background=>tt_background,
          lo_repo       LIKE LINE OF it_list,
          lv_opt        TYPE c LENGTH 1,
          lo_online     TYPE REF TO lcl_html_toolbar,
          lo_background TYPE REF TO lcl_html_toolbar,
          lo_offline    TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_online.
    CREATE OBJECT lo_offline.
    CREATE OBJECT lo_background.
    CREATE OBJECT lo_pback.
    lt_repo_bkg = lo_pback->list( ).

    IF lines( it_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_list INTO lo_repo.
      IF mv_show = lo_repo->get_key( ).
        lv_opt = gc_html_opt-emphas.
      ELSE.
        CLEAR lv_opt.
      ENDIF.

      IF lo_repo->is_offline( ) = abap_true.
        lo_offline->add( iv_txt = lo_repo->get_name( )
                         iv_act = |{ c_actions-show }?{ lo_repo->get_key( ) }|
                         iv_opt = lv_opt ).
      ELSE.
        READ TABLE lt_repo_bkg WITH KEY key = lo_repo->get_key( )
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lo_background->add( iv_txt = lo_repo->get_name( )
                              iv_act = |{ c_actions-show }?{ lo_repo->get_key( ) }|
                              iv_opt = lv_opt ).
        ELSE.
          lo_online->add( iv_txt = lo_repo->get_name( )
                          iv_act = |{ c_actions-show }?{ lo_repo->get_key( ) }|
                          iv_opt = lv_opt ).
        ENDIF.
      ENDIF.

    ENDLOOP.

    ro_html->add( '<div id="toc"><div class="toc_grid">' ) ##NO_TEXT.

    IF lo_online->count( ) > 0.
      ro_html->add( render_toc_line( io_toolbar   = lo_online
                                     iv_image_url = 'img/repo_online' ) ).
    ENDIF.

    IF lo_offline->count( ) > 0.
      ro_html->add( render_toc_line( io_toolbar   = lo_offline
                                     iv_image_url = 'img/repo_offline' ) ).
    ENDIF.

    IF lo_background->count( ) > 0.
      ro_html->add( render_toc_line( io_toolbar   = lo_background
                                     iv_image_url = 'img/sync' ) ).
    ENDIF.

    ro_html->add( '</div></div>' ).

  ENDMETHOD.  "render_toc

  METHOD render_toc_line.
    CREATE OBJECT ro_html.

    ro_html->add( '<div class="toc_row"><table><tr>' ).

    ro_html->add( '<td>' ).
    ro_html->add( |<img src="{ iv_image_url }">| ).
    ro_html->add( '</td>' ).

    ro_html->add( '<td>' ).
    ro_html->add( io_toolbar->render( iv_sort = abap_true ) ).
    ro_html->add( '</td>' ).

    ro_html->add( '</tr></table></div>' ).

  ENDMETHOD.  "render_toc_line

  METHOD render_error.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="dummydiv attention">' ).
    ro_html->add( |Error: { ix_error->mv_text }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_explore.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_txt = 'Explore new projects'
                     iv_act = 'explore' ) ##NO_TEXT.

    ro_html->add( '<div class="dummydiv">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    DATA: lv_key  TYPE lcl_persistence_repo=>ty_repo-key,
          lo_repo TYPE REF TO lcl_repo,
          lv_url  TYPE string.


    CASE iv_action.
      WHEN c_actions-newoffline.
        ev_state = gc_event_state-no_more_act.
        lo_repo  = lcl_popups=>repo_new_offline( ).
        IF lo_repo IS BOUND.
          mv_show = lo_repo->get_key( ).
          lcl_app=>user( )->set_repo_show( mv_show ).
          ev_state = gc_event_state-re_render.
        ENDIF.
      WHEN c_actions-delete_branch.
        lv_key   = iv_getdata.
        lcl_popups=>delete_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN c_actions-switch_branch.
        lv_key   = iv_getdata.
        lcl_popups=>switch_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN c_actions-show.
        mv_show = iv_getdata.
        lcl_app=>user( )->set_repo_show( mv_show ).
        ev_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lt_repos TYPE lcl_repo_srv=>ty_repo_tt,
          lx_error TYPE REF TO lcx_exception,
          lo_repo  LIKE LINE OF lt_repos.

    retrieve_active_repo( ). " Get and validate key of user default repo

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( iv_title = 'HOME'
                         io_menu  = build_main_menu( ) ) ).

    TRY.
        lt_repos = lcl_app=>repo_srv( )->list( ).
      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_error( lx_error ) ).
    ENDTRY.

    ro_html->add( render_toc( lt_repos ) ).

    IF lines( lt_repos ) = 0 AND lx_error IS INITIAL.
      ro_html->add( render_explore( ) ).
    ELSE.
      lo_repo = lcl_app=>repo_srv( )->get( mv_show ).
      ro_html->add( render_repo( lo_repo ) ).
    ENDIF.

    ro_html->add( footer( ) ).

  ENDMETHOD.

  METHOD lif_gui_page~get_assets.
* http://fa2png.io/r/octicons/
* colour: #808080
* size: 16
* https://www.base64-image.de/ can be used to convert images to base64

    DATA ls_image TYPE ty_web_asset.

    rt_assets = super->lif_gui_page~get_assets( ).

    ls_image-url     = 'img/sync' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA6ElEQVQYGY3BIWuUAQAG'
      && '4Pc7N72xsbGBYNE8tYpVZKDZX2CcYLEZ9yQxOQSz3D/YmkUsVovRQ2SYNJnlkFfH7VZu'
      && 'wefJgrGHXnjrpQeu5B93smCwr6qqqp54433mDI5Ucds1u577o+p35hyoqe2cMThWVatJ'
      && '7KiZrZxz18SJqqtJPFXPssRgw0oSH9WNXMCQU76qzSxx2cxxTlk3yhKb6mcSQy7kvjpM'
      && 'Ylt98tpjN3POyFTdSuKSqppayxkjE/Uhc36p+m7PhhXr7vmmfhhnzpHPJqqqquqdcRY8'
      && 'spq47sAXMyde2c3/+wvX7Y18BexhBwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/toc' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAFVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgIAO39T0AAAABnRSTlMABBCRlMXJzV0oAAAAN0lEQVQIW2NgwABuaWlB'
      && 'YWlpDgwJDAxiAgxACshgYwAz0tLY2NISSBWBMYAmg4ADyBZhARCJAQBBchGypGCbQgAA'
      && 'AABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_online' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAApVBMVEUAAABQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJz+TJ01AAAANnRSTlMAAQIDBAcJCgwSFBocHygqMTM1NkRHSU1QUWFiZGlweHuDiImL'
      && 'lZiio6a5vsfT3uTo6e3x9fsxY2JuAAAAgUlEQVQYGXXB6RaBUBSA0e+IEuIiMs9zhlDn'
      && '/R/NZWmt/LA3f1RcoaB50SydCbn20wjedkPu3sKSpMGH21PhLdZ0BATZ+cCXtxtDHGLV'
      && 'pgFW9QqJj2U0wvJvMF+5jiNGI3HK9dMQSouH6sRoFGoWd8l1dEDRWlWPQsFS98KPvvDH'
      && 'C3HLClrWc70ZAAAAAElFTkSuQmCC'.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_offline' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBB'
      && 'ZG9iZSBJbWFnZVJlYWR5ccllPAAAA3hpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/'
      && 'eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+'
      && 'IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2Jl'
      && 'IFhNUCBDb3JlIDUuNi1jMTMyIDc5LjE1OTI4NCwgMjAxNi8wNC8xOS0xMzoxMzo0MCAg'
      && 'ICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5'
      && 'LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9'
      && 'IiIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHht'
      && 'bG5zOnN0UmVmPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3Vy'
      && 'Y2VSZWYjIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHht'
      && 'cE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDpkOTZmYmU4Yi0yMjEzLTZlNDct'
      && 'ODZiZC05NGE1ZTM1ZmJiMzUiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6NDQ2RTFD'
      && 'MTA0OTkwMTFFNjlCMzk5MTg2OTAzMDVDRDIiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5p'
      && 'aWQ6NDQ2RTFDMEY0OTkwMTFFNjlCMzk5MTg2OTAzMDVDRDIiIHhtcDpDcmVhdG9yVG9v'
      && 'bD0iQWRvYmUgUGhvdG9zaG9wIENDIDIwMTUuNSAoV2luZG93cykiPiA8eG1wTU06RGVy'
      && 'aXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDpkOTZmYmU4Yi0yMjEzLTZl'
      && 'NDctODZiZC05NGE1ZTM1ZmJiMzUiIHN0UmVmOmRvY3VtZW50SUQ9InhtcC5kaWQ6ZDk2'
      && 'ZmJlOGItMjIxMy02ZTQ3LTg2YmQtOTRhNWUzNWZiYjM1Ii8+IDwvcmRmOkRlc2NyaXB0'
      && 'aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+SXqL'
      && 'ugAAAWVJREFUeNrEk7tKxEAYheeSm0RXUNBaEcTOB7CxsrBXwWIbEQuLiGAnhtQW6XwK'
      && 'O1/CN/ARBLuwmrC5jOfEGVhFIbCFP3z7zyY7J+f8s5HGGDFPKTFnSbfIskwopTbDMFzW'
      && 'Wi/hUg3e4bCWUmqsV8EE6xdeT5Kk3+c5ga7rbtDS6XTaosf2smFhE3Mq3/ffwCXWj3me'
      && 'G4r0EdI0HaHd4smR53kxnEiC4ofG2ouiiALr+N0dWPs5gyOwyEhwIJumEXVdC3Y4E+67'
      && 'HfgOOHARnMDYxvhzWE4MpSG0iwh9fAX7W+h7NrCAZYEoIgiCHtg2iCUI73EmZVmeF0Wx'
      && '7RyMZwb5DSvIefCERNu2oqoqRoxx78SdwjM4BQFngI2EwtIOcIF86XcfBJsnPFIncAgu'
      && 'fstNB3wqcaOwROAePCl7AkPrGGzQjdvnWSsrAwX27X9AzUa4Ag9gNEDgzPZXcN2/C//+'
      && 'Nn4KMABcS6Z2qzb7wwAAAABJRU5ErkJggg=='.

    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/pkg' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA30lEQVQoU43OIUuDcRSF'
      && '8fvqhuB0mFwaKLbVBVdkX0GTFss+wYL2H4rJIIgyQQSzZcUPoGHZ9CKCmAwTMS8Y/ga3'
      && 'BWVjT7hwOQ+HEzEbMhU7jrTd69q2KhtFRU2nrvS927dm3pyqPXcuNRVD7sxiRIQlDSc+'
      && 'PGjZUFDWkYekLfdoV2XYua4rSZ61pZBkEUq2XPty41XuXJIiZGNhPDVZiFCYIMSor+Db'
      && '7RQhYnQnCsNvNmGgPFFYMQh1PU9aqrLxyGUNx/p66r9mUc2hFx3JhU9vDtQU4y9KGjaV'
      && '/gXT+AGZVIinhU2EAwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/branch' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAqFBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgID/OyosAAAAN3RSTlMAAQIDBAYICQ8TFRweJScoKSo3Oj1FRk1dYWJjZmhzdIaJ'
      && 'j5GVm6CwsrS5vsHDyszV19ne7/X583teZAAAAIFJREFUGFdVytkagVAYheFvFzJlnqc0'
      && 'EEoR+u//zhxI7dbZ9z4LMJ1op9DmjpntdXiBigHbLiAYqukBVr63+YGRSazgCY/iEooP'
      && 'xKZxr0EnSbo14B1Rg4msKzj150fJrQpERPLBv7mIfNxlq+zRbZsu0JYpGlcdwjY9Twfr'
      && 'nAbNsr6IKQxJI/U5CgAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/link' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAXVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICVwFMKAAAAHnRSTlMAAwQFBgcK'
      && 'FR4gIiMmP0JHSm+RmKDByM/R09rg+/0jN/q+AAAAX0lEQVQYV43Nxw6AIBAE0FGw916Z'
      && '//9MRQ0S4sG5bPZlCxqSCyBGXgFUJKUA4A8PUOKONzuQOxOZIjcLkrMvxGQg3skSCFYL'
      && 'Kl1Ds5LWz+33yyf4rQOSf6CjnV6rHeAA87gJtKzI8ocAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/code' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAAA1SURBVAhbY2AODQ0NEWBgYGVg'
      && 'YGByhNAMKgIMrKyhAQxMDhA+QwCCZgVqIIUP1Q+yJzTUAAAfUAq+Os55uAAAAABJRU5E'
      && 'rkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/bin' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAABBSURBVAhbXcqxDYAwAMRAK8h9'
      && 'hmAARoANvuD3X4UCiojqZMlsbe8JAuN6ZZ9ozThRCVmsJe9H0HwdXf19W9v2eAA6Fws2'
      && 'RotPsQAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/obj' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAIVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgIDcWqnoAAAACnRSTlMABD1AZI+RlcPFIaFe1gAA'
      && 'AEVJREFUCFtjYF+1atVKAQYGLgYGBuaJEJrBUgBCM0+A0AwLgLQIgyOIZmwCSgNptgAG'
      && '1gQQfzKDhgCSPFw9Kg2yZ9WqAgBWJBENLk6V3AAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.

ENDCLASS.