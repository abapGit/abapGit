*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_VIEW_REPO
*&---------------------------------------------------------------------*

CLASS lcl_gui_view_repo_content DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_actions,
                 toggle_hide_files TYPE string VALUE 'toggle_hide_files' ##NO_TEXT,
               END OF c_actions.

    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   lcx_exception.
    METHODS render
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CONSTANTS: c_default_sortkey TYPE i VALUE 9999.

    TYPES: BEGIN OF ty_repo_item,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             is_first TYPE abap_bool,
             files    TYPE tt_repo_files,
             sortkey  TYPE i,
             changes  TYPE i,
           END OF ty_repo_item.
    TYPES tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY.

    DATA: mo_repo       TYPE REF TO lcl_repo,
          mv_hide_files TYPE abap_bool.

    METHODS:
      render_repo_menu
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
        RETURNING VALUE(rv_html) TYPE string.

ENDCLASS. "lcl_gui_view_repo_content

CLASS lcl_gui_view_repo_content IMPLEMENTATION.

  METHOD constructor.

    mo_repo ?= io_repo.
    mv_hide_files = lcl_app=>user( )->get_hide_files( ).

  ENDMETHOD. "constructor

  METHOD render.

    DATA: lt_repo_items TYPE tt_repo_items,
          lo_tab_menu   TYPE REF TO lcl_html_toolbar,
          lx_error      TYPE REF TO lcx_exception,
          lo_log        TYPE REF TO lcl_log.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    CREATE OBJECT lo_tab_menu.
    CREATE OBJECT ro_html.

    TRY.
        extract_repo_content( EXPORTING io_repo       = mo_repo
                              IMPORTING et_repo_items = lt_repo_items
                                        eo_log        = lo_log ).

        " extract_repo_content must be called before rendering the menu
        " so that lo_log is filled with errors from the serialization
        ro_html->add( render_repo_menu( mo_repo ) ).

        IF mo_repo->is_offline( ) = abap_false and lo_log->count( ) > 0.
          ro_html->add( '<div class="log">' ).
          ro_html->add( lo_log->to_html( ) ). " shows eg. list of unsupported objects
          ro_html->add( '</div>' ).
        ENDIF.

        ro_html->add( '<div class="repo_container">' ).
        IF mo_repo->is_offline( ) = abap_false.
          IF mv_hide_files = abap_true.
*            lo_tab_menu->add( iv_txt = 'Show files' iv_act = c_actions-toggle_hide_files ).
          ELSE.
*            lo_tab_menu->add( iv_txt = 'Hide files' iv_act = c_actions-toggle_hide_files ).
          ENDIF.
          ro_html->add( lo_tab_menu->render( iv_as_angle = abap_true ) ).
        ENDIF.

        ro_html->add( '<table width="100%" class="repo_tab">' ).
        IF lines( lt_repo_items ) = 0.
          ro_html->add( '<tr class="unsupported firstrow"><td class="paddings">'
                       && '<center>Empty package</center>'
                       && '</td></tr>' ) ##NO_TEXT.
        ELSE.
          LOOP AT lt_repo_items ASSIGNING <ls_item>.
            ro_html->add( render_repo_item( io_repo = mo_repo is_item = <ls_item> ) ).
          ENDLOOP.
        ENDIF.
        ro_html->add( '</table>' ).
        ro_html->add( '</div>' ).

      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_repo_menu( mo_repo ) ).
        ro_html->add( lcl_gui_page_super=>render_error( lx_error ) ).
    ENDTRY.

  ENDMETHOD.  "render

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
                         iv_act = |{ gc_action-go_branch_overview }?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Switch'
                         iv_act = |{ gc_action-git_branch_switch }?{ lv_key }|
                         iv_opt = lv_wp_opt ).
      lo_tb_branch->add( iv_txt = 'Create'
                         iv_act = |{ gc_action-git_branch_create }?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Delete'
                         iv_act = |{ gc_action-git_branch_delete }?{ lv_key }| ).
    ENDIF.

    " Build advanced drop-down ========================
    IF io_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_advanced->add( iv_txt = 'Reset local'
                           iv_act = |{ gc_action-git_reset }?{ lv_key }|
                           iv_opt = lv_wp_opt ).
      lo_tb_advanced->add( iv_txt = 'Background mode'
                           iv_act = |{ gc_action-go_background }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Change remote'
                           iv_act = |{ gc_action-repo_remote_change }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Make off-line'
                           iv_act = |{ gc_action-repo_remote_detach }?{ lv_key }| ).
    ELSE.
      lo_tb_advanced->add( iv_txt = 'Make on-line'
                           iv_act = |{ gc_action-repo_remote_attach }?{ lv_key }| ).
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
                             iv_act = |{ gc_action-git_pull }?{ lv_key }|
                             iv_opt = lv_pull_opt ).
          ENDIF.
          IF lcl_stage_logic=>count( lo_repo_online ) > 0.
            lo_toolbar->add( iv_txt = 'Stage'
                             iv_act = |{ gc_action-go_stage }?{ lv_key }|
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

  ENDMETHOD.  "render_repo_menu

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
          <ls_repo_item>-changes  = 0.
        ENDAT.

        IF <ls_result>-filename IS NOT INITIAL.
          ls_file-path        = <ls_result>-path.
          ls_file-filename    = <ls_result>-filename.
          ls_file-is_changed  = boolc( NOT <ls_result>-match = abap_true ).
          ls_file-new         = <ls_result>-new.
          APPEND ls_file TO <ls_repo_item>-files.

          IF ls_file-is_changed = abap_true OR ls_file-new IS NOT INITIAL.
            <ls_repo_item>-sortkey = 2. " Changed files
            <ls_repo_item>-changes = <ls_repo_item>-changes + 1.
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


  ENDMETHOD.  "extract_repo_content

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
      ro_html->add( '<td colspan="2"></td>'
                 && '<td class="object"><i class="grey">non-code and meta files</i></td>' ).
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
      IF mv_hide_files = abap_false OR is_item-obj_type IS INITIAL.
        LOOP AT is_item-files INTO ls_file.
          ro_html->add( |<span>{ ls_file-path && ls_file-filename }</span>| ).
        ENDLOOP.
      ENDIF.
      ro_html->add( '</td>' ).

      ro_html->add( '<td class="cmd">' ).
      IF lines( is_item-files ) = 0.
        ro_html->add( '<span class="grey">new @local</span>' ).
      ELSEIF is_item-changes > 0.
        IF mv_hide_files = abap_true AND is_item-obj_name IS NOT INITIAL.
          lv_difflink = lcl_html_action_utils=>obj_encode(
            iv_key    = io_repo->get_key( )
            ig_object = is_item ).
          ro_html->add_anchor(
            iv_txt = |diff ({ is_item-changes })|
            iv_act = |{ gc_action-go_diff }?{ lv_difflink }| ).
        ELSE.
          LOOP AT is_item-files INTO ls_file.
            IF ls_file-new = gc_new-remote.
              ro_html->add( '<span class="grey">new @remote</span>' ).
            ELSEIF ls_file-new = gc_new-local.
              ro_html->add( '<span class="grey">new @local</span>' ).
            ELSEIF ls_file-is_changed = abap_true.
              lv_difflink = lcl_html_action_utils=>file_encode(
                iv_key  = io_repo->get_key( )
                ig_file = ls_file ).
              ro_html->add_anchor(
                iv_txt = 'diff'
                iv_act = |{ gc_action-go_diff }?{ lv_difflink }| ).
            ELSE.
              ro_html->add( |<span>&nbsp;</span>| ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      ro_html->add( '</td>' ).

    ENDIF.

    ro_html->add( '</tr>' ).

  ENDMETHOD.  "render_repo_item

  METHOD render_obj_jump_link.

    DATA: lv_encode TYPE string,
          lo_html   TYPE REF TO lcl_html_helper.

    lv_encode = lcl_html_action_utils=>jump_encode( iv_obj_type = iv_obj_type
                                                    iv_obj_name = iv_obj_name ).

    CREATE OBJECT lo_html.
    lo_html->add_anchor( iv_txt = |{ iv_obj_name }| iv_act = |{ gc_action-jump }?{ lv_encode }| ).
    rv_html = lo_html->mv_html.

  ENDMETHOD.  "render_obj_jump_link

ENDCLASS. "lcl_gui_view_repo_content