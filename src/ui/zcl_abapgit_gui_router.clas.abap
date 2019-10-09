CLASS zcl_abapgit_gui_router DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_event_data,
        action    TYPE string,
        prev_page TYPE string,
        getdata   TYPE string,
        postdata  TYPE cnht_post_data_tab,
      END OF ty_event_data .

    METHODS general_page_routing
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS abapgit_services_actions
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS db_actions
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS file_download
      IMPORTING
        !iv_package TYPE devclass
        !iv_xstr    TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS git_services
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS remote_origin_manipulations
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS sap_gui_actions
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS zip_services
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS repository_services
      IMPORTING
        !is_event_data TYPE ty_event_data
      EXPORTING
        !ei_page       TYPE REF TO zif_abapgit_gui_renderable
        !ev_state      TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS get_page_diff
      IMPORTING
        !iv_getdata    TYPE clike
        !iv_prev_page  TYPE clike
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS get_page_branch_overview
      IMPORTING
        !iv_getdata    TYPE clike
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS get_page_stage
      IMPORTING
        !iv_getdata    TYPE clike
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS get_page_background
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS get_page_playground
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS jump_display_transport
      IMPORTING
        !iv_getdata TYPE clike
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_ROUTER IMPLEMENTATION.


  METHOD abapgit_services_actions.

    CASE is_event_data-action.
        " ABAPGIT services actions
      WHEN zif_abapgit_definitions=>c_action-abapgit_home.                    " Go abapGit homepage
        zcl_abapgit_services_abapgit=>open_abapgit_homepage( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-abapgit_install.                 " Install abapGit
        zcl_abapgit_services_abapgit=>install_abapgit( ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD db_actions.

    CASE is_event_data-action.
        " DB actions
      WHEN zif_abapgit_definitions=>c_action-db_edit.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_db_edit
          EXPORTING
            is_key = zcl_abapgit_html_action_utils=>dbkey_decode( is_event_data-getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
        IF is_event_data-prev_page = 'PAGE_DB_DIS'.
          ev_state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
        ENDIF.
      WHEN zif_abapgit_definitions=>c_action-db_display.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_db_dis
          EXPORTING
            is_key = zcl_abapgit_html_action_utils=>dbkey_decode( is_event_data-getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.


  METHOD file_download.

    DATA:
      lv_path    TYPE string,
      lv_default TYPE string,
      lo_fe_serv TYPE REF TO zif_abapgit_frontend_services,
      lv_package TYPE devclass.

    lv_package = iv_package.
    TRANSLATE lv_package USING '/#'.
    CONCATENATE lv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    lo_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = lo_fe_serv->show_file_save_dialog(
      iv_title            = 'Export ZIP'
      iv_extension        = 'zip'
      iv_default_filename = lv_default ).

    lo_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = iv_xstr ).

  ENDMETHOD.


  METHOD general_page_routing.

    DATA: lv_key TYPE zif_abapgit_persistence=>ty_repo-key.


    lv_key = is_event_data-getdata. " TODO refactor

    CASE is_event_data-action.
        " General PAGE routing
      WHEN zcl_abapgit_gui=>c_action-go_home.                          " Go Main page
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_main.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_explore.                     " Go Explore page
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_explore.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_repo_overview.               " Go Repository overview
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_repo_over.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_db.                          " Go DB util page
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_db.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_debuginfo.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_debuginfo.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_settings.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_settings.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_background_run.              " Go background run page
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_bkg_run.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_background.                   " Go Background page
        ei_page  = get_page_background( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_diff.                         " Go Diff page
        ei_page  = get_page_diff(
          iv_getdata   = is_event_data-getdata
          iv_prev_page = is_event_data-prev_page ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
      WHEN zif_abapgit_definitions=>c_action-go_stage.                        " Go Staging page
        ei_page  = get_page_stage( is_event_data-getdata ).
        IF is_event_data-prev_page = 'PAGE_DIFF'.
          ev_state = zcl_abapgit_gui=>c_event_state-new_page.
        ELSE.
          ev_state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
        ENDIF.
      WHEN zif_abapgit_definitions=>c_action-go_branch_overview.              " Go repo branch overview
        ei_page  = get_page_branch_overview( is_event_data-getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_playground.                   " Create playground page
        ei_page  = get_page_playground( ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_tutorial.                     " Go to tutorial
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( '' ).        " Clear show_id
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.          " Assume we are on main page
    ENDCASE.

  ENDMETHOD.


  METHOD get_page_background.

    CREATE OBJECT ri_page TYPE zcl_abapgit_gui_page_bkg
      EXPORTING
        iv_key = iv_key.

  ENDMETHOD.


  METHOD get_page_branch_overview.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          lo_page TYPE REF TO zcl_abapgit_gui_page_boverview,
          lv_key  TYPE zif_abapgit_persistence=>ty_repo-key.


    lv_key = iv_getdata.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

    CREATE OBJECT lo_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_page.

  ENDMETHOD.


  METHOD get_page_diff.

    DATA: ls_file   TYPE zif_abapgit_definitions=>ty_file,
          ls_object TYPE zif_abapgit_definitions=>ty_item,
          lo_page   TYPE REF TO zcl_abapgit_gui_page_diff,
          lv_key    TYPE zif_abapgit_persistence=>ty_repo-key.


    zcl_abapgit_html_action_utils=>file_obj_decode(
      EXPORTING
        iv_string = iv_getdata
      IMPORTING
        ev_key    = lv_key
        eg_file   = ls_file
        eg_object = ls_object ).

    CREATE OBJECT lo_page
      EXPORTING
        iv_key    = lv_key
        is_file   = ls_file
        is_object = ls_object.

    ri_page = lo_page.

  ENDMETHOD.


  METHOD get_page_playground.
    DATA: lv_class_name TYPE string,
          lv_cancel     TYPE abap_bool,
          li_popups     TYPE REF TO zif_abapgit_popups.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->run_page_class_popup(
      IMPORTING
        ev_name   = lv_class_name
        ev_cancel = lv_cancel ).

    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_class_name).
      CATCH cx_sy_create_object_error.
        zcx_abapgit_exception=>raise( |Cannot create page class { lv_class_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_page_stage.

    DATA: lo_repo                TYPE REF TO zcl_abapgit_repo_online,
          lv_key                 TYPE zif_abapgit_persistence=>ty_repo-key,
          lv_seed                TYPE string,
          lo_stage_page          TYPE REF TO zcl_abapgit_gui_page_stage,
          lo_code_inspector_page TYPE REF TO zcl_abapgit_gui_page_code_insp.

    FIND FIRST OCCURRENCE OF '=' IN iv_getdata.
    IF sy-subrc <> 0. " Not found ? -> just repo key in params
      lv_key = iv_getdata.
    ELSE.
      zcl_abapgit_html_action_utils=>stage_decode(
        EXPORTING iv_getdata = iv_getdata
        IMPORTING ev_key     = lv_key
                  ev_seed    = lv_seed ).
    ENDIF.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

    IF lo_repo->get_local_settings( )-code_inspector_check_variant IS NOT INITIAL.

      CREATE OBJECT lo_code_inspector_page
        EXPORTING
          io_repo = lo_repo.

      ri_page = lo_code_inspector_page.

    ELSE.

      " force refresh on stage, to make sure the latest local and remote files are used
      lo_repo->refresh( ).

      CREATE OBJECT lo_stage_page
        EXPORTING
          io_repo = lo_repo
          iv_seed = lv_seed.

      ri_page = lo_stage_page.

    ENDIF.

  ENDMETHOD.


  METHOD git_services.

    DATA: lv_key TYPE zif_abapgit_persistence=>ty_repo-key.


    lv_key = is_event_data-getdata. " TODO refactor

    CASE is_event_data-action.
        " GIT actions
      WHEN zif_abapgit_definitions=>c_action-git_pull.                      " GIT Pull
        zcl_abapgit_services_git=>pull( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_reset.                     " GIT Reset
        zcl_abapgit_services_git=>reset( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_create.             " GIT Create new branch
        zcl_abapgit_services_git=>create_branch( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_delete.             " GIT Delete remote branch
        zcl_abapgit_services_git=>delete_branch( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_switch.             " GIT Switch branch
        zcl_abapgit_services_git=>switch_branch( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-go_tag_overview.               " GIT Tag overview
        zcl_abapgit_services_git=>tag_overview( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_tag_create.                " GIT Tag create
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_tag
          EXPORTING
            io_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-git_tag_delete.                " GIT Tag create
        zcl_abapgit_services_git=>delete_tag( lv_key ).
        zcl_abapgit_services_repo=>refresh( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_tag_switch.                " GIT Switch Tag
        zcl_abapgit_services_git=>switch_tag( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD jump_display_transport.

    DATA: lv_transport         TYPE trkorr,
          lv_transport_adt_uri TYPE string,
          lv_adt_link          TYPE string,
          lv_adt_jump_enabled  TYPE abap_bool.

    lv_transport = iv_getdata.

    lv_adt_jump_enabled = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_adt_jump_enabled( ).
    IF lv_adt_jump_enabled = abap_true.
      TRY.
          CALL METHOD ('CL_CTS_ADT_TM_URI_BUILDER')=>('CREATE_ADT_URI')
            EXPORTING
              trnumber = lv_transport
            RECEIVING
              result   = lv_transport_adt_uri.
          lv_adt_link = |adt://{ sy-sysid }{ lv_transport_adt_uri }|.

          cl_gui_frontend_services=>execute( EXPORTING  document = lv_adt_link
                                             EXCEPTIONS OTHERS   = 1 ).
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
          ENDIF.
        CATCH cx_root.
          CALL FUNCTION 'TR_DISPLAY_REQUEST'
            EXPORTING
              i_trkorr = lv_transport.
      ENDTRY.
    ELSE.
      CALL FUNCTION 'TR_DISPLAY_REQUEST'
        EXPORTING
          i_trkorr = lv_transport.
    ENDIF.

  ENDMETHOD.


  METHOD remote_origin_manipulations.

    DATA: lv_key TYPE zif_abapgit_persistence=>ty_repo-key.


    lv_key = is_event_data-getdata. " TODO refactor

    CASE is_event_data-action.
        " Remote ORIGIN manipulations
      WHEN zif_abapgit_definitions=>c_action-repo_remote_attach.            " Remote attach
        zcl_abapgit_services_repo=>remote_attach( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_remote_detach.            " Remote detach
        zcl_abapgit_services_repo=>remote_detach( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_remote_change.            " Remote change
        zcl_abapgit_services_repo=>remote_change( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD repository_services.

    DATA: lv_url TYPE string,
          lv_key TYPE zif_abapgit_persistence=>ty_repo-key,
          li_log TYPE REF TO zif_abapgit_log.

    lv_key = is_event_data-getdata. " TODO refactor
    lv_url = is_event_data-getdata. " TODO refactor

    CASE is_event_data-action.
        " REPOSITORY services actions
      WHEN zif_abapgit_definitions=>c_action-repo_newoffline.                 " New offline repo
        zcl_abapgit_services_repo=>new_offline( ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_add_all_obj_to_trans_req.
        zcl_abapgit_transport=>add_all_objects_to_trans_req( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_refresh.                    " Repo refresh
        zcl_abapgit_services_repo=>refresh( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_syntax_check.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_syntax
          EXPORTING
            io_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_code_inspector.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_code_insp
          EXPORTING
            io_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_purge.                      " Repo remove & purge all objects
        zcl_abapgit_services_repo=>purge( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_remove.                     " Repo remove
        zcl_abapgit_services_repo=>remove( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_newonline.
        zcl_abapgit_services_repo=>new_online( lv_url ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN 'install'.    " 'install' is for explore page
        zcl_abapgit_services_repo=>new_online( lv_url ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_refresh_checksums.          " Rebuild local checksums
        zcl_abapgit_services_repo=>refresh_local_checksums( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_toggle_fav.                 " Toggle repo as favorite
        zcl_abapgit_services_repo=>toggle_favorite( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_transport_to_branch.
        zcl_abapgit_services_repo=>transport_to_branch( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_settings.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_repo_sett
          EXPORTING
            io_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_log.
        li_log = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->get_log( ).
        zcl_abapgit_log_viewer=>show_log( ii_log = li_log iv_header_text = li_log->get_title( ) ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.


  METHOD sap_gui_actions.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    CASE is_event_data-action.
        " SAP GUI actions
      WHEN zif_abapgit_definitions=>c_action-jump.                          " Open object editor
        zcl_abapgit_html_action_utils=>jump_decode(
          EXPORTING iv_string   = is_event_data-getdata
          IMPORTING ev_obj_type = ls_item-obj_type
                    ev_obj_name = ls_item-obj_name ).
        zcl_abapgit_objects=>jump( ls_item ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-jump_transport.
        jump_display_transport( is_event_data-getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: ls_event_data TYPE ty_event_data.

    ls_event_data-action    = iv_action.
    ls_event_data-prev_page = iv_prev_page.
    ls_event_data-getdata   = iv_getdata.
    ls_event_data-postdata  = it_postdata.


    general_page_routing(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    repository_services(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    git_services(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    zip_services(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    db_actions(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    abapgit_services_actions(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    remote_origin_manipulations(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    sap_gui_actions(
      EXPORTING
        is_event_data = ls_event_data
      IMPORTING
        ei_page      = ei_page
        ev_state     = ev_state ).

    IF ev_state IS INITIAL.
      ev_state = zcl_abapgit_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.


  METHOD zip_services.

    DATA: lv_key     TYPE zif_abapgit_persistence=>ty_repo-key,
          lo_repo    TYPE REF TO zcl_abapgit_repo,
          lv_package TYPE devclass,
          lv_path    TYPE string,
          lv_xstr    TYPE xstring.


    lv_key = is_event_data-getdata. " TODO refactor

    CASE is_event_data-action.
        " ZIP services actions
      WHEN zif_abapgit_definitions=>c_action-zip_import.                      " Import repo from ZIP
        lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        lv_path = zcl_abapgit_ui_factory=>get_frontend_services( )->show_file_open_dialog(
          iv_title            = 'Import ZIP'
          iv_default_filename = '*.zip' ).
        lv_xstr = zcl_abapgit_ui_factory=>get_frontend_services( )->file_upload( lv_path ).
        lo_repo->set_files_remote( zcl_abapgit_zip=>load( lv_xstr ) ).
        zcl_abapgit_services_repo=>refresh( lv_key ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-zip_export.                      " Export repo as ZIP
        lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        lv_xstr = zcl_abapgit_zip=>export( lo_repo ).
        file_download( iv_package = lo_repo->get_package( )
                       iv_xstr    = lv_xstr ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-zip_package.                     " Export package as ZIP
        zcl_abapgit_zip=>export_package( IMPORTING
          ev_xstr    = lv_xstr
          ev_package = lv_package ).
        file_download( iv_package = lv_package
                       iv_xstr    = lv_xstr ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-zip_transport.                   " Export transports as ZIP
        zcl_abapgit_transport_mass=>run( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-zip_object.                      " Export object as ZIP
        zcl_abapgit_zip=>export_object( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
