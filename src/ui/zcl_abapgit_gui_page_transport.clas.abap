"! GUI - Transport Overview page
CLASS zcl_abapgit_gui_page_transport DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_page
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        jump_all_transports TYPE string VALUE 'jump_all_transports',
        create_branch       TYPE string VALUE 'create_branch',
        commit_transport    TYPE string VALUE 'commit_transport',
        revert_transport    TYPE string VALUE 'revert_transport',
        refresh             TYPE string VALUE 'refresh',
      END OF c_action.

    METHODS:
      constructor IMPORTING io_repo TYPE REF TO zcl_abapgit_repo_online
                  RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_event_handler~on_event REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.
  PRIVATE SECTION.
    METHODS:
      create_branch_for_transport IMPORTING iv_transport             TYPE trkorr
                                  RETURNING VALUE(rv_created_branch) TYPE string
                                  RAISING   zcx_abapgit_cancel
                                            zcx_abapgit_exception,
      build_menu RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar
                 RAISING   zcx_abapgit_exception.
    DATA:
      mo_repo_content TYPE REF TO zcl_abapgit_gui_view_repo,
      mo_repo         TYPE REF TO zcl_abapgit_repo_online.
ENDCLASS.



CLASS zcl_abapgit_gui_page_transport IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    CREATE OBJECT mo_repo_content
      EXPORTING
        iv_key          = io_repo->get_key( )
        iv_display_mode = zcl_abapgit_gui_view_repo=>c_display_modes-transports.

    mo_repo   = io_repo.

    ms_control-page_title = 'Transport Overview'.
    ms_control-page_menu = build_menu( ).
  ENDMETHOD.

  METHOD render_content.
    CREATE OBJECT ro_html.
    ro_html->add( mo_repo_content->render( ) ).
  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.
    DATA: lv_branch TYPE string.

    CASE iv_action.
      WHEN c_action-create_branch.
        lv_branch = create_branch_for_transport( CONV #( iv_getdata ) ).
        MESSAGE |Created branch '{ lv_branch }'.| TYPE 'S'.
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-commit_transport.
        mo_repo->set_branch_name( zcl_abapgit_git_branch_list=>complete_heads_branch_name( iv_getdata ) ).
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_stage
          EXPORTING
            io_repo                = mo_repo
            iv_filter_by_transport = CONV #( iv_getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.

      WHEN c_action-revert_transport.

      WHEN c_action-refresh.
        mo_repo->refresh( ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.
  ENDMETHOD.

  METHOD create_branch_for_transport.
    DATA: lv_current_branch TYPE zif_abapgit_persistence=>ty_repo-branch_name,
          lv_new_branch     TYPE string,
          lv_answer         TYPE char1.

    lv_current_branch = mo_repo->get_branch_name( ).
    lv_new_branch = to_upper( iv_transport ). " branch name = transport request for now
    IF lv_new_branch IS INITIAL.
      zcx_abapgit_exception=>raise( 'Could not determine branch name for transport.' ).
    ENDIF.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Continue?'
      iv_text_question = |A new branch '{ lv_new_branch }' will be created based on 'master'. Continue?| ).

    IF lv_answer <> '1'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    " Not sure how to get the SHA1 of master here for iv_from
    mo_repo->set_branch_name( zcl_abapgit_git_branch_list=>complete_heads_branch_name( 'master' ) ) ##TODO.
    mo_repo->create_branch( iv_name = zcl_abapgit_git_branch_list=>complete_heads_branch_name( lv_new_branch ) ).
*    iv_from = CONV #( zcl_abapgit_git_branch_list=>complete_heads_branch_name( 'master' ) ) )

    mo_repo->set_branch_name( lv_current_branch ).

    rv_created_branch = lv_new_branch.
  ENDMETHOD.

  METHOD build_menu.
    CREATE OBJECT ro_menu.
    ro_menu->add( iv_txt = 'Refresh'
                  iv_act = c_action-refresh ).
  ENDMETHOD.
ENDCLASS.
