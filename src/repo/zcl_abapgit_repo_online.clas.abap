CLASS zcl_abapgit_repo_online DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_online .

    ALIASES create_branch
      FOR zif_abapgit_repo_online~create_branch .
    ALIASES get_current_remote
      FOR zif_abapgit_repo_online~get_current_remote .
    ALIASES get_selected_branch
      FOR zif_abapgit_repo_online~get_selected_branch .
    ALIASES get_selected_commit
      FOR zif_abapgit_repo_online~get_selected_commit .
    ALIASES get_url
      FOR zif_abapgit_repo_online~get_url .
    ALIASES push
      FOR zif_abapgit_repo_online~push .
    ALIASES select_branch
      FOR zif_abapgit_repo_online~select_branch .
    ALIASES select_commit
      FOR zif_abapgit_repo_online~select_commit .
    ALIASES set_url
      FOR zif_abapgit_repo_online~set_url .
    ALIASES switch_origin
      FOR zif_abapgit_repo_online~switch_origin .
    ALIASES get_switched_origin
      FOR zif_abapgit_repo_online~get_switched_origin.



    METHODS zif_abapgit_repo~get_files_remote
        REDEFINITION .
    METHODS zif_abapgit_repo~get_name
        REDEFINITION .
    METHODS has_remote_source
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt .
    DATA mv_current_commit TYPE zif_abapgit_git_definitions=>ty_sha1 .

    METHODS handle_stage_ignore
      IMPORTING
        !io_stage TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception .
    METHODS set_objects
      IMPORTING
        !it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    METHODS fetch_remote
      RAISING
        zcx_abapgit_exception .
    METHODS get_objects
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    METHODS raise_error_if_branch_exists
      IMPORTING
        iv_name TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_repo_online IMPLEMENTATION.


  METHOD fetch_remote.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          ls_pull     TYPE zcl_abapgit_git_porcelain=>ty_pull_result.

    IF mv_request_remote_refresh = abap_false.
      RETURN.
    ENDIF.

    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show( iv_current = 1
                       iv_text    = 'Fetch remote files' ).

    IF get_selected_commit( ) IS INITIAL.
      ls_pull = zcl_abapgit_git_porcelain=>pull_by_branch( iv_url         = get_url( )
                                                           iv_branch_name = get_selected_branch( ) ).
    ELSE.
      ls_pull = zcl_abapgit_git_porcelain=>pull_by_commit( iv_url         = get_url( )
                                                           iv_commit_hash = get_selected_commit( ) ).
    ENDIF.

    set_files_remote( ls_pull-files ).
    set_objects( ls_pull-objects ).
    mv_current_commit = ls_pull-commit.

  ENDMETHOD.


  METHOD get_objects.
    fetch_remote( ).
    rt_objects = mt_objects.
  ENDMETHOD.


  METHOD handle_stage_ignore.

    DATA: lv_add         TYPE abap_bool,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
          lt_stage       TYPE zif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lo_dot_abapgit = get_dot_abapgit( ).
    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = zif_abapgit_definitions=>c_method-ignore.

      lo_dot_abapgit->add_ignore(
        iv_path     = <ls_stage>-file-path
        iv_filename = <ls_stage>-file-filename ).

      " remove it from the staging object, as the action is handled here
      io_stage->reset( iv_path     = <ls_stage>-file-path
                       iv_filename = <ls_stage>-file-filename ).

      lv_add = abap_true.

    ENDLOOP.

    IF lv_add = abap_true.
      io_stage->add(
        iv_path     = zif_abapgit_definitions=>c_root_dir
        iv_filename = zif_abapgit_definitions=>c_dot_abapgit
        iv_data     = lo_dot_abapgit->serialize( ) ).

      set_dot_abapgit( lo_dot_abapgit ).
    ENDIF.

  ENDMETHOD.


  METHOD has_remote_source.
    rv_yes = abap_true.
  ENDMETHOD.


  METHOD raise_error_if_branch_exists.

    DATA:
      lt_branches     TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt,
      lv_display_name TYPE string.

    lt_branches = zcl_abapgit_git_transport=>branches( get_url( ) )->get_branches_only( ).

    READ TABLE lt_branches WITH TABLE KEY name_key
                           COMPONENTS name = iv_name
                           TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_display_name = zcl_abapgit_git_branch_list=>get_display_name( iv_name ).
      zcx_abapgit_exception=>raise( |Branch '{ lv_display_name }' already exists| ).
    ENDIF.

  ENDMETHOD.


  METHOD set_objects.
    mt_objects = it_objects.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~check_for_valid_branch.

    DATA:
      lo_branch_list TYPE REF TO zcl_abapgit_git_branch_list,
      lv_branch      TYPE string,
      lv_head        TYPE string,
      lv_msg         TYPE string.

    lv_branch = get_selected_branch( ).

    IF lv_branch IS NOT INITIAL.
      lo_branch_list = zcl_abapgit_git_transport=>branches( get_url( ) ).

      TRY.
          lo_branch_list->find_by_name( lv_branch ).
        CATCH zcx_abapgit_exception.
          " branch does not exist, fallback to head
          lv_head = lo_branch_list->get_head_symref( ).
          IF lo_branch_list->get_type( lv_branch ) = zif_abapgit_definitions=>c_git_branch_type-branch.
            lv_msg = 'Branch'.
          ELSE.
            lv_msg = 'Tag'.
          ENDIF.
          lv_msg = |{ lv_msg } { lo_branch_list->get_display_name( lv_branch ) } does not exist.|
                && | Switched to { lo_branch_list->get_display_name( lv_head ) }|.
          MESSAGE lv_msg TYPE 'S'.
          select_branch( lv_head ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~create_branch.

    DATA: lv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    ASSERT iv_name CP zif_abapgit_definitions=>c_git_branch-heads.

    IF iv_from IS INITIAL.
      lv_sha1 = get_current_remote( ).
    ELSE.
      lv_sha1 = iv_from.
    ENDIF.

    raise_error_if_branch_exists( iv_name ).

    zcl_abapgit_git_porcelain=>create_branch(
      iv_url  = get_url( )
      iv_name = iv_name
      iv_from = lv_sha1 ).

    " automatically switch to new branch
    select_branch( iv_name ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_current_remote.
    fetch_remote( ).
    rv_sha1 = mv_current_commit.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_selected_branch.
    rv_name = ms_data-branch_name.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_selected_commit.
    rv_selected_commit = ms_data-selected_commit.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_switched_origin.
    rv_switched_origin = ms_data-switched_origin.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_url.
    rv_url = ms_data-url.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~push.

* assumption: PUSH is done on top of the currently selected branch

    DATA: ls_push   TYPE zcl_abapgit_git_porcelain=>ty_push_result,
          lv_text   TYPE string,
          lv_parent TYPE zif_abapgit_git_definitions=>ty_sha1.


    IF ms_data-branch_name CP zif_abapgit_definitions=>c_git_branch-tags.
      lv_text = |You're working on a tag. Currently it's not |
             && |possible to push on tags. Consider creating a branch instead|.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-selected_commit IS NOT INITIAL.
      lv_text = 'You are currently checked out in a commit.'.
      lv_text = |{ lv_text } You must be on a branch to push|.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-local_settings-block_commit = abap_true
        AND zcl_abapgit_factory=>get_code_inspector( get_package( )
          )->is_successful( ) = abap_false.
      zcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

    IF get_selected_commit( ) IS INITIAL.
      lv_parent = get_current_remote( ).
    ELSE.
      lv_parent = get_selected_commit( ).
    ENDIF.

    ls_push = zcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = get_selected_branch( )
      iv_url         = get_url( )
      iv_parent      = lv_parent
      it_old_objects = get_objects( ) ).

    set_objects( ls_push-new_objects ).
    set_files_remote( ls_push-new_files ).

    mv_current_commit = ls_push-branch.

    zif_abapgit_repo~checksums( )->update( ls_push-updated_files ).

    reset_status( ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~select_branch.

    reset_remote( ).
    set( iv_branch_name     = iv_branch_name
         iv_selected_commit = space ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~select_commit.

    reset_remote( ).
    set( iv_selected_commit = iv_selected_commit ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~set_url.

    reset_remote( ).
    set( iv_url = iv_url ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~switch_origin.

    DATA lv_offs TYPE i.

    " For repo settings page
    IF iv_overwrite = abap_true.
      set( iv_switched_origin = iv_url ).
      RETURN.
    ENDIF.

    IF iv_url IS INITIAL.
      IF ms_data-switched_origin IS INITIAL.
        RETURN.
      ELSE.
        lv_offs = find(
          val = reverse( ms_data-switched_origin )
          sub = '@' ).
        IF lv_offs = -1.
          zcx_abapgit_exception=>raise( 'Incorrect format of switched origin' ).
        ENDIF.
        lv_offs = strlen( ms_data-switched_origin ) - lv_offs - 1.
        set_url( substring(
          val = ms_data-switched_origin
          len = lv_offs ) ).
        select_branch( substring(
          val = ms_data-switched_origin
          off = lv_offs + 1 ) ).
        set( iv_switched_origin = '' ).
      ENDIF.
    ELSEIF ms_data-switched_origin IS INITIAL.
      set( iv_switched_origin = ms_data-url && '@' && ms_data-branch_name ).
      set_url( iv_url ).
      select_branch( iv_branch ).
    ELSE.
      zcx_abapgit_exception=>raise( 'Cannot switch origin twice' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_files_remote.
    fetch_remote( ).
    rt_files = super->get_files_remote(
      ii_obj_filter   = ii_obj_filter
      iv_ignore_files = iv_ignore_files ).
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_name.
    rv_name = super->get_name( ).
    IF rv_name IS INITIAL.
      TRY.
          rv_name = zcl_abapgit_url=>name( ms_data-url ).
          rv_name = cl_http_utility=>unescape_url( rv_name ).
        CATCH zcx_abapgit_exception.
          rv_name = 'New online repo'. "unlikely fallback
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
