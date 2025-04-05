CLASS zcl_abapgit_repo_online DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_online .

    METHODS zif_abapgit_repo~get_files_remote
        REDEFINITION .
    METHODS zif_abapgit_repo~get_name
        REDEFINITION .
    METHODS zif_abapgit_repo~has_remote_source
        REDEFINITION .
    METHODS constructor
      IMPORTING
        is_data TYPE zif_abapgit_persistence=>ty_repo.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt .
    DATA mv_current_commit TYPE zif_abapgit_git_definitions=>ty_sha1 .
    DATA mi_repo_online TYPE REF TO zif_abapgit_repo_online .

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

  METHOD constructor.

    super->constructor( is_data ).
    mi_repo_online = me.

  ENDMETHOD.

  METHOD fetch_remote.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          ls_pull     TYPE zcl_abapgit_git_porcelain=>ty_pull_result.

    IF mv_request_remote_refresh = abap_false.
      RETURN.
    ENDIF.

    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show( iv_current = 1
                       iv_text    = 'Fetch remote files' ).

    IF mi_repo_online->get_selected_commit( ) IS INITIAL.
      ls_pull = zcl_abapgit_git_porcelain=>pull_by_branch( iv_url         = mi_repo_online->get_url( )
                                                           iv_branch_name = mi_repo_online->get_selected_branch( ) ).
    ELSE.
      ls_pull = zcl_abapgit_git_porcelain=>pull_by_commit( iv_url         = mi_repo_online->get_url( )
                                                           iv_commit_hash = mi_repo_online->get_selected_commit( ) ).
    ENDIF.

    mi_repo->set_files_remote( ls_pull-files ).
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


    lo_dot_abapgit = mi_repo->get_dot_abapgit( ).
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

      mi_repo->set_dot_abapgit( lo_dot_abapgit ).
    ENDIF.

  ENDMETHOD.


  METHOD raise_error_if_branch_exists.

    DATA:
      lt_branches     TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt,
      lv_display_name TYPE string.

    lt_branches = zcl_abapgit_git_factory=>get_git_transport(
                                        )->branches( mi_repo_online->get_url( )
                                        )->get_branches_only( ).

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
      lx_error       TYPE REF TO zcx_abapgit_exception,
      lv_branch      TYPE string,
      lv_head        TYPE string,
      lv_msg         TYPE string.

    lv_branch = mi_repo_online->get_selected_branch( ).

    IF lv_branch IS NOT INITIAL.
      lo_branch_list = zcl_abapgit_git_factory=>get_git_transport( )->branches( mi_repo_online->get_url( ) ).

      TRY.
          lo_branch_list->find_by_name( lv_branch ).
        CATCH zcx_abapgit_exception INTO lx_error.
          " branch does not exist, fallback to head
          lv_head = lo_branch_list->get_head_symref( ).
          lv_msg = |{ lx_error->get_text( ) }. Switched to { lo_branch_list->get_display_name( lv_head ) }|.
          MESSAGE lv_msg TYPE 'S'.
          mi_repo_online->select_branch( lv_head ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~create_branch.

    DATA: lv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    ASSERT iv_name CP zif_abapgit_git_definitions=>c_git_branch-heads.

    IF iv_from IS INITIAL.
      lv_sha1 = mi_repo_online->get_current_remote( ).
    ELSE.
      lv_sha1 = iv_from.
    ENDIF.

    raise_error_if_branch_exists( iv_name ).

    zcl_abapgit_git_porcelain=>create_branch(
      iv_url  = mi_repo_online->get_url( )
      iv_name = iv_name
      iv_from = lv_sha1 ).

    " automatically switch to new branch
    mi_repo_online->select_branch( iv_name ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_current_remote.
    fetch_remote( ).
    rv_sha1 = mv_current_commit.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_selected_branch.
    rv_name = mi_repo->ms_data-branch_name.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_selected_commit.
    rv_selected_commit = mi_repo->ms_data-selected_commit.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_switched_origin.
    rv_switched_origin = mi_repo->ms_data-switched_origin.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~get_url.
    rv_url = mi_repo->ms_data-url.
  ENDMETHOD.


  METHOD zif_abapgit_repo_online~push.

* assumption: PUSH is done on top of the currently selected branch

    DATA: ls_push   TYPE zcl_abapgit_git_porcelain=>ty_push_result,
          lv_text   TYPE string,
          lv_parent TYPE zif_abapgit_git_definitions=>ty_sha1.


    IF mi_repo->ms_data-branch_name CP zif_abapgit_git_definitions=>c_git_branch-tags.
      lv_text = |You're working on a tag. Currently it's not |
             && |possible to push on tags. Consider creating a branch instead|.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF mi_repo->ms_data-selected_commit IS NOT INITIAL.
      lv_text = 'You are currently checked out in a commit.'.
      lv_text = |{ lv_text } You must be on a branch to push|.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF mi_repo->ms_data-local_settings-block_commit = abap_true
        AND zcl_abapgit_code_inspector=>get_code_inspector( mi_repo->get_package( )
          )->is_successful( ) = abap_false.
      zcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

    IF mi_repo_online->get_selected_commit( ) IS INITIAL.
      lv_parent = mi_repo_online->get_current_remote( ).
    ELSE.
      lv_parent = mi_repo_online->get_selected_commit( ).
    ENDIF.

    ls_push = zcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = mi_repo_online->get_selected_branch( )
      iv_url         = mi_repo_online->get_url( )
      iv_parent      = lv_parent
      it_old_objects = get_objects( ) ).

    set_objects( ls_push-new_objects ).
    mi_repo->set_files_remote( ls_push-new_files ).

    mv_current_commit = ls_push-branch.

    mi_repo->checksums( )->update( ls_push-updated_files ).

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
      IF mi_repo->ms_data-switched_origin IS INITIAL.
        RETURN.
      ELSE.
        lv_offs = find(
          val = reverse( mi_repo->ms_data-switched_origin )
          sub = '@' ).
        IF lv_offs = -1.
          zcx_abapgit_exception=>raise( 'Incorrect format of switched origin' ).
        ENDIF.
        lv_offs = strlen( mi_repo->ms_data-switched_origin ) - lv_offs - 1.
        mi_repo_online->set_url( substring(
          val = mi_repo->ms_data-switched_origin
          len = lv_offs ) ).
        mi_repo_online->select_branch( substring(
          val = mi_repo->ms_data-switched_origin
          off = lv_offs + 1 ) ).
        set( iv_switched_origin = '' ).
      ENDIF.
    ELSEIF mi_repo->ms_data-switched_origin IS INITIAL.
      set( iv_switched_origin = mi_repo->ms_data-url && '@' && mi_repo->ms_data-branch_name ).
      mi_repo_online->set_url( iv_url ).
      mi_repo_online->select_branch( iv_branch ).
    ELSE.
      zcx_abapgit_exception=>raise( 'Cannot switch origin twice' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_files_remote.
    fetch_remote( ).
    rt_files = super->zif_abapgit_repo~get_files_remote(
      ii_obj_filter   = ii_obj_filter
      iv_ignore_files = iv_ignore_files ).
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_name.
    rv_name = super->zif_abapgit_repo~get_name( ).
    IF rv_name IS INITIAL.
      TRY.
          rv_name = zcl_abapgit_url=>name( mi_repo->ms_data-url ).
          rv_name = cl_http_utility=>unescape_url( rv_name ).
        CATCH zcx_abapgit_exception.
          rv_name = 'New online repo'. "unlikely fallback
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_repo~has_remote_source.
    rv_yes = abap_true.
  ENDMETHOD.
ENDCLASS.
