CLASS zcl_abapgit_repo_online DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_git_operations .

    ALIASES create_branch
      FOR zif_abapgit_git_operations~create_branch .
    ALIASES push
      FOR zif_abapgit_git_operations~push .

    METHODS get_url
      RETURNING
        VALUE(rv_url) TYPE zif_abapgit_persistence=>ty_repo-url .
    METHODS get_branch_name
      RETURNING
        VALUE(rv_name) TYPE zif_abapgit_persistence=>ty_repo-branch_name .
    METHODS set_url
      IMPORTING
        !iv_url TYPE zif_abapgit_persistence=>ty_repo-url
      RAISING
        zcx_abapgit_exception .
    METHODS set_branch_name
      IMPORTING
        !iv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name
      RAISING
        zcx_abapgit_exception .
    METHODS get_sha1_remote
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    METHODS get_objects
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .

    METHODS get_files_remote
        REDEFINITION .
    METHODS get_name
        REDEFINITION .
    METHODS has_remote_source
        REDEFINITION .
    METHODS rebuild_local_checksums
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt .
    DATA mv_branch TYPE zif_abapgit_definitions=>ty_sha1 .

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
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_ONLINE IMPLEMENTATION.


  METHOD fetch_remote.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          ls_pull     TYPE zcl_abapgit_git_porcelain=>ty_pull_result.

    IF mv_request_remote_refresh = abap_false.
      RETURN.
    ENDIF.

    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show( iv_current = 1
                       iv_text    = 'Fetch remote files' ) ##NO_TEXT.

    ls_pull = zcl_abapgit_git_porcelain=>pull(
      iv_url         = get_url( )
      iv_branch_name = get_branch_name( ) ).

    set_files_remote( ls_pull-files ).
    set_objects( ls_pull-objects ).
    mv_branch = ls_pull-branch.

  ENDMETHOD.


  METHOD get_branch_name.
    rv_name = ms_data-branch_name.
  ENDMETHOD.


  METHOD get_files_remote.
    fetch_remote( ).
    rt_files = super->get_files_remote( ).
  ENDMETHOD.


  METHOD get_name.
    rv_name = super->get_name( ).
    IF rv_name IS INITIAL.
      rv_name = zcl_abapgit_url=>name( ms_data-url ).
      rv_name = cl_http_utility=>if_http_utility~unescape_url( rv_name ).
    ENDIF.
  ENDMETHOD.


  METHOD get_objects.
    fetch_remote( ).
    rt_objects = mt_objects.
  ENDMETHOD.


  METHOD get_sha1_remote.
    fetch_remote( ).
    rv_sha1 = mv_branch.
  ENDMETHOD.


  METHOD get_url.
    rv_url = ms_data-url.
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


  METHOD rebuild_local_checksums.

    " TODO: method unify to base class !

    DATA:
      lt_remote    TYPE zif_abapgit_definitions=>ty_files_tt,
      lt_local     TYPE zif_abapgit_definitions=>ty_files_item_tt,
      ls_last_item TYPE zif_abapgit_definitions=>ty_item,
      lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    FIELD-SYMBOLS:
      <ls_checksum> LIKE LINE OF lt_checksums,
      <ls_file_sig> LIKE LINE OF <ls_checksum>-files,
      <ls_remote>   LIKE LINE OF lt_remote,
      <ls_local>    LIKE LINE OF lt_local.

    lt_local  = get_files_local( ).

    DELETE lt_local " Remove non-code related files except .abapgit
      WHERE item IS INITIAL
      AND NOT ( file-path = zif_abapgit_definitions=>c_root_dir
      AND file-filename = zif_abapgit_definitions=>c_dot_abapgit ).
    SORT lt_local BY item.

    lt_remote = get_files_remote( ).
    SORT lt_remote BY path filename.

    LOOP AT lt_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_checksum>.
        <ls_checksum>-item = <ls_local>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      READ TABLE lt_remote ASSIGNING <ls_remote>
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      CHECK sy-subrc = 0.  " Ignore new local ones

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_file_sig>.

      " If hashes are equal -> local sha1 is OK
      " Else if R-branch is ahead  -> assume changes were remote, state - local sha1
      "      Else (branches equal) -> assume changes were local, state - remote sha1
      IF <ls_local>-file-sha1 <> <ls_remote>-sha1.
        <ls_file_sig>-sha1 = <ls_remote>-sha1.
      ENDIF.
    ENDLOOP.

    set( it_checksums = lt_checksums ).
    reset_status( ).

  ENDMETHOD.


  METHOD set_branch_name.

    reset_remote( ).
    set( iv_branch_name = iv_branch_name ).

  ENDMETHOD.


  METHOD set_objects.
    mt_objects = it_objects.
  ENDMETHOD.


  METHOD set_url.

    reset_remote( ).
    set( iv_url = iv_url ).

  ENDMETHOD.


  METHOD zif_abapgit_git_operations~create_branch.

    DATA: lv_sha1 TYPE zif_abapgit_definitions=>ty_sha1.

    ASSERT iv_name CP 'refs/heads/+*'.

    IF iv_from IS INITIAL.
      lv_sha1 = get_sha1_remote( ).
    ELSE.
      lv_sha1 = iv_from.
    ENDIF.

    zcl_abapgit_git_porcelain=>create_branch(
      iv_url  = get_url( )
      iv_name = iv_name
      iv_from = lv_sha1 ).

    " automatically switch to new branch
    set_branch_name( iv_name ).

  ENDMETHOD.


  METHOD zif_abapgit_git_operations~push.

* assumption: PUSH is done on top of the currently selected branch

    DATA: ls_push TYPE zcl_abapgit_git_porcelain=>ty_push_result,
          lv_text TYPE string.


    IF ms_data-branch_name CP 'refs/tags*'.
      lv_text = |You're working on a tag. Currently it's not |
             && |possible to push on tags. Consider creating a branch instead|.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-local_settings-block_commit = abap_true
        AND zcl_abapgit_factory=>get_code_inspector( get_package( )
          )->is_successful( ) = abap_false.
      zcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

    ls_push = zcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = get_branch_name( )
      iv_url         = get_url( )
      iv_parent      = get_sha1_remote( )
      it_old_objects = get_objects( ) ).

    set_objects( ls_push-new_objects ).
    set_files_remote( ls_push-new_files ).

    mv_branch = ls_push-branch.

    update_local_checksums( ls_push-updated_files ).

    reset_status( ).

  ENDMETHOD.
ENDCLASS.
