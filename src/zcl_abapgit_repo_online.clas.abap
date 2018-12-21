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

    METHODS rebuild_local_checksums
      REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS handle_stage_ignore
      IMPORTING
        !io_stage TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_ONLINE IMPLEMENTATION.


  METHOD handle_stage_ignore.

    DATA: lv_add         TYPE abap_bool,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
          lt_stage       TYPE zcl_abapgit_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lo_dot_abapgit = get_dot_abapgit( ).
    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = zcl_abapgit_stage=>c_method-ignore.

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
      AND NOT ( file-path     = zif_abapgit_definitions=>c_root_dir
      AND       file-filename = zif_abapgit_definitions=>c_dot_abapgit ).
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


  METHOD zif_abapgit_git_operations~create_branch.

    DATA: lv_sha1 TYPE zif_abapgit_definitions=>ty_sha1.

    ASSERT iv_name CP 'refs/heads/+*'.

    IF iv_from IS INITIAL.
      lv_sha1 = get_remote_branch_sha1( ).
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
        AND mv_code_inspector_successful = abap_false.
      zcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

*   moved here from get_objects, probably refactor
*   mt_objects used only here, returned with pull ...

    fetch_remote( ).

    ls_push = zcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = get_branch_name( )
      iv_url         = get_url( )
      iv_parent      = get_remote_branch_sha1( )
      it_old_objects = mt_objects ).

    set_objects( ls_push-new_objects ).
    set_files_remote( ls_push-new_files ).

    mv_branch_sha1 = ls_push-branch.

    update_local_checksums( ls_push-updated_files ).

    reset_status( ).
    CLEAR: mv_code_inspector_successful.

  ENDMETHOD.
ENDCLASS.
