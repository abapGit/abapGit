CLASS zcl_abapgit_repo_online DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_git_operations .

    ALIASES create_branch
      FOR zif_abapgit_git_operations~create_branch .

    METHODS rebuild_local_checksums
      REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_ONLINE IMPLEMENTATION.


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

  ENDMETHOD.
ENDCLASS.
