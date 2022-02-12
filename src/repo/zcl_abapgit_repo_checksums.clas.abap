CLASS zcl_abapgit_repo_checksums DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_checksums.

    METHODS constructor
      IMPORTING
        !iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_local_files_by_item_tt TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_file_item WITH NON-UNIQUE KEY item.

    DATA mv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key.

    METHODS remove_non_code_related_files
      CHANGING
        !ct_local_files TYPE ty_local_files_by_item_tt.

    METHODS build_checksums_from_files
      IMPORTING
        it_remote    TYPE zif_abapgit_definitions=>ty_files_tt
        it_local     TYPE ty_local_files_by_item_tt
        iv_branches_equal TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    METHODS save_checksums
      IMPORTING
        it_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_repo_checksums IMPLEMENTATION.

  METHOD constructor.
    ASSERT iv_repo_key IS NOT INITIAL.
    mv_repo_key = iv_repo_key.
  ENDMETHOD.

  METHOD build_checksums_from_files.

    DATA ls_last_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS:
      <ls_checksum> LIKE LINE OF rt_checksums,
      <ls_local>    LIKE LINE OF it_local.

    FIELD-SYMBOLS:
      <ls_remote_file> LIKE LINE OF it_remote,
      <ls_cs_file_sig> LIKE LINE OF <ls_checksum>-files.

    LOOP AT it_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO rt_checksums ASSIGNING <ls_checksum>.
        MOVE-CORRESPONDING <ls_local>-item TO <ls_checksum>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      READ TABLE it_remote ASSIGNING <ls_remote_file>
        WITH TABLE KEY file_path
        COMPONENTS
          path     = <ls_local>-file-path
          filename = <ls_local>-file-filename.
      IF sy-subrc <> 0.  " Ignore new local ones
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_cs_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_cs_file_sig>.

      " If hashes are equal -> local sha1 is OK already (no change)
      " Else
      "   if branches equal -> assume changes were local, state - remote sha1
      "   if remote branch is ahead (not equal) -> assume changes were remote, state - local sha1 (no change)
      IF <ls_local>-file-sha1 <> <ls_remote_file>-sha1 AND iv_branches_equal = abap_true.
        <ls_cs_file_sig>-sha1 = <ls_remote_file>-sha1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD remove_non_code_related_files.

    DELETE ct_local_files
      WHERE item IS INITIAL
      AND NOT ( file-path = zif_abapgit_definitions=>c_root_dir
      AND file-filename = zif_abapgit_definitions=>c_dot_abapgit ).

  ENDMETHOD.

  METHOD zif_abapgit_repo_checksums~get.

    DATA lv_cs_blob TYPE string.

    TRY.
        lv_cs_blob = zcl_abapgit_persist_factory=>get_repo_cs( )->read( iv_key = mv_repo_key ).
      CATCH zcx_abapgit_exception zcx_abapgit_not_found.
        " Ignore currently, it's not critical for execution, just return empty
        RETURN.
    ENDTRY.

    IF lv_cs_blob IS NOT INITIAL.
      rt_checksums = lcl_checksum_serializer=>deserialize( lv_cs_blob ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_repo_checksums~rebuild.

    DATA lt_remote    TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA lt_local     TYPE ty_local_files_by_item_tt.
    DATA lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA li_repo      TYPE REF TO zif_abapgit_repo.

    li_repo = zcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
    " Should be safe as repo_srv is supposed to be single source of repo instances

    lt_local  = li_repo->get_files_local( ).
    lt_remote = li_repo->get_files_remote( ).

    remove_non_code_related_files( CHANGING ct_local_files = lt_local ).

    lt_checksums = build_checksums_from_files(
      it_remote         = lt_remote
      it_local          = lt_local
      iv_branches_equal = iv_branches_equal ).

    save_checksums( lt_checksums ).

  ENDMETHOD.

  METHOD zif_abapgit_repo_checksums~update.

    DATA lt_checksums   TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lt_local_files TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA li_repo        TYPE REF TO zif_abapgit_repo.

    li_repo        = zcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
    lt_checksums   = zif_abapgit_repo_checksums~get( ).
    lt_local_files = li_repo->get_files_local( ).

    lt_checksums = lcl_update_calculator=>calculate_updated(
      it_current_checksums = lt_checksums
      it_local_files       = lt_local_files
      it_updated_files     = it_updated_files ).

    save_checksums( lt_checksums ).

  ENDMETHOD.

  METHOD save_checksums.

    DATA lv_cs_blob TYPE string.

    lv_cs_blob = lcl_checksum_serializer=>serialize( it_checksums ).
    zcl_abapgit_persist_factory=>get_repo_cs( )->update(
      iv_key     = mv_repo_key
      iv_cs_blob = lv_cs_blob ).

  ENDMETHOD.

ENDCLASS.
