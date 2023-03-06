CLASS zcl_abapgit_repo_checksums DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_checksums.

    METHODS constructor
      IMPORTING
        !iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.

    METHODS force_write
      IMPORTING
        it_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_local_files_by_item_tt TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_file_item WITH NON-UNIQUE KEY item.

    DATA mv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key.
    DATA mi_repo TYPE REF TO zif_abapgit_repo.

    METHODS remove_non_code_related_files
      CHANGING
        !ct_local_files TYPE ty_local_files_by_item_tt.

    METHODS build_checksums_from_files
      IMPORTING
        it_local     TYPE ty_local_files_by_item_tt
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    METHODS save_checksums
      IMPORTING
        it_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt
      RAISING
        zcx_abapgit_exception.

    METHODS add_meta
      CHANGING
        cv_cs_blob TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS extract_meta
      CHANGING
*        co_string_map - return string map with meta when it is needed
        cv_cs_blob TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_CHECKSUMS IMPLEMENTATION.


  METHOD add_meta.

    DATA lv_meta_str TYPE string.

    lv_meta_str = |#repo_name#{ mi_repo->get_name( ) }|.

    cv_cs_blob = lv_meta_str && |\n| && cv_cs_blob.

  ENDMETHOD.


  METHOD build_checksums_from_files.

    DATA ls_last_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS:
      <ls_checksum>    LIKE LINE OF rt_checksums,
      <ls_local>       LIKE LINE OF it_local,
      <ls_cs_file_sig> LIKE LINE OF <ls_checksum>-files.

    " This methods is run at repo creation moment or manually by user
    " In the first case it assumes that the local state is the CURRENT state
    " Thus the idea is to copy local state to checksums
    " The second case is an exception, when we acknoledge that the state is unknown
    " Thus copying the local to checksums is the "best guess"

    LOOP AT it_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO rt_checksums ASSIGNING <ls_checksum>.
        MOVE-CORRESPONDING <ls_local>-item TO <ls_checksum>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_cs_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_cs_file_sig>.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    ASSERT iv_repo_key IS NOT INITIAL.
    mv_repo_key = iv_repo_key.
    mi_repo = zcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
    " Should be safe as repo_srv is supposed to be single source of repo instances
  ENDMETHOD.


  METHOD extract_meta.

    DATA lv_meta_str TYPE string.

    IF cv_cs_blob+0(1) <> '#'.
      RETURN. " No meta ? just ignore it
    ENDIF.

    SPLIT cv_cs_blob AT |\n| INTO lv_meta_str cv_cs_blob.
    " Just remove the header meta string - this is OK for now.
    " There is just repo name for the moment - needed to for DB util and potential debug

  ENDMETHOD.


  METHOD force_write.

    " for migration only for the moment

    save_checksums( it_checksums ).

  ENDMETHOD.


  METHOD remove_non_code_related_files.

    DELETE ct_local_files
      WHERE item IS INITIAL
      AND NOT ( file-path = zif_abapgit_definitions=>c_root_dir
      AND file-filename = zif_abapgit_definitions=>c_dot_abapgit ).

  ENDMETHOD.


  METHOD save_checksums.

    DATA lv_cs_blob TYPE string.

    lv_cs_blob = lcl_checksum_serializer=>serialize( it_checksums ).
    add_meta( CHANGING cv_cs_blob = lv_cs_blob ).
    zcl_abapgit_persist_factory=>get_repo_cs( )->update(
      iv_key     = mv_repo_key
      iv_cs_blob = lv_cs_blob ).

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
      extract_meta( CHANGING cv_cs_blob = lv_cs_blob ).
      rt_checksums = lcl_checksum_serializer=>deserialize( lv_cs_blob ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_checksums~get_checksums_per_file.

    DATA lt_checksums   TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_checksums.

    lt_checksums = zif_abapgit_repo_checksums~get( ).

    LOOP AT lt_checksums ASSIGNING <ls_object>.
      APPEND LINES OF <ls_object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_checksums~rebuild.

    DATA lt_local     TYPE ty_local_files_by_item_tt.
    DATA lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    lt_local  = mi_repo->get_files_local( ).
    remove_non_code_related_files( CHANGING ct_local_files = lt_local ).

    lt_checksums = build_checksums_from_files( lt_local ).
    save_checksums( lt_checksums ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_checksums~update.

    DATA lt_checksums   TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lt_local_files TYPE zif_abapgit_definitions=>ty_files_item_tt.

    lt_checksums   = zif_abapgit_repo_checksums~get( ).
    lt_local_files = mi_repo->get_files_local( ).

    lt_checksums = lcl_update_calculator=>calculate_updated(
      it_current_checksums = lt_checksums
      it_local_files       = lt_local_files
      it_updated_files     = it_updated_files ).

    save_checksums( lt_checksums ).

  ENDMETHOD.
ENDCLASS.
