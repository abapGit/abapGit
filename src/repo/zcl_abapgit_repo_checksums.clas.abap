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
    DATA mv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key.
ENDCLASS.



CLASS zcl_abapgit_repo_checksums IMPLEMENTATION.

  METHOD constructor.
    ASSERT iv_repo_key IS NOT INITIAL.
    mv_repo_key = iv_repo_key.
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

ENDCLASS.
