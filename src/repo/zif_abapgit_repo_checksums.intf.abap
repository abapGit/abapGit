INTERFACE zif_abapgit_repo_checksums
  PUBLIC.

  METHODS get
    RETURNING
      VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

  METHODS rebuild
    IMPORTING
      iv_branches_equal TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_exception.

  METHODS update
    IMPORTING
      !it_updated_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
