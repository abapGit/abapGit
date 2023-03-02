INTERFACE zif_abapgit_repo_checksums
  PUBLIC.

  METHODS get
    RETURNING
      VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

  METHODS get_checksums_per_file
    RETURNING
      VALUE(rt_checksums) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt .

  METHODS rebuild
    RAISING
      zcx_abapgit_exception.

  METHODS update
    IMPORTING
      !it_updated_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
