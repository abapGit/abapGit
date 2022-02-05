INTERFACE zif_abapgit_repo_checksums
  PUBLIC.

  METHODS get
    RETURNING
      VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

ENDINTERFACE.
