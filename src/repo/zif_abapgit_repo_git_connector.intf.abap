INTERFACE zif_abapgit_repo_git_connector
  PUBLIC .

  TYPES:
    BEGIN OF ty_snapshot,
      files             TYPE zif_abapgit_git_definitions=>ty_files_tt,
      objects           TYPE zif_abapgit_definitions=>ty_objects_tt,
      resolved_revision TYPE string,
    END OF ty_snapshot.

  METHODS fetch
    IMPORTING
      is_repo            TYPE zif_abapgit_persistence=>ty_repo
    RETURNING
      VALUE(rs_snapshot) TYPE ty_snapshot
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
