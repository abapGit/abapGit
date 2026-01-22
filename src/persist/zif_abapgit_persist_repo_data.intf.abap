INTERFACE zif_abapgit_persist_repo_data PUBLIC.

  METHODS update
    IMPORTING
      !iv_key  TYPE zif_abapgit_persistence=>ty_repo-key
      !iv_json TYPE zif_abapgit_persistence=>ty_content-data_str
    RAISING
      zcx_abapgit_exception.

  METHODS delete
    IMPORTING
      !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception.

  METHODS read
    IMPORTING
      !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
    RETURNING
      VALUE(rv_json) TYPE zif_abapgit_persistence=>ty_content-data_str
    RAISING
      zcx_abapgit_exception
      zcx_abapgit_not_found.

ENDINTERFACE.
