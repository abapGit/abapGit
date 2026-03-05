INTERFACE zif_abapgit_data_persistence PUBLIC.

  METHODS save_config
    IMPORTING
      !iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception.

  METHODS load_config
    IMPORTING
      !iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
