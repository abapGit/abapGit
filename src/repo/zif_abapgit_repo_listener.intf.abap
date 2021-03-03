INTERFACE zif_abapgit_repo_listener
  PUBLIC .

  METHODS on_meta_change
    IMPORTING
      !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      !is_meta TYPE zif_abapgit_persistence=>ty_repo_xml
      !is_change_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
