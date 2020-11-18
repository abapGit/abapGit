INTERFACE ZIF_ABAPGIT_REPO_ONLINE
  public .
  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE zif_abapgit_persistence=>ty_repo-url .
  METHODS get_selected_branch
    RETURNING
      VALUE(rv_name) TYPE zif_abapgit_persistence=>ty_repo-branch_name .
  METHODS set_url
    IMPORTING
       iv_url TYPE zif_abapgit_persistence=>ty_repo-url
    RAISING
      zcx_abapgit_exception .
  METHODS select_branch
    IMPORTING
       iv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name
    RAISING
      zcx_abapgit_exception .
  METHODS get_selected_commit
    RETURNING
      value(rv_selected_commit) TYPE zif_abapgit_persistence=>ty_repo-selected_commit
    RAISING
      zcx_abapgit_exception .
  METHODS get_current_remote
    RETURNING
      value(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
    RAISING
      zcx_abapgit_exception .
  METHODS select_commit
    IMPORTING
       iv_selected_commit TYPE zif_abapgit_persistence=>ty_repo-selected_commit
    RAISING
      zcx_abapgit_exception .
  METHODS get_objects
    RETURNING
      value(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_switched_origin
    RETURNING
      value(rv_url) TYPE zif_abapgit_persistence=>ty_repo-switched_origin .
  METHODS switch_origin
    IMPORTING
       iv_url       TYPE zif_abapgit_persistence=>ty_repo-url
       iv_overwrite TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_exception .

endinterface.
