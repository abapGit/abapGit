CLASS zcl_abapgit_repo_conn_online DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        iv_url TYPE string
      RETURNING
        VALUE(ro_connector) TYPE REF TO zcl_abapgit_repo_conn_online
      RAISING
        zcx_abapgit_exception.

    INTERFACES zif_abapgit_repo_connector .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_url TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_CONN_ONLINE IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT ro_connector.
    ro_connector->mv_url = iv_url.

  ENDMETHOD.


  METHOD zif_abapgit_repo_connector~fetch.

    DATA:
      ls_pull     TYPE zcl_abapgit_git_porcelain=>ty_pull_result.

    ls_pull = zcl_abapgit_git_porcelain=>pull(
      iv_url         = mv_url
      iv_branch_name = iv_branch_name ).

    et_files       = ls_pull-files.
    et_objects     = ls_pull-objects.
    ev_branch_sha1 = ls_pull-branch.

  ENDMETHOD.


  METHOD zif_abapgit_repo_connector~push.

    DATA ls_push TYPE zcl_abapgit_git_porcelain=>ty_push_result.

    ls_push = zcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = iv_branch_name
      iv_url         = mv_url
      iv_parent      = iv_parent
      it_old_objects = ct_objects ).

    et_files         = ls_push-new_files.
    et_updated_files = ls_push-updated_files.
    ev_branch_sha1   = ls_push-branch.
    ct_objects       = ls_push-new_objects.

  ENDMETHOD.

ENDCLASS.
