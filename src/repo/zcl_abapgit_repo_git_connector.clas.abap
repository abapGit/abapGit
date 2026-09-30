CLASS zcl_abapgit_repo_git_connector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_git_connector.
ENDCLASS.


CLASS zcl_abapgit_repo_git_connector IMPLEMENTATION.

  METHOD zif_abapgit_repo_git_connector~fetch.

    DATA ls_pull TYPE zcl_abapgit_git_porcelain=>ty_pull_result.

    IF is_repo-selected_commit IS INITIAL.
      ls_pull = zcl_abapgit_git_porcelain=>pull_by_branch(
        iv_url         = is_repo-url
        iv_branch_name = is_repo-branch_name ).
    ELSE.
      ls_pull = zcl_abapgit_git_porcelain=>pull_by_commit(
        iv_url         = is_repo-url
        iv_commit_hash = is_repo-selected_commit ).
    ENDIF.

    rs_snapshot-files = ls_pull-files.
    rs_snapshot-objects = ls_pull-objects.
    rs_snapshot-resolved_revision = ls_pull-commit.

  ENDMETHOD.

ENDCLASS.
