INTERFACE zif_abapgit_branch_overview
  PUBLIC .

  METHODS:
    get_branches
      RETURNING VALUE(rt_branches) TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,

    get_tags
      RETURNING VALUE(rt_tags) TYPE zif_abapgit_definitions=>ty_git_tag_list_tt,

    get_commits
      RETURNING
        VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt,

    compress
      IMPORTING it_commits        TYPE zif_abapgit_definitions=>ty_commit_tt
      RETURNING VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING   zcx_abapgit_exception.

ENDINTERFACE.
