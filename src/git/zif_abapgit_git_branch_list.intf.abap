INTERFACE zif_abapgit_git_branch_list PUBLIC.

  METHODS find_by_name
    IMPORTING
      !iv_branch_name  TYPE clike
    RETURNING
      VALUE(rs_branch) TYPE zif_abapgit_git_definitions=>ty_git_branch
    RAISING
      zcx_abapgit_exception .
  METHODS get_head_symref
    RETURNING
      VALUE(rv_head_symref) TYPE string .
  METHODS get_all
    RETURNING
      VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_branches_only
    RETURNING
      VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_tags_only
    RETURNING
      VALUE(rt_tags) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
