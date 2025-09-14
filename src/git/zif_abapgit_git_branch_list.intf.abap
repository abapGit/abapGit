INTERFACE zif_abapgit_git_branch_list PUBLIC.

  "! Find a branch or tag by name
  "! @parameter iv_branch_name | Branch or tag name to search for
  "! @parameter rs_branch | Branch/tag information
  "! @raising zcx_abapgit_exception | Branch/tag not found or name is empty
  METHODS find_by_name
    IMPORTING
      !iv_branch_name  TYPE clike
    RETURNING
      VALUE(rs_branch) TYPE zif_abapgit_git_definitions=>ty_git_branch
    RAISING
      zcx_abapgit_exception .

  "! Get the HEAD symbolic reference
  "! @parameter rv_head_symref | HEAD symbolic reference string
  METHODS get_head_symref
    RETURNING
      VALUE(rv_head_symref) TYPE string .

  "! Get all branches and tags
  "! @parameter rt_branches | Complete list of branches and tags
  "! @raising zcx_abapgit_exception | Error retrieving branch list
  METHODS get_all
    RETURNING
      VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
    RAISING
      zcx_abapgit_exception .

  "! Get only branches (excluding tags)
  "! @parameter rt_branches | List of branches only
  "! @raising zcx_abapgit_exception | Error retrieving branch list
  METHODS get_branches_only
    RETURNING
      VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
    RAISING
      zcx_abapgit_exception .

  "! Get only tags (excluding branches)
  "! @parameter rt_tags | List of tags only
  "! @raising zcx_abapgit_exception | Error retrieving tag list
  METHODS get_tags_only
    RETURNING
      VALUE(rt_tags) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
