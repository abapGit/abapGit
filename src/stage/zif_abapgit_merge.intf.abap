INTERFACE zif_abapgit_merge PUBLIC .

  TYPES:
    BEGIN OF ty_merge,
      repo     TYPE REF TO zcl_abapgit_repo_online,
      source   TYPE zif_abapgit_git_definitions=>ty_git_branch,
      target   TYPE zif_abapgit_git_definitions=>ty_git_branch,
      common   TYPE zif_abapgit_definitions=>ty_ancestor,
      stree    TYPE zif_abapgit_definitions=>ty_expanded_tt,
      ttree    TYPE zif_abapgit_definitions=>ty_expanded_tt,
      ctree    TYPE zif_abapgit_definitions=>ty_expanded_tt,
      result   TYPE zif_abapgit_definitions=>ty_expanded_tt,
      stage    TYPE REF TO zcl_abapgit_stage,
      conflict TYPE string,
    END OF ty_merge .
  TYPES:
    BEGIN OF ty_merge_conflict,
      path        TYPE string,
      filename    TYPE string,
      source_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1,
      source_data TYPE xstring,
      target_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1,
      target_data TYPE xstring,
      result_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1,
      result_data TYPE xstring,
    END OF ty_merge_conflict .
  TYPES:
    ty_merge_conflict_tt TYPE STANDARD TABLE OF ty_merge_conflict WITH DEFAULT KEY .

  METHODS get_conflicts
    RETURNING
      VALUE(rt_conflicts) TYPE ty_merge_conflict_tt .
  METHODS get_result
    RETURNING
      VALUE(rs_merge) TYPE ty_merge .
  METHODS get_source_branch
    RETURNING
      VALUE(rv_source_branch) TYPE string .
  METHODS has_conflicts
    RETURNING
      VALUE(rv_conflicts_exists) TYPE abap_bool .
  METHODS resolve_conflict
    IMPORTING
      !is_conflict TYPE ty_merge_conflict
    RAISING
      zcx_abapgit_exception .
  METHODS run
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
