INTERFACE zif_abapgit_gitv2_porcelain
  PUBLIC .

  METHODS list_branches
    IMPORTING
      !iv_url        TYPE string
      !iv_prefix     TYPE string OPTIONAL
    RETURNING
      VALUE(ro_list) TYPE REF TO zcl_abapgit_git_branch_list
    RAISING
      zcx_abapgit_exception .
  METHODS list_no_blobs
    IMPORTING
      !iv_url            TYPE string
      !iv_sha1           TYPE zif_abapgit_git_definitions=>ty_sha1
    RETURNING
      VALUE(rt_expanded) TYPE zif_abapgit_git_definitions=>ty_expanded_tt
    RAISING
      zcx_abapgit_exception .
  METHODS list_no_blobs_multi
    IMPORTING
      !iv_url           TYPE string
      !it_sha1          TYPE zif_abapgit_git_definitions=>ty_sha1_tt
    RETURNING
      VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
    RAISING
      zcx_abapgit_exception .
  METHODS commits_last_year
    IMPORTING
      !iv_url           TYPE string
      !it_sha1          TYPE zif_abapgit_git_definitions=>ty_sha1_tt
    RETURNING
      VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
