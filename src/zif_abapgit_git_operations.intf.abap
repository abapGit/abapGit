INTERFACE zif_abapgit_git_operations
  PUBLIC .

  METHODS push
    IMPORTING
      !is_comment TYPE zif_abapgit_definitions=>ty_comment
      !io_stage   TYPE REF TO zcl_abapgit_stage
    RAISING
      zcx_abapgit_exception .

  METHODS create_branch
    IMPORTING
      !iv_name TYPE string
      !iv_from TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
