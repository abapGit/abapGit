INTERFACE zif_abapgit_repo_connector
  PUBLIC .

    METHODS fetch
      IMPORTING
        iv_url         TYPE string
        iv_branch_name TYPE string
      EXPORTING
        et_files       TYPE zif_abapgit_definitions=>ty_files_tt
        et_objects     TYPE zif_abapgit_definitions=>ty_objects_tt
        ev_branch_sha1 TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception.

    METHODS push
      IMPORTING
        iv_branch_name TYPE string
        iv_branch_sha1 TYPE zif_abapgit_definitions=>ty_sha1
        is_comment     TYPE zif_abapgit_definitions=>ty_comment
        io_stage       TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception.

ENDINTERFACE.
