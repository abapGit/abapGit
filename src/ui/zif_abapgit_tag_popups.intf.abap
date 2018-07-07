INTERFACE zif_abapgit_tag_popups
  PUBLIC.

  METHODS:
    tag_list_popup
      IMPORTING
        io_repo       TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rs_tag) TYPE zif_abapgit_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception,

    tag_select_popup
      IMPORTING
        io_repo       TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rs_tag) TYPE zif_abapgit_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .

ENDINTERFACE.
