INTERFACE zif_abapgit_git_transport
  PUBLIC .
  METHODS branches
    IMPORTING
       iv_url               TYPE string
    RETURNING
      VALUE(ro_branch_list) TYPE REF TO zcl_abapgit_git_branch_list
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
