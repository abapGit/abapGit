INTERFACE zif_abapgit_repo_push
  PUBLIC .

  " The steps of a push, without any UI: the push exits, the push
  " and the commit. The caller builds the stage and the comment.
  METHODS push
    IMPORTING
      !is_comment TYPE zif_abapgit_git_definitions=>ty_comment
      !io_stage   TYPE REF TO zcl_abapgit_stage
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
