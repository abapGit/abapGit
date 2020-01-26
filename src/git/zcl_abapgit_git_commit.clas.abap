"! <p class="shorttext synchronized" lang="en">Git Commit</p>
CLASS zcl_abapgit_git_commit DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  INHERITING FROM zcl_abapgit_git_object .

  PUBLIC SECTION.
    CLASS-METHODS get
      IMPORTING
        io_repo           TYPE REF TO zcl_abapgit_repo_online
        iv_commit_hash    TYPE zif_abapgit_definitions=>ty_sha1
        iv_deepen_level   TYPE numc2 DEFAULT '1'
      RETURNING
        VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_git_commit IMPLEMENTATION.

  METHOD get.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.

    li_progress = zcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { io_repo->get_name( ) }| ) ##NO_TEXT.

    zcl_abapgit_git_transport=>upload_pack(
      EXPORTING
        iv_url          = io_repo->get_url( )
        iv_branch_name  = io_repo->get_branch_name( )
        iv_deepen       = abap_true
        iv_deepen_level = iv_deepen_level
        iv_commit_hash  = iv_commit_hash
      IMPORTING
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type NE zif_abapgit_definitions=>c_type-commit.

    rt_commits = zcl_abapgit_git_object=>parse_commits( lt_objects ).
    zcl_abapgit_git_object=>sort_commits( CHANGING ct_commits = rt_commits ).

  ENDMETHOD.

ENDCLASS.
