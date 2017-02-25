*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SERVICES_GIT
*&---------------------------------------------------------------------*

CLASS lcl_services_git DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_commit_fields,
             repo_key        TYPE lcl_persistence_repo=>ty_repo-key,
             committer_name  TYPE string,
             committer_email TYPE string,
             author_name     TYPE string,
             author_email    TYPE string,
             comment         TYPE string,
             body            TYPE string,
           END OF ty_commit_fields.

    CLASS-METHODS pull
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS reset
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS create_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS switch_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS delete_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS commit
      IMPORTING io_repo   TYPE REF TO lcl_repo_online
                is_commit TYPE ty_commit_fields
                io_stage  TYPE REF TO lcl_stage
      RAISING   lcx_exception lcx_cancel.

ENDCLASS. " lcl_services_git

CLASS lcl_services_git IMPLEMENTATION.

  METHOD reset.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          lv_answer TYPE c LENGTH 1.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot reset. Local code is write-protected by repo config' ).
    ENDIF.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = 'Reset local objects?'
      text_button_1         = 'Ok'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lo_repo->deserialize( ).

  ENDMETHOD.

  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    lcl_popups=>create_branch_popup(
      IMPORTING
        ev_name   = lv_name
        ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    ASSERT lv_name CP 'refs/heads/+*'.

    lcl_git_porcelain=>create_branch(
      io_repo = lo_repo
      iv_name = lv_name
      iv_from = lo_repo->get_sha1_local( ) ).

    " automatically switch to new branch
    lo_repo->set_branch_name( lv_name ).

    MESSAGE 'Switched to new branch' TYPE 'S' ##NO_TEXT.

  ENDMETHOD.

  METHOD pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot pull. Local code is write-protected by repo config' ).
    ENDIF.

    lo_repo->refresh( ).
    lo_repo->deserialize( ).

    COMMIT WORK.

  ENDMETHOD.                    "pull

  METHOD switch_branch.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          ls_branch TYPE lcl_git_branch_list=>ty_git_branch.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    ls_branch = lcl_popups=>branch_list_popup(
      iv_url             = lo_repo->get_url( )
      iv_default_branch  = lo_repo->get_branch_name( )
      iv_show_new_option = abap_true ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    IF ls_branch-name = lcl_popups=>c_new_branch_label.
      create_branch( iv_key ).
      RETURN.
    ENDIF.

    lo_repo->set_branch_name( ls_branch-name ).

    COMMIT WORK.

    lo_repo->deserialize( ).

  ENDMETHOD.  "switch_branch

  METHOD delete_branch.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          ls_branch TYPE lcl_git_branch_list=>ty_git_branch.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    ls_branch = lcl_popups=>branch_list_popup( lo_repo->get_url( ) ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    IF ls_branch-name = 'HEAD'.
      lcx_exception=>raise( 'Cannot delete HEAD' ).
    ELSEIF ls_branch-name = lo_repo->get_branch_name( ).
      lcx_exception=>raise( 'Switch branch before deleting current' ).
    ENDIF.

    lcl_git_porcelain=>delete_branch(
      io_repo   = lo_repo
      is_branch = ls_branch ).

    MESSAGE 'Branch deleted' TYPE 'S'.

  ENDMETHOD.  "delete_branch

  METHOD commit.

    DATA: ls_comment TYPE ty_comment,
          lo_user    TYPE REF TO lcl_persistence_user.

    lo_user = lcl_app=>user( ).
    lo_user->set_repo_git_user_name( iv_url      = io_repo->get_url( )
                                     iv_username = is_commit-committer_name ).
    lo_user->set_repo_git_user_email( iv_url     = io_repo->get_url( )
                                      iv_email   = is_commit-committer_email ).

    IF is_commit-committer_name IS INITIAL.
      lcx_exception=>raise( 'Commit: Committer name empty' ).
    ELSEIF is_commit-committer_email IS INITIAL.
      lcx_exception=>raise( 'Commit: Committer email empty' ).
    ELSEIF is_commit-author_email IS NOT INITIAL AND is_commit-author_name IS INITIAL.
      lcx_exception=>raise( 'Commit: Author email empty' ). " Opposite should be OK ?
    ELSEIF is_commit-comment IS INITIAL.
      lcx_exception=>raise( 'Commit: empty comment' ).
    ENDIF.

    ls_comment-committer-name  = is_commit-committer_name.
    ls_comment-committer-email = is_commit-committer_email.
    ls_comment-author-name     = is_commit-author_name.
    ls_comment-author-email    = is_commit-author_email.
    ls_comment-comment         = is_commit-comment.

    IF NOT is_commit-body IS INITIAL.
      CONCATENATE ls_comment-comment '' is_commit-body
        INTO ls_comment-comment SEPARATED BY gc_newline.
    ENDIF.

    io_repo->push( is_comment = ls_comment
                   io_stage   = io_stage ).

    COMMIT WORK.

  ENDMETHOD.  "commit

ENDCLASS. " lcl_services_git
