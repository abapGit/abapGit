CLASS zcl_abapgit_services_git DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_commit_fields,
        repo_key        TYPE zif_abapgit_persistence=>ty_repo-key,
        committer_name  TYPE string,
        committer_email TYPE string,
        author_name     TYPE string,
        author_email    TYPE string,
        comment         TYPE string,
        body            TYPE string,
      END OF ty_commit_fields.

    CLASS-METHODS pull
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS reset
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS create_branch
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS switch_branch
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS delete_branch
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .

    CLASS-METHODS delete_tag
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS switch_tag
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS tag_overview
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS commit
      IMPORTING
        !io_repo   TYPE REF TO zcl_abapgit_repo_online
        !is_commit TYPE ty_commit_fields
        !io_stage  TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_abapgit_services_git IMPLEMENTATION.


  METHOD commit.

    DATA: ls_comment TYPE zif_abapgit_definitions=>ty_comment,
          lo_user    TYPE REF TO zcl_abapgit_persistence_user.

    lo_user = zcl_abapgit_persistence_user=>get_instance( ).
    lo_user->set_repo_git_user_name( iv_url      = io_repo->get_url( )
                                     iv_username = is_commit-committer_name ).
    lo_user->set_repo_git_user_email( iv_url     = io_repo->get_url( )
                                      iv_email   = is_commit-committer_email ).

    IF is_commit-committer_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: Committer name empty' ).
    ELSEIF is_commit-committer_email IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: Committer email empty' ).
    ELSEIF is_commit-author_email IS NOT INITIAL AND is_commit-author_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: Author name empty' ). " Opposite should be OK ?
    ELSEIF is_commit-comment IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: empty comment' ).
    ENDIF.

    ls_comment-committer-name  = is_commit-committer_name.
    ls_comment-committer-email = is_commit-committer_email.
    ls_comment-author-name     = is_commit-author_name.
    ls_comment-author-email    = is_commit-author_email.
    ls_comment-comment         = is_commit-comment.

    IF NOT is_commit-body IS INITIAL.
      CONCATENATE ls_comment-comment '' is_commit-body
        INTO ls_comment-comment SEPARATED BY zif_abapgit_definitions=>gc_newline.
    ENDIF.

    io_repo->push( is_comment = ls_comment
                   io_stage   = io_stage ).

    COMMIT WORK.

  ENDMETHOD.  "commit


  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO zcl_abapgit_repo_online.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    zcl_abapgit_ui_factory=>get_popups( )->create_branch_popup(
      IMPORTING
        ev_name   = lv_name
        ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    ASSERT lv_name CP 'refs/heads/+*'.

    zcl_abapgit_git_porcelain=>create_branch(
      io_repo = lo_repo
      iv_name = lv_name
      iv_from = lo_repo->get_sha1_remote( ) ).

    " automatically switch to new branch
    lo_repo->set_branch_name( lv_name ).

    MESSAGE 'Switched to new branch' TYPE 'S' ##NO_TEXT.

  ENDMETHOD.


  METHOD delete_branch.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          ls_branch TYPE zif_abapgit_definitions=>ty_git_branch.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_branch = zcl_abapgit_ui_factory=>get_popups( )->branch_list_popup( lo_repo->get_url( ) ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    IF ls_branch-name = 'HEAD'.
      zcx_abapgit_exception=>raise( 'Cannot delete HEAD' ).
    ELSEIF ls_branch-name = lo_repo->get_branch_name( ).
      zcx_abapgit_exception=>raise( 'Switch branch before deleting current' ).
    ENDIF.

    zcl_abapgit_git_porcelain=>delete_branch(
      io_repo   = lo_repo
      is_branch = ls_branch ).

    MESSAGE 'Branch deleted' TYPE 'S'.

  ENDMETHOD.  "delete_branch


  METHOD delete_tag.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          ls_tag  TYPE zif_abapgit_definitions=>ty_git_tag,
          lv_text TYPE string.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_tag = zcl_abapgit_ui_factory=>get_tag_popups( )->tag_select_popup( lo_repo ).
    IF ls_tag IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_git_porcelain=>delete_tag(
      io_repo = lo_repo
      is_tag  = ls_tag ).

    lv_text = |Tag { zcl_abapgit_tag=>remove_tag_prefix( ls_tag-name ) } deleted| ##NO_TEXT.

    MESSAGE lv_text TYPE 'S'.

  ENDMETHOD.


  METHOD pull.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lo_repo->refresh( ).

    zcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "pull


  METHOD reset.

    DATA: lo_repo                   TYPE REF TO zcl_abapgit_repo_online,
          lv_answer                 TYPE c LENGTH 1,
          lt_unnecessary_local_objs TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_selected               LIKE lt_unnecessary_local_objs,
          lt_columns                TYPE stringtab,
          ls_checks                 TYPE zif_abapgit_definitions=>ty_delete_checks.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    IF lo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot reset. Local code is write-protected by repo config' ).
    ENDIF.

* todo, separate UI and logic
    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = 'Reset local objects?'
      text_button_1         = 'Ok'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lt_unnecessary_local_objs = lo_repo->get_unnecessary_local_objs( ).

    IF lines( lt_unnecessary_local_objs ) > 0.

      INSERT `OBJECT` INTO TABLE lt_columns.
      INSERT `OBJ_NAME` INTO TABLE lt_columns.

      zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_from_list(
        EXPORTING
          it_list              = lt_unnecessary_local_objs
          i_header_text        = |Which unnecessary objects should be deleted?|
          i_select_column_text = 'Delete?'
          it_columns_to_display = lt_columns
        IMPORTING
          et_list              = lt_selected ).

      IF lines( lt_selected ) > 0.
        ls_checks = lo_repo->delete_checks( ).
        IF ls_checks-transport-required = abap_true.
          ls_checks-transport-transport = zcl_abapgit_ui_factory=>get_popups(
                                            )->popup_transport_request( ls_checks-transport-type ).
        ENDIF.

        zcl_abapgit_objects=>delete( it_tadir  = lt_selected
                                     is_checks = ls_checks ).
* update repo cache
        lo_repo->refresh( ).
      ENDIF.

    ENDIF.

    zcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

  ENDMETHOD.


  METHOD switch_branch.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          ls_branch TYPE zif_abapgit_definitions=>ty_git_branch.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_branch = zcl_abapgit_ui_factory=>get_popups( )->branch_list_popup(
      iv_url             = lo_repo->get_url( )
      iv_default_branch  = lo_repo->get_branch_name( )
      iv_show_new_option = abap_true ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    IF ls_branch-name = zcl_abapgit_ui_factory=>get_popups( )->c_new_branch_label.
      create_branch( iv_key ).
      RETURN.
    ENDIF.

    lo_repo->set_branch_name( ls_branch-name ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD switch_tag.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          ls_tag  TYPE zif_abapgit_definitions=>ty_git_tag.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_tag = zcl_abapgit_ui_factory=>get_tag_popups( )->tag_select_popup( lo_repo ).
    IF ls_tag IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo->set_branch_name( ls_tag-name ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD tag_overview.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    zcl_abapgit_ui_factory=>get_tag_popups( )->tag_list_popup( lo_repo ).

  ENDMETHOD.
ENDCLASS.
