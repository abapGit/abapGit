*&---------------------------------------------------------------------*
*&  Include  zabapgit_services_trans_branch
*&---------------------------------------------------------------------*
CLASS lcl_transport_to_branch DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create
        IMPORTING io_repository          TYPE REF TO lcl_repo_online
                  is_transport_to_branch TYPE ty_transport_to_branch
                  it_transport_objects   TYPE scts_tadir
        RAISING   lcx_exception.
  PRIVATE SECTION.

    METHODS create_new_branch
      IMPORTING
        io_repository  TYPE REF TO lcl_repo_online
        iv_branch_name TYPE string
      RAISING
        lcx_exception.
    METHODS add_new_and_changes_to_stage
      IMPORTING
        it_transport_objects TYPE scts_tadir
        io_stage             TYPE REF TO lcl_stage
        is_stage_objects     TYPE ty_stage_files
      RAISING
        lcx_exception.
    METHODS generate_commit_message
      IMPORTING
        is_transport_to_branch TYPE ty_transport_to_branch
      RETURNING
        VALUE(rs_comment)      TYPE ty_comment.
ENDCLASS.

CLASS lcl_transport_to_branch IMPLEMENTATION.

  METHOD create.
    DATA:
      ls_transport_object TYPE LINE OF scts_tadir,
      lt_items            TYPE ty_files_item_tt,
      ls_local_file       TYPE LINE OF ty_files_item_tt,
      ls_remote_file      TYPE LINE OF ty_files_tt,
      ls_item             TYPE string,
      lv_branch_name      TYPE string,
      ls_comment          TYPE ty_comment,
      lo_stage            TYPE REF TO lcl_stage,
      ls_stage_objects    TYPE ty_stage_files,
      ls_branch_to_delete TYPE lcl_git_branch_list=>ty_git_branch.

    lv_branch_name = lcl_git_branch_list=>complete_heads_branch_name(
        lcl_git_branch_list=>normalize_branch_name( is_transport_to_branch-branch_name ) ).

    create_new_branch(
      io_repository  = io_repository
      iv_branch_name = lv_branch_name ).

    CREATE OBJECT lo_stage
      EXPORTING
        iv_branch_name = lv_branch_name
        iv_branch_sha1 = io_repository->get_sha1_remote( ).

    ls_stage_objects = lcl_stage_logic=>get( io_repository ).

    add_new_and_changes_to_stage(
      it_transport_objects = it_transport_objects
      io_stage             = lo_stage
      is_stage_objects     = ls_stage_objects ).

    "Stage remote files as removal, for they could be deleted
    LOOP AT ls_stage_objects-remote INTO ls_remote_file.
      lo_stage->rm(
        iv_path       = ls_remote_file-path
        iv_filename   = ls_remote_file-filename ).
    ENDLOOP.

    ls_comment = generate_commit_message( is_transport_to_branch ).

    io_repository->push( is_comment = ls_comment
                         io_stage   = lo_stage ).
  ENDMETHOD.

  METHOD create_new_branch.
    ASSERT iv_branch_name CP 'refs/heads/+*'.
    TRY.
        lcl_git_porcelain=>create_branch(
          io_repo = io_repository
          iv_name = iv_branch_name
          iv_from = io_repository->get_sha1_local( ) ).

        io_repository->set_branch_name( iv_branch_name ).
      CATCH lcx_exception.
        lcx_exception=>raise( 'Error when creating new branch').
    ENDTRY.
  ENDMETHOD.


  METHOD add_new_and_changes_to_stage.
    DATA ls_transport_object TYPE tadir.
    DATA ls_local_file TYPE ty_file_item.

    LOOP AT it_transport_objects INTO ls_transport_object.
      IF ls_transport_object-delflag = abap_false.
        LOOP AT is_stage_objects-local
           INTO ls_local_file
          WHERE item-obj_name = ls_transport_object-obj_name.
          "Looping is needed to also add XMLs, other includes (e.g. local class implementation)
          io_stage->add(
            iv_path       = ls_local_file-file-path
            iv_filename   = ls_local_file-file-filename
            iv_data       = ls_local_file-file-data ).
        ENDLOOP.
        IF sy-subrc = 4.
          lcx_exception=>raise( |Object { ls_transport_object-obj_name } not found in the local repository files | ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_commit_message.
    rs_comment-committer-name  = sy-uname.
    rs_comment-committer-email = |{ rs_comment-committer-name }@localhost|.
    rs_comment-comment        = is_transport_to_branch-commit_text.
  ENDMETHOD.

ENDCLASS.
