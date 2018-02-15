CLASS zcl_abapgit_transport_2_branch DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      create
        IMPORTING io_repository          TYPE REF TO zcl_abapgit_repo_online
                  is_transport_to_branch TYPE zif_abapgit_definitions=>ty_transport_to_branch
                  it_transport_objects   TYPE scts_tadir
        RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.

    METHODS create_new_branch
      IMPORTING
        io_repository  TYPE REF TO zcl_abapgit_repo_online
        iv_branch_name TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS generate_commit_message
      IMPORTING
        is_transport_to_branch TYPE zif_abapgit_definitions=>ty_transport_to_branch
      RETURNING
        VALUE(rs_comment)      TYPE zif_abapgit_definitions=>ty_comment.
    METHODS stage_transport_objects
      IMPORTING
        it_transport_objects TYPE scts_tadir
        io_stage             TYPE REF TO zcl_abapgit_stage
        is_stage_objects     TYPE zif_abapgit_definitions=>ty_stage_files
        it_object_statuses   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_TRANSPORT_2_BRANCH IMPLEMENTATION.


  METHOD create.
    DATA:
      lv_branch_name     TYPE string,
      ls_comment         TYPE zif_abapgit_definitions=>ty_comment,
      lo_stage           TYPE REF TO zcl_abapgit_stage,
      ls_stage_objects   TYPE zif_abapgit_definitions=>ty_stage_files,
      lt_object_statuses TYPE zif_abapgit_definitions=>ty_results_tt.

    lv_branch_name = zcl_abapgit_git_branch_list=>complete_heads_branch_name(
        zcl_abapgit_git_branch_list=>normalize_branch_name( is_transport_to_branch-branch_name ) ).

    create_new_branch(
      io_repository  = io_repository
      iv_branch_name = lv_branch_name ).

    CREATE OBJECT lo_stage
      EXPORTING
        iv_branch_name = lv_branch_name
        iv_branch_sha1 = io_repository->get_sha1_remote( ).

    ls_stage_objects = zcl_abapgit_stage_logic=>get( io_repository ).

    lt_object_statuses = io_repository->status( ).

    stage_transport_objects(
       it_transport_objects = it_transport_objects
       io_stage             = lo_stage
       is_stage_objects     = ls_stage_objects
       it_object_statuses   = lt_object_statuses ).

    ls_comment = generate_commit_message( is_transport_to_branch ).

    io_repository->push( is_comment = ls_comment
                         io_stage   = lo_stage ).
  ENDMETHOD.


  METHOD create_new_branch.
    ASSERT iv_branch_name CP 'refs/heads/+*'.
    TRY.
        zcl_abapgit_git_porcelain=>create_branch(
          io_repo = io_repository
          iv_name = iv_branch_name
          iv_from = io_repository->get_sha1_local( ) ).

        io_repository->set_branch_name( iv_branch_name ).
      CATCH zcx_abapgit_exception.
        zcx_abapgit_exception=>raise( 'Error when creating new branch').
    ENDTRY.
  ENDMETHOD.


  METHOD generate_commit_message.
    rs_comment-committer-name  = sy-uname.
    rs_comment-committer-email = |{ rs_comment-committer-name }@localhost|.
    rs_comment-comment         = is_transport_to_branch-commit_text.
  ENDMETHOD.


  METHOD stage_transport_objects.
    DATA lo_transport_objects TYPE REF TO zcl_abapgit_transport_objects.
    CREATE OBJECT lo_transport_objects
      EXPORTING
        it_transport_objects = it_transport_objects.

    lo_transport_objects->to_stage(
      io_stage           = io_stage
      is_stage_objects   = is_stage_objects
      it_object_statuses = it_object_statuses ).
  ENDMETHOD.
ENDCLASS.
