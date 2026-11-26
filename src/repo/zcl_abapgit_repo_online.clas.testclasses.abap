CLASS ltcl_create_branch DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      name_without_prefix FOR TESTING.

    DATA mi_cut TYPE REF TO zif_abapgit_repo_online.

ENDCLASS.

CLASS ltcl_create_branch IMPLEMENTATION.

  METHOD setup.

    DATA ls_data TYPE zif_abapgit_persistence=>ty_repo.

    ls_data-key = '1'.
    ls_data-url = 'https://github.com/abapGit/abapGit.git'.

    CREATE OBJECT mi_cut TYPE zcl_abapgit_repo_online
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD name_without_prefix.

    " a caller without the GUI must get an exception, not a short dump
    TRY.
        mi_cut->create_branch( 'feature' ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltd_git_transport DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_git_transport.
    INTERFACES zif_abapgit_git_branch_list.

    DATA mv_head TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA mv_calls TYPE i.

ENDCLASS.

CLASS ltd_git_transport IMPLEMENTATION.

  METHOD zif_abapgit_git_transport~branches.
    mv_calls = mv_calls + 1.
    ri_branch_list = me.
  ENDMETHOD.

  METHOD zif_abapgit_git_branch_list~find_by_name.
    rs_branch-name = iv_branch_name.
    rs_branch-sha1 = mv_head.
  ENDMETHOD.

  METHOD zif_abapgit_git_branch_list~get_head_symref.
  ENDMETHOD.

  METHOD zif_abapgit_git_branch_list~get_all.
  ENDMETHOD.

  METHOD zif_abapgit_git_branch_list~get_branches_only.
  ENDMETHOD.

  METHOD zif_abapgit_git_branch_list~get_tags_only.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_fetch_remote DEFINITION DEFERRED.
CLASS zcl_abapgit_repo_online DEFINITION LOCAL FRIENDS ltcl_fetch_remote.

CLASS ltcl_fetch_remote DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_filename TYPE string VALUE 'file.txt'.

    DATA mo_transport TYPE REF TO ltd_git_transport.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA mv_commit TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS:
      setup RAISING zcx_abapgit_exception,
      teardown,
      create_repo
        IMPORTING
          iv_selected_commit TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        RETURNING
          VALUE(ro_repo)     TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,
      assert_files_of_commit
        IMPORTING
          io_repo TYPE REF TO zcl_abapgit_repo_online
        RAISING
          zcx_abapgit_exception,
      branch_unchanged FOR TESTING RAISING zcx_abapgit_exception,
      commit_unchanged FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_fetch_remote IMPLEMENTATION.

  METHOD setup.

    " one commit with a tree holding one file
    DATA ls_object LIKE LINE OF mt_objects.
    DATA ls_node   TYPE zcl_abapgit_git_pack=>ty_node.
    DATA lt_nodes  TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = '616263'.
    ls_object-sha1 = zcl_abapgit_hash=>sha1_blob( ls_object-data ).
    INSERT ls_object INTO TABLE mt_objects.

    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = c_filename.
    ls_node-sha1  = ls_object-sha1.
    APPEND ls_node TO lt_nodes.

    ls_object-type = zif_abapgit_git_definitions=>c_type-tree.
    ls_object-data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
    ls_object-sha1 = zcl_abapgit_hash=>sha1_tree( ls_object-data ).
    INSERT ls_object INTO TABLE mt_objects.

    ls_commit-tree      = ls_object-sha1.
    ls_commit-author    = 'Author <author@example.com> 0 +0000'.
    ls_commit-committer = ls_commit-author.
    ls_commit-body      = 'commit'.

    ls_object-type = zif_abapgit_git_definitions=>c_type-commit.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    ls_object-sha1 = zcl_abapgit_hash=>sha1_commit( ls_object-data ).
    INSERT ls_object INTO TABLE mt_objects.

    mv_commit = ls_object-sha1.

    CREATE OBJECT mo_transport.
    zcl_abapgit_git_injector=>set_git_transport( mo_transport ).

  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_git_injector=>set_git_transport( ).
  ENDMETHOD.

  METHOD create_repo.

    " repository that already fetched mv_commit
    DATA ls_data TYPE zif_abapgit_persistence=>ty_repo.

    ls_data-key             = '1'.
    ls_data-url             = 'https://example.com/repo.git'.
    ls_data-branch_name     = 'refs/heads/main'.
    ls_data-selected_commit = iv_selected_commit.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_data.

    ro_repo->set_objects( mt_objects ).
    ro_repo->mv_current_commit = mv_commit.

  ENDMETHOD.

  METHOD assert_files_of_commit.

    " without the reuse, this call would try to reach the remote and fail
    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file  LIKE LINE OF lt_files.

    lt_files = io_repo->zif_abapgit_repo~get_files_remote( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).

    READ TABLE lt_files INDEX 1 INTO ls_file.
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-filename
      exp = c_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-data
      exp = '616263' ).

    cl_abap_unit_assert=>assert_equals(
      act = io_repo->get_current_remote( )
      exp = mv_commit ).

  ENDMETHOD.

  METHOD branch_unchanged.

    mo_transport->mv_head = mv_commit.

    assert_files_of_commit( create_repo( ) ).

    " only the branch list was requested
    cl_abap_unit_assert=>assert_equals(
      act = mo_transport->mv_calls
      exp = 1 ).

  ENDMETHOD.

  METHOD commit_unchanged.

    assert_files_of_commit( create_repo( iv_selected_commit = mv_commit ) ).

    " a commit never changes, the remote is not asked at all
    cl_abap_unit_assert=>assert_equals(
      act = mo_transport->mv_calls
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
