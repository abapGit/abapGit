CLASS lcl_test_git_connector DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_git_connector.
    DATA ms_snapshot TYPE zif_abapgit_repo_git_connector=>ty_snapshot.
    DATA mv_fetch_count TYPE i.
    DATA mv_last_branch TYPE zif_abapgit_persistence=>ty_repo-branch_name.
ENDCLASS.

CLASS lcl_test_git_connector IMPLEMENTATION.
  METHOD zif_abapgit_repo_git_connector~fetch.
    mv_fetch_count = mv_fetch_count + 1.
    mv_last_branch = is_repo-branch_name.
    rs_snapshot = ms_snapshot.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_repo_online DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS fetch_snapshot_cache FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_repo_online IMPLEMENTATION.
  METHOD fetch_snapshot_cache.
    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_file TYPE zif_abapgit_git_definitions=>ty_file,
          lo_connector TYPE REF TO lcl_test_git_connector,
          li_repo TYPE REF TO zif_abapgit_repo,
          li_repo_online TYPE REF TO zif_abapgit_repo_online,
          lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt,
          lv_commit TYPE zif_abapgit_git_definitions=>ty_sha1.

    ls_repo-key = '01'.
    ls_repo-url = 'https://example.com/team/library.git'.
    ls_repo-branch_name = 'refs/heads/main'.
    ls_repo-repo_kind = zif_abapgit_persistence=>c_repo_kind-git.

    ls_file-path = '/'.
    ls_file-filename = 'readme.md'.
    ls_file-data = '41'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).

    CREATE OBJECT lo_connector.
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    lo_connector->ms_snapshot-resolved_revision =
      'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.

    CREATE OBJECT li_repo_online TYPE zcl_abapgit_repo_online
      EXPORTING
        is_data = ls_repo
        ii_connector = lo_connector.
    li_repo = li_repo_online.

    lt_files = li_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_last_branch
      exp = 'refs/heads/main' ).

    lt_files = li_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 1 ).

    li_repo_online->select_branch( 'refs/heads/next' ).
    lt_files = li_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_last_branch
      exp = 'refs/heads/next' ).

    lv_commit = li_repo_online->get_current_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_commit
      exp = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' ).
  ENDMETHOD.
ENDCLASS.

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
