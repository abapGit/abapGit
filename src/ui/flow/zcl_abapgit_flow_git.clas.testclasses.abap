CLASS lcl_test_data DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor RAISING zcx_abapgit_exception.

    METHODS get_url
      RETURNING
        VALUE(rv_url) TYPE string.

    METHODS get_branches
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.

    METHODS get_commits
      RETURNING
        VALUE(rt_commits) TYPE zif_abapgit_definitions=>ty_objects_tt.

    METHODS get_object
      IMPORTING
        iv_sha1          TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_object.

    METHODS add_branch
      IMPORTING
        iv_name     TYPE string
        iv_parent   TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_filename TYPE string DEFAULT 'feature.abap'
        iv_content  TYPE string DEFAULT 'feature content'
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mv_url      TYPE string.
    DATA mt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA mt_commits  TYPE zif_abapgit_definitions=>ty_objects_tt.

    METHODS create_commit
      IMPORTING
        iv_parent      TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_parent2     TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_filename    TYPE string DEFAULT 'test.abap'
        iv_content     TYPE string DEFAULT 'test content'
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception.

    METHODS create_blob
      IMPORTING
        iv_content     TYPE string
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception.

    METHODS create_tree
      IMPORTING
        it_nodes       TYPE zcl_abapgit_git_pack=>ty_nodes_tt
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_test_data IMPLEMENTATION.
  METHOD constructor.
    DATA ls_branch TYPE zif_abapgit_git_definitions=>ty_git_branch.

    mv_url = 'https://github.com/test/repo.git'.

    " Create main branch with one commit
    ls_branch-display_name = zif_abapgit_flow_logic=>c_main.
    ls_branch-sha1 = create_commit( ).
    ls_branch-is_head = abap_true.
    INSERT ls_branch INTO TABLE mt_branches.
  ENDMETHOD.

  METHOD get_url.
    rv_url = mv_url.
  ENDMETHOD.

  METHOD get_branches.
    rt_branches = mt_branches.
  ENDMETHOD.

  METHOD get_commits.
    rt_commits = mt_commits.
  ENDMETHOD.

  METHOD get_object.
    READ TABLE mt_commits INTO rs_object
      WITH KEY sha1 = iv_sha1.
  ENDMETHOD.

  METHOD add_branch.
    DATA ls_branch TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_parent TYPE zif_abapgit_git_definitions=>ty_sha1.

    IF iv_parent IS INITIAL.
      " Default to main branch if no parent specified
      READ TABLE mt_branches INTO ls_branch
        WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
      lv_parent = ls_branch-sha1.
    ELSE.
      lv_parent = iv_parent.
    ENDIF.

    ls_branch-display_name = iv_name.
    ls_branch-sha1 = create_commit(
      iv_parent   = lv_parent
      iv_filename = iv_filename
      iv_content  = iv_content ).
    ls_branch-is_head = abap_false.
    INSERT ls_branch INTO TABLE mt_branches.
  ENDMETHOD.

  METHOD create_commit.
    DATA ls_commit    TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA ls_node      TYPE zcl_abapgit_git_pack=>ty_node.
    DATA ls_object    TYPE zif_abapgit_definitions=>ty_object.
    DATA lt_nodes     TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA lv_blob_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_data      TYPE xstring.
    DATA lv_tree_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    " Create blob for file content
    lv_blob_sha1 = create_blob( iv_content ).

    " Create tree with the blob
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name = iv_filename.
    ls_node-sha1 = lv_blob_sha1.
    APPEND ls_node TO lt_nodes.
    lv_tree_sha1 = create_tree( lt_nodes ).

    " Create a minimal commit structure
    ls_commit-tree = lv_tree_sha1.
    ls_commit-parent = iv_parent.
    ls_commit-parent2 = iv_parent2.
    ls_commit-author = 'Test User <test@test.com>'.
    ls_commit-committer = 'Test User <test@test.com>'.
    ls_commit-body = 'Test commit'.

    lv_data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).

    ls_object-sha1 = zcl_abapgit_hash=>sha1_raw( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-commit.
    ls_object-data = lv_data.

    INSERT ls_object INTO TABLE mt_commits.

    rv_sha1 = ls_object-sha1.
  ENDMETHOD.

  METHOD create_blob.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA lv_data   TYPE xstring.

    " Convert string content to xstring
    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( iv_content ).

    ls_object-sha1 = zcl_abapgit_hash=>sha1_blob( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = lv_data.

    INSERT ls_object INTO TABLE mt_commits.

    rv_sha1 = ls_object-sha1.
  ENDMETHOD.

  METHOD create_tree.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA lv_data   TYPE xstring.

    lv_data = zcl_abapgit_git_pack=>encode_tree( it_nodes ).

    ls_object-sha1 = zcl_abapgit_hash=>sha1_tree( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-tree.
    ls_object-data = lv_data.

    INSERT ls_object INTO TABLE mt_commits.

    rv_sha1 = ls_object-sha1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mock_gitv2 DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gitv2_porcelain.

    METHODS constructor
      IMPORTING
        io_test_data TYPE REF TO lcl_test_data.

  PRIVATE SECTION.
    DATA mo_test_data TYPE REF TO lcl_test_data.
ENDCLASS.

CLASS lcl_mock_gitv2 IMPLEMENTATION.
  METHOD constructor.
    mo_test_data = io_test_data.
  ENDMETHOD.

  METHOD zif_abapgit_gitv2_porcelain~list_branches.
    " Not used in find_up_to_date
    ASSERT 1 = 2.
  ENDMETHOD.

  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs.
    " Not used in find_up_to_date
    ASSERT 1 = 2.
  ENDMETHOD.

  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs_multi.
    " Not used in find_up_to_date
    ASSERT 1 = 2.
  ENDMETHOD.

  METHOD zif_abapgit_gitv2_porcelain~commits_last_year.
    rt_objects = mo_test_data->get_commits( ).
  ENDMETHOD.

  METHOD zif_abapgit_gitv2_porcelain~fetch_blob.
    " Not used in find_up_to_date
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.

***************************************************************************

CLASS ltcl_find_up_to_date DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    METHODS setup.
    METHODS teardown.

    METHODS single_branch_up_to_date FOR TESTING RAISING zcx_abapgit_exception.
    METHODS branch_not_up_to_date FOR TESTING RAISING zcx_abapgit_exception.
    METHODS only_main_branch FOR TESTING RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mo_test_data TYPE REF TO lcl_test_data.
ENDCLASS.

CLASS ltcl_find_up_to_date IMPLEMENTATION.

  METHOD setup.
    DATA lo_mock_gitv2 TYPE REF TO lcl_mock_gitv2.

    CREATE OBJECT mo_test_data.

    CREATE OBJECT lo_mock_gitv2
      EXPORTING
        io_test_data = mo_test_data.

    zcl_abapgit_git_injector=>set_v2_porcelain( lo_mock_gitv2 ).
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_git_injector=>set_v2_porcelain( ).
  ENDMETHOD.

  METHOD single_branch_up_to_date.
    " Scenario: feature branch is based on main and has one commit
    " Expected: branch should be marked as up-to-date = false
    " (because it has commits that are not in main)

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature  LIKE LINE OF lt_features.
    DATA ls_branch   LIKE LINE OF lt_branches.

    " Add a feature branch
    mo_test_data->add_branch( 'feature/test' ).

    lt_branches = mo_test_data->get_branches( ).

    " Create features structure
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      CLEAR ls_feature.
      ls_feature-branch = ls_branch.
      INSERT ls_feature INTO TABLE lt_features.
    ENDLOOP.

    " Call the method under test
    zcl_abapgit_flow_git=>find_up_to_date(
      EXPORTING
        iv_url      = mo_test_data->get_url( )
        it_branches = lt_branches
      CHANGING
        ct_features = lt_features ).

    " Assert: feature branch should have up_to_date = false
    " (it has commits not in main)
    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-up_to_date
      exp = abap_false
      msg = 'Branch with new commits should not be up-to-date' ).
  ENDMETHOD.

  METHOD branch_not_up_to_date.
    " Scenario: main has moved forward, feature branch is behind
    " Expected: branch should be marked as up-to-date = false

    DATA lt_branches      TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features      TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature       LIKE LINE OF lt_features.
    DATA ls_main          TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA ls_branch        LIKE LINE OF lt_branches.
    DATA lv_old_main_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_new_main_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    FIELD-SYMBOLS <ls_main> LIKE LINE OF lt_branches.

    " Get current main SHA
    lt_branches = mo_test_data->get_branches( ).
    READ TABLE lt_branches INTO ls_main
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    lv_old_main_sha1 = ls_main-sha1.

    " Add a feature branch based on current main
    mo_test_data->add_branch( 'feature/old' ).

    " Advance main with a new commit
    lv_new_main_sha1 = mo_test_data->create_commit( iv_parent = lv_old_main_sha1 ).

    " Update main branch to new commit
    lt_branches = mo_test_data->get_branches( ).
    READ TABLE lt_branches ASSIGNING <ls_main>
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    <ls_main>-sha1 = lv_new_main_sha1.

    " Create features structure
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      CLEAR ls_feature.
      ls_feature-branch = ls_branch.
      INSERT ls_feature INTO TABLE lt_features.
    ENDLOOP.

    " Call the method under test
    zcl_abapgit_flow_git=>find_up_to_date(
      EXPORTING
        iv_url      = mo_test_data->get_url( )
        it_branches = lt_branches
      CHANGING
        ct_features = lt_features ).

    " Assert: feature branch should have up_to_date = false
    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-up_to_date
      exp = abap_false
      msg = 'Branch behind main should not be up-to-date' ).
  ENDMETHOD.

  METHOD only_main_branch.
    " Scenario: only main branch exists
    " Expected: method should return without error

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.

    lt_branches = mo_test_data->get_branches( ).

    " Call the method under test - should return immediately
    zcl_abapgit_flow_git=>find_up_to_date(
      EXPORTING
        iv_url      = mo_test_data->get_url( )
        it_branches = lt_branches
      CHANGING
        ct_features = lt_features ).

    " Assert: features should be empty
    cl_abap_unit_assert=>assert_initial(
      act = lt_features
      msg = 'Features should be empty when only main branch exists' ).
  ENDMETHOD.

ENDCLASS.
