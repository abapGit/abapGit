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

    METHODS get_all_objects
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt.

    METHODS get_object
      IMPORTING
        iv_sha1          TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_object.

    METHODS add_branch
      IMPORTING
        iv_name     TYPE string
        iv_parent   TYPE zif_abapgit_git_definitions=>ty_sha1
        iv_filename TYPE string
        iv_content  TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS create_commit
      IMPORTING
        iv_parent      TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_parent2     TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_filename    TYPE string
        iv_content     TYPE string
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception.

    METHODS get_main_branch_sha1
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS get_dotabapgit
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit.

  PRIVATE SECTION.
    DATA mv_url TYPE string.
    DATA mt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

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
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    mv_url = 'https://github.com/test/repo.git'.

    add_branch(
      iv_name     = zif_abapgit_flow_logic=>c_main
      iv_parent   = '0000000000000000000000000000000000000000'
      iv_filename = 'main.abap'
      iv_content  = 'main' ).

    READ TABLE mt_branches ASSIGNING <ls_branch>
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    ASSERT sy-subrc = 0.
    <ls_branch>-is_head = abap_true.
  ENDMETHOD.

  METHOD get_dotabapgit.
    DATA ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

    ls_data-starting_folder = '/'.

    CREATE OBJECT ro_dot
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD get_main_branch_sha1.
    DATA ls_branch LIKE LINE OF mt_branches.

    READ TABLE mt_branches INTO ls_branch
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    ASSERT sy-subrc = 0.

    rv_sha1 = ls_branch-sha1.
  ENDMETHOD.

  METHOD get_url.
    rv_url = mv_url.
  ENDMETHOD.

  METHOD get_branches.
    rt_branches = mt_branches.
  ENDMETHOD.

  METHOD get_commits.
    DATA ls_object LIKE LINE OF rt_commits.
    LOOP AT mt_objects USING KEY type INTO ls_object
        WHERE type = zif_abapgit_git_definitions=>c_type-commit.
      APPEND ls_object TO rt_commits.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_all_objects.
    rt_objects = mt_objects.
  ENDMETHOD.

  METHOD get_object.
    READ TABLE mt_objects INTO rs_object WITH TABLE KEY sha
      COMPONENTS sha1 = iv_sha1.
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
    DATA ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA ls_node TYPE zcl_abapgit_git_pack=>ty_node.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA ls_parent_commit TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA lt_nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA lt_parent_nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA lv_blob_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_data TYPE xstring.
    DATA lv_tree_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    " If we have a parent commit, inherit its tree nodes
    IF iv_parent IS NOT INITIAL.
      READ TABLE mt_objects INTO ls_object WITH TABLE KEY sha COMPONENTS sha1 = iv_parent.
      IF sy-subrc = 0.
        ls_parent_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).
        " Get parent tree
        READ TABLE mt_objects INTO ls_object WITH TABLE KEY sha COMPONENTS sha1 = ls_parent_commit-tree.
        IF sy-subrc = 0.
          lt_parent_nodes = zcl_abapgit_git_pack=>decode_tree( ls_object-data ).
          lt_nodes = lt_parent_nodes.
        ENDIF.
      ENDIF.
    ENDIF.

    " Create blob for file content
    lv_blob_sha1 = create_blob( iv_content ).

    " Add or update the file in the tree
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name = iv_filename.
    ls_node-sha1 = lv_blob_sha1.

    " Remove existing node with same name if it exists
    DELETE lt_nodes WHERE name = iv_filename.

    " Add the new/updated node
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

    INSERT ls_object INTO TABLE mt_objects.

    rv_sha1 = ls_object-sha1.
  ENDMETHOD.

  METHOD create_blob.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA lv_data TYPE xstring.

    " Convert string content to xstring
    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( iv_content ).

    ls_object-sha1 = zcl_abapgit_hash=>sha1_blob( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_object-data = lv_data.

    INSERT ls_object INTO TABLE mt_objects.

    rv_sha1 = ls_object-sha1.
  ENDMETHOD.

  METHOD create_tree.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA lv_data TYPE xstring.

    lv_data = zcl_abapgit_git_pack=>encode_tree( it_nodes ).

    ls_object-sha1 = zcl_abapgit_hash=>sha1_tree( lv_data ).
    ls_object-type = zif_abapgit_git_definitions=>c_type-tree.
    ls_object-data = lv_data.

    INSERT ls_object INTO TABLE mt_objects.

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
    " Return all objects (trees, commits) for the given SHA1s - no blobs
    rt_objects = mo_test_data->get_all_objects( ).
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

CLASS ltcl_find_changes_in_git DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    METHODS basic_test FOR TESTING RAISING zcx_abapgit_exception.
    METHODS handles_empty_features FOR TESTING RAISING zcx_abapgit_exception.
    METHODS both_branches_diverged FOR TESTING RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS setup RAISING zcx_abapgit_exception.
    METHODS teardown RAISING zcx_abapgit_exception.

    DATA mo_test_data TYPE REF TO lcl_test_data.
ENDCLASS.

CLASS ltcl_find_changes_in_git IMPLEMENTATION.

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

  METHOD basic_test.
    " Scenario: Basic test that method can be called without errors
    " Expected: Method executes without exception with valid input

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature LIKE LINE OF lt_features.
    DATA ls_branch LIKE LINE OF lt_branches.
    DATA lt_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA ls_changed_file LIKE LINE OF ls_feature-changed_files.

    " Add a feature branch
    mo_test_data->add_branch(
      iv_name     = 'feature/test'
      iv_parent   = mo_test_data->get_main_branch_sha1( )
      iv_filename = 'feature.abap'
      iv_content  = 'feature content' ).

    lt_branches = mo_test_data->get_branches( ).

    " Create features structure
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      CLEAR ls_feature.
      ls_feature-branch-display_name = ls_branch-display_name.
      ls_feature-branch-sha1 = ls_branch-sha1.
      INSERT ls_feature INTO TABLE lt_features.
    ENDLOOP.

    " Call the method under test - should not raise exception
    zcl_abapgit_flow_git=>find_changes_in_git(
      EXPORTING
        iv_url         = mo_test_data->get_url( )
        io_dot         = mo_test_data->get_dotabapgit( )
        iv_package     = 'ZTEST_PACKAGE'
        it_branches      = lt_branches
      IMPORTING
        et_main_expanded = lt_main_expanded
      CHANGING
        ct_features      = lt_features ).

    " Assert: Feature branch structure should be populated
    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( msg = 'Feature branch should be in features table' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-display_name
      exp = 'feature/test'
      msg = 'Feature branch name should match' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_feature-changed_files )
      exp = 1 ).

    LOOP AT ls_feature-changed_files INTO ls_changed_file.
      cl_abap_unit_assert=>assert_equals(
        act = ls_changed_file-filename
        exp = 'feature.abap'
        msg = 'Changed file name should match' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD handles_empty_features.
    " Scenario: Only main branch exists (no feature branches)
    " Expected: Method executes without error when ct_features is empty

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA lt_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.

    lt_branches = mo_test_data->get_branches( ).

    " Call the method under test - should not raise exception
    zcl_abapgit_flow_git=>find_changes_in_git(
      EXPORTING
        iv_url         = mo_test_data->get_url( )
        io_dot         = mo_test_data->get_dotabapgit( )
        iv_package     = 'ZTEST_PACKAGE'
        it_branches      = lt_branches
      IMPORTING
        et_main_expanded = lt_main_expanded
      CHANGING
        ct_features      = lt_features ).

    " Assert: features should remain empty (no feature branches)
    cl_abap_unit_assert=>assert_initial(
      act = lt_features
      msg = 'Features should be empty when only main branch exists' ).

  ENDMETHOD.

  METHOD both_branches_diverged.
    " Scenario: Both feature and main branch have additional commits
    " Expected: Method detects changes in both branches

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature LIKE LINE OF lt_features.
    DATA ls_branch LIKE LINE OF lt_branches.
    DATA ls_main TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA lt_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lv_old_main_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_new_main_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    FIELD-SYMBOLS <ls_main> LIKE LINE OF lt_branches.


    " Get current main SHA
    lt_branches = mo_test_data->get_branches( ).
    READ TABLE lt_branches INTO ls_main
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    lv_old_main_sha1 = ls_main-sha1.

    " Add a feature branch based on current main
    mo_test_data->add_branch(
      iv_name     = 'feature/diverged'
      iv_parent   = mo_test_data->get_main_branch_sha1( )
      iv_filename = 'feature-file.abap'
      iv_content  = 'feature content' ).

    " Advance main with a new commit
    lv_new_main_sha1 = mo_test_data->create_commit(
      iv_parent   = lv_old_main_sha1
      iv_filename = 'main-file.abap'
      iv_content  = 'main content' ).

    " Update main branch to new commit
    lt_branches = mo_test_data->get_branches( ).
    READ TABLE lt_branches ASSIGNING <ls_main>
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    <ls_main>-sha1 = lv_new_main_sha1.

    " Create features structure
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      CLEAR ls_feature.
      ls_feature-branch-display_name = ls_branch-display_name.
      ls_feature-branch-sha1 = ls_branch-sha1.
      INSERT ls_feature INTO TABLE lt_features.
    ENDLOOP.

    " Call the method under test
    zcl_abapgit_flow_git=>find_changes_in_git(
      EXPORTING
        iv_url           = mo_test_data->get_url( )
        io_dot           = mo_test_data->get_dotabapgit( )
        iv_package       = 'ZTEST_PACKAGE'
        it_branches      = lt_branches
      IMPORTING
        et_main_expanded = lt_main_expanded
      CHANGING
        ct_features      = lt_features ).

    " Assert: Feature branch structure should be populated
    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-display_name
      exp = 'feature/diverged' ).

    " Assert: Feature branch should have at least one changed file
    " todo: LOOP AT ls_feature-changed_files INTO ls_changed_file.
    " todo:   cl_abap_unit_assert=>assert_equals(
    " todo:     act = ls_changed_file-filename
    " todo:     exp = 'feature-file.abap' ).
    " todo: ENDLOOP.

    " Assert: Feature branch should be marked as not up-to-date
    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-up_to_date
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.

***************************************************************************

CLASS ltcl_find_up_to_date DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    METHODS single_branch_up_to_date FOR TESTING RAISING zcx_abapgit_exception.
    METHODS branch_not_up_to_date FOR TESTING RAISING zcx_abapgit_exception.
    METHODS only_main_branch FOR TESTING RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS setup RAISING zcx_abapgit_exception.
    METHODS teardown RAISING zcx_abapgit_exception.

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
    " Expected: branch should be marked as up-to-date = true
    " (because it is directly based on main without any divergence)

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature  LIKE LINE OF lt_features.
    DATA ls_branch   LIKE LINE OF lt_branches.

    " Add a feature branch based on main
    mo_test_data->add_branch(
      iv_name     = 'feature/test'
      iv_parent   = mo_test_data->get_main_branch_sha1( )
      iv_filename = 'new.txt'
      iv_content  = 'new content' ).

    lt_branches = mo_test_data->get_branches( ).

    " Create features structure
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      CLEAR ls_feature.
      ls_feature-branch-display_name = ls_branch-display_name.
      ls_feature-branch-sha1 = ls_branch-sha1.
      INSERT ls_feature INTO TABLE lt_features.
    ENDLOOP.

    " Call the method under test
    zcl_abapgit_flow_git=>find_up_to_date(
      EXPORTING
        it_branches = lt_branches
        it_objects  = mo_test_data->get_all_objects( )
      CHANGING
        ct_features = lt_features ).

    " Assert: feature branch should have up_to_date = true
    " (it is ahead of main but directly based on it)
    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-up_to_date
      exp = abap_true
      msg = 'Branch ahead of main but directly based on it should be up-to-date' ).
  ENDMETHOD.

  METHOD branch_not_up_to_date.
    " Scenario: main has moved forward, feature branch is behind
    " Expected: branch should be marked as up-to-date = false

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature LIKE LINE OF lt_features.
    DATA ls_main TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA ls_branch LIKE LINE OF lt_branches.
    DATA lv_old_main_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_new_main_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    FIELD-SYMBOLS <ls_main> LIKE LINE OF lt_branches.

    " Get current main SHA
    lt_branches = mo_test_data->get_branches( ).
    READ TABLE lt_branches INTO ls_main
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    lv_old_main_sha1 = ls_main-sha1.

    " Add a feature branch based on current main
    mo_test_data->add_branch(
      iv_name     = 'feature/old'
      iv_parent   = mo_test_data->get_main_branch_sha1( )
      iv_filename = 'feature.old.abap'
      iv_content  = 'feature old content' ).

    " Advance main with a new commit
    lv_new_main_sha1 = mo_test_data->create_commit(
      iv_parent   = lv_old_main_sha1
      iv_filename = 'something.abap'
      iv_content  = 'something' ).

    " Update main branch to new commit
    lt_branches = mo_test_data->get_branches( ).
    READ TABLE lt_branches ASSIGNING <ls_main>
      WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    <ls_main>-sha1 = lv_new_main_sha1.

    " Create features structure
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      CLEAR ls_feature.
      ls_feature-branch-display_name = ls_branch-display_name.
      ls_feature-branch-sha1 = ls_branch-sha1.
      INSERT ls_feature INTO TABLE lt_features.
    ENDLOOP.

    " Call the method under test
    zcl_abapgit_flow_git=>find_up_to_date(
      EXPORTING
        it_branches = lt_branches
        it_objects  = mo_test_data->get_all_objects( )
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
        it_branches = lt_branches
        it_objects  = mo_test_data->get_all_objects( )
      CHANGING
        ct_features = lt_features ).

    " Assert: features should be empty
    cl_abap_unit_assert=>assert_initial(
      act = lt_features
      msg = 'Features should be empty when only main branch exists' ).
  ENDMETHOD.

ENDCLASS.
