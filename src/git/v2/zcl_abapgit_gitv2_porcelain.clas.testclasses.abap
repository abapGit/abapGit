CLASS ltcl_gitv2_porcelain DEFINITION DEFERRED.
CLASS zcl_abapgit_gitv2_porcelain DEFINITION LOCAL FRIENDS ltcl_gitv2_porcelain.

CLASS ltcl_gitv2_porcelain DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS build_tree_object
      IMPORTING
        it_nodes   TYPE zcl_abapgit_git_pack=>ty_nodes_tt
      EXPORTING
        ev_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1
      CHANGING
        ct_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

    METHODS build_commit_object
      IMPORTING
        iv_tree_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1
      EXPORTING
        ev_sha1      TYPE zif_abapgit_git_definitions=>ty_sha1
      CHANGING
        ct_objects   TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

    " path_needed tests
    METHODS path_needed_no_filter FOR TESTING.
    METHODS path_needed_ancestor FOR TESTING.
    METHODS path_needed_descendant FOR TESTING.
    METHODS path_needed_exact_match FOR TESTING.
    METHODS path_needed_no_match FOR TESTING.

    " compute_max_depth tests
    METHODS depth_empty_paths FOR TESTING.
    METHODS depth_single_level FOR TESTING.
    METHODS depth_three_levels FOR TESTING.
    METHODS depth_multiple_paths FOR TESTING.

    " walk_tree_level tests
    METHODS walk_flat_tree FOR TESTING
      RAISING zcx_abapgit_exception.
    METHODS walk_with_path_filter FOR TESTING
      RAISING zcx_abapgit_exception.
    METHODS walk_nested_tree FOR TESTING
      RAISING zcx_abapgit_exception.

    " walk_tree_from_objects tests
    METHODS walk_from_objects_basic FOR TESTING
      RAISING zcx_abapgit_exception.
    METHODS walk_from_objects_no_commit FOR TESTING.

ENDCLASS.


CLASS ltcl_gitv2_porcelain IMPLEMENTATION.

  METHOD build_tree_object.
    DATA ls_obj  LIKE LINE OF ct_objects.
    DATA lv_data TYPE xstring.

    lv_data = zcl_abapgit_git_pack=>encode_tree( it_nodes ).
    ls_obj-data = lv_data.
    ls_obj-type = zif_abapgit_git_definitions=>c_type-tree.
    ls_obj-sha1 = zcl_abapgit_hash=>sha1_tree( lv_data ).
    APPEND ls_obj TO ct_objects.
    ev_sha1 = ls_obj-sha1.
  ENDMETHOD.

  METHOD build_commit_object.
    DATA ls_obj    LIKE LINE OF ct_objects.
    DATA ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    ls_commit-tree      = iv_tree_sha1.
    ls_commit-author    = 'Test <t@t.com> 0 +0000'.
    ls_commit-committer = 'Test <t@t.com> 0 +0000'.
    ls_commit-body      = 'test'.

    ls_obj-data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    ls_obj-type = zif_abapgit_git_definitions=>c_type-commit.
    ls_obj-sha1 = zcl_abapgit_hash=>sha1_commit( ls_obj-data ).
    APPEND ls_obj TO ct_objects.
    ev_sha1 = ls_obj-sha1.
  ENDMETHOD.


  " -- path_needed ------------------------------------------------------------

  METHOD path_needed_no_filter.
    " Empty wanted list -> every path is needed
    DATA lt_wanted TYPE string_table.

    cl_abap_unit_assert=>assert_true(
      act = zcl_abapgit_gitv2_porcelain=>path_needed(
              iv_path         = '/src/foo/'
              it_wanted_paths = lt_wanted )
      msg = 'All paths needed when filter is empty' ).
  ENDMETHOD.

  METHOD path_needed_ancestor.
    " iv_path = '/src/' is an ancestor of wanted '/src/pkg/'  -> needed
    DATA lt_wanted TYPE string_table.
    APPEND '/src/pkg/' TO lt_wanted.

    cl_abap_unit_assert=>assert_true(
      act = zcl_abapgit_gitv2_porcelain=>path_needed(
              iv_path         = '/src/'
              it_wanted_paths = lt_wanted )
      msg = 'Ancestor path must be needed' ).
  ENDMETHOD.

  METHOD path_needed_descendant.
    " iv_path = '/src/pkg/sub/' is a descendant of wanted '/src/' -> needed
    DATA lt_wanted TYPE string_table.
    APPEND '/src/' TO lt_wanted.

    cl_abap_unit_assert=>assert_true(
      act = zcl_abapgit_gitv2_porcelain=>path_needed(
              iv_path         = '/src/pkg/sub/'
              it_wanted_paths = lt_wanted )
      msg = 'Descendant path must be needed' ).
  ENDMETHOD.

  METHOD path_needed_exact_match.
    " iv_path exactly equals a wanted path -> needed
    DATA lt_wanted TYPE string_table.
    APPEND '/src/pkg/' TO lt_wanted.

    cl_abap_unit_assert=>assert_true(
      act = zcl_abapgit_gitv2_porcelain=>path_needed(
              iv_path         = '/src/pkg/'
              it_wanted_paths = lt_wanted )
      msg = 'Exact match must be needed' ).
  ENDMETHOD.

  METHOD path_needed_no_match.
    " '/other/' has no relationship to '/src/' -> not needed
    DATA lt_wanted TYPE string_table.
    APPEND '/src/' TO lt_wanted.

    cl_abap_unit_assert=>assert_false(
      act = zcl_abapgit_gitv2_porcelain=>path_needed(
              iv_path         = '/other/'
              it_wanted_paths = lt_wanted )
      msg = 'Unrelated path must not be needed' ).
  ENDMETHOD.


  " -- compute_max_depth ------------------------------------------------------

  METHOD depth_empty_paths.
    " No wanted paths -> depth 0 (full fetch fallback)
    DATA lt_paths TYPE string_table.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gitv2_porcelain=>compute_max_depth( lt_paths )
      exp = 0
      msg = 'Empty paths must return depth 0' ).
  ENDMETHOD.

  METHOD depth_single_level.
    " '/src/' has 2 slashes -> depth 2
    DATA lt_paths TYPE string_table.
    APPEND '/src/' TO lt_paths.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gitv2_porcelain=>compute_max_depth( lt_paths )
      exp = 2
      msg = '/src/ must return depth 2' ).
  ENDMETHOD.

  METHOD depth_three_levels.
    " '/src/pkg/sub/' has 4 slashes -> depth 4
    DATA lt_paths TYPE string_table.
    APPEND '/src/pkg/sub/' TO lt_paths.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gitv2_porcelain=>compute_max_depth( lt_paths )
      exp = 4
      msg = '/src/pkg/sub/ must return depth 4' ).
  ENDMETHOD.

  METHOD depth_multiple_paths.
    " Deepest of '/src/' and '/src/pkg/sub/' determines the result
    DATA lt_paths TYPE string_table.
    APPEND '/src/' TO lt_paths.
    APPEND '/src/pkg/sub/' TO lt_paths.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gitv2_porcelain=>compute_max_depth( lt_paths )
      exp = 4
      msg = 'Must return depth of deepest path' ).
  ENDMETHOD.


  " -- walk_tree_level --------------------------------------------------------

  METHOD walk_flat_tree.
    " A single-level tree with two files, no filter -> both files returned
    DATA lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lt_nodes    TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node     LIKE LINE OF lt_nodes.
    DATA lt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lv_tree_sha TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_wanted   TYPE string_table.   " empty = no filter

    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'foo.abap'.
    ls_node-sha1  = '1111111111111111111111111111111111111111'.
    APPEND ls_node TO lt_nodes.

    ls_node-name = 'bar.xml'.
    ls_node-sha1 = '2222222222222222222222222222222222222222'.
    APPEND ls_node TO lt_nodes.

    build_tree_object(
      EXPORTING it_nodes   = lt_nodes
      IMPORTING ev_sha1    = lv_tree_sha
      CHANGING  ct_objects = lt_objects ).

    zcl_abapgit_gitv2_porcelain=>walk_tree_level(
      EXPORTING
        iv_url          = ''
        it_objects      = lt_objects
        iv_tree_sha1    = lv_tree_sha
        iv_base         = '/'
        it_wanted_paths = lt_wanted
      CHANGING
        ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 2
      msg = 'Both files must appear with no filter' ).
  ENDMETHOD.

  METHOD walk_with_path_filter.
    " Only /src/ is wanted -> /other/ subtree should be skipped
    DATA lt_objects   TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lt_nodes     TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node      LIKE LINE OF lt_nodes.
    DATA lt_expanded  TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lv_root_sha  TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_src_sha   TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_wanted    TYPE string_table.
    DATA lt_src_nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_exp       LIKE LINE OF lt_expanded.

    APPEND '/src/' TO lt_wanted.

    " Build /src/ subtree with one file
    CLEAR lt_src_nodes.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'cl_foo.abap'.
    ls_node-sha1  = '1111111111111111111111111111111111111111'.
    APPEND ls_node TO lt_src_nodes.
    build_tree_object(
      EXPORTING it_nodes   = lt_src_nodes
      IMPORTING ev_sha1    = lv_src_sha
      CHANGING  ct_objects = lt_objects ).

    " Build root tree: /src/ (dir) and /other/ (dir, pruned)
    CLEAR lt_nodes.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-dir.
    ls_node-name  = 'src'.
    ls_node-sha1  = lv_src_sha.
    APPEND ls_node TO lt_nodes.
    ls_node-name  = 'other'.
    ls_node-sha1  = '9999999999999999999999999999999999999999'.
    APPEND ls_node TO lt_nodes.
    build_tree_object(
      EXPORTING it_nodes   = lt_nodes
      IMPORTING ev_sha1    = lv_root_sha
      CHANGING  ct_objects = lt_objects ).

    zcl_abapgit_gitv2_porcelain=>walk_tree_level(
      EXPORTING
        iv_url          = ''
        it_objects      = lt_objects
        iv_tree_sha1    = lv_root_sha
        iv_base         = '/'
        it_wanted_paths = lt_wanted
      CHANGING
        ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 1
      msg = 'Only file under /src/ must be returned' ).

    READ TABLE lt_expanded INTO ls_exp INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_exp-path
      exp = '/src/'
      msg = 'Returned file must be in /src/' ).
  ENDMETHOD.

  METHOD walk_nested_tree.
    " Two-level tree: /src/pkg/ with one file -> verify path and name
    DATA lt_objects   TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lt_nodes     TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node      LIKE LINE OF lt_nodes.
    DATA lt_expanded  TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lv_root_sha  TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_src_sha   TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_pkg_sha   TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_wanted    TYPE string_table.
    DATA lt_sub_nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_exp       LIKE LINE OF lt_expanded.

    " /src/pkg/cl_bar.abap
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'cl_bar.abap'.
    ls_node-sha1  = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.
    APPEND ls_node TO lt_sub_nodes.
    build_tree_object(
      EXPORTING it_nodes   = lt_sub_nodes
      IMPORTING ev_sha1    = lv_pkg_sha
      CHANGING  ct_objects = lt_objects ).

    CLEAR lt_sub_nodes.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-dir.
    ls_node-name  = 'pkg'.
    ls_node-sha1  = lv_pkg_sha.
    APPEND ls_node TO lt_sub_nodes.
    build_tree_object(
      EXPORTING it_nodes   = lt_sub_nodes
      IMPORTING ev_sha1    = lv_src_sha
      CHANGING  ct_objects = lt_objects ).

    CLEAR lt_nodes.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-dir.
    ls_node-name  = 'src'.
    ls_node-sha1  = lv_src_sha.
    APPEND ls_node TO lt_nodes.
    build_tree_object(
      EXPORTING it_nodes   = lt_nodes
      IMPORTING ev_sha1    = lv_root_sha
      CHANGING  ct_objects = lt_objects ).

    zcl_abapgit_gitv2_porcelain=>walk_tree_level(
      EXPORTING
        iv_url          = ''
        it_objects      = lt_objects
        iv_tree_sha1    = lv_root_sha
        iv_base         = '/'
        it_wanted_paths = lt_wanted
      CHANGING
        ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 1
      msg = 'Exactly one file from nested tree' ).
    READ TABLE lt_expanded INTO ls_exp INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_exp-path
      exp = '/src/pkg/'
      msg = 'File path must reflect nesting' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_exp-name
      exp = 'cl_bar.abap'
      msg = 'Filename must match tree node' ).
  ENDMETHOD.


  " -- walk_tree_from_objects --------------------------------------------------

  METHOD walk_from_objects_basic.
    " Full object set (commit + tree + blob) -> file is returned
    DATA lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lt_nodes    TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node     LIKE LINE OF lt_nodes.
    DATA lt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lv_tree_sha TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_wanted   TYPE string_table.
    DATA ls_exp      LIKE LINE OF lt_expanded.

    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'readme.txt'.
    ls_node-sha1  = 'cccccccccccccccccccccccccccccccccccccccc'.
    APPEND ls_node TO lt_nodes.

    build_tree_object(
      EXPORTING it_nodes   = lt_nodes
      IMPORTING ev_sha1    = lv_tree_sha
      CHANGING  ct_objects = lt_objects ).

    build_commit_object(
      EXPORTING iv_tree_sha1 = lv_tree_sha
      CHANGING  ct_objects   = lt_objects ).

    zcl_abapgit_gitv2_porcelain=>walk_tree_from_objects(
      EXPORTING
        iv_url          = ''
        it_objects      = lt_objects
        iv_base         = '/'
        it_wanted_paths = lt_wanted
      CHANGING
        ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 1
      msg = 'One file expected from commit+tree' ).
    READ TABLE lt_expanded INTO ls_exp INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_exp-name
      exp = 'readme.txt'
      msg = 'Filename must match tree node' ).
  ENDMETHOD.

  METHOD walk_from_objects_no_commit.
    " Missing commit in object list -> exception raised
    DATA lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lt_wanted   TYPE string_table.
    DATA lx_error    TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_gitv2_porcelain=>walk_tree_from_objects(
          EXPORTING
            iv_url          = ''
            it_objects      = lt_objects
            iv_base         = '/'
            it_wanted_paths = lt_wanted
          CHANGING
            ct_expanded     = lt_expanded ).
        cl_abap_unit_assert=>fail( 'Exception expected for missing commit' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( )
          exp = '*Commit*'
          msg = 'Exception message must mention Commit' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
