CLASS ltcl_git_porcelain DEFINITION DEFERRED.
CLASS zcl_abapgit_git_porcelain DEFINITION LOCAL FRIENDS ltcl_git_porcelain.

CLASS ltcl_git_porcelain DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      append
        IMPORTING iv_path TYPE string
                  iv_name TYPE string,
      single_file FOR TESTING
        RAISING zcx_abapgit_exception,
      two_files_same_path FOR TESTING
        RAISING zcx_abapgit_exception,
      root_empty FOR TESTING
        RAISING zcx_abapgit_exception,
      namespaces FOR TESTING
        RAISING zcx_abapgit_exception,
      more_sub FOR TESTING
        RAISING zcx_abapgit_exception,
      sub FOR TESTING
        RAISING zcx_abapgit_exception,
      pull_full_walk_no_filter FOR TESTING
        RAISING zcx_abapgit_exception,
      filter_expanded_keeps_match FOR TESTING
        RAISING zcx_abapgit_exception,
      filter_expanded_no_match FOR TESTING
        RAISING zcx_abapgit_exception,
      filter_expanded_empty_wanted FOR TESTING
        RAISING zcx_abapgit_exception,
      filter_expanded_dot_files FOR TESTING
        RAISING zcx_abapgit_exception.

    METHODS build_tree_object
      IMPORTING
        it_nodes   TYPE zcl_abapgit_git_pack=>ty_nodes_tt
      EXPORTING
        ev_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1
      CHANGING
        ct_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

    DATA: mt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt,
          mt_trees    TYPE zcl_abapgit_git_porcelain=>ty_trees_tt.

ENDCLASS.

CLASS ltcl_git_porcelain IMPLEMENTATION.

  METHOD setup.
    CLEAR mt_expanded.
    CLEAR mt_trees.
  ENDMETHOD.

  METHOD append.

    FIELD-SYMBOLS: <ls_expanded> LIKE LINE OF mt_expanded.


    APPEND INITIAL LINE TO mt_expanded ASSIGNING <ls_expanded>.
    <ls_expanded>-path  = iv_path.
    <ls_expanded>-name  = iv_name.
    <ls_expanded>-sha1  = 'a'.
    <ls_expanded>-chmod = zif_abapgit_git_definitions=>c_chmod-file.

  ENDMETHOD.

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

  METHOD single_file.

    append( iv_path = '/'
            iv_name = 'foobar.txt' ).

    mt_trees = zcl_abapgit_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD two_files_same_path.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/'
            iv_name = 'bar.txt' ).

    mt_trees = zcl_abapgit_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD sub.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = zcl_abapgit_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

  METHOD more_sub.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF mt_trees.

    append( iv_path = '/src/foo_a/foo_a1/'
            iv_name = 'a1.txt' ).

    append( iv_path = '/src/foo_a/foo_a2/'
            iv_name = 'a2.txt' ).

    mt_trees = zcl_abapgit_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 5 ).

    LOOP AT mt_trees ASSIGNING <ls_tree>.
      cl_abap_unit_assert=>assert_not_initial( <ls_tree>-data ).
    ENDLOOP.

  ENDMETHOD.

  METHOD namespaces.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF mt_trees.

    append( iv_path = '/src/#foo#a/#foo#a1/'
            iv_name = 'a1.txt' ).

    append( iv_path = '/src/#foo#a/#foo#a2/'
            iv_name = 'a2.txt' ).

    mt_trees = zcl_abapgit_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 5 ).

    LOOP AT mt_trees ASSIGNING <ls_tree>.
      cl_abap_unit_assert=>assert_not_initial( <ls_tree>-data ).
    ENDLOOP.

  ENDMETHOD.

  METHOD root_empty.

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = zcl_abapgit_git_porcelain=>build_trees( mt_expanded ).

* so 2 total trees are expected: '/' and '/sub/'
    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

  METHOD pull_full_walk_no_filter.
    " pull without it_wanted_files/iv_url should use full walk and populate blob data

    DATA lt_objects    TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_obj        LIKE LINE OF lt_objects.
    DATA lt_nodes      TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node       LIKE LINE OF lt_nodes.
    DATA lt_files      TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file       LIKE LINE OF lt_files.
    DATA lv_blob_sha   TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_tree_sha   TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_commit_sha TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_blob_data  TYPE xstring.
    DATA ls_commit     TYPE zcl_abapgit_git_pack=>ty_commit.

    " Build blob
    lv_blob_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'hello abapgit' ).
    lv_blob_sha  = zcl_abapgit_hash=>sha1_blob( lv_blob_data ).

    ls_obj-sha1 = lv_blob_sha.
    ls_obj-type = zif_abapgit_git_definitions=>c_type-blob.
    ls_obj-data = lv_blob_data.
    APPEND ls_obj TO lt_objects.

    " Build tree pointing to blob
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'readme.txt'.
    ls_node-sha1  = lv_blob_sha.
    APPEND ls_node TO lt_nodes.

    build_tree_object(
      EXPORTING it_nodes   = lt_nodes
      IMPORTING ev_sha1    = lv_tree_sha
      CHANGING  ct_objects = lt_objects ).

    " Build commit pointing to tree
    ls_commit-tree      = lv_tree_sha.
    ls_commit-author    = 'Test User <test@example.com> 0 +0000'.
    ls_commit-committer = 'Test User <test@example.com> 0 +0000'.
    ls_commit-body      = 'test commit'.

    ls_obj-data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
    ls_obj-type = zif_abapgit_git_definitions=>c_type-commit.
    ls_obj-sha1 = zcl_abapgit_hash=>sha1_commit( ls_obj-data ).
    APPEND ls_obj TO lt_objects.
    lv_commit_sha = ls_obj-sha1.

    " Call pull without filter - should use full walk
    lt_files = zcl_abapgit_git_porcelain=>pull(
      iv_commit  = lv_commit_sha
      it_objects = lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1
      msg = 'Exactly one file expected from full walk' ).

    READ TABLE lt_files INTO ls_file INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-filename
      exp = 'readme.txt'
      msg = 'Filename must match tree node' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_file-data
      msg = 'Blob data must be populated by full walk' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-data
      exp = lv_blob_data
      msg = 'Blob data must match original content' ).

  ENDMETHOD.

  METHOD filter_expanded_keeps_match.
    " filter_expanded with a prefix entry keeps all files for that object

    DATA lt_expanded     TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA ls_exp          LIKE LINE OF lt_expanded.
    DATA lt_wanted_files TYPE string_table.

    ls_exp-name  = 'zcl_myclass.clas.abap'.
    ls_exp-path  = '/src/'.
    ls_exp-sha1  = '1111111111111111111111111111111111111111'.
    ls_exp-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    APPEND ls_exp TO lt_expanded.

    APPEND 'zcl_myclass.' TO lt_wanted_files.

    zcl_abapgit_git_porcelain=>filter_expanded(
      EXPORTING it_wanted_files = lt_wanted_files
      CHANGING  ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 1
      msg = 'Matching entry must be kept' ).

  ENDMETHOD.

  METHOD filter_expanded_no_match.
    " filter_expanded removes non-matching entries and is case-insensitive

    DATA lt_expanded     TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA ls_exp          LIKE LINE OF lt_expanded.
    DATA lt_wanted_files TYPE string_table.

    " Entry 1: matches (uppercase in name, lowercase prefix in wanted list)
    ls_exp-name  = 'ZCL_MYCLASS.clas.abap'.
    ls_exp-path  = '/src/'.
    ls_exp-sha1  = '1111111111111111111111111111111111111111'.
    ls_exp-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    APPEND ls_exp TO lt_expanded.

    " Entry 2: no match
    ls_exp-name  = 'zcl_other.clas.abap'.
    ls_exp-sha1  = '2222222222222222222222222222222222222222'.
    APPEND ls_exp TO lt_expanded.

    APPEND 'zcl_myclass.' TO lt_wanted_files.

    zcl_abapgit_git_porcelain=>filter_expanded(
      EXPORTING it_wanted_files = lt_wanted_files
      CHANGING  ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 1
      msg = 'Only matching entry must remain' ).

    READ TABLE lt_expanded INTO ls_exp INDEX 1.
    cl_abap_unit_assert=>assert_char_cp(
      act = to_lower( ls_exp-name )
      exp = 'zcl_myclass*'
      msg = 'Remaining entry must be the matching one' ).

  ENDMETHOD.

  METHOD filter_expanded_empty_wanted.
    " filter_expanded with empty it_wanted_files removes all non-dot-file entries.

    DATA lt_expanded     TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA ls_exp          LIKE LINE OF lt_expanded.
    DATA lt_wanted_files TYPE string_table.  " intentionally empty

    ls_exp-name  = 'zcl_myclass.clas.abap'.
    ls_exp-path  = '/src/'.
    ls_exp-sha1  = '1111111111111111111111111111111111111111'.
    ls_exp-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    APPEND ls_exp TO lt_expanded.

    ls_exp-name  = 'zcl_other.clas.xml'.
    ls_exp-sha1  = '2222222222222222222222222222222222222222'.
    APPEND ls_exp TO lt_expanded.

    zcl_abapgit_git_porcelain=>filter_expanded(
      EXPORTING it_wanted_files = lt_wanted_files
      CHANGING  ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 0
      msg = 'All entries must be removed when wanted list is empty' ).

  ENDMETHOD.

  METHOD filter_expanded_dot_files.
    " filter_expanded always keeps root-level dot-files regardless of wanted list

    DATA lt_expanded     TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA ls_exp          LIKE LINE OF lt_expanded.
    DATA lt_wanted_files TYPE string_table.  " intentionally empty

    " Root dot-file - must always be kept
    ls_exp-name  = '.abapgit.xml'.
    ls_exp-path  = zif_abapgit_definitions=>c_root_dir.
    ls_exp-sha1  = '1111111111111111111111111111111111111111'.
    ls_exp-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    APPEND ls_exp TO lt_expanded.

    " Normal src file - must be removed (no match)
    ls_exp-name  = 'zcl_myclass.clas.abap'.
    ls_exp-path  = '/src/'.
    ls_exp-sha1  = '2222222222222222222222222222222222222222'.
    APPEND ls_exp TO lt_expanded.

    zcl_abapgit_git_porcelain=>filter_expanded(
      EXPORTING it_wanted_files = lt_wanted_files
      CHANGING  ct_expanded     = lt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 1
      msg = 'Only root dot-file must remain' ).

    READ TABLE lt_expanded INTO ls_exp INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_exp-name
      exp = '.abapgit.xml'
      msg = 'Remaining entry must be the dot-file' ).

  ENDMETHOD.

ENDCLASS.
