*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UNIT_TEST
*&---------------------------------------------------------------------*

DEFINE _append_local.
  APPEND INITIAL LINE TO lt_local ASSIGNING <local>.
  <local>-item-obj_type = &1.
  <local>-item-obj_name = &2.
  <local>-item-devclass = '$Z$'.
  <local>-file-path     = '/'.
  <local>-file-filename = &3.
  <local>-file-sha1     = &4.
END-OF-DEFINITION.

DEFINE _append_remote.
  APPEND INITIAL LINE TO lt_remote ASSIGNING <remote>.
  <remote>-path     = '/'.
  <remote>-filename = &1.
  <remote>-sha1     = &2.
END-OF-DEFINITION.

DEFINE _append_state.
  APPEND INITIAL LINE TO lt_state ASSIGNING <state>.
  <state>-path     = '/'.
  <state>-filename = &1.
  <state>-sha1     = &2.
END-OF-DEFINITION.

DEFINE _append_result.
  APPEND INITIAL LINE TO lt_results ASSIGNING <result>.
  <result>-obj_type = &1.
  <result>-obj_name = &2.
  <result>-match    = &3.
  <result>-lstate   = &4.
  <result>-rstate   = &5.
  <result>-package  = &6.
  <result>-path     = &7.
  <result>-filename = &8.
END-OF-DEFINITION.

* todo, should the tests be in the same include as the classes
* they are testing?


CLASS ltcl_critical_tests DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_run_permission.
ENDCLASS.

CLASS ltcl_critical_tests IMPLEMENTATION.

  METHOD check_run_permission.
    DATA: lo_settings TYPE REF TO lcl_settings.
    lo_settings = lcl_app=>settings( )->read( ).

    "Objects will be created and deleted, do not run in customer system!
    "These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)

    IF lo_settings->get_run_critical_tests( ) = abap_false.
      cl_abap_unit_assert=>fail(
        msg   = 'Cancelled. You can enable these tests at the Settings page'
        level = if_aunit_constants=>tolerable ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION LONG FINAL.
* if this test class does not run, parameters in transaction SAUNIT_CLIENT_SETUP
* might need to be adjusted

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup.

    METHODS:
      run FOR TESTING
        RAISING zcx_abapgit_exception.

    CONSTANTS: c_package TYPE devclass VALUE '$ABAPGIT_UNIT_TEST'.

ENDCLASS.                    "ltcl_dangerous DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous IMPLEMENTATION.

  METHOD class_setup.
    ltcl_critical_tests=>check_run_permission( ).
  ENDMETHOD.                    "class_setup

  METHOD run.

    DATA: lo_repo    TYPE REF TO lcl_repo_online,
          lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_msg     TYPE string,
          lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_types   TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <lv_type>   LIKE LINE OF lt_types.


    lcl_sap_package=>create_local( c_package ).

    lt_types = lcl_objects=>supported_list( ).

    lo_repo = lcl_app=>repo_srv( )->new_online(
      iv_url         = 'https://github.com/larshp/abapGit-Unit-Test.git'
      iv_branch_name = 'refs/heads/master'
      iv_package     = c_package ).
    lo_repo->status( ).
    lo_repo->deserialize( ).

    lt_tadir = lcl_tadir=>read( c_package ).
    LOOP AT lt_types ASSIGNING <lv_type>.
      READ TABLE lt_tadir WITH KEY object = <lv_type> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        lv_msg = |Missing object type { <lv_type> }|.
        cl_abap_unit_assert=>fail(
            msg   = lv_msg
            level = if_aunit_constants=>tolerable
            quit  = if_aunit_constants=>no ).
      ENDIF.
    ENDLOOP.

    lt_results = lo_repo->status( ).
    LOOP AT lt_results ASSIGNING <ls_result> WHERE match = abap_false.
      lv_msg = |Does not match { <ls_result>-obj_type } { <ls_result>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    lcl_objects=>delete( lt_tadir ).
    lt_tadir = lcl_tadir=>read( c_package ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lv_msg = |Not deleted properly { <ls_tadir>-object } { <ls_tadir>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "run

ENDCLASS.                    "ltcl_dangerous IMPLEMENTATION

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
        RAISING zcx_abapgit_exception.

    DATA: mt_expanded TYPE lcl_git_porcelain=>ty_expanded_tt,
          mt_trees    TYPE lcl_git_porcelain=>ty_trees_tt.

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
    <ls_expanded>-chmod = zif_abapgit_definitions=>gc_chmod-file.

  ENDMETHOD.

  METHOD single_file.

    append( iv_path = '/'
            iv_name = 'foobar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD two_files_same_path.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD sub.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

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

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

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

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

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

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

* so 2 total trees are expected: '/' and '/sub/'
    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_url DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_url DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      repo_host FOR TESTING RAISING zcx_abapgit_exception,
      repo_name1 FOR TESTING RAISING zcx_abapgit_exception,
      repo_name2 FOR TESTING RAISING zcx_abapgit_exception,
      repo_error FOR TESTING.

ENDCLASS.                    "ltcl_url DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_url IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_url IMPLEMENTATION.

  METHOD repo_error.

    TRY.
        zcl_abapgit_url=>host( 'not a real url' ).          "#EC NOTEXT
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.                      "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "repo_error

  METHOD repo_host.

    DATA: lv_host TYPE string.

    lv_host = zcl_abapgit_url=>host( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'https://github.com'
        act = lv_host ).

  ENDMETHOD.                    "repo_url

  METHOD repo_name1.

    DATA: lv_name TYPE string.

    lv_name = zcl_abapgit_url=>name( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'Foobar'
        act = lv_name ).

  ENDMETHOD.

  METHOD repo_name2.

    DATA: lv_name TYPE string.

    lv_name = zcl_abapgit_url=>name( 'https://git.hanatrial.ondemand.com/p12345trial/yay' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'yay'
        act = lv_name ).

  ENDMETHOD.

ENDCLASS.                    "ltcl_url IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      is_supported FOR TESTING,
      not_exist FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.                    "ltcl_object_types DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types IMPLEMENTATION.

  METHOD is_supported.

    DATA: ls_item      TYPE zif_abapgit_definitions=>ty_item,
          lv_supported TYPE abap_bool,
          lt_types     TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = lcl_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.

      CLEAR ls_item.
      ls_item-obj_type = <lv_type>.
      lv_supported = lcl_objects=>is_supported( ls_item ).

      cl_abap_unit_assert=>assert_equals(
          act  = lv_supported
          exp  = abap_true
          msg  = ls_item-obj_type
          quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.                    "is_supported

  METHOD not_exist.

    DATA: ls_item   TYPE zif_abapgit_definitions=>ty_item,
          lv_exists TYPE abap_bool,
          lt_types  TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = lcl_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.
      CLEAR ls_item.
      ls_item-obj_name = 'ZABAPGIT_FOOBAR'.
      ls_item-obj_type = <lv_type>.
      lv_exists = lcl_objects=>exists( ls_item ).

      cl_abap_unit_assert=>assert_equals(
          act  = lv_exists
          exp  = abap_false
          msg  = ls_item-obj_type
          quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.                    "not_exist

ENDCLASS.                    "ltcl_object_types IMPLEMENTATION

CLASS ltcl_git_pack_decode_commit DEFINITION FOR TESTING
    RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    METHODS:
      decode1 FOR TESTING
        RAISING zcx_abapgit_exception,
      decode2 FOR TESTING
        RAISING zcx_abapgit_exception,
      decode3 FOR TESTING
        RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: ms_raw TYPE lcl_git_pack=>ty_commit,
          mv_str TYPE string.

    METHODS:
      setup,
      decode
        RAISING zcx_abapgit_exception,
      add
        IMPORTING iv_string TYPE string.

ENDCLASS.

CLASS ltcl_git_pack_decode_commit IMPLEMENTATION.

  METHOD setup.
    CLEAR ms_raw.
    CLEAR mv_str.
  ENDMETHOD.

  METHOD add.

    CONCATENATE mv_str iv_string zif_abapgit_definitions=>gc_newline INTO mv_str.

  ENDMETHOD.

  METHOD decode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( mv_str ).

    ms_raw = lcl_git_pack=>decode_commit( lv_xstr ).

  ENDMETHOD.

  METHOD decode1.

    add( 'tree tree' ).
    add( 'parent parent1' ).
    add( 'parent parent2' ).
    add( 'author author' ).
    add( 'committer committer' ).
    add( '' ).
    add( 'comment' ).

    decode( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-tree
      exp = 'tree' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-author
      exp = 'author' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-committer
      exp = 'committer' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = 'parent1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent2
      exp = 'parent2' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ms_raw-body
      exp = 'comment+' ).

  ENDMETHOD.

  METHOD decode2.

    add( 'tree tree' ).
    add( 'author author' ).
    add( 'committer committer' ).
    add( '' ).
    add( 'comment' ).

    decode( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-tree
      exp = 'tree' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-author
      exp = 'author' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-committer
      exp = 'committer' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = '' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ms_raw-body
      exp = 'comment+' ).

  ENDMETHOD.

  METHOD decode3.

    add( 'tree tree' ).
    add( 'parent parent1' ).
    add( 'author author' ).
    add( 'committer committer' ).
    add( '' ).
    add( 'comment' ).

    decode( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-tree
      exp = 'tree' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-author
      exp = 'author' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-committer
      exp = 'committer' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = 'parent1' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ms_raw-body
      exp = 'comment+' ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_git_pack DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    CONSTANTS: c_sha TYPE zif_abapgit_definitions=>ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    METHODS:
      tree FOR TESTING
        RAISING zcx_abapgit_exception,
      commit FOR TESTING
        RAISING zcx_abapgit_exception,
      commit_newline FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_short FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_long FOR TESTING
        RAISING zcx_abapgit_exception,
      pack_multiple FOR TESTING
        RAISING zcx_abapgit_exception,
      sort_tree1 FOR TESTING,
      sort_tree2 FOR TESTING,
      type_and_length01 FOR TESTING
        RAISING zcx_abapgit_exception,
      type_and_length02 FOR TESTING
        RAISING zcx_abapgit_exception.

    METHODS:
      object_blob
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_object
        RAISING   zcx_abapgit_exception.

ENDCLASS.                    "test DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_git_pack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_git_pack IMPLEMENTATION.

  METHOD type_and_length01.

    DATA: lv_result TYPE xstring.

    lv_result = lcl_git_pack=>type_and_length(
      iv_type   = zif_abapgit_definitions=>gc_type-commit
      iv_length = 100 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '9406' ).

  ENDMETHOD.

  METHOD type_and_length02.

    DATA: lv_result TYPE xstring.

    lv_result = lcl_git_pack=>type_and_length(
      iv_type   = zif_abapgit_definitions=>gc_type-blob
      iv_length = 90000 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'B0F92B' ).

  ENDMETHOD.

  METHOD sort_tree1.

    DATA: lt_tree TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF lt_tree.


    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>gc_chmod-file.
    <ls_tree>-name  = 'b.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>gc_chmod-file.
    <ls_tree>-name  = 'a.txt'.
    <ls_tree>-sha1  = '0123'.

    lt_tree = lcl_git_pack=>sort_tree( lt_tree ).

    READ TABLE lt_tree INDEX 1 ASSIGNING <ls_tree>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_tree>-name
      exp = 'a.txt' ).

  ENDMETHOD.

  METHOD sort_tree2.

    DATA: lt_tree TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF lt_tree.


    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>gc_chmod-file.
    <ls_tree>-name  = 'foo.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = zif_abapgit_definitions=>gc_chmod-dir.
    <ls_tree>-name  = 'foo'.
    <ls_tree>-sha1  = '0123'.

    lt_tree = lcl_git_pack=>sort_tree( lt_tree ).

    READ TABLE lt_tree INDEX 1 ASSIGNING <ls_tree>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_tree>-name
      exp = 'foo.txt' ).

  ENDMETHOD.

  METHOD pack_multiple.

    CONSTANTS: lc_data TYPE x LENGTH 15 VALUE '123456789ABCDEF545794254754554',
               lc_sha  TYPE zif_abapgit_definitions=>ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_nodes   TYPE lcl_git_pack=>ty_nodes_tt,
          ls_node    LIKE LINE OF lt_nodes,
          ls_commit  TYPE lcl_git_pack=>ty_commit,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring.


* blob
    lv_data = lc_data.
    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob iv_data = lv_data ).
    ls_object-type = zif_abapgit_definitions=>gc_type-blob.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* commit
    CLEAR ls_commit.
    ls_commit-tree      = lc_sha.
    ls_commit-parent    = lc_sha.
    ls_commit-author    = 'John Foobar'.
    ls_commit-committer = 'John Foobar'.
    ls_commit-body      = 'body'.
    lv_data = lcl_git_pack=>encode_commit( ls_commit ).
    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-commit iv_data = lv_data ).
    ls_object-type = zif_abapgit_definitions=>gc_type-commit.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* tree
    CLEAR ls_node.
    ls_node-chmod     = '12456'.
    ls_node-name      = 'foobar.abap'.
    ls_node-sha1      = lc_sha.
    APPEND ls_node TO lt_nodes.
    lv_data = lcl_git_pack=>encode_tree( lt_nodes ).
    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-tree iv_data = lv_data ).
    ls_object-type = zif_abapgit_definitions=>gc_type-tree.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.


    CLEAR lv_data.
    lv_data = lcl_git_pack=>encode( lt_objects ).
    lt_result = lcl_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack_multiple

  METHOD object_blob.

    rs_object-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob
                                             iv_data = iv_data ).
    rs_object-type = zif_abapgit_definitions=>gc_type-blob.
    rs_object-data = iv_data.

  ENDMETHOD.                    "object_blob

  METHOD pack_short.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring.


    lv_data = lc_data.

    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = lcl_git_pack=>encode( lt_objects ).
    lt_result = lcl_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack

  METHOD pack_long.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lv_xstring TYPE xstring,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_data    TYPE xstring.


    lv_xstring = lc_data.
    DO 20 TIMES.
      CONCATENATE lv_xstring lv_data INTO lv_data IN BYTE MODE.
    ENDDO.

    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = lcl_git_pack=>encode( lt_objects ).
    lt_result = lcl_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack_long

  METHOD tree.

    DATA: lt_nodes  TYPE lcl_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE lcl_git_pack=>ty_nodes_tt.

    CLEAR ls_node.
    ls_node-chmod = zif_abapgit_definitions=>gc_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = c_sha.
    APPEND ls_node TO lt_nodes.

    lv_data = lcl_git_pack=>encode_tree( lt_nodes ).
    lt_result = lcl_git_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.                    "tree

  METHOD commit.

    DATA: ls_commit TYPE lcl_git_pack=>ty_commit,
          ls_result TYPE lcl_git_pack=>ty_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = c_sha.
    ls_commit-parent    = c_sha.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.

    lv_data = lcl_git_pack=>encode_commit( ls_commit ).
    ls_result = lcl_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.                    "commit

  METHOD commit_newline.

    DATA: ls_commit TYPE lcl_git_pack=>ty_commit,
          ls_result TYPE lcl_git_pack=>ty_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = c_sha.
    ls_commit-parent    = c_sha.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'
                        && zif_abapgit_definitions=>gc_newline
                        && zif_abapgit_definitions=>gc_newline.

    lv_data = lcl_git_pack=>encode_commit( ls_commit ).
    ls_result = lcl_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.

ENDCLASS.                    "lcl_abap_unit IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      check
        IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
        RAISING   zcx_abapgit_exception,
      serialize_tabl FOR TESTING RAISING zcx_abapgit_exception,
      serialize_enqu FOR TESTING RAISING zcx_abapgit_exception,
      serialize_shlp FOR TESTING RAISING zcx_abapgit_exception,
      serialize_view FOR TESTING RAISING zcx_abapgit_exception,
      serialize_auth FOR TESTING RAISING zcx_abapgit_exception,
      serialize_clas FOR TESTING RAISING zcx_abapgit_exception,
      serialize_doma FOR TESTING RAISING zcx_abapgit_exception,
      serialize_dtel FOR TESTING RAISING zcx_abapgit_exception,
      serialize_fugr FOR TESTING RAISING zcx_abapgit_exception,
      serialize_msag FOR TESTING RAISING zcx_abapgit_exception,
      serialize_prog FOR TESTING RAISING zcx_abapgit_exception,
      serialize_tran FOR TESTING RAISING zcx_abapgit_exception,
      serialize_ttyp FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.                    "ltcl_serialize DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_table

  METHOD serialize_auth.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'AUTH'.
    ls_item-obj_name = 'AREA'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_auth

  METHOD serialize_clas.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_GUI_FRONTEND_SERVICES'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_clas

  METHOD serialize_doma.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_doma

  METHOD serialize_dtel.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_dtel

  METHOD serialize_fugr.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SRFC'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_fugr

  METHOD serialize_msag.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'MSAG'.
    ls_item-obj_name = '00'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_msag

  METHOD serialize_prog.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPLWBABAP'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_prog

  METHOD serialize_tran.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TRAN'.
    ls_item-obj_name = 'SE38'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_tran

  METHOD serialize_ttyp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TTYP'.
    ls_item-obj_name = 'ABAPPROG'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_ttyp

  METHOD check.

    DATA: lt_files TYPE zif_abapgit_definitions=>ty_files_tt.

    lt_files = lcl_objects=>serialize( is_item     = is_item
                                       iv_language = zif_abapgit_definitions=>gc_english ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "check

ENDCLASS.                    "ltcl_serialize IMPLEMENTATION

CLASS ltcl_login_manager DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_username TYPE string VALUE 'Aladdin' ##NO_TEXT,
               c_password TYPE string VALUE 'OpenSesame' ##NO_TEXT.

    METHODS:
      setup,
      teardown,
      encoding FOR TESTING
        RAISING zcx_abapgit_exception,
      same_server FOR TESTING
        RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_login_manager IMPLEMENTATION.

  METHOD setup.
    lcl_login_manager=>clear( ).
  ENDMETHOD.

  METHOD teardown.
    lcl_login_manager=>clear( ).
  ENDMETHOD.

  METHOD encoding.

    DATA: lv_auth TYPE string.

    lv_auth = lcl_login_manager=>set(
      iv_uri      = 'https://github.com/larshp/abapGit.git'
      iv_username = c_username
      iv_password = c_password ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_auth
      exp = 'Basic QWxhZGRpbjpPcGVuU2VzYW1l' ).

  ENDMETHOD.

  METHOD same_server.

    CONSTANTS: lc_github1 TYPE string VALUE 'https://github.com/larshp/abapGit.git',
               lc_github2 TYPE string VALUE 'https://github.com/larshp/Foobar.git'.

    DATA: lv_auth1 TYPE string,
          lv_auth2 TYPE string.


    lcl_login_manager=>set(
      iv_uri      = lc_github1
      iv_username = c_username
      iv_password = c_password ).

    lv_auth1 = lcl_login_manager=>load( lc_github1 ).
    lv_auth2 = lcl_login_manager=>load( lc_github2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_auth1
      exp = lv_auth2 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_html_action_utils DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL
  INHERITING FROM cl_aunit_assert.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    METHODS add_field FOR TESTING.
    METHODS get_field FOR TESTING.
    METHODS parse_fields_simple_case FOR TESTING.
    METHODS parse_fields_advanced_case FOR TESTING.
    METHODS parse_fields_unescape FOR TESTING.
    METHODS parse_fields_german_umlauts FOR TESTING.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF co_german_umlaut_as_hex,
                 lower_case_ae TYPE xstring VALUE 'C3A4',
                 lower_case_oe TYPE xstring VALUE 'C3B6',
                 lower_case_ue TYPE xstring VALUE 'C3BC',
               END OF co_german_umlaut_as_hex.

    CLASS-DATA: BEGIN OF ms_german_umlaut_as_char,
                  lower_case_ae TYPE string,
                  lower_case_oe TYPE string,
                  lower_case_ue TYPE string,
                END OF ms_german_umlaut_as_char.

    DATA m_given_parse_string TYPE string.
    DATA mt_parsed_fields TYPE tihttpnvp.

    METHODS _given_string_is
      IMPORTING
        i_string TYPE string.
    METHODS _when_fields_are_parsed.
    METHODS _then_fields_should_be
      IMPORTING
        index TYPE i
        name  TYPE string
        value TYPE string.

    CLASS-METHODS _hex_to_char
      IMPORTING
        i_x        TYPE xstring
      RETURNING
        VALUE(r_s) TYPE string.

ENDCLASS. "ltcl_html_action_utils

CLASS ltcl_html_action_utils IMPLEMENTATION.

  METHOD class_constructor.

    ms_german_umlaut_as_char-lower_case_ae = _hex_to_char( co_german_umlaut_as_hex-lower_case_ae ).
    ms_german_umlaut_as_char-lower_case_oe = _hex_to_char( co_german_umlaut_as_hex-lower_case_oe ).
    ms_german_umlaut_as_char-lower_case_ue = _hex_to_char( co_german_umlaut_as_hex-lower_case_ue ).

  ENDMETHOD.

  METHOD add_field.

    DATA: lt_fields TYPE tihttpnvp,
          lt_answer TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    ls_field-name  = 'NAME'.
    ls_field-value = 'TEST'.
    APPEND ls_field TO lt_answer.

    ls_field-name  = 'VALUE'.
    ls_field-value = 'TEST'.
    APPEND ls_field TO lt_answer.

    lcl_html_action_utils=>add_field( EXPORTING name = 'NAME' iv = 'TEST'
                                      CHANGING  ct = lt_fields ).
    lcl_html_action_utils=>add_field( EXPORTING name = 'VALUE' iv = ls_field
                                      CHANGING  ct = lt_fields ).

    assert_equals( act = lt_fields exp = lt_answer ).

  ENDMETHOD.  "add_field

  METHOD get_field.

    DATA: lt_fields TYPE tihttpnvp,
          ls_answer LIKE LINE OF lt_fields,
          ls_field  LIKE LINE OF lt_fields.

    ls_answer-name  = 'NAME'.
    ls_answer-value = 'TEST'.
    APPEND ls_answer TO lt_fields.

    lcl_html_action_utils=>get_field( EXPORTING name = 'NAME' it = lt_fields
                                      CHANGING  cv   = ls_field-value ).
    lcl_html_action_utils=>get_field( EXPORTING name = 'NAME' it = lt_fields
                                      CHANGING  cv   = ls_field ).

    ls_answer-name  = 'TEST'.
    ls_answer-value = 'TEST'.
    assert_equals( act = ls_field exp = ls_answer ). " Both field are filled!

  ENDMETHOD.  "get_field

  METHOD parse_fields_simple_case.

    _given_string_is( `committer_name=Gustav Gans` ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( index = 1 name = `COMMITTER_NAME` value = `Gustav Gans` ).

  ENDMETHOD.

  METHOD parse_fields_advanced_case.

    _given_string_is( `committer_name=Albert Schweitzer&`
                   && `committer_email=albert.schweitzer@googlemail.com&`
                   && `comment=dummy comment&`
                   && `body=Message body<<new>><<new>>with line break<<new>>&`
                   && `author_name=Karl Klammer&`
                   && `author_email=karl@klammer.com` ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( index = 1
                            name  = `COMMITTER_NAME`
                            value = `Albert Schweitzer` ).

    _then_fields_should_be( index = 2
                            name  = `COMMITTER_EMAIL`
                            value = `albert.schweitzer@googlemail.com` ).

    _then_fields_should_be( index = 3
                            name  = `COMMENT`
                            value = `dummy comment` ).

    _then_fields_should_be( index = 4
                            name  = `BODY`
                            value = `Message body<<new>><<new>>with line break<<new>>` ).

    _then_fields_should_be( index = 5
                            name  = `AUTHOR_NAME`
                            value = `Karl Klammer` ).

    _then_fields_should_be( index = 6
                            name  = `AUTHOR_EMAIL`
                            value = `karl@klammer.com` ).

  ENDMETHOD.

  METHOD parse_fields_unescape.
* file status = '?', used in staging page

    _given_string_is( '/SRC/ZFOOBAR.PROG.ABAP=%3F' ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( index = 1
                            name  = '/SRC/ZFOOBAR.PROG.ABAP'
                            value = '?' ).

  ENDMETHOD.

  METHOD parse_fields_german_umlauts.

    DATA: ae       TYPE string,
          oe       TYPE string,
          ue       TYPE string,
          ae_oe_ue TYPE string.

    ae = ms_german_umlaut_as_char-lower_case_ae.
    oe = ms_german_umlaut_as_char-lower_case_oe.
    ue = ms_german_umlaut_as_char-lower_case_ue.

    ae_oe_ue = ae && oe && ue.

    _given_string_is( |committer_name=Christian G{ ue }nter&|
                   && |committer_email=guenne@googlemail.com&|
                   && |comment={ ae_oe_ue }&|
                   && |body=Message body<<new>><<new>>with line break<<new>>and umlauts. { ae_oe_ue }&|
                   && |author_name=Gerd Schr{ oe }der&|
                   && |author_email=gerd@schroeder.com| ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( index = 1
                            name  = `COMMITTER_NAME`
                            value = |Christian G{ ue }nter| ).

    _then_fields_should_be( index = 2
                            name  = `COMMITTER_EMAIL`
                            value = `guenne@googlemail.com` ).

    _then_fields_should_be( index = 3
                            name  = `COMMENT`
                            value = ae_oe_ue ).

    _then_fields_should_be( index = 4
                            name  = `BODY`
                            value = |Message body<<new>><<new>>with line break<<new>>and umlauts. { ae_oe_ue }| ).

    _then_fields_should_be( index = 5
                            name  = `AUTHOR_NAME`
                            value = |Gerd Schr{ oe }der| ).

    _then_fields_should_be( index = 6
                            name  = `AUTHOR_EMAIL`
                            value = `gerd@schroeder.com` ).

  ENDMETHOD.

  METHOD _given_string_is.

    m_given_parse_string = i_string.

  ENDMETHOD.

  METHOD _when_fields_are_parsed.

    mt_parsed_fields = lcl_html_action_utils=>parse_fields_upper_case_name( m_given_parse_string ).

  ENDMETHOD.

  METHOD _then_fields_should_be.

    FIELD-SYMBOLS: <parsed_field> LIKE LINE OF mt_parsed_fields.

    READ TABLE mt_parsed_fields ASSIGNING <parsed_field>
                                INDEX index.

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       msg = |No parsed field found at index { index }| ).

    cl_abap_unit_assert=>assert_equals( act = <parsed_field>-name
                                        exp = name
                                        msg = |Name at index { index } should be { name }| ).

    cl_abap_unit_assert=>assert_equals( act = <parsed_field>-value
                                        exp = value
                                        msg = |Value at index { index } should be { value }| ).

  ENDMETHOD.

  METHOD _hex_to_char.

    DATA lr_conv TYPE REF TO cl_abap_conv_in_ce.

    lr_conv = cl_abap_conv_in_ce=>create( ).
    lr_conv->convert( EXPORTING input = i_x IMPORTING data = r_s ).

  ENDMETHOD.

ENDCLASS. "ltcl_html_action_utils

CLASS ltcl_file_status DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL
  INHERITING FROM cl_aunit_assert.

  PUBLIC SECTION.
    METHODS calculate_status FOR TESTING
      RAISING zcx_abapgit_exception.

ENDCLASS.   "ltcl_file_status

CLASS ltcl_file_status IMPLEMENTATION.

  METHOD calculate_status.

    DATA: lt_local       TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_remote      TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_state       TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lt_results     TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_results_exp TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_dot         TYPE REF TO zcl_abapgit_dot_abapgit.

    FIELD-SYMBOLS: <local>  LIKE LINE OF lt_local,
                   <remote> LIKE LINE OF lt_remote,
                   <result> LIKE LINE OF lt_results,
                   <state>  LIKE LINE OF lt_state.

    "STATE         FILE                              SHA1
    _append_state '$$zclass1.clas.xml'              'C1_F1'.
    " class1 testclasses is new locally, abap is new remotely
    " class2 is completely new remotely
    _append_state '$$zdoma1.doma.xml'               'D1'.
    _append_state '$$zdoma2.doma.xml'               'D2'.
    _append_state '$$zdoma3.doma.xml'               'D3'.
    " doma4 is new locally
    " doma5 is new remotely
    _append_state '$$zdoma6.doma.xml'               'D6'.
    " doma7 is not in state - emulate brocken cache
    " doma8 is not in state - emulate brocken cache
    _append_state '$$zdoma9.doma.xml'               'D9'.
    _append_state 'num01.doma.xml'                  'NUM01'. " another from different package
    _append_state 'xfeld.doma.xml'                  'XFELD'. " from different package

    "LOCAL         TYPE   NAME        FILE                              SHA1
    _append_local 'CLAS' '$$ZCLASS1' '$$zclass1.clas.testclasses.abap' 'C1_F3'.
    _append_local 'CLAS' '$$ZCLASS1' '$$zclass1.clas.xml'              'C1_F1'.
    _append_local 'DOMA' '$$ZDOMA1'  '$$zdoma1.doma.xml'               'D1'.
    _append_local 'DOMA' '$$ZDOMA2'  '$$zdoma2.doma.xml'               'D2_CHANGED_L'.
    _append_local 'DOMA' '$$ZDOMA3'  '$$zdoma3.doma.xml'               'D3'.
    _append_local 'DOMA' '$$ZDOMA4'  '$$zdoma4.doma.xml'               'D4'.
    _append_local 'DOMA' '$$ZDOMA6'  '$$zdoma6.doma.xml'               'D6_CHANGED_L'.
    _append_local 'DOMA' '$$ZDOMA7'  '$$zdoma7.doma.xml'               'D7'.
    _append_local 'DOMA' '$$ZDOMA8'  '$$zdoma8.doma.xml'               'D8'.
    " dome9 was deleted from local system. Can be found by existing state

    "REMOTE         FILE                  SHA1
    _append_remote 'textfile.txt'        'T1'.
    _append_remote '$$zclass1.clas.abap' 'C1_F2'. " Must be before xml for tougher test
    _append_remote '$$zclass1.clas.xml'  'C1_F1'.
    _append_remote '$$zclass2.clas.abap' 'C1_F2'. " Must be before xml for tougher test
    _append_remote '$$zclass2.clas.xml'  'C1_F1'.
    _append_remote '$$zdoma1.doma.xml'   'D1'.
    _append_remote '$$zdoma2.doma.xml'   'D2'.
    _append_remote '$$zdoma3.doma.xml'   'D3_CHANGED_R'.
    _append_remote '$$zdoma5.doma.xml'   'D5'.
    _append_remote '$$zdoma6.doma.xml'   'D6_CHANGED_R'.
    _append_remote '$$zdoma7.doma.xml'   'D7'.
    _append_remote '$$zdoma8.doma.xml'   'D8_CHANGED_R'.  " This one is changed
    _append_remote '$$zdoma9.doma.xml'   'D9'.            " This one is deleted locally
    _append_remote 'xfeld.doma.xml'      'XFELD'.         " Object from different package
    _append_remote 'num01.doma.xml'      'NUM01_CHANGED'. " Changed object from different package

    "EXP RESULT    TYPE   NAME        MATCH LST   RST  PKG    PATH FILE
    _append_result ''     ''          ' '   ' '   'A'  ''     '/'  'textfile.txt'.
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'  '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' ' '   'A'   ' '  '$Z$'  '/'  '$$zclass1.clas.testclasses.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'  '$$zclass1.clas.xml'.
    _append_result 'CLAS' '$$ZCLASS2' ' '   ' '   'A'  ''     '/'  '$$zclass2.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS2' ' '   ' '   'A'  ''     '/'  '$$zclass2.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'  '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'  '$$zdoma2.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA3'  ' '   ' '   'M'  '$Z$'  '/'  '$$zdoma3.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA4'  ' '   'A'   ' '  '$Z$'  '/'  '$$zdoma4.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA5'  ' '   ' '   'A'  ''     '/'  '$$zdoma5.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA6'  ' '   'M'   'M'  '$Z$'  '/'  '$$zdoma6.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA7'  'X'   ' '   ' '  '$Z$'  '/'  '$$zdoma7.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA8'  ' '   'M'   'M'  '$Z$'  '/'  '$$zdoma8.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA9'  ' '   'D'   ' '  ''     '/'  '$$zdoma9.doma.xml'.
    _append_result 'DOMA' 'NUM01'     ' '   ' '   'M'  'SUTI' '/'  'num01.doma.xml'.
    _append_result 'DOMA' 'XFELD'     'X'   ' '   ' '  'SUTI' '/'  'xfeld.doma.xml'.
    lt_results_exp = lt_results.

    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
*    lo_dot->set_starting_folder( 'SRC' ).
*    lo_dot->set_folder_logic( lcl_dot_abapgit=>c_folder_logic-prefix ).

    lt_results = lcl_file_status=>calculate_status(
      iv_devclass        = '$Z$'
      io_dot             = lo_dot
      it_local           = lt_local
      it_remote          = lt_remote
      it_cur_state       = lt_state ).

    assert_equals( act = lt_results exp = lt_results_exp ).

  ENDMETHOD.  "calculate_status

ENDCLASS.   "ltcl_file_status

CLASS ltcl_file_status2 DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL
  INHERITING FROM cl_aunit_assert.

  PUBLIC SECTION.
    METHODS check FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.   "ltcl_sap_package

CLASS ltcl_file_status2 IMPLEMENTATION.

  METHOD check.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_log     TYPE REF TO zcl_abapgit_log.

    FIELD-SYMBOLS: <result> LIKE LINE OF lt_results.

*** 0 Positive

    CLEAR lt_results.
    CREATE OBJECT lo_log.
    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH FILE
    _append_result 'CLAS' 'ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'  'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'  'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'  'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'  'zdoma2.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( ) exp = 0 ).

*** 1 Negative, different path for same object

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' 'ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/sub' 'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    'zdoma2.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    " This one is not pure - incorrect path also triggers path vs package check
    assert_equals( act = lo_log->count( ) exp = 2 ).
    assert_equals( act = lo_log->has_rc( '1' ) exp = abap_true ).

*** 2 Negative, incorrect path vs package

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/sub' '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    '$$zdoma2.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( ) exp = 1 ).
    assert_equals( act = lo_log->has_rc( '2' ) exp = abap_true ).

*** 3 Negative, similar filenames

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    '$$zdoma1.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( ) exp = 1 ).
    assert_equals( act = lo_log->has_rc( '3' ) exp = abap_true ).

*** 4 Negative, empty filenames

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    ''.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( )
                   exp = 1 ).

    assert_equals( act = lo_log->has_rc( '4' )
                   exp = abap_true ).

  ENDMETHOD.  " check.

ENDCLASS. "ltcl_sap_package

CLASS ltcl_persistence_settings DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      clear_settings_database,
      "Proxy
      modify_settings_proxy_url     FOR TESTING RAISING cx_static_check,
      modify_settings_proxy_port    FOR TESTING RAISING cx_static_check,
      read_proxy_settings           FOR TESTING RAISING cx_static_check,
      read_not_found_url            FOR TESTING RAISING cx_static_check,
      read_not_found_port           FOR TESTING RAISING cx_static_check,
      "Run critical tests
      modify_run_critical_tests     FOR TESTING RAISING cx_static_check,
      read_run_critical_tests       FOR TESTING RAISING cx_static_check,
      read_not_found_critical_tests FOR TESTING RAISING cx_static_check.
    DATA:
      mo_persistence_settings TYPE REF TO lcl_persist_settings,
      mo_settings             TYPE REF TO lcl_settings.
ENDCLASS.

CLASS ltcl_persistence_settings IMPLEMENTATION.

  METHOD class_setup.
    ltcl_critical_tests=>check_run_permission( ).
  ENDMETHOD.

  METHOD setup.
    CREATE OBJECT mo_persistence_settings.
    CREATE OBJECT mo_settings.
    clear_settings_database( ).
  ENDMETHOD.

  METHOD modify_settings_proxy_url.
    DATA lv_proxy_url TYPE string.

    mo_settings->set_proxy_url( 'http://proxy' ).

    mo_persistence_settings->modify( mo_settings ).

    lv_proxy_url = lcl_app=>db( )->read(
      iv_type  = 'SETTINGS'
      iv_value = 'PROXY_URL' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_proxy_url
      exp = 'http://proxy' ).
  ENDMETHOD.

  METHOD modify_settings_proxy_port.
    DATA lv_proxy_port TYPE string.
    mo_settings->set_proxy_port( '8080' ).

    mo_persistence_settings->modify( mo_settings ).

    lv_proxy_port = lcl_app=>db( )->read(
      iv_type  = 'SETTINGS'
      iv_value = 'PROXY_PORT' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_proxy_port
      exp = '8080' ).
  ENDMETHOD.

  METHOD read_proxy_settings.
    lcl_app=>db( )->modify(
      iv_type       = 'SETTINGS'
      iv_value      = 'PROXY_URL'
      iv_data       = 'A_URL' ).

    lcl_app=>db( )->modify(
      iv_type       = 'SETTINGS'
      iv_value      = 'PROXY_PORT'
      iv_data       = '1000' ).

    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_url( )
      exp = 'A_URL' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_port( )
      exp = '1000' ).
  ENDMETHOD.

  METHOD read_not_found_port.
    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_port( )
      exp = '' ).
  ENDMETHOD.

  METHOD read_not_found_url.

    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_url( )
      exp = '' ).
  ENDMETHOD.

  METHOD modify_run_critical_tests.
    DATA lv_run_critical_tests TYPE abap_bool.
    mo_settings->set_run_critical_tests( abap_true ).

    mo_persistence_settings->modify( mo_settings ).

    lv_run_critical_tests = lcl_app=>db( )->read(
      iv_type  = 'SETTINGS'
      iv_value = 'CRIT_TESTS' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_run_critical_tests
      exp = abap_true ).
  ENDMETHOD.

  METHOD read_run_critical_tests.
    lcl_app=>db( )->modify(
      iv_type       = 'SETTINGS'
      iv_value      = 'CRIT_TESTS'
      iv_data       = 'X' ).
    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_run_critical_tests( )
      exp = abap_true ).
  ENDMETHOD.
  METHOD read_not_found_critical_tests.
    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_run_critical_tests( )
      exp = abap_false ).
  ENDMETHOD.


  METHOD clear_settings_database.

    TRY.
        lcl_app=>db( )->delete(
          iv_type       = 'SETTINGS'
          iv_value      = 'PROXY_URL' ).
      CATCH cx_static_check ##NO_HANDLER.
        "If entry didn't exist, that's okay
    ENDTRY.
    TRY.
        lcl_app=>db( )->delete(
          iv_type       = 'SETTINGS'
          iv_value      = 'PROXY_PORT' ).
      CATCH cx_static_check ##NO_HANDLER.
        "If entry didn't exist, that's okay
    ENDTRY.
    TRY.
        lcl_app=>db( )->delete(
          iv_type       = 'SETTINGS'
          iv_value      = 'CRIT_TESTS' ).
      CATCH cx_static_check ##NO_HANDLER.
        "If entry didn't exist, that's okay
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_oo_serialize DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      empty_include FOR TESTING RAISING cx_static_check,
      one_line_include FOR TESTING RAISING cx_static_check,
      one_line_include_2 FOR TESTING RAISING cx_static_check,
      one_line_include_3 FOR TESTING RAISING cx_static_check,
      two_line_include FOR TESTING RAISING cx_static_check,
      two_line_include_2 FOR TESTING RAISING cx_static_check,
      two_line_include_3 FOR TESTING RAISING cx_static_check,
      more_than_two_lines FOR TESTING RAISING cx_static_check,

      _given_source_is
        IMPORTING
          i_source TYPE LINE OF zif_abapgit_definitions=>ty_string_tt,
      _given_empty_test_include,
      _when_skip_is_calculated,
      _then_should_be_skipped,
      _then_should_not_be_skipped.

    DATA: mo_oo_serializer  TYPE REF TO lcl_oo_serializer,
          mt_source         TYPE zif_abapgit_definitions=>ty_string_tt,
          mv_skip_testclass TYPE abap_bool.

ENDCLASS.


CLASS ltcl_oo_serialize IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_oo_serializer.

  ENDMETHOD.

  METHOD empty_include.

    _given_empty_test_include( ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD one_line_include.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD one_line_include_2.

    _given_source_is( `*` ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD one_line_include_3.

    _given_source_is( `write: 'This is ABAP'.` ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD two_line_include.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
    _given_source_is( ``                                                         ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD two_line_include_2.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
    _given_source_is( `write: 'This is ABAP'.`                                   ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD two_line_include_3.

    _given_source_is( ` `                                                        ).
    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD more_than_two_lines.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
    _given_source_is( `CLASS ltcl_test DEFINITION FINAL FOR TESTING`             ).
    _given_source_is( `  DURATION SHORT`                                         ).
    _given_source_is( `  RISK LEVEL HARMLESS.`                                   ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `  PRIVATE SECTION.`                                       ).
    _given_source_is( `    METHODS:`                                             ).
    _given_source_is( `      first_test FOR TESTING RAISING cx_static_check.`    ).
    _given_source_is( `ENDCLASS.`                                                ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `CLASS ltcl_test IMPLEMENTATION.`                          ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `  METHOD first_test.`                                     ).
    _given_source_is( `    cl_abap_unit_assert=>fail( 'This is a real test' ).`  ).
    _given_source_is( `  ENDMETHOD.`                                             ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `ENDCLASS.`                                                ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD _given_source_is.

    INSERT i_source INTO TABLE mt_source.

  ENDMETHOD.

  METHOD _given_empty_test_include.

  ENDMETHOD.

  METHOD _when_skip_is_calculated.

    mv_skip_testclass = mo_oo_serializer->calculate_skip_testclass( mt_source ).

  ENDMETHOD.

  METHOD _then_should_be_skipped.

    cl_abap_unit_assert=>assert_equals(
      act = mv_skip_testclass
      exp = abap_true
      msg = |Testclass should be skipped| ).

  ENDMETHOD.


  METHOD _then_should_not_be_skipped.

    cl_abap_unit_assert=>assert_equals(
      act = mv_skip_testclass
      exp = abap_false
      msg = |Testclass should not be skipped| ).

  ENDMETHOD.

ENDCLASS.

INCLUDE zabapgit_unit_test_clas_intf.
INCLUDE zabapgit_unit_test_transport.
