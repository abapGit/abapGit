*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UNIT_TEST
*&---------------------------------------------------------------------*

* todo, should the tests be in the same include as the classes
* they are testing?

*----------------------------------------------------------------------*
*       CLASS ltcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS convert_int FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert IMPLEMENTATION.

  METHOD convert_int.
    DATA: lv_xstring TYPE xstring,
          lv_input   TYPE i,
          lv_result  TYPE i.


    DO 1000 TIMES.
      lv_input = sy-index.
      lv_xstring = lcl_convert=>int_to_xstring( iv_i      = lv_input
                                                iv_length = 4 ).
      lv_result = lcl_convert=>xstring_to_int( lv_xstring ).

      cl_abap_unit_assert=>assert_equals(
          exp = lv_input
          act = lv_result ).
    ENDDO.

  ENDMETHOD.                    "convert_int

ENDCLASS.                    "ltcl_convert IMPLEMENTATION

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
        RAISING lcx_exception.

    CONSTANTS: c_package TYPE devclass VALUE '$ABAPGIT_UNIT_TEST'.

ENDCLASS.                    "ltcl_dangerous DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous IMPLEMENTATION.

  METHOD class_setup.

    DATA: lv_text   TYPE c LENGTH 100,
          lv_answer TYPE c LENGTH 1.


    lv_text = 'Objects will be created and deleted, do not run in customer system!'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = lv_text
        text_button_1         = 'Run'
        text_button_2         = 'Cancel'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer.

    IF lv_answer = '2'.
      cl_abap_unit_assert=>fail( 'Cancelled' ).
    ENDIF.

  ENDMETHOD.                    "class_setup

  METHOD run.

    DATA: lo_repo    TYPE REF TO lcl_repo_online,
          lt_tadir   TYPE ty_tadir_tt,
          lv_msg     TYPE string,
          lt_results TYPE ty_results_tt,
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

*----------------------------------------------------------------------*
*       CLASS ltcl_diff DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_diff DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_local    TYPE TABLE OF string,
          mt_remote   TYPE TABLE OF string,
          mt_expected TYPE lcl_diff=>ty_diffs_tt,
          ms_expected LIKE LINE OF mt_expected.

    METHODS: setup.
    METHODS: test.

    METHODS:
      diff01 FOR TESTING,
      diff02 FOR TESTING,
      diff03 FOR TESTING,
      diff04 FOR TESTING,
      diff05 FOR TESTING,
      diff06 FOR TESTING.

ENDCLASS.                    "ltcl_diff DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_diff IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_diff IMPLEMENTATION.

  DEFINE _local.
    APPEND &1 TO mt_local.
  END-OF-DEFINITION.

  DEFINE _remote.
    APPEND &1 TO mt_remote.
  END-OF-DEFINITION.

  DEFINE _expected.
    CLEAR ms_expected.
    ms_expected-local = &1.
    ms_expected-result = &2.
    ms_expected-remote = &3.
    APPEND ms_expected TO mt_expected.
  END-OF-DEFINITION.

  METHOD setup.
    CLEAR mt_local.
    CLEAR mt_remote.
    CLEAR mt_expected.
  ENDMETHOD.                    "setup

  METHOD test.

    DATA: lv_local   TYPE string,
          lv_xlocal  TYPE xstring,
          lv_remote  TYPE string,
          lv_xremote TYPE xstring,
          lo_diff    TYPE REF TO lcl_diff,
          lt_diff    TYPE lcl_diff=>ty_diffs_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF lt_diff.


    CONCATENATE LINES OF mt_local  INTO lv_local SEPARATED BY gc_newline.
    CONCATENATE LINES OF mt_remote INTO lv_remote SEPARATED BY gc_newline.

    lv_xlocal  = lcl_convert=>string_to_xstring_utf8( lv_local ).
    lv_xremote = lcl_convert=>string_to_xstring_utf8( lv_remote ).

    CREATE OBJECT lo_diff
      EXPORTING
        iv_local  = lv_xlocal
        iv_remote = lv_xremote.

    lt_diff = lo_diff->get( ).

    LOOP AT lt_diff ASSIGNING <ls_diff>.
      CLEAR <ls_diff>-local_line.
      CLEAR <ls_diff>-remote_line.
      CLEAR <ls_diff>-short.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( act = lt_diff
                                        exp = mt_expected ).


  ENDMETHOD.                    "test

  METHOD diff01.

* insert
    _local '1'.
    _expected '1' lcl_diff=>c_diff-insert ''.
    test( ).

  ENDMETHOD.                    "diff01

  METHOD diff02.

* identical
    _local '1'.
    _remote '1'.
    _expected '1' '' '1'.
    test( ).

  ENDMETHOD.                    "diff02

  METHOD diff03.

* delete
    _remote '1'.
    _expected '' lcl_diff=>c_diff-delete '1'.
    test( ).

  ENDMETHOD.                    "diff03

  METHOD diff04.

* update
    _local '1+'.
    _remote '1'.
    _expected '1+' lcl_diff=>c_diff-update '1'.
    test( ).

  ENDMETHOD.                    "diff04

  METHOD diff05.

* identical
    _local '1'.
    _local '2'.
    _remote '1'.
    _remote '2'.
    _expected '1' '' '1'.
    _expected '2' '' '2'.
    test( ).

  ENDMETHOD.                    "diff05

  METHOD diff06.

    _local '1'.
    _local '2'.
    _local 'inserted'.
    _local '3'.
    _local '4 update'.

    _remote '1'.
    _remote '2'.
    _remote '3'.
    _remote '4'.

    _expected '1' '' '1'.
    _expected '2' '' '2'.
    _expected 'inserted' lcl_diff=>c_diff-insert ''.
    _expected '3' '' '3'.
    _expected '4 update' lcl_diff=>c_diff-update '4'.

    test( ).

  ENDMETHOD.                    "diff06

ENDCLASS.                    "ltcl_diff IMPLEMENTATION

CLASS ltcl_dot_abapgit DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      identity FOR TESTING
        RAISING lcx_exception,
      ignore FOR TESTING.

ENDCLASS.

CLASS ltcl_dot_abapgit IMPLEMENTATION.

  METHOD identity.

    DATA: lo_dot    TYPE REF TO lcl_dot_abapgit,
          ls_before TYPE lcl_dot_abapgit=>ty_dot_abapgit,
          ls_after  TYPE lcl_dot_abapgit=>ty_dot_abapgit.


    lo_dot = lcl_dot_abapgit=>build_default( gc_english ).
    ls_before = lo_dot->ms_data.

    lo_dot = lcl_dot_abapgit=>deserialize( lo_dot->serialize( ) ).
    ls_after = lo_dot->ms_data.

    cl_abap_unit_assert=>assert_equals(
      act = ls_after
      exp = ls_before ).

  ENDMETHOD.

  METHOD ignore.

    CONSTANTS: lc_path     TYPE string VALUE '/',
               lc_filename TYPE string VALUE 'foobar.txt'.

    DATA: lv_ignored TYPE abap_bool,
          lo_dot     TYPE REF TO lcl_dot_abapgit.


    lo_dot = lcl_dot_abapgit=>build_default( gc_english ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

    lo_dot->add_ignore( iv_path = lc_path iv_filename = lc_filename ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_true ).

    lo_dot->remove_ignore( iv_path = lc_path iv_filename = lc_filename ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_git_porcelain DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      append
        IMPORTING iv_path TYPE string
                  iv_name TYPE string,
      single_file FOR TESTING
        RAISING lcx_exception,
      two_files_same_path FOR TESTING
        RAISING lcx_exception,
      root_empty FOR TESTING
        RAISING lcx_exception,
      sub FOR TESTING
        RAISING lcx_exception.

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
    <ls_expanded>-chmod = gc_chmod-file.

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
*       CLASS ltcl_zlib DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_zlib DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      decompress FOR TESTING RAISING cx_dynamic_check.

ENDCLASS.                    "ltcl_zlib DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_zlib IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_zlib IMPLEMENTATION.

  METHOD decompress.

    DATA: ls_data TYPE lcl_zlib=>ty_decompress.

    CONSTANTS:
      lc_raw        TYPE xstring VALUE '48656C6C6F20576F726C64210D0A',
      lc_compressed TYPE xstring VALUE 'F348CDC9C95708CF2FCA4951E4E5020024E90455'.


    ls_data = lcl_zlib=>decompress( lc_compressed ).

    cl_abap_unit_assert=>assert_not_initial( ls_data-raw ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-raw
                                        exp = lc_raw ).

  ENDMETHOD.                    "decompress

ENDCLASS.                    "ltcl_zlib IMPLEMENTATION

CLASS ltcl_xml DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    METHODS:
      up FOR TESTING
        RAISING lcx_exception,
      down FOR TESTING
        RAISING lcx_exception.

    TYPES: BEGIN OF st_old,
             foo TYPE i,
             bar TYPE c LENGTH 1,
           END OF st_old.

    TYPES: BEGIN OF st_new,
             foo TYPE i,
             bar TYPE c LENGTH 1,
             moo TYPE f,
           END OF st_new.

ENDCLASS.

CLASS ltcl_xml IMPLEMENTATION.

  METHOD up.

    DATA: ls_old    TYPE st_old,
          ls_new    TYPE st_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO lcl_xml_input,
          lo_output TYPE REF TO lcl_xml_output.


    ls_old-foo = 2.
    ls_old-bar = 'A'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_old ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_new ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-foo
      exp = ls_old-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-bar
      exp = ls_old-bar ).

  ENDMETHOD.

  METHOD down.

    DATA: ls_old    TYPE st_old,
          ls_new    TYPE st_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO lcl_xml_input,
          lo_output TYPE REF TO lcl_xml_output.


    ls_new-foo = 2.
    ls_new-bar = 'A'.
    ls_new-moo = 5.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_new ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_old ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_old-foo
      exp = ls_new-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_old-bar
      exp = ls_new-bar ).

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
    repo_host FOR TESTING RAISING lcx_exception,
    repo_name1 FOR TESTING RAISING lcx_exception,
    repo_name2 FOR TESTING RAISING lcx_exception,
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
        lcl_url=>host( 'not a real url' ).                  "#EC NOTEXT
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_exception.                              "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "repo_error

  METHOD repo_host.

    DATA: lv_host TYPE string.

    lv_host = lcl_url=>host( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'https://github.com'
        act = lv_host ).

  ENDMETHOD.                    "repo_url

  METHOD repo_name1.

    DATA: lv_name TYPE string.

    lv_name = lcl_url=>name( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'Foobar'
        act = lv_name ).

  ENDMETHOD.

  METHOD repo_name2.

    DATA: lv_name TYPE string.

    lv_name = lcl_url=>name( 'https://git.hanatrial.ondemand.com/p12345trial/yay' ).

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
      not_exist FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_object_types DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types IMPLEMENTATION.

  METHOD is_supported.

    DATA: ls_item      TYPE ty_item,
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

    DATA: ls_item   TYPE ty_item,
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
        RAISING lcx_exception,
      decode2 FOR TESTING
        RAISING lcx_exception,
      decode3 FOR TESTING
        RAISING lcx_exception.

  PRIVATE SECTION.
    DATA: ms_raw TYPE lcl_git_pack=>ty_commit,
          mv_str TYPE string.

    METHODS:
      setup,
      decode
        RAISING lcx_exception,
      add
        IMPORTING iv_string TYPE string.

ENDCLASS.

CLASS ltcl_git_pack_decode_commit IMPLEMENTATION.

  METHOD setup.
    CLEAR ms_raw.
    CLEAR mv_str.
  ENDMETHOD.

  METHOD add.

    CONCATENATE mv_str iv_string gc_newline INTO mv_str.

  ENDMETHOD.

  METHOD decode.

    DATA: lv_xstr TYPE xstring.


    lv_xstr = lcl_convert=>string_to_xstring_utf8( mv_str ).

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
      act = ms_raw-body
      exp = 'comment' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = 'parent1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent2
      exp = 'parent2' ).

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
      act = ms_raw-body
      exp = 'comment' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = '' ).

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
      act = ms_raw-body
      exp = 'comment' ).
    cl_abap_unit_assert=>assert_equals(
      act = ms_raw-parent
      exp = 'parent1' ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_git_pack DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      tree FOR TESTING
        RAISING lcx_exception,
      commit FOR TESTING
        RAISING lcx_exception,
      pack_short FOR TESTING
        RAISING lcx_exception,
      pack_long FOR TESTING
        RAISING lcx_exception,
      pack_multiple FOR TESTING
        RAISING lcx_exception,
      sort_tree1 FOR TESTING,
      sort_tree2 FOR TESTING.

    METHODS:
      object_blob
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rs_object) TYPE ty_object
        RAISING   lcx_exception.

ENDCLASS.                    "test DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_git_pack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_git_pack IMPLEMENTATION.

  METHOD sort_tree1.

    DATA: lt_tree TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF lt_tree.


    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = gc_chmod-file.
    <ls_tree>-name  = 'b.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = gc_chmod-file.
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
    <ls_tree>-chmod = gc_chmod-file.
    <ls_tree>-name  = 'foo.txt'.
    <ls_tree>-sha1  = '0123'.

    APPEND INITIAL LINE TO lt_tree ASSIGNING <ls_tree>.
    <ls_tree>-chmod = gc_chmod-dir.
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
               lc_sha  TYPE ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_objects TYPE ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_nodes   TYPE lcl_git_pack=>ty_nodes_tt,
          ls_node    LIKE LINE OF lt_nodes,
          ls_commit  TYPE lcl_git_pack=>ty_commit,
          lt_result  TYPE ty_objects_tt,
          lv_data    TYPE xstring.


* blob
    lv_data = lc_data.
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = lv_data ).
    ls_object-type = gc_type-blob.
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
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-commit iv_data = lv_data ).
    ls_object-type = gc_type-commit.
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
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-tree iv_data = lv_data ).
    ls_object-type = gc_type-tree.
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

    rs_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob
                                     iv_data = iv_data ).
    rs_object-type = gc_type-blob.
    rs_object-data = iv_data.

  ENDMETHOD.                    "object_blob

  METHOD pack_short.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE ty_objects_tt,
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

    DATA: lt_objects TYPE ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lv_xstring TYPE xstring,
          lt_result  TYPE ty_objects_tt,
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

    CONSTANTS: lc_sha TYPE ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_nodes  TYPE lcl_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE lcl_git_pack=>ty_nodes_tt.

    CLEAR ls_node.
    ls_node-chmod = gc_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = lc_sha.
    APPEND ls_node TO lt_nodes.

    lv_data = lcl_git_pack=>encode_tree( lt_nodes ).
    lt_result = lcl_git_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.                    "tree

  METHOD commit.

    CONSTANTS: lc_tree   TYPE ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc',
               lc_parent TYPE ty_sha1 VALUE '1236cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: ls_commit TYPE lcl_git_pack=>ty_commit,
          ls_result TYPE lcl_git_pack=>ty_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = lc_tree.
    ls_commit-parent    = lc_parent.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.

    lv_data = lcl_git_pack=>encode_commit( ls_commit ).
    ls_result = lcl_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.                    "commit

ENDCLASS.                    "lcl_abap_unit IMPLEMENTATION

CLASS ltcl_html_helper DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mo_html TYPE REF TO lcl_html_helper.

    METHODS:
      indent1 FOR TESTING RAISING lcx_exception,
      indent2 FOR TESTING RAISING lcx_exception,
      indent3 FOR TESTING RAISING lcx_exception,
      indent4 FOR TESTING RAISING lcx_exception,
      style1  FOR TESTING RAISING lcx_exception.

    METHODS:
      setup,
      last_line
        RETURNING VALUE(rv_line) TYPE string.

ENDCLASS.

CLASS ltcl_html_helper IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_html.
  ENDMETHOD.

  METHOD indent1.

    mo_html->add( '<td>' ).
    mo_html->add( 'hello world' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD indent2.

    mo_html->add( '<td>' ).
    mo_html->add( '<input name="comment" type="text">' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD indent3.

    mo_html->add( '<td>' ).
    mo_html->add( '<textarea name="body" rows="10" cols="72"></textarea>' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD indent4.

    mo_html->add( '<td>' ).
    mo_html->add( 'foo<br>bar' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD last_line.

    DATA: lt_strings TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    SPLIT mo_html->mv_html AT gc_newline INTO TABLE lt_strings.

    READ TABLE lt_strings INDEX lines( lt_strings ) INTO rv_line.

  ENDMETHOD.

  METHOD style1.

    mo_html->add( '<style type="text/css">' ).
    mo_html->add( '.class1 { color: red }' ).
    mo_html->add( '.class2 {' ).
    mo_html->add( 'color: red' ).

    cl_abap_unit_assert=>assert_equals( act = last_line( ) exp = '    color: red' ).

    mo_html->add( '}' ).

    cl_abap_unit_assert=>assert_equals( act = last_line( ) exp = '  }' ).

    mo_html->add( '</style>' ).

    cl_abap_unit_assert=>assert_equals( act = last_line( ) exp = '</style>' ).

  ENDMETHOD.


ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      check
        IMPORTING is_item TYPE ty_item
        RAISING   lcx_exception,
      serialize_tabl FOR TESTING RAISING lcx_exception,
      serialize_enqu FOR TESTING RAISING lcx_exception,
      serialize_shlp FOR TESTING RAISING lcx_exception,
      serialize_view FOR TESTING RAISING lcx_exception,
      serialize_auth FOR TESTING RAISING lcx_exception,
      serialize_clas FOR TESTING RAISING lcx_exception,
      serialize_doma FOR TESTING RAISING lcx_exception,
      serialize_dtel FOR TESTING RAISING lcx_exception,
      serialize_fugr FOR TESTING RAISING lcx_exception,
      serialize_msag FOR TESTING RAISING lcx_exception,
      serialize_prog FOR TESTING RAISING lcx_exception,
      serialize_tran FOR TESTING RAISING lcx_exception,
      serialize_ttyp FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_serialize DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_table

  METHOD serialize_auth.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'AUTH'.
    ls_item-obj_name = 'AREA'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_auth

  METHOD serialize_clas.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_GUI_FRONTEND_SERVICES'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_clas

  METHOD serialize_doma.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_doma

  METHOD serialize_dtel.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_dtel

  METHOD serialize_fugr.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SRFC'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_fugr

  METHOD serialize_msag.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'MSAG'.
    ls_item-obj_name = '00'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_msag

  METHOD serialize_prog.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPLWBABAP'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_prog

  METHOD serialize_tran.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'TRAN'.
    ls_item-obj_name = 'SE38'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_tran

  METHOD serialize_ttyp.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'TTYP'.
    ls_item-obj_name = 'ABAPPROG'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_ttyp

  METHOD check.

    DATA: lt_files TYPE ty_files_tt.

    lt_files = lcl_objects=>serialize( is_item     = is_item
                                       iv_language = gc_english ).

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
        RAISING lcx_exception,
      same_server FOR TESTING
        RAISING lcx_exception.

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

CLASS ltcl_html_action_utils DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL
  INHERITING FROM CL_AUNIT_ASSERT.

  PUBLIC SECTION.

    METHODS add_field FOR TESTING.
    METHODS get_field FOR TESTING.

ENDCLASS. "ltcl_html_action_utils

CLASS ltcl_html_action_utils IMPLEMENTATION.

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

ENDCLASS. "ltcl_html_action_utils

CLASS ltcl_path DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL
  INHERITING FROM CL_AUNIT_ASSERT.

  PUBLIC SECTION.
    METHODS is_root FOR TESTING.
    METHODS split_file_location FOR TESTING.
    METHODS is_subdir FOR TESTING.
    METHODS change_dir FOR TESTING.

ENDCLASS.   "ltcl_path

CLASS ltcl_path IMPLEMENTATION.


  METHOD is_root.

    assert_equals( exp = abap_true  act = lcl_path=>is_root( '/' ) ).
    assert_equals( exp = abap_false act = lcl_path=>is_root( '' ) ).
    assert_equals( exp = abap_false act = lcl_path=>is_root( 'somedir' ) ).
    assert_equals( exp = abap_false act = lcl_path=>is_root( '/somedir' ) ).

  ENDMETHOD.

  METHOD split_file_location.

    DATA: lv_path TYPE string,
          lv_name TYPE string.

    lcl_path=>split_file_location( EXPORTING iv_fullpath = ''
                                   IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    assert_equals( act = lv_path exp = '' ).
    assert_equals( act = lv_name exp = '' ).

    lcl_path=>split_file_location( EXPORTING iv_fullpath = 'somefile'
                                   IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    assert_equals( act = lv_path exp = '' ).
    assert_equals( act = lv_name exp = 'somefile' ).

    lcl_path=>split_file_location( EXPORTING iv_fullpath = '/'
                                   IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    assert_equals( act = lv_path exp = '/' ).
    assert_equals( act = lv_name exp = '' ).

    lcl_path=>split_file_location( EXPORTING iv_fullpath = '/somefile'
                                   IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    assert_equals( act = lv_path exp = '/' ).
    assert_equals( act = lv_name exp = 'somefile' ).

    lcl_path=>split_file_location( EXPORTING iv_fullpath = '/somedir/'
                                   IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    assert_equals( act = lv_path exp = '/somedir/' ).
    assert_equals( act = lv_name exp = '' ).

    lcl_path=>split_file_location( EXPORTING iv_fullpath = '/somedir/somefile'
                                   IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    assert_equals( act = lv_path exp = '/somedir/' ).
    assert_equals( act = lv_name exp = 'somefile' ).


  ENDMETHOD.

  METHOD is_subdir.

    DATA lv_yes TYPE abap_bool.

    lv_yes = lcl_path=>is_subdir( iv_path   = '/dir/subdir'
                                  iv_parent = '/dir' ).
    assert_equals( act = lv_yes exp = abap_true ).

    lv_yes = lcl_path=>is_subdir( iv_path   = '/dir/subdir'
                                  iv_parent = '/dir/' ).
    assert_equals( act = lv_yes exp = abap_true ).

    lv_yes = lcl_path=>is_subdir( iv_path   = '/another'
                                  iv_parent = '/dir' ).
    assert_equals( act = lv_yes exp = abap_false ).

    lv_yes = lcl_path=>is_subdir( iv_path   = '/dir'
                                  iv_parent = '/dir' ).
    assert_equals( act = lv_yes exp = abap_false ).

    lv_yes = lcl_path=>is_subdir( iv_path   = '/dir'
                                  iv_parent = '/' ).
    assert_equals( act = lv_yes exp = abap_true ).

    lv_yes = lcl_path=>is_subdir( iv_path   = '/dir2'
                                  iv_parent = '/dir' ).
    assert_equals( act = lv_yes exp = abap_false ).

  ENDMETHOD.

  METHOD change_dir.

    DATA lv_path TYPE string.

    lv_path = lcl_path=>change_dir( iv_cur_dir = ''
                                    iv_cd      = '' ).
    assert_equals( act = lv_path exp = '' ).

    lv_path = lcl_path=>change_dir( iv_cur_dir = '/dir'
                                    iv_cd      = '' ).
    assert_equals( act = lv_path exp = '/dir' ).

    lv_path = lcl_path=>change_dir( iv_cur_dir = '/dir'
                                    iv_cd      = '.' ).
    assert_equals( act = lv_path exp = '/dir' ).

    lv_path = lcl_path=>change_dir( iv_cur_dir = '/dir'
                                    iv_cd      = '..' ).
    assert_equals( act = lv_path exp = '/' ).

    lv_path = lcl_path=>change_dir( iv_cur_dir = '/dir/sub'
                                    iv_cd      = '..' ).
    assert_equals( act = lv_path exp = '/dir/' ).

    lv_path = lcl_path=>change_dir( iv_cur_dir = '/dir/'
                                    iv_cd      = 'sub' ).
    assert_equals( act = lv_path exp = '/dir/sub' ).

    lv_path = lcl_path=>change_dir( iv_cur_dir = '/dir'
                                    iv_cd      = 'sub' ).
    assert_equals( act = lv_path exp = '/dir/sub' ).


  ENDMETHOD.

ENDCLASS.   "ltcl_path

CLASS ltcl_file_status DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL
  INHERITING FROM CL_AUNIT_ASSERT.

  PUBLIC SECTION.
    METHODS calculate_status FOR TESTING.

ENDCLASS.   "ltcl_file_status

CLASS ltcl_file_status IMPLEMENTATION.

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
    <remote>-path     = '/'.
    <remote>-filename = &1.
    <remote>-sha1     = &2.
  END-OF-DEFINITION.

  DEFINE _append_tadir.
    APPEND INITIAL LINE TO lt_tadir ASSIGNING <tadir>.
    <tadir>-object   = &1.
    <tadir>-obj_name = &2.
    <tadir>-devclass = '$Z$'.
  END-OF-DEFINITION.

  DEFINE _append_result.
    APPEND INITIAL LINE TO lt_results_exp ASSIGNING <result>.
    <result>-obj_type = &1.
    <result>-obj_name = &2.
    <result>-match    = &3.
    <result>-lstate   = &4.
    <result>-rstate   = &5.
    <result>-package  = &6.
    <result>-filename = &7.
    <result>-path     = '/'.
  END-OF-DEFINITION.

  METHOD calculate_status.

    DATA: lt_local       TYPE ty_files_item_tt,
          lt_remote      TYPE ty_files_tt,
          lt_state       TYPE ty_file_signatures_tt,
          lt_tadir       TYPE ty_tadir_tt,
          lt_results     TYPE ty_results_tt,
          lt_results_exp TYPE ty_results_tt.

    FIELD-SYMBOLS: <local>  LIKE LINE OF lt_local,
                   <remote> LIKE LINE OF lt_remote,
                   <result> LIKE LINE OF lt_results,
                   <state>  LIKE LINE OF lt_state,
                   <tadir>  LIKE LINE OF lt_tadir.

    "STATE        FILE                            SHA1
    _append_state 'zdoma1.doma.xml'               'D1'.
    _append_state 'zdoma2.doma.xml'               'D2'.
    _append_state 'zdoma3.doma.xml'               'D3'.
    _append_state 'zclass1.clas.xml'              'C1_F1'.
    _append_state 'zclass1.clas.testclasses.abap' 'C1_F3'.
    _append_state 'zdoma5.doma.xml'               'D5'.
    _append_state 'zdoma6.doma.xml'               'D6'.

    "LOCAL        TYPE   NAME      FILE                            SHA1
    _append_local 'DOMA' 'ZDOMA1'  'zdoma1.doma.xml'               'D1'.
    _append_local 'DOMA' 'ZDOMA2'  'zdoma2.doma.xml'               'D2_CHANGED_L'.
    _append_local 'DOMA' 'ZDOMA3'  'zdoma3.doma.xml'               'D3'.
    _append_local 'CLAS' 'ZCLASS1' 'zclass1.clas.xml'              'C1_F1'.
    _append_local 'CLAS' 'ZCLASS1' 'zclass1.clas.testclasses.abap' 'C1_F3'.
    _append_local 'DOMA' 'ZDOMA5'  'zdoma5.doma.xml'               'D5'.
    _append_local 'DOMA' 'ZDOMA6'  'zdoma6.doma.xml'               'D6_CHANGED_L'.

    "REMOTE        FILE                SHA1
    _append_remote 'zdoma1.doma.xml'   'D1'.
    _append_remote 'zdoma2.doma.xml'   'D2'.
    _append_remote 'zdoma3.doma.xml'   'D3_CHANGED_R'.
    _append_remote 'zclass1.clas.xml'  'C1_F1'.
    _append_remote 'zclass1.clas.abap' 'C1_F2'.
    _append_remote 'zdoma4.doma.xml'   'D4'.
    _append_remote 'zdoma6.doma.xml'   'D6_CHANGED_R'.
    _append_remote 'textfile.txt'      'T1'.

    "TADIR        TYPE   NAME
    _append_tadir 'DOMA' 'ZDOMA1'.
    _append_tadir 'DOMA' 'ZDOMA2'.
    _append_tadir 'DOMA' 'ZDOMA3'.
    _append_tadir 'CLAS' 'ZCLASS1'.
    _append_tadir 'DOMA' 'ZDOMA5'.
    _append_tadir 'DOMA' 'ZDOMA6'.

    "EXP RESULT    TYPE   NAME      MATCH LST  RST  PKG   FILE
    _append_result ''     ''        ''    ''   'A'  ''    'textfile.txt'.
    _append_result 'CLAS' 'ZCLASS1' ''    'R'  'L'  '$Z$' 'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' ''    'L'  'R'  '$Z$' 'zclass1.clas.testclasses.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ''   ''   '$Z$' 'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ''   ''   '$Z$' 'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ''    'M'  ''   '$Z$' 'zdoma2.doma.xml'.
    _append_result 'DOMA' 'ZDOMA3'  ''    ''   'M'  '$Z$' 'zdoma3.doma.xml'.
    _append_result 'DOMA' 'ZDOMA4'  ''    ''   'A'  '$Z$' 'zdoma4.doma.xml'.
    _append_result 'DOMA' 'ZDOMA5'  ''    'A'  ''   '$Z$' 'zdoma5.doma.xml'.
    _append_result 'DOMA' 'ZDOMA6'  ''    'M'  'M'  '$Z$' 'zdoma6.doma.xml'.

    lt_results = lcl_file_status=>calculate_status_old(
      it_local           = lt_local
      it_remote          = lt_remote
      it_tadir           = lt_tadir
      iv_starting_folder = '/' ).

    assert_equals( act = lt_results exp = lt_results_exp ).

  ENDMETHOD.  "calculate_status

ENDCLASS.   "ltcl_file_status