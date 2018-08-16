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
        RAISING zcx_abapgit_exception.

    DATA: mt_expanded TYPE zif_abapgit_definitions=>ty_expanded_tt,
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
    <ls_expanded>-chmod = zif_abapgit_definitions=>c_chmod-file.

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

ENDCLASS.
