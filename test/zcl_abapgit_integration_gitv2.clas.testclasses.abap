CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS list_branches FOR TESTING RAISING cx_static_check.
    METHODS test02 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD list_branches.

    DATA lo_list     TYPE REF TO zcl_abapgit_git_branch_list.
    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.

    lo_list = zcl_abapgit_gitv2_porcelain=>list_branches(
      iv_url    = 'https://github.com/abapGit/abapGit.git'
      iv_prefix = 'refs/heads' ).

    lt_branches = lo_list->get_all( ).

    cl_abap_unit_assert=>assert_number_between(
      number = lines( lt_branches )
      lower  = 0
      upper  = 100 ).

  ENDMETHOD.

  METHOD test02.

    zcl_abapgit_gitv2_porcelain=>list_no_blobs(
       iv_url = 'https://github.com/abapGit/abapGit.git'
       iv_sha1 = '7bdd8f9f4c6bb0ece461b78c7b559957fad6c3ae' ).
* todo, check response

  ENDMETHOD.

ENDCLASS.
