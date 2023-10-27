CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS list_branches FOR TESTING RAISING cx_static_check.
    METHODS test02 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD list_branches.

    zcl_abapgit_gitv2_porcelain=>list_branches(
      iv_url    = 'https://github.com/abapGit/abapGit.git'
      iv_prefix = 'refs/heads' ).
* todo, check response

  ENDMETHOD.

  METHOD test02.

    zcl_abapgit_gitv2_porcelain=>list_no_blobs(
       iv_url = 'https://github.com/abapGit/abapGit.git'
       iv_sha1 = '7bdd8f9f4c6bb0ece461b78c7b559957fad6c3ae' ).
* todo, check response

  ENDMETHOD.

ENDCLASS.
