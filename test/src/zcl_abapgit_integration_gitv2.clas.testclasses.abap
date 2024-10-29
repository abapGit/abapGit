CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS list_branches FOR TESTING RAISING cx_static_check.
    METHODS list_no_blobs FOR TESTING RAISING cx_static_check.
    METHODS commits_last_year FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD list_branches.

    DATA lo_list     TYPE REF TO zcl_abapgit_git_branch_list.
    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.

    lo_list = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
      iv_url    = 'https://github.com/abapGit/abapGit.git'
      iv_prefix = 'refs/heads' ).

    lt_branches = lo_list->get_all( ).

    cl_abap_unit_assert=>assert_number_between(
      number = lines( lt_branches )
      lower  = 0
      upper  = 100 ).

  ENDMETHOD.

  METHOD list_no_blobs.

    DATA lt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.

    lt_expanded = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_no_blobs(
       iv_url  = 'https://github.com/abapGit/abapGit.git'
       iv_sha1 = '7bdd8f9f4c6bb0ece461b78c7b559957fad6c3ae' ).

    cl_abap_unit_assert=>assert_number_between(
      number = lines( lt_expanded )
      lower  = 0
      upper  = 5000 ).

  ENDMETHOD.

  METHOD commits_last_year.

    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.

* todo, given the sha1, this test might fail after a year?
    INSERT 'e83a31ebafde4e8e7e80ca36662e42e8f20895c5' INTO TABLE lt_sha1.

    lt_objects = zcl_abapgit_git_factory=>get_v2_porcelain( )->commits_last_year(
      iv_url  = 'https://github.com/abapGit/abapGit.git'
      it_sha1 = lt_sha1 ).

* just check it doesn't throw an exception

  ENDMETHOD.

ENDCLASS.
