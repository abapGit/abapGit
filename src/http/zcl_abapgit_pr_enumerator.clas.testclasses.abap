class ltcl_pr_test definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    methods test1 for testing raising zcx_abapgit_exception.

endclass.

class ltcl_pr_test implementation.

  method test1.

    data lo_pr type ref to zcl_abapgit_pr_enumerator.
    create object lo_pr exporting iv_repo_url = 'https://github.com/sbcgua/abapgit'.

    lo_pr->get_repo_info( ).

  endmethod.

endclass.
