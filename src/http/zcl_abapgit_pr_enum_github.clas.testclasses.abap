class ltcl_github_test definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    methods test1 for testing raising zcx_abapgit_exception.

endclass.

class ltcl_github_test implementation.

  method test1.

*    data lo_pr type ref to zcl_abapgit_pr_enum_github.
*    create object lo_pr
*      exporting
*        iv_user_repo = 'sbcgua/abapgit'
*        ii_http_agent = zcl_abapgit_http_agent=>create( ).
*
*    lo_pr->zif_abapgit_pr_enum_provider~list_pull_requests( ).

  endmethod.

endclass.
