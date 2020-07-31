CLASS ltcl_github_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS test1 FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_github_test IMPLEMENTATION.

  METHOD test1.

    DATA lo_pr TYPE REF TO zcl_abapgit_pr_enum_github.
    CREATE OBJECT lo_pr
      EXPORTING
        iv_user_repo  = 'sbcgua/abapgit'
        ii_http_agent = zcl_abapgit_http_agent=>create( ).

    lo_pr->zif_abapgit_pr_enum_provider~list_pull_requests( ).

  ENDMETHOD.

ENDCLASS.
