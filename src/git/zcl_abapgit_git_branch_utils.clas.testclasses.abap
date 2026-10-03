CLASS ltcl_complete_heads_name DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      short_name FOR TESTING,
      full_name FOR TESTING,
      upper_case_prefix FOR TESTING.

ENDCLASS.

CLASS ltcl_complete_heads_name IMPLEMENTATION.

  METHOD short_name.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_git_branch_utils=>complete_heads_branch_name( 'feature' )
      exp = 'refs/heads/feature' ).

  ENDMETHOD.

  METHOD full_name.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_git_branch_utils=>complete_heads_branch_name( 'refs/heads/feature' )
      exp = 'refs/heads/feature' ).

  ENDMETHOD.

  METHOD upper_case_prefix.

    " refs are case-sensitive in Git: REFS/HEADS/x is outside refs/heads/,
    " and the remote refuses it as a "funny refname"
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_git_branch_utils=>complete_heads_branch_name( 'REFS/HEADS/feature' )
      exp = 'refs/heads/REFS/HEADS/feature' ).

  ENDMETHOD.

ENDCLASS.
