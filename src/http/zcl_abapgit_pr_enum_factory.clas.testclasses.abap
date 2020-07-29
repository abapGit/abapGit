CLASS ltcl_pr_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS simple_test FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_pr_test IMPLEMENTATION.

  METHOD simple_test .

    DATA li_provider TYPE REF TO zif_abapgit_pr_enum_provider.
    DATA lx_err TYPE REF TO zcx_abapgit_exception.

    li_provider = zcl_abapgit_pr_enum_factory=>create_provider( 'https://github.com/larshp/abapGit.git' ).
    cl_abap_unit_assert=>assert_bound( li_provider ).

    CLEAR li_provider.
    TRY.
        li_provider = zcl_abapgit_pr_enum_factory=>create_provider( 'https://another.com/larshp/abapGit.git' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_err.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_err->get_text( )
          exp = '*not supported*' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
