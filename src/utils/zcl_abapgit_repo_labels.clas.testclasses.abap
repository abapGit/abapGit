CLASS ltcl_tags DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS split FOR TESTING.
    METHODS validate FOR TESTING RAISING zcx_abapgit_exception.
    METHODS normalize FOR TESTING.
ENDCLASS.

CLASS ltcl_tags IMPLEMENTATION.

  METHOD split.

    DATA lt_exp TYPE string_table.

    CLEAR lt_exp.
    APPEND `a` TO lt_exp.
    APPEND `ab` TO lt_exp.
    APPEND `a_b` TO lt_exp.
    APPEND `a-b` TO lt_exp.
    APPEND `a.b` TO lt_exp.
    APPEND `Ab` TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>split( 'a,ab, a_b ,,a-b,a.b,Ab' )
      exp = lt_exp ).

  ENDMETHOD.

  METHOD validate.

    zcl_abapgit_repo_labels=>validate( 'a,ab1, a_b ,,a-b,a.b,Ab' ).
    zcl_abapgit_repo_labels=>validate( '' ).
    zcl_abapgit_repo_labels=>validate( ',' ).

    TRY.
        zcl_abapgit_repo_labels=>validate( 'a,ab#' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD normalize.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize( 'a,ab, a_b ,,a-b,a.b,Ab' )
      exp = 'Ab,a,a-b,a.b,a_b,ab' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize( 'a,ab#,a_b' )
      exp = 'a,a_b' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize( '' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize( ',, ,' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals( " duplicates and sorting
      act = zcl_abapgit_repo_labels=>normalize( 'ba,ab,ab' )
      exp = 'ab,ba' ).

  ENDMETHOD.

ENDCLASS.
