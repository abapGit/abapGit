CLASS ltcl_tags DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS split FOR TESTING.
    METHODS validate FOR TESTING RAISING zcx_abapgit_exception.
    METHODS normalize FOR TESTING.

    METHODS split_colors FOR TESTING.
    METHODS validate_colors FOR TESTING RAISING zcx_abapgit_exception.
    METHODS normalize_colors FOR TESTING.

ENDCLASS.

CLASS ltcl_tags IMPLEMENTATION.

  METHOD split.

    DATA lt_exp TYPE string_table.

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

  METHOD validate_colors.

    zcl_abapgit_repo_labels=>validate_colors( 'a:red, b : #123456 ,,' ).
    zcl_abapgit_repo_labels=>validate_colors( '' ).
    zcl_abapgit_repo_labels=>validate_colors( ',' ).

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( 'a,ab' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( 'a:' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( ':red' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( 'a:1234' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( 'a:#1234' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD split_colors.

    DATA lt_exp TYPE zcl_abapgit_repo_labels=>ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_exp.

    APPEND INITIAL LINE TO lt_exp ASSIGNING <ls_c>.
    <ls_c>-label = 'a'.
    <ls_c>-color = 'red'.
    APPEND INITIAL LINE TO lt_exp ASSIGNING <ls_c>.
    <ls_c>-label = 'b'.
    <ls_c>-color = '#123456'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>split_colors( 'a:red, b : #123456 ,,' )
      exp = lt_exp ).

  ENDMETHOD.

  METHOD normalize_colors.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize_colors( 'a:red , b : #123456' )
      exp = 'a:red,b:#123456' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize_colors( 'a:red,b:,:blue' )
      exp = 'a:red' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize_colors( '' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_repo_labels=>normalize_colors( ',, ,' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals( " duplicates and sorting
      act = zcl_abapgit_repo_labels=>normalize_colors( 'b:blue,a:red,a:red,a:blue' )
      exp = 'a:red,b:blue' ).


  ENDMETHOD.

ENDCLASS.
