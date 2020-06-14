
CLASS ltcl_html DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_html TYPE REF TO zif_abapgit_html.

    METHODS:
      indent1 FOR TESTING RAISING zcx_abapgit_exception,
      indent2 FOR TESTING RAISING zcx_abapgit_exception,
      indent3 FOR TESTING RAISING zcx_abapgit_exception,
      indent4 FOR TESTING RAISING zcx_abapgit_exception,
      style1  FOR TESTING RAISING zcx_abapgit_exception.

    METHODS:
      setup.

ENDCLASS.


CLASS ltcl_html IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_html TYPE zcl_abapgit_html.
  ENDMETHOD.

  METHOD indent1.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( 'hello world' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && zif_abapgit_definitions=>c_newline &&
             '  hello world' && zif_abapgit_definitions=>c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent2.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<input name="comment" type="text">' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && zif_abapgit_definitions=>c_newline &&
             '  <input name="comment" type="text">' && zif_abapgit_definitions=>c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent3.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<textarea name="body" rows="10" cols="72"></textarea>' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && zif_abapgit_definitions=>c_newline &&
             '  <textarea name="body" rows="10" cols="72"></textarea>' && zif_abapgit_definitions=>c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent4.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( 'foo<br>bar' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && zif_abapgit_definitions=>c_newline &&
             '  foo<br>bar' && zif_abapgit_definitions=>c_newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD style1.

    DATA lv_exp TYPE string.

    mo_html->add( '<style type="text/css">' ).
    mo_html->add( '.class1 { color: red }' ).
    mo_html->add( '.class2 {' ).
    mo_html->add( 'color: red' ).
    mo_html->add( '}' ).
    mo_html->add( '</style>' ).

    lv_exp = '<style type="text/css">' && zif_abapgit_definitions=>c_newline &&
             '  .class1 { color: red }' && zif_abapgit_definitions=>c_newline &&
             '  .class2 {' && zif_abapgit_definitions=>c_newline &&
             '    color: red' && zif_abapgit_definitions=>c_newline &&
             '  }' && zif_abapgit_definitions=>c_newline &&
             '</style>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.
