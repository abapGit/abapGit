
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

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  hello world' && cl_abap_char_utilities=>newline &&
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

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  <input name="comment" type="text">' && cl_abap_char_utilities=>newline &&
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

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  <textarea name="body" rows="10" cols="72"></textarea>' && cl_abap_char_utilities=>newline &&
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

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  foo<br>bar' && cl_abap_char_utilities=>newline &&
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

    lv_exp = '<style type="text/css">' && cl_abap_char_utilities=>newline &&
             '  .class1 { color: red }' && cl_abap_char_utilities=>newline &&
             '  .class2 {' && cl_abap_char_utilities=>newline &&
             '    color: red' && cl_abap_char_utilities=>newline &&
             '  }' && cl_abap_char_utilities=>newline &&
             '</style>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.
