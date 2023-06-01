CLASS lcl_good_renderable DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_renderable.
ENDCLASS.
CLASS lcl_good_renderable IMPLEMENTATION.
  METHOD zif_abapgit_gui_renderable~render.
    ri_html = zcl_abapgit_html=>create( 'Hello' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_bad_renderable DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_renderable.
ENDCLASS.
CLASS lcl_bad_renderable IMPLEMENTATION.
  METHOD zif_abapgit_gui_renderable~render.
    zcx_abapgit_exception=>raise( 'Fail!' ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_html DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_html TYPE REF TO zif_abapgit_html.

    METHODS:
      wrap    FOR TESTING RAISING zcx_abapgit_exception,
      add_renderable FOR TESTING RAISING zcx_abapgit_exception,
      td      FOR TESTING RAISING zcx_abapgit_exception,
      th      FOR TESTING RAISING zcx_abapgit_exception,
      wrap_ii FOR TESTING RAISING zcx_abapgit_exception,
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

  METHOD td.

    mo_html->td( 'Hello' ).
    mo_html->td(
      iv_format_single_line = abap_false
      iv_content = 'Hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<td>Hello</td>' && cl_abap_char_utilities=>newline &&
        '<td>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</td>' ).

  ENDMETHOD.

  METHOD th.

    mo_html->th( 'Hello' ).
    mo_html->th(
      iv_format_single_line = abap_false
      iv_content = 'Hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<th>Hello</th>' && cl_abap_char_utilities=>newline &&
        '<th>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</th>' ).

  ENDMETHOD.

  METHOD wrap_ii.

    mo_html->wrap(
      iv_tag     = 'td'
      ii_content = zcl_abapgit_html=>create( )->add( 'Hello' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<td>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</td>' ).

  ENDMETHOD.

  METHOD wrap.

    mo_html->wrap( iv_tag = 'td' ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_content = 'Hello' ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_class   = 'class'
      iv_hint    = 'hint'
      iv_id      = 'id'
      iv_content = 'Hello' ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_content = 'Hello'
      iv_format_single_line = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<td></td>' && cl_abap_char_utilities=>newline &&
        '<td>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</td>' && cl_abap_char_utilities=>newline &&
        '<td id="id" class="class" title="hint">' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</td>' && cl_abap_char_utilities=>newline &&
        '<td>Hello</td>' ).

  ENDMETHOD.

  METHOD add_renderable.

    DATA lo_good TYPE REF TO lcl_good_renderable.
    DATA lo_bad TYPE REF TO lcl_bad_renderable.

    CREATE OBJECT lo_good.
    CREATE OBJECT lo_bad.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_html=>create( lo_good )->render( )
      exp = 'Hello' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = zcl_abapgit_html=>create( lo_bad )->render( )
      exp = '<span*Fail!*' ).

  ENDMETHOD.

ENDCLASS.
