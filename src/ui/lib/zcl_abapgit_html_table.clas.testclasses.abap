CLASS ltcl_test_simple_table DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_html_table.

    TYPES:
      BEGIN OF ty_simple_data,
        col1 TYPE string,
        col2 TYPE i,
        col3 TYPE REF TO data,
      END OF ty_simple_data.

    METHODS simple_render FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_test_simple_table IMPLEMENTATION.
  METHOD zif_abapgit_html_table~get_row_attrs.
    rs_attrs-css_class = |r{ iv_row_index }|.
  ENDMETHOD.

  METHOD zif_abapgit_html_table~render_cell.
    rs_render-css_class = 'cell'.
    rs_render-content   = |{ iv_value }|.
  ENDMETHOD.

  METHOD simple_render.

    DATA lo_tab TYPE REF TO zcl_abapgit_html_table.
    DATA lt_dummy_data TYPE TABLE OF ty_simple_data.
    DATA lv_html_act TYPE string.
    DATA li_html_exp TYPE REF TO zif_abapgit_html.
    FIELD-SYMBOLS <ls_i> LIKE LINE OF lt_dummy_data.

    lo_tab = zcl_abapgit_html_table=>create( ii_renderer = me
      )->define_column(
        iv_column_name = 'col1'
        iv_column_title = 'Col 1'
      )->define_column(
        iv_column_name = 'col2'
        iv_column_title = 'Col 2'
      ).

    APPEND INITIAL LINE TO lt_dummy_data ASSIGNING <ls_i>.
    <ls_i>-col1 = 'Hello'.
    <ls_i>-col2 = 1.
    APPEND INITIAL LINE TO lt_dummy_data ASSIGNING <ls_i>.
    <ls_i>-col1 = 'World'.
    <ls_i>-col2 = 2.

    lv_html_act = lo_tab->render(
      iv_id        = 'tabid'
      iv_css_class = 'tabclass'
      it_data      = lt_dummy_data )->render( ).

    CREATE OBJECT li_html_exp TYPE zcl_abapgit_html.

    li_html_exp->add(
      '<table> id="tabid" class="tabclass"' )->add(
      '<thead>' )->add(
      '<tr>' )->add(
      '<th>Col 1</th>' )->add(
      '<th>Col 2</th>' )->add(
      '</tr>' )->add(
      '</thead>' )->add(
      '<tbody>' )->add(
      '<tr class="r1">' )->add(
      '<td class="cell">Hello</td>' )->add(
      '<td class="cell">1</td>' )->add(
      '</tr>' )->add(
      '<tr class="r2">' )->add(
      '<td class="cell">World</td>' )->add(
      '<td class="cell">2</td>' )->add(
      '</tr>' )->add(
      '</tbody>' )->add(
      '</table>' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_html_act
      exp = li_html_exp->render( ) ).

  ENDMETHOD.

ENDCLASS.
