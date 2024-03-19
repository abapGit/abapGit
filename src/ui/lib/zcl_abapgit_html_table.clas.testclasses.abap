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
        col3 TYPE i,
        col4 TYPE REF TO data,
      END OF ty_simple_data,
      ty_simple_data_tab TYPE STANDARD TABLE OF ty_simple_data WITH DEFAULT KEY.

    METHODS simple_render FOR TESTING RAISING zcx_abapgit_exception.
    METHODS with_cids FOR TESTING RAISING zcx_abapgit_exception.
    METHODS with_sort FOR TESTING RAISING zcx_abapgit_exception.

    METHODS test_data_set
      RETURNING
        VALUE(rt_data_set) TYPE ty_simple_data_tab.
ENDCLASS.

CLASS ltcl_test_simple_table IMPLEMENTATION.
  METHOD zif_abapgit_html_table~get_row_attrs.
    IF iv_table_id = 'simple'.
      rs_attrs-css_class = |r{ iv_row_index }|.
    ELSEIF iv_table_id = 'with-cids'.
      rs_attrs-data = zcl_abapgit_html=>parse_data_attr( |attr={ iv_row_index }| ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_html_table~render_cell.
    IF iv_table_id = 'simple'.
      rs_render-css_class = 'cell'.
      IF iv_column_id = 'colX'.
        IF iv_row_index = 2.
          rs_render-html = zcl_abapgit_html=>create( )->add( 'XHTML' ).
        ELSE.
          rs_render-content = 'X'.
        ENDIF.
      ELSE.
        rs_render-content = |{ iv_value }|.
      ENDIF.
    ELSEIF iv_table_id = 'with-cids'.
      rs_render-content = iv_value.
    ELSEIF iv_table_id = 'with-sort'.
      rs_render-content = iv_value.
    ENDIF.

  ENDMETHOD.

  METHOD test_data_set.

    FIELD-SYMBOLS <ls_i> LIKE LINE OF rt_data_set.

    APPEND INITIAL LINE TO rt_data_set ASSIGNING <ls_i>.
    <ls_i>-col1 = 'Hello'.
    <ls_i>-col2 = 1.
    <ls_i>-col3 = 10.
    APPEND INITIAL LINE TO rt_data_set ASSIGNING <ls_i>.
    <ls_i>-col1 = 'World'.
    <ls_i>-col2 = 2.
    <ls_i>-col3 = 20.

  ENDMETHOD.

  METHOD simple_render.

    DATA lo_tab TYPE REF TO zcl_abapgit_html_table.
    DATA lv_html_act TYPE string.
    DATA li_html_exp TYPE REF TO zif_abapgit_html.

    lo_tab = zcl_abapgit_html_table=>create( me
      )->define_column(
        iv_column_id = 'col1'
        iv_column_title = 'Col 1'
      )->define_column(
        iv_column_id = 'col2'
        iv_column_title = 'Col 2'
        iv_from_field = 'col3'
      )->define_column( 'colX' ).

    lv_html_act = lo_tab->render(
      iv_id        = 'simple'
      iv_css_class = 'tabclass'
      it_data      = test_data_set( ) )->render( ).

    CREATE OBJECT li_html_exp TYPE zcl_abapgit_html.

    li_html_exp->add(
      '<table id="simple" class="tabclass">' )->add(
      '<thead>' )->add(
      '<tr>' )->add(
      '<th>Col 1</th>' )->add(
      '<th>Col 2</th>' )->add(
      '<th></th>' )->add(
      '</tr>' )->add(
      '</thead>' )->add(
      '<tbody>' )->add(
      '<tr class="r1">' )->add(
      '<td class="cell">Hello</td>' )->add(
      '<td class="cell">10</td>' )->add(
      '<td class="cell">X</td>' )->add(
      '</tr>' )->add(
      '<tr class="r2">' )->add(
      '<td class="cell">World</td>' )->add(
      '<td class="cell">20</td>' )->add(
      '<td class="cell">' )->add(
      'XHTML' )->add(
      '</td>' )->add(
      '</tr>' )->add(
      '</tbody>' )->add(
      '</table>' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_html_act
      exp = li_html_exp->render( ) ).

  ENDMETHOD.

  METHOD with_cids.

    DATA lo_tab TYPE REF TO zcl_abapgit_html_table.
    DATA lv_html_act TYPE string.
    DATA li_html_exp TYPE REF TO zif_abapgit_html.

    lo_tab = zcl_abapgit_html_table=>create( me
      )->define_column(
        iv_column_id    = 'col1'
        iv_column_title = 'Col 1'
      )->define_column(
        iv_column_id    = 'col2'
        iv_column_title = 'Col 2' ).

    lv_html_act = lo_tab->render(
      iv_id        = 'with-cids'
      iv_with_cids = abap_true
      it_data      = test_data_set( ) )->render( ).

    CREATE OBJECT li_html_exp TYPE zcl_abapgit_html.

    li_html_exp->add(
      '<table id="with-cids">' )->add(
      '<thead>' )->add(
      '<tr>' )->add(
      '<th data-cid="col1">Col 1</th>' )->add(
      '<th data-cid="col2">Col 2</th>' )->add(
      '</tr>' )->add(
      '</thead>' )->add(
      '<tbody>' )->add(
      '<tr data-attr="1">' )->add(
      '<td data-cid="col1">Hello</td>' )->add(
      '<td data-cid="col2">1</td>' )->add(
      '</tr>' )->add(
      '<tr data-attr="2">' )->add(
      '<td data-cid="col1">World</td>' )->add(
      '<td data-cid="col2">2</td>' )->add(
      '</tr>' )->add(
      '</tbody>' )->add(
      '</table>' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_html_act
      exp = li_html_exp->render( ) ).

  ENDMETHOD.

  METHOD with_sort.

    DATA lo_tab TYPE REF TO zcl_abapgit_html_table.
    DATA lv_html_act TYPE string.
    DATA li_html_exp TYPE REF TO zif_abapgit_html.
    DATA ls_sort TYPE zif_abapgit_html_table=>ty_sorting_state.

    lo_tab = zcl_abapgit_html_table=>create( me
      )->define_column(
        iv_column_id    = 'col1'
        iv_column_title = 'Col 1'
      )->define_column(
        iv_column_id    = 'col2'
        iv_column_title = 'Col 2'
      )->define_column(
        iv_column_id    = 'col3'
        iv_column_title = 'Col 3'
        iv_sortable     = abap_false ).

    ls_sort-column_id = 'col1'.

    lv_html_act = lo_tab->render(
      is_sorting_state = ls_sort
      iv_id        = 'with-sort'
      it_data      = test_data_set( ) )->render( ).

    CREATE OBJECT li_html_exp TYPE zcl_abapgit_html.

    li_html_exp->add(
      '<table id="with-sort">' )->add(
      '<thead>' )->add(
      '<tr>' )->add(
      '<th><a href="sapevent:sort_by:col1:dsc">Col 1</a><span class="sort-arrow sort-active">&#x25BE;</span></th>'
      )->add(
      '<th><a href="sapevent:sort_by:col2:asc">Col 2</a><span class="sort-arrow">&#x25BE;</span></th>' )->add(
      '<th>Col 3</th>' )->add(
      '</tr>' )->add(
      '</thead>' )->add(
      '<tbody>' )->add(
      '<tr>' )->add(
      '<td>Hello</td>' )->add(
      '<td>1</td>' )->add(
      '<td>10</td>' )->add(
      '</tr>' )->add(
      '<tr>' )->add(
      '<td>World</td>' )->add(
      '<td>2</td>' )->add(
      '<td>20</td>' )->add(
      '</tr>' )->add(
      '</tbody>' )->add(
      '</table>' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_html_act
      exp = li_html_exp->render( ) ).

  ENDMETHOD.

ENDCLASS.
