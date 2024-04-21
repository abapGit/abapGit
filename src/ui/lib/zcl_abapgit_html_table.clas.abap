CLASS zcl_abapgit_html_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ii_renderer    TYPE REF TO zif_abapgit_html_table OPTIONAL " Can be passed to renderer
        !is_initial_sorting_state TYPE zif_abapgit_html_table=>ty_sorting_state OPTIONAL
        PREFERRED PARAMETER ii_renderer
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_html_table .
    " maybe also th css_class
    METHODS define_column
      IMPORTING
        !iv_column_id    TYPE string
        !iv_column_title TYPE string OPTIONAL
        !iv_from_field   TYPE abap_compname OPTIONAL
        !iv_sortable     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_table .
    METHODS define_column_group
      IMPORTING
        !iv_group_id    TYPE string OPTIONAL " not mandatory, but can be used for CSS (TODO data-gid)
        !iv_group_title TYPE string OPTIONAL " can be empty !
        PREFERRED PARAMETER iv_group_title
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_table
      RAISING
        zcx_abapgit_exception .
    " Maybe also data_provider
    " TODO record Limiter
    METHODS render
      IMPORTING
        !ii_renderer   TYPE REF TO zif_abapgit_html_table OPTIONAL
        !it_data       TYPE ANY TABLE
        !iv_id         TYPE csequence OPTIONAL
        !iv_css_class  TYPE csequence OPTIONAL
        !iv_with_cids  TYPE abap_bool DEFAULT abap_false
        !is_sorting_state TYPE zif_abapgit_html_table=>ty_sorting_state OPTIONAL
        !iv_wrap_in_div   TYPE string OPTIONAL " div class name
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .

    " Sorting utils
    CLASS-METHODS detect_sorting_request
      IMPORTING
        iv_event TYPE string
      RETURNING
        VALUE(rs_sorting_request) TYPE zif_abapgit_html_table=>ty_sorting_state.
    METHODS process_sorting_request
      IMPORTING
        iv_event TYPE string
      RETURNING
        VALUE(rv_processed) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_sort_by_event_prefix TYPE string VALUE `sort_by:`.
    CONSTANTS c_sort_by_event_regex TYPE string VALUE `^sort_by:\w+:(asc|dsc)$`.

    TYPES:
      BEGIN OF ty_column,
        column_id    TYPE string,
        column_title TYPE string,
        from_field   TYPE abap_compname,
        sortable     TYPE abap_bool,
        is_group     TYPE abap_bool,
        group_span   TYPE i,
      END OF ty_column,
      ty_columns TYPE STANDARD TABLE OF ty_column WITH KEY column_id.

    DATA mi_renderer TYPE REF TO zif_abapgit_html_table.
    DATA mt_columns TYPE ty_columns.
    DATA mi_html TYPE REF TO zif_abapgit_html.
    DATA mv_with_cids TYPE abap_bool.
    DATA mv_table_id TYPE string.
    DATA ms_sorting_state TYPE zif_abapgit_html_table=>ty_sorting_state.
    DATA mr_last_grp TYPE REF TO ty_column.

    " potentially receive from outside
    DATA mv_sort_span_class TYPE string VALUE `sort-arrow`.
    DATA mv_sort_active_class TYPE string VALUE `sort-active`.

    METHODS render_thead
      RAISING
        zcx_abapgit_exception .

    METHODS render_tbody
      IMPORTING
        it_data TYPE ANY TABLE
      RAISING
        zcx_abapgit_exception .

    METHODS render_row
      IMPORTING
        iv_row_index TYPE i
        is_row TYPE any
      RAISING
        zcx_abapgit_exception .

    METHODS render_column_title
      IMPORTING
        is_col TYPE ty_column
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS cid_attr
      IMPORTING
        iv_column_id TYPE string
      RETURNING
        VALUE(rs_data_attr) TYPE zif_abapgit_html=>ty_data_attr.

    CLASS-METHODS gid_attr
      IMPORTING
        iv_column_id TYPE string
      RETURNING
        VALUE(rs_data_attr) TYPE zif_abapgit_html=>ty_data_attr.

    METHODS apply_sorting
      CHANGING
        ct_data TYPE STANDARD TABLE.

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_TABLE IMPLEMENTATION.


  METHOD apply_sorting.

    DATA lv_field TYPE abap_compname.
    DATA ls_col LIKE LINE OF mt_columns.

    IF ms_sorting_state-column_id IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE mt_columns INTO ls_col WITH KEY column_id = ms_sorting_state-column_id.
    IF sy-subrc <> 0.
      RETURN. " ??? but let's not throw errors here
    ENDIF.

    IF ls_col-from_field IS NOT INITIAL.
      lv_field = to_upper( ls_col-from_field ).
    ELSE.
      lv_field = to_upper( ms_sorting_state-column_id ).
    ENDIF.

    " What to do if column_id is not a table field ?
    " Well ... then it is a complex case for an external sorting, don't use the simple one

    IF ms_sorting_state-descending = abap_true.
      SORT ct_data BY (lv_field) DESCENDING.
    ELSE.
      SORT ct_data BY (lv_field) ASCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD cid_attr.

    rs_data_attr-name  = 'cid'.
    rs_data_attr-value = iv_column_id.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_instance.
    ro_instance->mi_renderer = ii_renderer.
    ro_instance->ms_sorting_state = is_initial_sorting_state.
  ENDMETHOD.


  METHOD define_column.

    FIELD-SYMBOLS <ls_c> LIKE LINE OF mt_columns.

    ASSERT iv_column_id IS NOT INITIAL.
    ro_self = me.

    APPEND INITIAL LINE TO mt_columns ASSIGNING <ls_c>.
    <ls_c>-column_id    = iv_column_id.
    <ls_c>-column_title = iv_column_title.
    <ls_c>-from_field   = to_upper( iv_from_field ).
    <ls_c>-sortable     = iv_sortable.

    IF mr_last_grp IS NOT INITIAL.
      mr_last_grp->group_span = mr_last_grp->group_span + 1.
    ENDIF.

  ENDMETHOD.


  METHOD define_column_group.

    IF lines( mt_columns ) > 0 AND mr_last_grp IS INITIAL.
      " Groups should cover all columns
      " you can create a group with empty title if groups start later VISUALLY
      zcx_abapgit_exception=>raise( 'Start groups from the beginning' ).
    ENDIF.

    ro_self = me.

    APPEND INITIAL LINE TO mt_columns REFERENCE INTO mr_last_grp.
    mr_last_grp->is_group     = abap_true.
    mr_last_grp->column_id    = iv_group_id.
    mr_last_grp->column_title = iv_group_title.

  ENDMETHOD.


  METHOD detect_sorting_request.

    DATA lv_req TYPE string.

    IF find(
        val = iv_event
        regex = c_sort_by_event_regex ) = 0.

      lv_req = replace(
        val  = iv_event
        sub  = c_sort_by_event_prefix
        with = '' ).
      SPLIT lv_req AT ':' INTO rs_sorting_request-column_id lv_req.
      rs_sorting_request-descending = boolc( lv_req = 'dsc' ).

    ENDIF.

  ENDMETHOD.


  METHOD gid_attr.

    rs_data_attr-name  = 'gid'.
    rs_data_attr-value = iv_column_id.

  ENDMETHOD.


  METHOD process_sorting_request.

    DATA ls_sorting_req LIKE ms_sorting_state.

    ls_sorting_req = detect_sorting_request( iv_event ).
    IF ls_sorting_req IS NOT INITIAL.
      ms_sorting_state = ls_sorting_req.
      rv_processed     = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD render.

    DATA lv_attrs TYPE string.
    DATA lr_data_copy TYPE REF TO data.
    FIELD-SYMBOLS <lt_data> TYPE ANY TABLE.

    IF ii_renderer IS BOUND.
      mi_renderer = ii_renderer.
    ENDIF.

    ASSERT mi_renderer IS BOUND.

    mv_with_cids     = iv_with_cids.
    mv_table_id      = iv_id.
    ASSIGN it_data TO <lt_data>.

    IF is_sorting_state IS NOT INITIAL.
      ms_sorting_state = is_sorting_state.
    ELSEIF ms_sorting_state IS NOT INITIAL.
      " If sorting state is not passed,
      " but there is non empty sort state then suppose simple sorting mode
      " so that table sorts the data itself before rendering
      " TODO not efficient, maybe bind changing data from outside
      CREATE DATA lr_data_copy LIKE it_data.
      ASSIGN lr_data_copy->* TO <lt_data>.
      <lt_data> = it_data.
      apply_sorting( CHANGING ct_data = <lt_data> ).
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_attrs = lv_attrs && | id="{ iv_id }"|.
    ENDIF.

    IF iv_css_class IS NOT INITIAL.
      lv_attrs = lv_attrs && | class="{ iv_css_class }"|.
    ENDIF.

    CREATE OBJECT mi_html TYPE zcl_abapgit_html.
    ri_html = mi_html.

    IF iv_wrap_in_div IS NOT INITIAL.
      mi_html->add( |<div class="{ iv_wrap_in_div }">| ).
    ENDIF.

    mi_html->add( |<table{ lv_attrs }>| ).
    render_thead( ).
    render_tbody( <lt_data> ).
    mi_html->add( '</table>' ).

    IF iv_wrap_in_div IS NOT INITIAL.
      mi_html->add( '</div>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_column_title.

    DATA lv_direction TYPE string.
    DATA lv_arrow TYPE string.
    DATA lv_sort_active TYPE string.

    IF is_col-sortable = abap_true AND ms_sorting_state IS NOT INITIAL.

      IF is_col-column_id = ms_sorting_state-column_id AND ms_sorting_state-descending = abap_false.
        lv_direction = 'dsc'.
      ELSE.
        lv_direction = 'asc'.
      ENDIF.

      IF is_col-column_id = ms_sorting_state-column_id AND ms_sorting_state-descending = abap_true.
        lv_arrow     = '&#x25B4;'. " arrow up
      ELSE.
        lv_arrow     = '&#x25BE;'. " arrow down
      ENDIF.

      IF is_col-column_id = ms_sorting_state-column_id.
        lv_sort_active = | { mv_sort_active_class }|.
      ENDIF.

      rv_text = mi_html->a(
        iv_txt   = is_col-column_title
        iv_act   = |{ c_sort_by_event_prefix }{ is_col-column_id }:{ lv_direction }| ).

      rv_text = rv_text && |<span class="{ mv_sort_span_class }{ lv_sort_active }">{ lv_arrow }</span>|.

    ELSE.
      rv_text = is_col-column_title.
    ENDIF.

  ENDMETHOD.


  METHOD render_row.

    DATA ls_render TYPE zif_abapgit_html_table=>ty_cell_render.
    DATA lv_dummy TYPE string.
    DATA lt_attrs TYPE zif_abapgit_html=>ty_data_attrs.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.
    FIELD-SYMBOLS <ls_grp> LIKE LINE OF mt_columns.
    FIELD-SYMBOLS <lv_val> TYPE any.

    LOOP AT mt_columns ASSIGNING <ls_col>.
      IF <ls_col>-is_group = abap_true.
        ASSIGN <ls_col> TO <ls_grp>.
        CONTINUE.
      ENDIF.

      IF <ls_col>-from_field IS NOT INITIAL AND <ls_col>-from_field <> '-'.
        ASSIGN COMPONENT <ls_col>-from_field OF STRUCTURE is_row TO <lv_val>.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |html_table: cannot assign field [{ <ls_col>-from_field }]| ).
        ENDIF.
      ELSEIF <ls_col>-from_field <> '-'.
        <ls_col>-from_field = to_upper( <ls_col>-column_id ). " Try column_id
        ASSIGN COMPONENT <ls_col>-from_field OF STRUCTURE is_row TO <lv_val>.
        IF sy-subrc <> 0.
          <ls_col>-from_field = '-'. " Don't try assignments anymore
          ASSIGN lv_dummy TO <lv_val>.
        ENDIF.
      ELSE.
        ASSIGN lv_dummy TO <lv_val>.
      ENDIF.

      ls_render = mi_renderer->render_cell(
        iv_table_id  = mv_table_id
        iv_row_index = iv_row_index
        is_row       = is_row
        iv_column_id = <ls_col>-column_id
        iv_value     = |{ <lv_val> }| ).

      IF mv_with_cids = abap_true.
        CLEAR lt_attrs.
        APPEND cid_attr( <ls_col>-column_id ) TO lt_attrs.
        IF <ls_grp> IS ASSIGNED AND <ls_grp>-column_id IS NOT INITIAL.
          APPEND gid_attr( <ls_grp>-column_id ) TO lt_attrs.
        ENDIF.
      ENDIF.

      mi_html->td(
        iv_content = ls_render-content
        ii_content = ls_render-html
        it_data_attrs = lt_attrs
        iv_class   = ls_render-css_class ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_tbody.

    DATA ls_row_attrs TYPE zif_abapgit_html_table=>ty_row_attrs.
    DATA lv_row_attrs TYPE string.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_i> TYPE any.

    mi_html->add( '<tbody>' ).

    LOOP AT it_data ASSIGNING <ls_i>.
      lv_index = sy-tabix.
      ls_row_attrs = mi_renderer->get_row_attrs(
        iv_table_id  = mv_table_id
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      CLEAR lv_row_attrs.
      IF ls_row_attrs-css_class IS NOT INITIAL.
        lv_row_attrs = lv_row_attrs && | class="{ ls_row_attrs-css_class }"|.
      ENDIF.
      IF ls_row_attrs-data IS NOT INITIAL.
        lv_row_attrs = lv_row_attrs && | data-{ ls_row_attrs-data-name }="{ ls_row_attrs-data-value }"|.
      ENDIF.
      mi_html->add( |<tr{ lv_row_attrs }>| ).
      render_row(
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      mi_html->add( '</tr>' ).
    ENDLOOP.

    mi_html->add( '</tbody>' ).

  ENDMETHOD.


  METHOD render_thead.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.
    FIELD-SYMBOLS <ls_grp> LIKE LINE OF mt_columns.
    DATA lt_attrs TYPE zif_abapgit_html=>ty_data_attrs.
    DATA ls_grp_span TYPE string.
    DATA lv_grp_data TYPE string.

    mi_html->add( '<thead>' ).

    " Group headers
    IF mr_last_grp IS NOT INITIAL. " Has groups

      mi_html->add( '<tr>' ).

      LOOP AT mt_columns ASSIGNING <ls_grp> WHERE is_group = abap_true.
        IF mv_with_cids = abap_true AND <ls_grp>-column_id IS NOT INITIAL.
          lv_grp_data = | data-gid="{ <ls_grp>-column_id }"|.
        ELSE.
          CLEAR lv_grp_data.
        ENDIF.

        IF <ls_grp>-group_span > 1.
          ls_grp_span = | colspan="{ <ls_grp>-group_span }"|.
        ELSE.
          CLEAR ls_grp_span.
        ENDIF.

        mi_html->add( |<th{ ls_grp_span }{ lv_grp_data }>{ <ls_grp>-column_title }</th>| ).
      ENDLOOP.

      mi_html->add( '</tr>' ).

    ENDIF.

    " Regular headers
    mi_html->add( '<tr>' ).

    LOOP AT mt_columns ASSIGNING <ls_col>.
      IF <ls_col>-is_group = abap_true.
        ASSIGN <ls_col> TO <ls_grp>.
        CONTINUE.
      ENDIF.

      IF mv_with_cids = abap_true.
        CLEAR lt_attrs.
        APPEND cid_attr( <ls_col>-column_id ) TO lt_attrs.
        IF <ls_grp> IS ASSIGNED AND <ls_grp>-column_id IS NOT INITIAL.
          APPEND gid_attr( <ls_grp>-column_id ) TO lt_attrs.
        ENDIF.
      ENDIF.

      mi_html->th(
        iv_content    = render_column_title( <ls_col> )
        it_data_attrs = lt_attrs ).
    ENDLOOP.

    mi_html->add( '</tr>' ).
    mi_html->add( '</thead>' ).

  ENDMETHOD.
ENDCLASS.
