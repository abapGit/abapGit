CLASS zcl_abapgit_html_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !ii_renderer    TYPE REF TO zif_abapgit_html_table
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_html_table .
    " probably th css_class
    " maybe auto class for td
    METHODS define_column
      IMPORTING
        !iv_column_id  TYPE string
        !iv_column_title TYPE string OPTIONAL
        !iv_from_field TYPE abap_compname OPTIONAL
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_abapgit_html_table .
    " Maybe also data_provider
    " Record Limit
    METHODS render
      IMPORTING
        !iv_id         TYPE csequence OPTIONAL
        !iv_css_class  TYPE csequence OPTIONAL
        !it_data       TYPE ANY TABLE
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_column,
        column_id TYPE string,
        column_title TYPE string,
        from_field  TYPE abap_compname,
      END OF ty_column,
      ty_columns TYPE STANDARD TABLE OF ty_column WITH KEY column_id.


    DATA mi_renderer TYPE REF TO zif_abapgit_html_table.
    DATA mt_columns TYPE ty_columns.
    DATA mi_html TYPE REF TO zif_abapgit_html.

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

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_TABLE IMPLEMENTATION.


  METHOD create.
    ASSERT ii_renderer IS BOUND.
    CREATE OBJECT ro_instance.
    ro_instance->mi_renderer = ii_renderer.
  ENDMETHOD.


  METHOD define_column.

    FIELD-SYMBOLS <ls_c> LIKE LINE OF mt_columns.

    ASSERT iv_column_id IS NOT INITIAL.
    ro_self = me.

    APPEND INITIAL LINE TO mt_columns ASSIGNING <ls_c>.
    <ls_c>-column_id    = iv_column_id.
    <ls_c>-column_title = iv_column_title.
    <ls_c>-from_field   = to_upper( iv_from_field ).

  ENDMETHOD.


  METHOD render.

    DATA lv_attrs TYPE string.

    IF iv_id IS NOT INITIAL.
      lv_attrs = lv_attrs && | id="{ iv_id }"|.
    ENDIF.

    IF iv_css_class IS NOT INITIAL.
      lv_attrs = lv_attrs && | class="{ iv_css_class }"|.
    ENDIF.

    CREATE OBJECT mi_html TYPE zcl_abapgit_html.
    ri_html = mi_html.

    mi_html->add( |<table{ lv_attrs }>| ).
    render_thead( ).
    render_tbody( it_data ).
    mi_html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_row.

    DATA ls_render TYPE zif_abapgit_html_table=>ty_cell_render.
    DATA lv_dummy TYPE string.
    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.
    FIELD-SYMBOLS <lv_val> TYPE any.

    LOOP AT mt_columns ASSIGNING <ls_col>.
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
        iv_row_index = iv_row_index
        is_row       = is_row
        iv_column_id = <ls_col>-column_id
        iv_value     = <lv_val> ).
      mi_html->td(
        iv_content = ls_render-content
        ii_content = ls_render-html
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
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      CLEAR lv_row_attrs.
      IF ls_row_attrs-css_class IS NOT INITIAL.
        lv_row_attrs = lv_row_attrs && | class="{ ls_row_attrs-css_class }"|.
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

    mi_html->add( '<thead>' ).
    mi_html->add( '<tr>' ).

    LOOP AT mt_columns ASSIGNING <ls_col>.
      mi_html->th( iv_content = <ls_col>-column_title ).
    ENDLOOP.

    mi_html->add( '</tr>' ).
    mi_html->add( '</thead>' ).

  ENDMETHOD.
ENDCLASS.
