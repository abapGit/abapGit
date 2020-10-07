CLASS ltcl_part_collections DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PUBLIC SECTION.

  PRIVATE SECTION.

    METHODS test FOR TESTING.

ENDCLASS.

CLASS ltcl_part_collections IMPLEMENTATION.

  METHOD test.

    DATA lo_html1 TYPE REF TO zcl_abapgit_html.
    DATA lo_html2 TYPE REF TO zcl_abapgit_html.
    DATA lo_html3 TYPE REF TO zcl_abapgit_html.
    DATA lo_html_tmp TYPE REF TO zif_abapgit_html.
    DATA lo_parts TYPE REF TO zcl_abapgit_html_parts.
    DATA lt_col_exp TYPE string_table.
    DATA lt_parts_act TYPE zif_abapgit_html=>ty_table_of.

    CREATE OBJECT lo_html1.
    CREATE OBJECT lo_html2.
    CREATE OBJECT lo_html3.

    CREATE OBJECT lo_parts.
    lo_parts->add_part(
      iv_collection = 'ABC'
      ii_part = lo_html1 ).
    lo_parts->add_part(
      iv_collection = 'ABC'
      ii_part = lo_html2 ).
    lo_parts->add_part(
      iv_collection = 'XYZ'
      ii_part = lo_html3 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_parts->get_collection_size( 'ABC' )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_parts->get_collection_size( 'XYZ' )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_parts->get_collection_size( '123' )
      exp = 0 ).

    APPEND 'ABC' TO lt_col_exp.
    APPEND 'XYZ' TO lt_col_exp.

    cl_abap_unit_assert=>assert_equals(
      act = lo_parts->get_collection_names( )
      exp = lt_col_exp ).

    lt_parts_act = lo_parts->get_parts( 'ABC' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_parts_act )
      exp = 2 ).
    READ TABLE lt_parts_act INTO lo_html_tmp INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = lo_html_tmp
      exp = lo_html1 ).
    READ TABLE lt_parts_act INTO lo_html_tmp INDEX 2.
    cl_abap_unit_assert=>assert_equals(
      act = lo_html_tmp
      exp = lo_html2 ).

    lt_parts_act = lo_parts->get_parts( 'XYZ' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_parts_act )
      exp = 1 ).
    READ TABLE lt_parts_act INTO lo_html_tmp INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = lo_html_tmp
      exp = lo_html3 ).

    lt_parts_act = lo_parts->get_parts( '123' ).
    cl_abap_unit_assert=>assert_initial( lt_parts_act ).

    lo_parts->clear( ).
    cl_abap_unit_assert=>assert_initial( lo_parts->get_collection_names( ) ).

  ENDMETHOD.

ENDCLASS.
