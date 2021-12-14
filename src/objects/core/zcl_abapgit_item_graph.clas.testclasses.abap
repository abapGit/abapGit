CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS basic FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD basic.

    DATA lo_graph TYPE REF TO zcl_abapgit_item_graph.
    DATA li_log   TYPE REF TO zif_abapgit_log.
    DATA lt_items TYPE zif_abapgit_definitions=>ty_items_tt.
    DATA ls_item1 LIKE LINE OF lt_items.
    DATA ls_item2 LIKE LINE OF lt_items.
    DATA ls_next LIKE LINE OF lt_items.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    ls_item1-obj_type = 'TYPE'.
    ls_item1-obj_type = '1111'.
    APPEND ls_item1 TO lt_items.

    ls_item2-obj_type = 'TYPE'.
    ls_item2-obj_type = '2222'.
    APPEND ls_item2 TO lt_items.

    CREATE OBJECT lo_graph EXPORTING it_items = lt_items.

    lo_graph->add_edge(
      is_from = ls_item1
      is_to   = ls_item2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_graph->has_vertices( )
      exp = abap_true ).

    ls_next = lo_graph->get_next( li_log ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_next-obj_name
      exp = ls_item1-obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_graph->has_vertices( )
      exp = abap_true ).

    ls_next = lo_graph->get_next( li_log ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_next-obj_name
      exp = ls_item2-obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_graph->has_vertices( )
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.
