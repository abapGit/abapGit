
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mi_cut TYPE REF TO zif_abapgit_log.

    METHODS:
      setup,
      empty FOR TESTING,
      add FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mi_cut TYPE zcl_abapgit_log.
  ENDMETHOD.

  METHOD empty.

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_title( )
      exp = 'Log' ).

  ENDMETHOD.

  METHOD add.

    DATA lv_message TYPE string.
    DATA lt_messages TYPE zif_abapgit_log=>ty_log_outs.
    DATA ls_message LIKE LINE OF lt_messages.

    lv_message = 'hello'.

    mi_cut->add( lv_message ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_status( )
      exp = 'E' ).

    lt_messages = mi_cut->get_messages( ).
    READ TABLE lt_messages INDEX 1 INTO ls_message.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_message-text
      exp = lv_message ).

  ENDMETHOD.

ENDCLASS.
