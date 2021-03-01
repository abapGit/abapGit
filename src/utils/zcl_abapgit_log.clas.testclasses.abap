
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mi_cut TYPE REF TO zif_abapgit_log.

    METHODS:
      setup,
      get_status FOR TESTING,
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

  METHOD get_status.

    DATA lo_x TYPE REF TO zcx_abapgit_exception.

    mi_cut->add_success( 'success' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_status( )
      exp = zif_abapgit_log=>c_status-ok ).

    mi_cut->add_warning( 'warn' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_status( )
      exp = zif_abapgit_log=>c_status-warning ).

    mi_cut->add_error( 'err' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_status( )
      exp = zif_abapgit_log=>c_status-error ).

    mi_cut->clear( ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_status( )
      exp = zif_abapgit_log=>c_status-ok ).

    CREATE OBJECT lo_x EXPORTING msgv1 = 'x'.
    mi_cut->add_exception( lo_x ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_status( )
      exp = zif_abapgit_log=>c_status-error ).

  ENDMETHOD.

ENDCLASS.
