
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mi_cut TYPE REF TO zif_abapgit_log.

    METHODS:
      setup,
      from_x FOR TESTING,
      get_status FOR TESTING,
      get_log_level FOR TESTING,
      merge_with FOR TESTING,
      merge_with_min_level FOR TESTING,
      empty FOR TESTING,
      clone FOR TESTING,
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

  METHOD merge_with.

    DATA li_secondary_log LIKE mi_cut.
    DATA lt_act_msgs TYPE zif_abapgit_log=>ty_log_outs.

    CREATE OBJECT li_secondary_log TYPE zcl_abapgit_log.

    mi_cut->add_success( 'success' ).
    li_secondary_log->add_warning( 'warn' ).
    mi_cut->merge_with( li_secondary_log ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 2 ).

    lt_act_msgs = mi_cut->get_messages( ).

    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY text = 'success'.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY text = 'warn'.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD merge_with_min_level.

    DATA li_secondary_log LIKE mi_cut.
    DATA lt_act_msgs TYPE zif_abapgit_log=>ty_log_outs.

    CREATE OBJECT li_secondary_log TYPE zcl_abapgit_log.

    mi_cut->add_success( 'success' ).
    li_secondary_log->add_warning( 'warn' ).
    mi_cut->merge_with(
      ii_log = li_secondary_log
      iv_min_level = zif_abapgit_log=>c_log_level-error ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 1 ).

    lt_act_msgs = mi_cut->get_messages( ).

    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY text = 'success'.
    cl_abap_unit_assert=>assert_subrc( ).

    " change level to warning
    mi_cut->merge_with(
      ii_log = li_secondary_log
      iv_min_level = zif_abapgit_log=>c_log_level-warning ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 2 ).

    lt_act_msgs = mi_cut->get_messages( ).

    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY text = 'success'.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY text = 'warn'.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD from_x.

    DATA lo_x TYPE REF TO zcx_abapgit_exception.
    DATA lt_act_msgs TYPE zif_abapgit_log=>ty_log_outs.

    " Uninitialized
    mi_cut = zcl_abapgit_log=>from_exception( lo_x ).
    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 0 ).

    " Notmal exception
    TRY.
        zcx_abapgit_exception=>raise( 'Error!' ).
      CATCH zcx_abapgit_exception INTO lo_x.
        mi_cut = zcl_abapgit_log=>from_exception( lo_x ).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( mi_cut ).
    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->count( )
      exp = 1 ).

    lt_act_msgs = mi_cut->get_messages( ).

    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_act_msgs TRANSPORTING NO FIELDS WITH KEY text = 'Error!'.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD get_log_level.

    DATA lo_x TYPE REF TO zcx_abapgit_exception.

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_log_level( )
      exp = zif_abapgit_log=>c_log_level-empty ).

    mi_cut->add_success( 'success' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_log_level( )
      exp = zif_abapgit_log=>c_log_level-info ).

    mi_cut->add_warning( 'warn' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_log_level( )
      exp = zif_abapgit_log=>c_log_level-warning ).

    mi_cut->add_error( 'err' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_log_level( )
      exp = zif_abapgit_log=>c_log_level-error ).

    CREATE OBJECT lo_x EXPORTING msgv1 = 'x'.
    mi_cut->add_exception( lo_x ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_cut->get_log_level( )
      exp = zif_abapgit_log=>c_log_level-error ).

  ENDMETHOD.

  METHOD clone.

    DATA li_clone TYPE REF TO zif_abapgit_log.

    mi_cut->add( 'Hello' ).
    mi_cut->set_title( 'My log' ).

    li_clone = mi_cut->clone( ).

    mi_cut->add( 'World' ).
    mi_cut->set_title( 'My log CHANGED' ).

    cl_abap_unit_assert=>assert_equals(
      act = li_clone->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = li_clone->get_title( )
      exp = 'My log' ).

  ENDMETHOD.

ENDCLASS.
