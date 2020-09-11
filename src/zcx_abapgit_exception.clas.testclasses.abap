CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS:
      test_direct_text FOR TESTING,
      test_t100_text1 FOR TESTING,
      test_t100_text2 FOR TESTING,
      test_t100_text3 FOR TESTING,
      test_t100_text4 FOR TESTING,
      test_t100_text5 FOR TESTING,
      test_t100_text6 FOR TESTING,
      test_t100_text7 FOR TESTING,
      test_t100_text8 FOR TESTING,
      test_no_text FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_t100_message,
        msgid TYPE symsgid,
        msgno TYPE symsgno,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF gty_t100_message.
    CLASS-METHODS:
      get_exc_text IMPORTING ix_ex          TYPE REF TO cx_root
                   RETURNING VALUE(rv_text) TYPE string,
      get_t100_text IMPORTING is_message     TYPE gty_t100_message
                    RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_direct_text.
    CONSTANTS: lc_text1 TYPE string VALUE `This is a test error message.`,
               lc_text2 TYPE string VALUE ``.
    DATA: lx_ex       TYPE REF TO zcx_abapgit_exception,
          lx_previous TYPE REF TO cx_root.

    TRY.
        CREATE OBJECT lx_previous TYPE cx_sy_dyn_call_illegal_method
          EXPORTING
            textid     = cx_sy_dyn_call_illegal_method=>private_method
            classname  = 'CLASS'
            methodname = 'METHOD'.

        zcx_abapgit_exception=>raise( iv_text     = lx_previous->get_text( )
                                      ix_previous = lx_previous ).
        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lx_previous->get_text( ) ).
        cl_abap_unit_assert=>assert_equals( act = lx_ex->previous
                                            exp = lx_previous ).
    ENDTRY.

    FREE: lx_ex, lx_previous.

    TRY.
        zcx_abapgit_exception=>raise( lc_text1 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lc_text1 ).
    ENDTRY.

    FREE lx_ex.

    TRY.
        zcx_abapgit_exception=>raise( lc_text2 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals(
          act = get_exc_text( lx_ex )
          exp = zcx_abapgit_exception=>gc_generic_error_msg ).
    ENDTRY.

    FREE lx_ex.
  ENDMETHOD.

  METHOD test_no_text.
    DATA: lx_ex TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcx_abapgit_exception=>raise( space ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals(
          act = get_exc_text( lx_ex )
          exp = zcx_abapgit_exception=>gc_generic_error_msg ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_t100_text1.
    CONSTANTS: BEGIN OF lc_msg1,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg1.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg1 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.
  ENDMETHOD.


  METHOD test_t100_text2.
    CONSTANTS: BEGIN OF lc_msg2,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg2.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg2 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_t100_text3.
    CONSTANTS: BEGIN OF lc_msg3,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg3.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg3 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_t100_text4.
    CONSTANTS: BEGIN OF lc_msg4,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE 'Variable 3',
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg4.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg4 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_t100_text5.
    CONSTANTS: BEGIN OF lc_msg5,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE 'Variable 3',
                 msgv4 TYPE symsgv VALUE 'Variable 4',
               END OF lc_msg5.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg5 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_t100_text6.
    CONSTANTS: BEGIN OF lc_msg6,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '003',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE 'Variable 3',
                 msgv4 TYPE symsgv VALUE 'Variable 4',
               END OF lc_msg6.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg6 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_t100_text7.
    CONSTANTS: BEGIN OF lc_msg7,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '003',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg7.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg7 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_t100_text8.
    CONSTANTS: BEGIN OF lc_msg8,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '002',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg8.

    DATA: lx_ex   TYPE REF TO zcx_abapgit_exception,
          lv_text TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg8 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        cl_abap_unit_assert=>assert_equals( act = get_exc_text( lx_ex )
                                            exp = lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_exc_text.
    cl_message_helper=>set_msg_vars_for_if_msg( ix_ex ).
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO rv_text.
  ENDMETHOD.

  METHOD get_t100_text.
    MESSAGE ID is_message-msgid TYPE 'S' NUMBER is_message-msgno
            WITH is_message-msgv1 is_message-msgv2 is_message-msgv3 is_message-msgv4
            INTO rv_text.
  ENDMETHOD.
ENDCLASS.
