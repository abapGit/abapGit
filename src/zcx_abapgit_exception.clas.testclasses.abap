CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS:
      test_direct_text FOR TESTING,
      test_t100_text FOR TESTING,
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
      get_t100_text IMPORTING is_message     TYPE gty_t100_message
                    RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_direct_text.
    CONSTANTS: lc_text1 TYPE string VALUE `This is a test error message.`,
               lc_text2 TYPE string VALUE ``.
    DATA: lv_exception_text TYPE string,
          lx_ex             TYPE REF TO zcx_abapgit_exception,
          lx_previous       TYPE REF TO cx_root.

    CREATE OBJECT lx_ex
      EXPORTING
        text = lc_text1.

    lv_exception_text = lx_ex->get_text( ).
    cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lc_text1 ).

    FREE lx_ex.

    CREATE OBJECT lx_ex
      EXPORTING
        text = lc_text2.

    lv_exception_text = lx_ex->get_text( ).
    cl_abap_unit_assert=>assert_equals( act = lv_exception_text
                                        exp = zcx_abapgit_exception=>gc_generic_error_msg ).

    FREE lx_ex.

    CREATE OBJECT lx_previous TYPE cx_sy_dyn_call_illegal_method
      EXPORTING
        textid     = cx_sy_dyn_call_illegal_method=>private_method
        classname  = 'CLASS'
        methodname = 'METHOD'.

    CREATE OBJECT lx_ex
      EXPORTING
        text     = lc_text2
        previous = lx_previous.

    lv_exception_text = lx_ex->get_text( ).
    cl_abap_unit_assert=>assert_equals( act = lv_exception_text
                                        exp = lx_previous->get_text( ) ).

    FREE: lx_ex, lx_previous.

    TRY.
        zcx_abapgit_exception=>raise( lc_text1 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lc_text1 ).
    ENDTRY.

    FREE lx_ex.

    TRY.
        zcx_abapgit_exception=>raise( lc_text2 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text
                                            exp = zcx_abapgit_exception=>gc_generic_error_msg ).
    ENDTRY.

    FREE lx_ex.
  ENDMETHOD.

  METHOD test_no_text.
    DATA: lv_exception_text TYPE string,
          lx_ex             TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT lx_ex.

    lv_exception_text = lx_ex->get_text( ).
    cl_abap_unit_assert=>assert_equals( act = lv_exception_text
                                        exp = zcx_abapgit_exception=>gc_generic_error_msg ).
  ENDMETHOD.

  METHOD test_t100_text.
    CONSTANTS: BEGIN OF lc_msg1,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg1,
               BEGIN OF lc_msg2,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg2,
               BEGIN OF lc_msg3,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg3,
               BEGIN OF lc_msg4,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE'Variable 3',
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg4,
               BEGIN OF lc_msg5,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '001',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE 'Variable 3',
                 msgv4 TYPE symsgv VALUE 'Variable 4',
               END OF lc_msg5,
               BEGIN OF lc_msg6,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '003',
                 msgv1 TYPE symsgv VALUE 'Variable 1',
                 msgv2 TYPE symsgv VALUE 'Variable 2',
                 msgv3 TYPE symsgv VALUE 'Variable 3',
                 msgv4 TYPE symsgv VALUE 'Variable 4',
               END OF lc_msg6,
               BEGIN OF lc_msg7,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '003',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg7,
               BEGIN OF lc_msg8,
                 msgid TYPE symsgid VALUE '00',
                 msgno TYPE symsgno VALUE '002',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg8,
               BEGIN OF lc_msg9,
                 msgid TYPE symsgid VALUE '!"(/&&(%!)"(',
                 msgno TYPE symsgno VALUE '000',
                 msgv1 TYPE symsgv VALUE IS INITIAL,
                 msgv2 TYPE symsgv VALUE IS INITIAL,
                 msgv3 TYPE symsgv VALUE IS INITIAL,
                 msgv4 TYPE symsgv VALUE IS INITIAL,
               END OF lc_msg9.

    DATA: lv_exception_text TYPE string,
          lx_ex             TYPE REF TO zcx_abapgit_exception,
          lv_text           TYPE string.

    TRY.
        lv_text = get_t100_text( lc_msg1 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg2 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg3 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg4 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg5 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg6 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg7 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg8 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text exp = lv_text ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.

    TRY.
        lv_text = get_t100_text( lc_msg9 ).
        zcx_abapgit_exception=>raise_t100( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_ex.
        lv_exception_text = lx_ex->get_text( ).
        cl_abap_unit_assert=>assert_equals( act = lv_exception_text
                                            exp = zcx_abapgit_exception=>gc_generic_error_msg ).
    ENDTRY.

    CLEAR lv_text.
    FREE lx_ex.
  ENDMETHOD.

  METHOD get_t100_text.
    MESSAGE ID is_message-msgid TYPE 'S' NUMBER is_message-msgno
            WITH is_message-msgv1 is_message-msgv2 is_message-msgv3 is_message-msgv4
            INTO rv_text.
  ENDMETHOD.
ENDCLASS.
