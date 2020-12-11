CLASS ltcl_test DEFINITION DEFERRED.
CLASS ltcl_split_text DEFINITION DEFERRED.
CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS ltcl_test ltcl_split_text.

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
      BEGIN OF ty_t100_message,
        msgid TYPE symsgid,
        msgno TYPE symsgno,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ty_t100_message.
    CLASS-METHODS:
      get_exc_text IMPORTING ix_ex          TYPE REF TO cx_root
                   RETURNING VALUE(rv_text) TYPE string,
      get_t100_text IMPORTING is_message     TYPE ty_t100_message
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
    rv_text = ix_ex->get_text( ).
  ENDMETHOD.

  METHOD get_t100_text.
    MESSAGE ID is_message-msgid TYPE 'S' NUMBER is_message-msgno
            WITH is_message-msgv1 is_message-msgv2 is_message-msgv3 is_message-msgv4
            INTO rv_text.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_get_t100_longtext DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test01 FOR TESTING.

ENDCLASS.

CLASS ltcl_get_t100_longtext IMPLEMENTATION.

  METHOD test01.

    DATA: lx_err    TYPE REF TO zcx_abapgit_exception,
          lv_dummy  TYPE string,
          lv_result TYPE string.

    TRY.
        MESSAGE e058(00) WITH 'Value_1' 'Value_2' 'Value_3' 'Value_4' INTO lv_dummy.
        zcx_abapgit_exception=>raise_t100( ).
      CATCH zcx_abapgit_exception INTO lx_err.
        lv_result = lx_err->get_longtext( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_initial( lv_result ).

    IF lv_result NS 'Value_1'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    IF lv_result NS 'Value_2'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    IF lv_result NS 'Value_3'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    IF lv_result NS 'Value_4'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_split_text DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS:
      test_set_msg_b FOR TESTING,
      test_set_msg_c FOR TESTING,
      test_set_msg_d FOR TESTING,
      test_set_msg_e FOR TESTING,
      test_set_msg_z FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_msg,
        v1 TYPE sy-msgv1,
        v2 TYPE sy-msgv2,
        v3 TYPE sy-msgv3,
        v4 TYPE sy-msgv4,
      END OF ty_msg.
    CLASS-METHODS:
      test_set_msg_vars
        IMPORTING
          iv_text TYPE string
          is_msg  TYPE ty_msg.
ENDCLASS.

CLASS ltcl_split_text IMPLEMENTATION.
  METHOD test_set_msg_b.
    CONSTANTS:
      lc_text_b TYPE string VALUE '0123456789012345678901234567890123456789012345678 0123456789',
      BEGIN OF lc_msg_b,
        v1 TYPE sy-msgv1 VALUE '0123456789012345678901234567890123456789012345678',
        v2 TYPE sy-msgv2 VALUE ' 0123456789',
        v3 TYPE sy-msgv3 VALUE '',
        v4 TYPE sy-msgv4 VALUE '',
      END OF lc_msg_b.
    test_set_msg_vars( iv_text = lc_text_b
                       is_msg  = lc_msg_b ).
  ENDMETHOD.
  METHOD test_set_msg_c.
    CONSTANTS:
      lc_text_c TYPE string VALUE '01234567890123456789012345678901234567890123456789 123456789',
      BEGIN OF lc_msg_c,
        v1 TYPE sy-msgv1 VALUE '01234567890123456789012345678901234567890123456789',
        v2 TYPE sy-msgv2 VALUE ' 123456789',
        v3 TYPE sy-msgv3 VALUE '',
        v4 TYPE sy-msgv4 VALUE '',
      END OF lc_msg_c.
    test_set_msg_vars( iv_text = lc_text_c
                       is_msg  = lc_msg_c ).
  ENDMETHOD.
  METHOD test_set_msg_d.
    CONSTANTS:
      lc_text_d TYPE string VALUE '012345678901234567890123456789012345678901234567890 23456789',
      BEGIN OF lc_msg_d,
        v1 TYPE sy-msgv1 VALUE '01234567890123456789012345678901234567890123456789',
        v2 TYPE sy-msgv2 VALUE '0 23456789',
        v3 TYPE sy-msgv3 VALUE '',
        v4 TYPE sy-msgv4 VALUE '',
      END OF lc_msg_d.
    test_set_msg_vars( iv_text = lc_text_d
                       is_msg  = lc_msg_d ).
  ENDMETHOD.
  METHOD test_set_msg_e.
    CONSTANTS:
      lc_text_e TYPE string VALUE '012345678901234567890123456789012345678901234567890123456789',
      BEGIN OF lc_msg_e,
        v1 TYPE sy-msgv1 VALUE '01234567890123456789012345678901234567890123456789',
        v2 TYPE sy-msgv2 VALUE '0123456789',
        v3 TYPE sy-msgv3 VALUE '',
        v4 TYPE sy-msgv4 VALUE '',
      END OF lc_msg_e.
    test_set_msg_vars( iv_text = lc_text_e
                       is_msg  = lc_msg_e ).
  ENDMETHOD.
  METHOD test_set_msg_z.
    CONSTANTS:
      lc_text_x TYPE string VALUE `Here is a very long text more than 200 characters and we have to invent `,
      lc_text_y TYPE string VALUE `a nice story about abapGit to fill this long message. abapGit is simply `,
      lc_text_z TYPE string VALUE `the greatest! #abapGit #awesome #community #opensource`,
      BEGIN OF lc_msg_z,
        v1 TYPE sy-msgv1 VALUE `Here is a very long text more than 200 characters`,
        v2 TYPE sy-msgv2 VALUE ` and we have to invent a nice story about abapGit`,
        v3 TYPE sy-msgv3 VALUE ` to fill this long message. abapGit is simply the`,
        v4 TYPE sy-msgv4 VALUE ` greatest! #abapGit #awesome #community`,
      END OF lc_msg_z.
    test_set_msg_vars( iv_text = lc_text_x && lc_text_y && lc_text_z
                       is_msg  = lc_msg_z ).
  ENDMETHOD.
  METHOD test_set_msg_vars.

    zcx_abapgit_exception=>split_text_to_symsg( iv_text ).

    cl_abap_unit_assert=>assert_equals( act = sy-msgv1
                                        exp = is_msg-v1 ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgv2
                                        exp = is_msg-v2 ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgv3
                                        exp = is_msg-v3 ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgv4
                                        exp = is_msg-v4 ).

  ENDMETHOD.
ENDCLASS.
