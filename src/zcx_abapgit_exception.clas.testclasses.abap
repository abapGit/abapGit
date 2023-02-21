CLASS ltcl_split_text DEFINITION DEFERRED.
CLASS ltcl_base DEFINITION DEFERRED.
CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS ltcl_split_text ltcl_base.

CLASS ltcl_base DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT ABSTRACT.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_t100_text IMPORTING is_message     TYPE symsg
                    RETURNING VALUE(rv_text) TYPE string.
  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF gs_test_data,
        text       TYPE string VALUE `This is a test error message.`,
        empty_text TYPE string VALUE ``,
      END OF gs_test_data.
    DATA:
      mo_cut TYPE REF TO zcx_abapgit_exception,
      BEGIN OF ms_given,
        text               TYPE string,
        t100_message       TYPE symsg,
        longtext           TYPE string,
        previous_exception TYPE REF TO cx_root,
      END OF ms_given.
    METHODS:
      given_the_text IMPORTING iv_text TYPE csequence,
      given_the_longtext IMPORTING iv_longtext TYPE csequence,
      given_the_t100_message IMPORTING is_message TYPE symsg,
      given_the_previous_exception IMPORTING ix_previous_exception TYPE REF TO cx_root,
      when_instantiated_using_raise,
      when_instan_using_raise_t100,
      when_inst_usng_raise_with_text,
      then_the_text_should_equal IMPORTING iv_text TYPE csequence,
      then_the_longtext_should_equal IMPORTING iv_longtext TYPE csequence,
      then_the_prev_exc_should_equal IMPORTING ix_exception TYPE REF TO cx_root.
  PRIVATE SECTION.
    METHODS:
      teardown.
ENDCLASS.

CLASS ltcl_base IMPLEMENTATION.
  METHOD get_t100_text.
    MESSAGE ID is_message-msgid TYPE 'S' NUMBER is_message-msgno
            WITH is_message-msgv1 is_message-msgv2 is_message-msgv3 is_message-msgv4
            INTO rv_text.
  ENDMETHOD.

  METHOD given_the_text.
    ms_given-text = iv_text.
  ENDMETHOD.

  METHOD given_the_longtext.
    ms_given-longtext = iv_longtext.
  ENDMETHOD.

  METHOD given_the_t100_message.
    ms_given-t100_message = is_message.
  ENDMETHOD.

  METHOD given_the_previous_exception.
    ms_given-previous_exception = ix_previous_exception.
  ENDMETHOD.

  METHOD when_instantiated_using_raise.
    TRY.
        zcx_abapgit_exception=>raise( iv_text     = ms_given-text
                                      ix_previous = ms_given-previous_exception
                                      iv_longtext = ms_given-longtext ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO mo_cut ##NEEDED.
    ENDTRY.
  ENDMETHOD.

  METHOD when_instan_using_raise_t100.
    TRY.
        zcx_abapgit_exception=>raise_t100( iv_msgid    = ms_given-t100_message-msgid
                                           iv_msgno    = ms_given-t100_message-msgno
                                           iv_msgv1    = ms_given-t100_message-msgv1
                                           iv_msgv2    = ms_given-t100_message-msgv2
                                           iv_msgv3    = ms_given-t100_message-msgv3
                                           iv_msgv4    = ms_given-t100_message-msgv4
                                           ix_previous = ms_given-previous_exception
                                           iv_longtext = ms_given-longtext ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO mo_cut ##NEEDED.
    ENDTRY.
  ENDMETHOD.

  METHOD when_inst_usng_raise_with_text.
    TRY.
        zcx_abapgit_exception=>raise_with_text( ix_previous = ms_given-previous_exception
                                                iv_longtext = ms_given-longtext ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO mo_cut ##NEEDED.
    ENDTRY.
  ENDMETHOD.

  METHOD then_the_text_should_equal.
    cl_abap_unit_assert=>assert_equals( exp = iv_text
                                        act = mo_cut->get_text( ) ).
  ENDMETHOD.

  METHOD then_the_longtext_should_equal.
    cl_abap_unit_assert=>assert_equals( exp = iv_longtext
                                        act = mo_cut->get_longtext( abap_true ) ).
  ENDMETHOD.

  METHOD then_the_prev_exc_should_equal.
    cl_abap_unit_assert=>assert_equals( exp = ix_exception
                                        act = mo_cut->previous ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR ms_given.
    FREE mo_cut.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_general DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS INHERITING FROM ltcl_base.
  PUBLIC SECTION.
    METHODS:
      direct_text FOR TESTING,
      no_text FOR TESTING,
      text_from_exception FOR TESTING.
ENDCLASS.

CLASS ltcl_general IMPLEMENTATION.
  METHOD direct_text.
    given_the_text( gs_test_data-text ).
    when_instantiated_using_raise( ).
    then_the_text_should_equal( gs_test_data-text ).
  ENDMETHOD.

  METHOD no_text.
    given_the_text( gs_test_data-empty_text ).
    when_instantiated_using_raise( ).
    then_the_text_should_equal( zcx_abapgit_exception=>c_generic_error_msg ).
  ENDMETHOD.

  METHOD text_from_exception.
    DATA: lx_previous TYPE REF TO cx_sy_dyn_call_illegal_method.

    CREATE OBJECT lx_previous TYPE cx_sy_dyn_call_illegal_method
      EXPORTING
        textid     = cx_sy_dyn_call_illegal_method=>private_method
        classname  = 'CLASS'
        methodname = 'METHOD'.

    given_the_previous_exception( lx_previous ).

    when_inst_usng_raise_with_text( ).

    then_the_text_should_equal( lx_previous->get_text( ) ).
    then_the_prev_exc_should_equal( lx_previous ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_t100 DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS INHERITING FROM ltcl_base.
  PUBLIC SECTION.
    METHODS:
      text1 FOR TESTING,
      text2 FOR TESTING,
      text3 FOR TESTING,
      text4 FOR TESTING,
      text5 FOR TESTING,
      text6 FOR TESTING,
      text7 FOR TESTING,
      text8 FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      BEGIN OF gs_t100_test_data,
        message1 TYPE symsg,
        message2 TYPE symsg,
        message3 TYPE symsg,
        message4 TYPE symsg,
        message5 TYPE symsg,
        message6 TYPE symsg,
        message7 TYPE symsg,
        message8 TYPE symsg,
      END OF gs_t100_test_data.
    CLASS-METHODS:
      class_setup.
ENDCLASS.

CLASS ltcl_t100 IMPLEMENTATION.
  METHOD text1.
    given_the_t100_message( gs_t100_test_data-message1 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message1 ) ).
  ENDMETHOD.

  METHOD text2.
    given_the_t100_message( gs_t100_test_data-message2 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message2 ) ).
  ENDMETHOD.

  METHOD text3.
    given_the_t100_message( gs_t100_test_data-message3 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message3 ) ).
  ENDMETHOD.

  METHOD text4.
    given_the_t100_message( gs_t100_test_data-message4 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message4 ) ).
  ENDMETHOD.

  METHOD text5.
    given_the_t100_message( gs_t100_test_data-message5 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message5 ) ).
  ENDMETHOD.

  METHOD text6.
    given_the_t100_message( gs_t100_test_data-message6 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message6 ) ).
  ENDMETHOD.

  METHOD text7.
    given_the_t100_message( gs_t100_test_data-message7 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message7 ) ).
  ENDMETHOD.

  METHOD text8.
    given_the_t100_message( gs_t100_test_data-message8 ).
    when_instan_using_raise_t100( ).
    then_the_text_should_equal( get_t100_text( gs_t100_test_data-message8 ) ).
  ENDMETHOD.

  METHOD class_setup.
    gs_t100_test_data-message1-msgid = '00'.
    gs_t100_test_data-message1-msgno = '001'.

    gs_t100_test_data-message2-msgid = '00'.
    gs_t100_test_data-message2-msgno = '001'.
    gs_t100_test_data-message2-msgv1 = 'Variable 1'.

    gs_t100_test_data-message3-msgid = '00'.
    gs_t100_test_data-message3-msgno = '001'.
    gs_t100_test_data-message3-msgv1 = 'Variable 1'.
    gs_t100_test_data-message3-msgv2 = 'Variable 2'.

    gs_t100_test_data-message4-msgid = '00'.
    gs_t100_test_data-message4-msgno = '001'.
    gs_t100_test_data-message4-msgv1 = 'Variable 1'.
    gs_t100_test_data-message4-msgv2 = 'Variable 2'.
    gs_t100_test_data-message4-msgv3 = 'Variable 3'.

    gs_t100_test_data-message5-msgid = '00'.
    gs_t100_test_data-message5-msgno = '001'.
    gs_t100_test_data-message5-msgv1 = 'Variable 1'.
    gs_t100_test_data-message5-msgv2 = 'Variable 2'.
    gs_t100_test_data-message5-msgv3 = 'Variable 3'.
    gs_t100_test_data-message5-msgv4 = 'Variable 4'.

    gs_t100_test_data-message6-msgid = '00'.
    gs_t100_test_data-message6-msgno = '003'.
    gs_t100_test_data-message6-msgv1 = 'Variable 1'.
    gs_t100_test_data-message6-msgv2 = 'Variable 2'.
    gs_t100_test_data-message6-msgv3 = 'Variable 3'.
    gs_t100_test_data-message6-msgv4 = 'Variable 4'.

    gs_t100_test_data-message7-msgid = '00'.
    gs_t100_test_data-message7-msgno = '003'.

    gs_t100_test_data-message8-msgid = '00'.
    gs_t100_test_data-message8-msgno = '002'.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_get_t100_longtext DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test01 FOR TESTING.

ENDCLASS.

CLASS ltcl_get_t100_longtext IMPLEMENTATION.

  METHOD test01.

    DATA: lx_err    TYPE REF TO zcx_abapgit_exception,
          lv_result TYPE string.

    TRY.
        MESSAGE e058(00) WITH 'Value_1' 'Value_2' 'Value_3' 'Value_4' INTO zcx_abapgit_exception=>null.
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

CLASS ltcl_longtext DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT INHERITING FROM ltcl_base.
  PUBLIC SECTION.
    METHODS:
      empty_longtext FOR TESTING,
      longtext FOR TESTING,
      multiline_longtext FOR TESTING,
      t100_longtext_override FOR TESTING,
      text_from_previous_exception FOR TESTING.
  PRIVATE SECTION.
    CLASS-DATA:
      BEGIN OF gs_longtext_test_data,
        longtext_500          TYPE string,
        longtext_500_multline TYPE string,
        empty_longtext        TYPE string VALUE ``,
        t100_with_longtext    TYPE symsg,
      END OF gs_longtext_test_data.
    CLASS-METHODS:
      class_setup.
ENDCLASS.

CLASS ltcl_longtext IMPLEMENTATION.
  METHOD empty_longtext.
    given_the_text( gs_test_data-text ).
    given_the_longtext( gs_longtext_test_data-empty_longtext ).

    when_instantiated_using_raise( ).

    then_the_text_should_equal( gs_test_data-text ).
    then_the_longtext_should_equal( gs_longtext_test_data-empty_longtext ).
  ENDMETHOD.

  METHOD longtext.
    given_the_text( gs_test_data-text ).
    given_the_longtext( gs_longtext_test_data-longtext_500 ).

    when_instantiated_using_raise( ).

    then_the_text_should_equal( gs_test_data-text ).
    then_the_longtext_should_equal( gs_longtext_test_data-longtext_500 ).
  ENDMETHOD.

  METHOD multiline_longtext.
    given_the_text( gs_test_data-text ).
    given_the_longtext( gs_longtext_test_data-longtext_500_multline ).

    when_instantiated_using_raise( ).

    then_the_text_should_equal( gs_test_data-text ).
    then_the_longtext_should_equal( gs_longtext_test_data-longtext_500_multline ).
  ENDMETHOD.

  METHOD t100_longtext_override.
    given_the_longtext( gs_longtext_test_data-longtext_500 ).
    given_the_t100_message( gs_longtext_test_data-t100_with_longtext ).

    when_instan_using_raise_t100( ).

    then_the_longtext_should_equal( gs_longtext_test_data-longtext_500 ).
  ENDMETHOD.

  METHOD text_from_previous_exception.
    DATA: lx_previous TYPE REF TO cx_sy_dyn_call_illegal_method.

    CREATE OBJECT lx_previous.

    given_the_previous_exception( lx_previous ).
    given_the_longtext( gs_longtext_test_data-longtext_500 ).

    when_inst_usng_raise_with_text( ).

    then_the_text_should_equal( condense( lx_previous->get_text( ) ) ).
    then_the_longtext_should_equal( gs_longtext_test_data-longtext_500 ).
    then_the_prev_exc_should_equal( lx_previous ).
  ENDMETHOD.

  METHOD class_setup.
    gs_longtext_test_data-longtext_500 =
      `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et` &&
      ` dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Ste` &&
      `t clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, ` &&
      `consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,` &&
      ` sed diam voluptua. At vero eos et accusam et justo duo dolores et e`.

    gs_longtext_test_data-longtext_500_multline =
      `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et` &&
      ` dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.` &&
      cl_abap_char_utilities=>newline &&
      `Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.` &&
      cl_abap_char_utilities=>newline &&
      cl_abap_char_utilities=>newline &&
      `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et` &&
      ` dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores ete`.

    gs_longtext_test_data-t100_with_longtext-msgid = '00'.
    gs_longtext_test_data-t100_with_longtext-msgno = '002'.
  ENDMETHOD.

ENDCLASS.
