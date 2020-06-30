CLASS ltcl_test DEFINITION
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

CLASS ltcl_test IMPLEMENTATION.
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

    zcl_abapgit_message_helper=>set_msg_vars_for_clike( iv_text ).

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
