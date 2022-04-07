CLASS ltcl_abapgit_syntax_json DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
     mo_cut TYPE REF TO zcl_abapgit_syntax_json.

    METHODS:
      setup,
      key_value FOR TESTING RAISING cx_static_check,
      comment_1 FOR TESTING RAISING cx_static_check,
      comment_2 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_abapgit_syntax_json IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD key_value.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( |"key":"value"| )
      exp = |<span class="text">"key"</span>:<span class="properties">"value"</span>| ).
  ENDMETHOD.

  METHOD comment_1.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( |"key":"value" // comment| )
      exp = |<span class="text">"key"</span>:<span class="properties">"value"</span>|
         && | <span class="comment">//</span> comment| ).
  ENDMETHOD.

  METHOD comment_2.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( |/* comment */| )
      exp = |<span class="comment">/* comment */</span>| ).
  ENDMETHOD.

ENDCLASS.
