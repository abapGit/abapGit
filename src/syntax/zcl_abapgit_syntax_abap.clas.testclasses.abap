CLASS ltcl_abapgit_syntax_abap DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
     mo_cut TYPE REF TO zcl_abapgit_syntax_abap.

    METHODS:
      setup,
      report_header FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_abapgit_syntax_abap IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD report_header.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="keyword">REPORT</span> zfoo.|
      act = mo_cut->process_line( |REPORT zfoo.| ) ).

  ENDMETHOD.


ENDCLASS.
