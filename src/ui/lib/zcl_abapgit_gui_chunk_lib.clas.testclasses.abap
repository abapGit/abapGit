*"* use this source file for your ABAP unit test classes
CLASS ltcl_normalize_program_name DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_chunk_lib TYPE REF TO zcl_abapgit_gui_chunk_lib.

    METHODS:
      setup,
      class FOR TESTING RAISING cx_static_check,
      program FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS zcl_abapgit_gui_chunk_lib DEFINITION LOCAL FRIENDS ltcl_normalize_program_name.

CLASS ltcl_normalize_program_name IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_chunk_lib.

  ENDMETHOD.


  METHOD class.

    cl_abap_unit_assert=>assert_equals(
      act = mo_chunk_lib->normalize_program_name( 'ZCL_ABAPGIT_FRONTEND_SERVICES=CP' )
      exp = `ZCL_ABAPGIT_FRONTEND_SERVICES` ).

  ENDMETHOD.


  METHOD program.

    cl_abap_unit_assert=>assert_equals(
      act = mo_chunk_lib->normalize_program_name( 'ZABAPGIT_FULL' )
      exp = `ZABAPGIT_FULL` ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_chunk_lib->normalize_program_name( 'ZSOME_PROG_ENDING_WITH_CP' )
      exp = `ZSOME_PROG_ENDING_WITH_CP` ).

  ENDMETHOD.

ENDCLASS.
