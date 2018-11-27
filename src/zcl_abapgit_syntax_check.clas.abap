CLASS zcl_abapgit_syntax_check DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory.


  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_code_inspector.

    METHODS:
      constructor
        IMPORTING
          iv_package TYPE devclass
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      mo_adhoc_code_inspector TYPE REF TO zif_abapgit_code_inspector.

ENDCLASS.



CLASS zcl_abapgit_syntax_check IMPLEMENTATION.


  METHOD constructor.

    mo_adhoc_code_inspector = zcl_abapgit_factory=>get_adhoc_code_inspector(
                                iv_package   = iv_package
                                iv_test_name = 'CL_CI_TEST_SYNTAX_CHECK' ).

  ENDMETHOD.


  METHOD zif_abapgit_code_inspector~get_inspection.

    ro_inspection = mo_adhoc_code_inspector->get_inspection( ).

  ENDMETHOD.


  METHOD zif_abapgit_code_inspector~run.

    rt_list = mo_adhoc_code_inspector->run( ).

  ENDMETHOD.
ENDCLASS.
