CLASS ltcl_packages DEFINITION
  FOR TESTING
  RISK LEVEL CRITICAL
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_package    TYPE tdevc-devclass VALUE '$TEST$ABAPGIT$',
      c_component  TYPE tdevc-component VALUE 'HLB0009083',  "BC-ABA
      c_comp_posid TYPE scompkdtln-comp_posid VALUE 'BC-ABA'.

    METHODS:
      test_package FOR TESTING,
      teardown.

ENDCLASS.

CLASS ltcl_packages IMPLEMENTATION.

  METHOD test_package.

    DATA lo_packages TYPE REF TO zcl_abapgit_persist_packages.
    DATA ls_package TYPE zcl_abapgit_persist_packages=>ty_package.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    lo_packages = zcl_abapgit_persist_packages=>get_instance( ).

    TRY.
        lo_packages->modify(
          iv_package    = c_package
          iv_component  = c_component
          iv_comp_posid = c_comp_posid ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>fail( msg = lx_error->get_text( ) ).
    ENDTRY.

    TRY.
        ls_package = lo_packages->read( c_package ).

        cl_abap_unit_assert=>assert_equals(
          act = ls_package-component
          exp = c_component ).

        cl_abap_unit_assert=>assert_equals(
          act = ls_package-comp_posid
          exp = c_comp_posid ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>fail( msg = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD teardown.

    DATA lo_packages TYPE REF TO zcl_abapgit_persist_packages.

    lo_packages = zcl_abapgit_persist_packages=>get_instance( ).

    " Remove test data
    TRY.
        lo_packages->modify( c_package ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
