*"* use this source file for your ABAP unit test classes
class ltcl DEFINITION for TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    methods sotr_wda_0001 for TESTING.
    methods sotr_cx_0002 for TESTING.
endclass.

class ltcl IMPLEMENTATION.
  method sotr_wda_0001.
    data lt_sotr TYPE ZIF_ABAPGIT_DEFINITIONS=>TY_SOTR_TT.
    lt_sotr = zcl_abapgit_sotr_handler=>read_sotr_wda( iv_object_name = 'ZWD_ABAPGIT_TEST_SOTR' ).
    cl_aunit_assert=>assert_not_initial( lt_sotr ).
  endmethod.
  method sotr_cx_0002.
    data lt_sotr TYPE ZIF_ABAPGIT_DEFINITIONS=>TY_SOTR_TT.
    lt_sotr = zcl_abapgit_sotr_handler=>read_sotr_seocomp( iv_object_name = 'CX_ABAP_INVALID_NAME' ).
    cl_aunit_assert=>assert_not_initial( lt_sotr ).
  endmethod.
endclass.
