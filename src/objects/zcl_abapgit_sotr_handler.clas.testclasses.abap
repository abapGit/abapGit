*"* use this source file for your ABAP unit test classes
CLASS ltcl DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS sotr_wda_0001 FOR TESTING.
    METHODS sotr_cx_0002 FOR TESTING.
ENDCLASS.

CLASS ltcl IMPLEMENTATION.
  METHOD sotr_wda_0001.
    DATA lt_sotr TYPE zif_abapgit_definitions=>ty_sotr_tt.
    TRY.
        lt_sotr = zcl_abapgit_sotr_handler=>read_sotr_wda( iv_object_name = 'ZWD_ABAPGIT_TEST_SOTR' ).
        cl_aunit_assert=>assert_not_initial( lt_sotr ).
      CATCH zcx_abapgit_exception.
        cl_aunit_assert=>fail( quit = if_aunit_constants=>method ).
    ENDTRY.
  ENDMETHOD.
  METHOD sotr_cx_0002.
    DATA lt_sotr TYPE zif_abapgit_definitions=>ty_sotr_tt.
    TRY.
        lt_sotr = zcl_abapgit_sotr_handler=>read_sotr_seocomp( iv_object_name = 'CX_ABAP_INVALID_NAME' ).
        cl_aunit_assert=>assert_not_initial( lt_sotr ).
      CATCH zcx_abapgit_exception.
        cl_aunit_assert=>fail( quit = if_aunit_constants=>method ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
