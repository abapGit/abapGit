*"* use this source file for your ABAP unit test classes
CLASS ltcl_sotr_handler DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS is_wd_component_existing
      IMPORTING iv_component_name                  TYPE sobj_name
      RETURNING VALUE(rv_is_wd_component_existing) TYPE abap_bool.
    METHODS sotr_wda_0001 FOR TESTING.
    METHODS sotr_wda_0003_not_exist FOR TESTING.
    METHODS sotr_cx_0002 FOR TESTING.
ENDCLASS.

CLASS ltcl_sotr_handler IMPLEMENTATION.
  METHOD is_wd_component_existing.
    DATA ls_repository TYPE wdy_rr_cluster.
    DATA lv_component_name TYPE string.

    lv_component_name = iv_component_name.
    CALL FUNCTION 'WDR_REPOSITORY_INFO'
      EXPORTING
        component_name = lv_component_name
      IMPORTING
        repository     = ls_repository.
    IF ls_repository IS NOT INITIAL.
      rv_is_wd_component_existing = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD sotr_wda_0001.
    CONSTANTS lc_wd_component_name TYPE sobj_name VALUE 'SALV_WD_TEST_TABLE_SIMPLE'.
    DATA lt_sotr TYPE zif_abapgit_definitions=>ty_sotr_tt.
    IF is_wd_component_existing( lc_wd_component_name ) = abap_true.
      TRY.
          lt_sotr = zcl_abapgit_sotr_handler=>read_sotr_wda( iv_object_name = lc_wd_component_name ).
          cl_aunit_assert=>assert_not_initial( lt_sotr ).
        CATCH zcx_abapgit_exception.
          cl_aunit_assert=>fail( quit = if_aunit_constants=>method ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD sotr_wda_0003_not_exist.
    CONSTANTS lc_wd_not_exist_component_name TYPE sobj_name VALUE '_NOT_EXISTING'.
    DATA lt_sotr TYPE zif_abapgit_definitions=>ty_sotr_tt.
    IF is_wd_component_existing( lc_wd_not_exist_component_name ) <> abap_true.
      TRY.
          lt_sotr = zcl_abapgit_sotr_handler=>read_sotr_wda( iv_object_name = lc_wd_not_exist_component_name ).
          cl_aunit_assert=>assert_initial( lt_sotr ).
        CATCH zcx_abapgit_exception.
          cl_aunit_assert=>fail( quit = if_aunit_constants=>method ).
      ENDTRY.
    ENDIF.
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
