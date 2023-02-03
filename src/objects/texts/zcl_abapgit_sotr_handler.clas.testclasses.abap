CLASS ltcl_sotr_handler DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS is_wd_component_existing
      IMPORTING iv_component_name                  TYPE sobj_name
      RETURNING VALUE(rv_is_wd_component_existing) TYPE abap_bool.
    METHODS sotr_wda_0001 FOR TESTING.
    METHODS sotr_wda_0003_not_exist FOR TESTING.
    METHODS sotr_wda_0004 FOR TESTING.
    METHODS sotr_cx_0002 FOR TESTING.
ENDCLASS.

CLASS ltcl_sotr_handler IMPLEMENTATION.
  METHOD is_wd_component_existing.
    DATA ls_repository TYPE wdy_rr_cluster.
    DATA lv_component_name TYPE string.

    lv_component_name = iv_component_name.
    TRY.
        "need to regenerate for unit test to work
        CALL FUNCTION 'WDR_REPOSITORY_INFO'
          EXPORTING
            component_name = lv_component_name
            extended_read  = abap_true
          IMPORTING
            repository     = ls_repository.
        IF ls_repository IS NOT INITIAL.
          rv_is_wd_component_existing = abap_true.
        ENDIF.
      CATCH cx_wdr_rr_exception.
        "ignore and return false
    ENDTRY.
  ENDMETHOD.
  METHOD sotr_wda_0001.
    CONSTANTS lc_wd_component_name TYPE sobj_name VALUE 'SALV_WD_TEST_TABLE_SIMPLE'.
    DATA lt_sotr TYPE zcl_abapgit_sotr_handler=>ty_sotr_tt.
    IF is_wd_component_existing( lc_wd_component_name ) = abap_true.
      TRY.
          zcl_abapgit_sotr_handler=>read_sotr(
            EXPORTING
              iv_pgmid    = 'LIMU'
              iv_object   = 'WDYV'
              iv_obj_name = lc_wd_component_name
            IMPORTING
              et_sotr     = lt_sotr ).
          cl_abap_unit_assert=>assert_not_initial( lt_sotr ).
        CATCH zcx_abapgit_exception.
          cl_abap_unit_assert=>fail( quit = if_aunit_constants=>method ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD sotr_wda_0003_not_exist.
    CONSTANTS lc_wd_not_exist_component_name TYPE sobj_name VALUE '_NOT_EXISTING'.
    DATA lt_sotr TYPE zcl_abapgit_sotr_handler=>ty_sotr_tt.
    IF is_wd_component_existing( lc_wd_not_exist_component_name ) <> abap_true.
      TRY.
          zcl_abapgit_sotr_handler=>read_sotr(
            EXPORTING
              iv_pgmid    = 'LIMU'
              iv_object   = 'WDYV'
              iv_obj_name = lc_wd_not_exist_component_name
            IMPORTING
              et_sotr     = lt_sotr ).
          cl_abap_unit_assert=>assert_initial( lt_sotr ).
        CATCH zcx_abapgit_exception.
          cl_abap_unit_assert=>fail( quit = if_aunit_constants=>method ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD sotr_wda_0004.
    CONSTANTS lc_wd_component_name TYPE sobj_name VALUE 'SALV_WD_TEST_TABLE_SELECT'.
    DATA lt_sotr TYPE zcl_abapgit_sotr_handler=>ty_sotr_tt.
    IF is_wd_component_existing( lc_wd_component_name ) = abap_true.
      TRY.
          zcl_abapgit_sotr_handler=>read_sotr(
            EXPORTING
              iv_pgmid    = 'LIMU'
              iv_object   = 'WDYV'
              iv_obj_name = lc_wd_component_name
            IMPORTING
              et_sotr     = lt_sotr ).
          IF lines( lt_sotr ) < 50.
            cl_abap_unit_assert=>fail( quit = if_aunit_constants=>method ).
          ENDIF.
        CATCH zcx_abapgit_exception.
          cl_abap_unit_assert=>fail( quit = if_aunit_constants=>method ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD sotr_cx_0002.
    DATA lt_sotr TYPE zcl_abapgit_sotr_handler=>ty_sotr_tt.
    TRY.
        zcl_abapgit_sotr_handler=>read_sotr(
          EXPORTING
            iv_pgmid    = 'LIMU'
            iv_object   = 'CPUB'
            iv_obj_name = 'CX_ABAP_INVALID_NAME'
          IMPORTING
            et_sotr     = lt_sotr ).
        cl_abap_unit_assert=>assert_not_initial( lt_sotr ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( quit = if_aunit_constants=>method ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
