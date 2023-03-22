"! @testing zcl_abapgit_filename_logic
CLASS ltcl_aff_registry DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      assert_that
        IMPORTING
          iv_obj_type     TYPE tadir-object
          iv_is_supported TYPE abap_bool
          iv_experimental TYPE abap_bool DEFAULT abap_false,
      clas_not_supported FOR TESTING RAISING cx_static_check,
      chkc FOR TESTING RAISING cx_static_check,
      chko FOR TESTING RAISING cx_static_check,
      chkv FOR TESTING RAISING cx_static_check,
      evtb FOR TESTING RAISING cx_static_check,
      gsmp FOR TESTING RAISING cx_static_check,
      intf_not_supported FOR TESTING RAISING cx_static_check,
      intf_experimental FOR TESTING RAISING cx_static_check,
      smbc FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_aff_registry IMPLEMENTATION.

  METHOD assert_that.
    DATA:
      lo_cut           TYPE REF TO zif_abapgit_aff_registry,
      lo_settings_stub TYPE REF TO zcl_abapgit_settings,
      lv_act           TYPE abap_bool.


    CREATE OBJECT lo_settings_stub.
    lo_settings_stub->set_experimental_features( iv_experimental ).
    CREATE OBJECT lo_cut TYPE zcl_abapgit_aff_registry
      EXPORTING
        io_settings = lo_settings_stub.
    lv_act = lo_cut->is_supported_object_type( iv_obj_type ).
    cl_abap_unit_assert=>assert_equals( exp = iv_is_supported
                                        act = lv_act ).
  ENDMETHOD.

  METHOD clas_not_supported.
    assert_that( iv_obj_type = 'CLAS'
                 iv_is_supported = abap_false ).
  ENDMETHOD.

  METHOD chkc.
    assert_that( iv_obj_type = 'CHKC'
                 iv_is_supported = abap_true ).
  ENDMETHOD.

  METHOD chko.
    assert_that( iv_obj_type = 'CHKO'
                 iv_is_supported = abap_true ).
  ENDMETHOD.

  METHOD chkv.
    assert_that( iv_obj_type = 'CHKV'
                 iv_is_supported = abap_true ).
  ENDMETHOD.

  METHOD evtb.
    assert_that( iv_obj_type = 'EVTB'
                 iv_is_supported = abap_true ).
  ENDMETHOD.

  METHOD gsmp.
    assert_that( iv_obj_type = 'GSMP'
                 iv_is_supported = abap_true ).
  ENDMETHOD.

  METHOD intf_not_supported.
    assert_that( iv_obj_type = 'INTF'
                 iv_is_supported = abap_false ).
  ENDMETHOD.

  METHOD intf_experimental.
    assert_that( iv_obj_type = 'INTF'
                 iv_is_supported = abap_true
                 iv_experimental = abap_true ).
  ENDMETHOD.

  METHOD smbc.
    assert_that( iv_obj_type = 'SMBC'
                 iv_is_supported = abap_true ).
  ENDMETHOD.

ENDCLASS.
