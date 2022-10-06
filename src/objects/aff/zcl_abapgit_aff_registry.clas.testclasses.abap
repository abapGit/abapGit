"! @testing zcl_abapgit_filename_logic
CLASS ltcl_aff_registry DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      assert_that
        IMPORTING
          obj_type     TYPE tadir-object
          is_supported TYPE abap_bool
          experimental TYPE abap_bool DEFAULT abap_false,
      clas_not_supported FOR TESTING RAISING cx_static_check,
      chkc FOR TESTING RAISING cx_static_check,
      chko FOR TESTING RAISING cx_static_check,
      chkv FOR TESTING RAISING cx_static_check,
      evtb FOR TESTING RAISING cx_static_check,
      intf_not_supported FOR TESTING RAISING cx_static_check,
      intf_experimental FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_aff_registry IMPLEMENTATION.

  METHOD assert_that.
    DATA:
      cut           TYPE REF TO zif_abapgit_aff_registry,
      settings_stub TYPE REF TO zcl_abapgit_settings,
      act           TYPE abap_bool.


    CREATE OBJECT settings_stub.
    settings_stub->set_experimental_features( experimental ).
    CREATE OBJECT cut TYPE zcl_abapgit_aff_registry
      EXPORTING
        settings = settings_stub.
    act = cut->is_supported_object_type( obj_type ).
    cl_abap_unit_assert=>assert_equals( exp = is_supported act = act ).
  ENDMETHOD.

  METHOD clas_not_supported.
    assert_that( obj_type = 'CLAS' is_supported = abap_false ).
  ENDMETHOD.

  METHOD chkc.
    assert_that( obj_type = 'CHKC' is_supported = abap_true ).
  ENDMETHOD.

  METHOD chko.
    assert_that( obj_type = 'CHKO' is_supported = abap_true ).
  ENDMETHOD.

  METHOD chkv.
    assert_that( obj_type = 'CHKV' is_supported = abap_true ).
  ENDMETHOD.

  METHOD evtb.
    assert_that( obj_type = 'EVTB' is_supported = abap_true ).
  ENDMETHOD.

  METHOD intf_not_supported.
    assert_that( obj_type = 'INTF' is_supported = abap_false ).
  ENDMETHOD.

  METHOD intf_experimental.
    assert_that( obj_type = 'INTF' is_supported = abap_true experimental = abap_true ).
  ENDMETHOD.


ENDCLASS.
