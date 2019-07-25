CLASS ltcl_test_simple_positive DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_abapgit_environment.

    METHODS:
      setup,
      compare_with_inactive FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING,
      is_sap_cloud_platform FOR TESTING.

ENDCLASS.

CLASS ltcl_test_simple_negative DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_abapgit_environment.

    METHODS:
      setup,
      compare_with_inactive FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING,
      is_sap_cloud_platform FOR TESTING.

ENDCLASS.

CLASS ltcl_test_simple_main DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_abapgit_environment.

    METHODS:
      setup,
      compare_with_inactive FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING,
      is_sap_cloud_platform FOR TESTING.

ENDCLASS.

CLASS ltcl_test_simple_positive IMPLEMENTATION.

  METHOD setup.
    DATA ls_config TYPE ltcl_abapgit_environment_stub=>ty_return_value_config.
    DATA lo_stub TYPE REF TO zif_abapgit_environment.
    DATA lo_stub_downcast TYPE REF TO ltcl_abapgit_environment_stub.

    CREATE OBJECT lo_stub TYPE ltcl_abapgit_environment_stub.

    " Inject the test double
    zcl_abapgit_injector=>set_environment( lo_stub ).

    " Retrieve code under test
    mo_cut = zcl_abapgit_factory=>get_environment( ).

    "Define stub configuration
    ls_config-compare_with_inactive-return_value = abap_true.
    ls_config-is_merged-return_value = abap_true.
    ls_config-is_repo_object_changes_allowed-return_value = abap_true.
    ls_config-is_restart_required-return_value = abap_true.
    ls_config-is_sap_cloud_platform-return_value = abap_true.

    lo_stub_downcast ?= lo_stub.
    lo_stub_downcast->define_return_value_config( ls_config ).
  ENDMETHOD.

  METHOD compare_with_inactive.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->compare_with_inactive( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_merged.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_merged( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_repo_object_changes_allowed( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_restart_required.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_restart_required( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_sap_cloud_platform( )
                                        exp =  abap_true ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test_simple_negative IMPLEMENTATION.

  METHOD setup.
    DATA ls_config TYPE ltcl_abapgit_environment_stub=>ty_return_value_config.
    DATA lo_stub TYPE REF TO zif_abapgit_environment.
    DATA lo_stub_downcast TYPE REF TO ltcl_abapgit_environment_stub.

    CREATE OBJECT lo_stub TYPE ltcl_abapgit_environment_stub.

    " Inject the test double
    zcl_abapgit_injector=>set_environment( lo_stub ).

    " Retrieve code under test
    mo_cut = zcl_abapgit_factory=>get_environment( ).

    "Define stub configuration
    ls_config-compare_with_inactive-return_value = abap_false.
    ls_config-is_merged-return_value = abap_false.
    ls_config-is_repo_object_changes_allowed-return_value = abap_false.
    ls_config-is_restart_required-return_value = abap_false.
    ls_config-is_sap_cloud_platform-return_value = abap_false.

    lo_stub_downcast ?= lo_stub.
    lo_stub_downcast->define_return_value_config( ls_config ).
  ENDMETHOD.

  METHOD compare_with_inactive.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->compare_with_inactive( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_merged.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_merged( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_repo_object_changes_allowed( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_restart_required.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_restart_required( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_sap_cloud_platform( )
                                        exp =  abap_false ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test_simple_main IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_abapgit_factory=>get_environment( ).
  ENDMETHOD.

  METHOD compare_with_inactive.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->compare_with_inactive( )
                                        exp = mo_cut->compare_with_inactive( ) ).
  ENDMETHOD.

  METHOD is_merged.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_merged( )
                                        exp = mo_cut->is_merged( ) ).
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_repo_object_changes_allowed( )
                                        exp = mo_cut->is_repo_object_changes_allowed( ) ).
  ENDMETHOD.

  METHOD is_restart_required.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_restart_required( )
                                        exp = mo_cut->is_restart_required( ) ).
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_sap_cloud_platform( )
                                        exp = mo_cut->is_sap_cloud_platform( ) ).
  ENDMETHOD.
ENDCLASS.
