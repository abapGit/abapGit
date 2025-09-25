CLASS lcl_supporter_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_data_supporter.
ENDCLASS.

CLASS lcl_supporter_mock IMPLEMENTATION.
  METHOD zif_abapgit_data_supporter~is_object_supported.

    IF iv_type = zif_abapgit_data_config=>c_data_type-tabu AND iv_name = 'T005'.
      rv_supported = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_supporter DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      is_not_supported FOR TESTING,
      is_supported FOR TESTING,
      repo_factory_integration FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_data_supporter DEFINITION LOCAL FRIENDS ltcl_supporter.

CLASS ltcl_supporter IMPLEMENTATION.

  METHOD is_not_supported.

    DATA lv_act TYPE abap_bool.

    " By default, SAP tables are not supported
    lv_act = zcl_abapgit_data_factory=>get_supporter( )->is_object_supported(
      iv_type = zif_abapgit_data_config=>c_data_type-tabu
      iv_name = 'T005' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = abap_false ).

  ENDMETHOD.

  METHOD is_supported.

    DATA lo_mock TYPE REF TO lcl_supporter_mock.
    DATA lo_inject TYPE REF TO zcl_abapgit_data_injector.
    DATA lv_act TYPE abap_bool.

    " Mock sets table T005 to be supported
    CREATE OBJECT lo_mock.
    CREATE OBJECT lo_inject.
    lo_inject->set_supporter( lo_mock ).

    lv_act = zcl_abapgit_data_factory=>get_supporter( )->is_object_supported(
      iv_type = zif_abapgit_data_config=>c_data_type-tabu
      iv_name = 'T005' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = abap_true ).

  ENDMETHOD.

  METHOD repo_factory_integration.

    DATA: lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
          lo_supporter   TYPE REF TO zif_abapgit_data_supporter,
          ls_dot_data    TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          lt_objects     TYPE zif_abapgit_data_supporter=>ty_objects,
          ls_object      LIKE LINE OF lt_objects,
          lo_empty_repo  TYPE REF TO zif_abapgit_repo.

    " Create dot_abapgit with supported data objects
    ls_object-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_object-name = 'ZTESTCUSTOM'.
    INSERT ls_object INTO TABLE lt_objects.

    ls_dot_data-supported_data_objects = lt_objects.
    CREATE OBJECT lo_dot_abapgit EXPORTING is_data = ls_dot_data.

    " Test that dot_abapgit correctly stores and retrieves supported data objects
    cl_abap_unit_assert=>assert_not_initial(
      act = lo_dot_abapgit->get_supported_data_objects( )
      msg = 'Supported data objects should be stored in dot_abapgit' ).

    " Test that the factory method for repository-specific supporter works
    " Note: We can't easily mock a full repository interface in ABAPv702,
    " so this test verifies the basic factory method works without repository
    lo_supporter = zcl_abapgit_data_factory=>get_supporter_for_repo( lo_empty_repo ).

    cl_abap_unit_assert=>assert_bound(
      act = lo_supporter
      msg = 'Factory should return a supporter instance' ).

  ENDMETHOD.

ENDCLASS.