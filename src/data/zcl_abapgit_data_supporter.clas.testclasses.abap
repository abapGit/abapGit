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
      is_supported FOR TESTING.

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

ENDCLASS.
