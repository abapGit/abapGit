CLASS ltcl_data_utils_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS build_filename FOR TESTING.
    METHODS get_item FOR TESTING.

ENDCLASS.

CLASS ltcl_data_utils_test IMPLEMENTATION.

  METHOD build_filename.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-name = 'T100'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>build_filename( ls_config )
      exp = 't100.tabu.json' ).

    ls_config-name = '/NSPC/T200'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>build_filename( ls_config )
      exp = '#nspc#t200.tabu.json' ).

  ENDMETHOD.

  METHOD get_item.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-name = 'T100'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>get_item( ls_config )-obj_type
      exp = 'TABU' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>get_item( ls_config )-obj_name
      exp = 'T100' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>get_item( ls_config )-devclass
      exp = 'SDIC' ).

  ENDMETHOD.

ENDCLASS.
