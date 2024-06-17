CLASS ltcl_data_utils_test DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS build_data_filename FOR TESTING RAISING cx_static_check.
    METHODS build_config_filename FOR TESTING RAISING cx_static_check.
    METHODS build_table_itab FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_data_utils_test IMPLEMENTATION.

  METHOD build_data_filename.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-name = 'T100'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>build_data_filename( ls_config )
      exp = 't100.tabu.json' ).

    ls_config-name = '/NSPC/T200'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>build_data_filename( ls_config )
      exp = '#nspc#t200.tabu.json' ).

  ENDMETHOD.

  METHOD build_config_filename.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    ls_config-name = 'T100'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>build_config_filename( ls_config )
      exp = 't100.conf.json' ).

    ls_config-name = '/NSPC/T200'.
    ls_config-type = 'TABU'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_data_utils=>build_config_filename( ls_config )
      exp = '#nspc#t200.conf.json' ).

  ENDMETHOD.

  METHOD build_table_itab.

    DATA lr_data TYPE REF TO data.
    DATA ls_row  TYPE t100.
    FIELD-SYMBOLS <lt_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_row> TYPE any.

    lr_data = zcl_abapgit_data_utils=>build_table_itab( 'T100' ).
    ASSIGN lr_data->* TO <lt_tab>.

* test that the table works with basic itab operations,
    INSERT ls_row INTO TABLE <lt_tab>.
    cl_abap_unit_assert=>assert_subrc( ).

    READ TABLE <lt_tab> ASSIGNING <ls_row> FROM ls_row.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.
