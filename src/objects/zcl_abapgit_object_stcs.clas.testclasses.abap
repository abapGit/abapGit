CLASS ltcl_field_rules DEFINITION DEFERRED.
CLASS zcl_abapgit_object_stcs DEFINITION LOCAL FRIENDS ltcl_field_rules.

CLASS ltcl_field_rules DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    METHODS normalize_header FOR TESTING.
ENDCLASS.

CLASS ltcl_field_rules IMPLEMENTATION.
  METHOD normalize_header.
    " Keep this test independent of STC DDIC types, including on 7.02.
    TYPES:
      BEGIN OF ty_header,
        scenario_id  TYPE c LENGTH 32,
        created_by   TYPE syuname,
        created_on   TYPE d,
        changed_by   TYPE syuname,
        changed_on   TYPE d,
        config_class TYPE seoclsname,
        build_no     TYPE n LENGTH 5,
      END OF ty_header.
    DATA lt_header TYPE STANDARD TABLE OF ty_header WITH DEFAULT KEY.
    DATA ls_header TYPE ty_header.
    DATA lo_rules TYPE REF TO zif_abapgit_field_rules.

    ls_header-scenario_id = 'Z_TASK_LIST'.
    ls_header-created_by = 'AUTHOR'.
    ls_header-created_on = '20200101'.
    ls_header-changed_by = 'EDITOR'.
    ls_header-changed_on = '20210101'.
    ls_header-config_class = 'ZCL_TASK_LIST_CONFIG'.
    ls_header-build_no = '00003'.
    APPEND ls_header TO lt_header.

    lo_rules = zcl_abapgit_object_stcs=>get_field_rules( ).
    lo_rules->apply_clear_logic( EXPORTING iv_table = 'STC_SCN_HDR'
                                 CHANGING ct_data = lt_header ).
    READ TABLE lt_header INDEX 1 INTO ls_header.
    cl_abap_unit_assert=>assert_initial( ls_header-created_by ).
    cl_abap_unit_assert=>assert_initial( ls_header-created_on ).
    cl_abap_unit_assert=>assert_initial( ls_header-changed_by ).
    cl_abap_unit_assert=>assert_initial( ls_header-changed_on ).
    cl_abap_unit_assert=>assert_equals( act = ls_header-scenario_id
                                         exp = 'Z_TASK_LIST' ).
    cl_abap_unit_assert=>assert_equals( act = ls_header-config_class
                                         exp = 'ZCL_TASK_LIST_CONFIG' ).
    cl_abap_unit_assert=>assert_equals( act = ls_header-build_no
                                         exp = '00003' ).

    lo_rules->apply_fill_logic( EXPORTING iv_table   = 'STC_SCN_HDR'
                                         iv_package = '$TMP'
                                CHANGING ct_data    = lt_header ).
    READ TABLE lt_header INDEX 1 INTO ls_header.
    cl_abap_unit_assert=>assert_equals( act = ls_header-created_by
                                         exp = sy-uname ).
    cl_abap_unit_assert=>assert_equals( act = ls_header-created_on
                                         exp = sy-datum ).
    cl_abap_unit_assert=>assert_equals( act = ls_header-changed_by
                                         exp = sy-uname ).
    cl_abap_unit_assert=>assert_equals( act = ls_header-changed_on
                                         exp = sy-datum ).
  ENDMETHOD.
ENDCLASS.
