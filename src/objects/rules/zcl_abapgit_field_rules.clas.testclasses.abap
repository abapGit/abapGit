CLASS ltcl_field_rules DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    CONSTANTS:
      c_abap_cloud TYPE uccheck VALUE '5',
      c_package    TYPE devclass VALUE '$TMP',
      c_table      TYPE tabname VALUE 'ZTEST'.

    TYPES:
      BEGIN OF ty_test,
        key  TYPE i,
        user TYPE sy-uname,
        date TYPE d,
        time TYPE t,
        ts   TYPE timestamp,
        tl   TYPE timestampl,
        alav TYPE uccheck,
      END OF ty_test.

    DATA mo_cut TYPE REF TO zcl_abapgit_field_rules.

    METHODS:
      setup,
      fill_value
        IMPORTING
          iv_rule TYPE zif_abapgit_field_rules=>ty_fill_rule
          iv_exp  TYPE string
          iv_len  TYPE i OPTIONAL,
      fill1 FOR TESTING,
      fill2 FOR TESTING,
      fill3 FOR TESTING,
      fill4 FOR TESTING,
      fill5 FOR TESTING,
      fill6 FOR TESTING,
      fill7 FOR TESTING,
      fill8 FOR TESTING,
      get_rules
        RETURNING
          VALUE(ri_rules) TYPE REF TO zif_abapgit_field_rules,
      apply_clear_logic FOR TESTING,
      apply_fill_logic FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_field_rules DEFINITION LOCAL FRIENDS ltcl_field_rules.

CLASS ltcl_field_rules IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD fill_value.

    DATA lv_act TYPE string.

    mo_cut->fill_value(
      EXPORTING
        iv_rule                  = iv_rule
        iv_package               = c_package
        iv_abap_language_version = c_abap_cloud
      CHANGING
        cv_value                 = lv_act ).

    IF iv_len IS NOT INITIAL.
      lv_act = lv_act(iv_len).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = iv_exp ).

  ENDMETHOD.

  METHOD fill1.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-date
      iv_exp  = |{ sy-datum }| ).
  ENDMETHOD.

  METHOD fill2.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-time
      iv_len  = 2
      iv_exp  = |{ sy-uzeit(2) }| ). " avoid comparing minutes
  ENDMETHOD.

  METHOD fill3.
    DATA lv_timestamp TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp
      iv_len  = 10
      iv_exp  = |{ lv_timestamp DIV 10000 }| ). " avoid comparing minutes
  ENDMETHOD.

  METHOD fill4.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-user
      iv_exp  = |{ sy-uname }| ).
  ENDMETHOD.

  METHOD fill5.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-client
      iv_exp  = |{ sy-mandt }| ).
  ENDMETHOD.

  METHOD fill6.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-package
      iv_exp  = |{ c_package }| ).
  ENDMETHOD.

  METHOD fill7.
    fill_value(
      iv_rule = 'XY'
      iv_exp  = '' ).
  ENDMETHOD.

  METHOD fill8.
    fill_value(
      iv_rule = zif_abapgit_field_rules=>c_fill_rule-abap_language_version
      iv_exp  = |{ c_abap_cloud }| ).
  ENDMETHOD.

  METHOD get_rules.

    ri_rules = zcl_abapgit_field_rules=>create( )->add(
      iv_table     = c_table
      iv_field     = 'USER'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = c_table
      iv_field     = 'DATE'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = c_table
      iv_field     = 'TIME'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time
    )->add(
      iv_table     = c_table
      iv_field     = 'TS'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = c_table
      iv_field     = 'TL'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = c_table
      iv_field     = 'ALAV'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-abap_language_version ).

  ENDMETHOD.

  METHOD apply_clear_logic.

    DATA:
      li_rules TYPE REF TO zif_abapgit_field_rules,
      ls_act   TYPE ty_test,
      lt_act   TYPE STANDARD TABLE OF ty_test.

    ls_act-key  = 1.
    ls_act-user = 'MARC'.
    ls_act-date = '19720101'.
    ls_act-time = '123456'.
    ls_act-ts   = '19720101123456'.
    ls_act-tl   = '19720101123456.123456'.
    INSERT ls_act INTO TABLE lt_act.
    ls_act-key  = 2.
    ls_act-user = 'FRANK'.
    ls_act-date = '20230101'.
    ls_act-time = '000001'.
    ls_act-alav = '5'. " ABAP Cloud
    INSERT ls_act INTO TABLE lt_act.

    li_rules = get_rules( ).

    li_rules->apply_clear_logic(
      EXPORTING
        iv_table = c_table
      CHANGING
        ct_data  = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_act )
      exp = 2 ).

    READ TABLE lt_act INTO ls_act INDEX 1.

    cl_abap_unit_assert=>assert_not_initial( ls_act-key ).
    cl_abap_unit_assert=>assert_initial( ls_act-user ).
    cl_abap_unit_assert=>assert_initial( ls_act-date ).
    cl_abap_unit_assert=>assert_initial( ls_act-time ).
    cl_abap_unit_assert=>assert_initial( ls_act-ts ).
    cl_abap_unit_assert=>assert_initial( ls_act-tl ).

    READ TABLE lt_act INTO ls_act INDEX 2.

    cl_abap_unit_assert=>assert_not_initial( ls_act-key ).
    cl_abap_unit_assert=>assert_initial( ls_act-user ).
    cl_abap_unit_assert=>assert_initial( ls_act-date ).
    cl_abap_unit_assert=>assert_initial( ls_act-time ).
    cl_abap_unit_assert=>assert_initial( ls_act-ts ).
    cl_abap_unit_assert=>assert_initial( ls_act-tl ).

  ENDMETHOD.

  METHOD apply_fill_logic.

    DATA:
      li_rules     TYPE REF TO zif_abapgit_field_rules,
      ls_act       TYPE ty_test,
      lt_act       TYPE STANDARD TABLE OF ty_test,
      lv_ts        TYPE string,
      lv_timestamp TYPE timestamp.

    GET TIME STAMP FIELD lv_timestamp.

    ls_act-key  = 1.
    INSERT ls_act INTO TABLE lt_act.
    ls_act-key  = 2.
    ls_act-user = 'XYZ'.
    ls_act-date = ''.
    ls_act-time = '123456'.
    INSERT ls_act INTO TABLE lt_act.

    li_rules = get_rules( ).

    li_rules->apply_fill_logic(
      EXPORTING
        iv_table                 = c_table
        iv_package               = c_package
        iv_abap_language_version = c_abap_cloud
      CHANGING
        ct_data                  = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_act )
      exp = 2 ).

    READ TABLE lt_act INTO ls_act INDEX 1.

    cl_abap_unit_assert=>assert_equals(
      act = ls_act-key
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-user
      exp = sy-uname ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-date
      exp = sy-datum ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-time(4)
      exp = sy-uzeit(4) ). " avoid comparing seconds

    lv_ts = ls_act-ts.
    lv_ts = lv_ts(10).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ts
      exp = |{ lv_timestamp DIV 10000 }| ). " avoid comparing second
    lv_ts = ls_act-tl.
    lv_ts = lv_ts(10).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ts
      exp = |{ lv_timestamp DIV 10000 }| ). " avoid comparing seconds

    READ TABLE lt_act INTO ls_act INDEX 2.

    cl_abap_unit_assert=>assert_equals(
      act = ls_act-key
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-user
      exp = sy-uname ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-date
      exp = sy-datum ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-time(4)
      exp = sy-uzeit(4) ). " avoid comparing seconds
    cl_abap_unit_assert=>assert_equals(
      act = ls_act-alav
      exp = c_abap_cloud ).

  ENDMETHOD.
ENDCLASS.
