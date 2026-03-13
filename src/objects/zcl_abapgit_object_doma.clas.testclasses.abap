CLASS ltcl_aff_metadata DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS serialize_deserialize FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_aff_metadata IMPLEMENTATION.

  METHOD serialize_deserialize.
    DATA lv_json TYPE xstring.
    DATA ls_dd01v_exp TYPE dd01v.
    DATA ls_dd01v_act TYPE dd01v.
    DATA lt_dd07v_exp TYPE dd07v_tab.
    DATA lt_dd07v_act TYPE dd07v_tab.
    DATA ls_dd07v TYPE dd07v.

    ls_dd01v_exp-domname = 'ZABAPGIT_TEST_DOMA'.
    ls_dd01v_exp-ddtext = 'Domain for unit tests'.
    ls_dd01v_exp-ddlanguage = 'E'.
    ls_dd01v_exp-datatype = 'CHAR'.
    ls_dd01v_exp-leng = 10.
    ls_dd01v_exp-outputlen = 12.
    ls_dd01v_exp-convexit = 'ALPHA'.
    ls_dd01v_exp-lowercase = abap_true.
    ls_dd01v_exp-signflag = abap_true.
    ls_dd01v_exp-entitytab = 'T000'.

    ls_dd07v-domname = ls_dd01v_exp-domname.
    ls_dd07v-valpos = 1.
    ls_dd07v-domvalue_l = 'A'.
    ls_dd07v-domvalue_h = 'A'.
    ls_dd07v-ddtext = 'Active'.
    ls_dd07v-ddlanguage = ls_dd01v_exp-ddlanguage.
    APPEND ls_dd07v TO lt_dd07v_exp.

    CLEAR ls_dd07v.
    ls_dd07v-domname = ls_dd01v_exp-domname.
    ls_dd07v-valpos = 2.
    ls_dd07v-domvalue_l = 'B'.
    ls_dd07v-domvalue_h = 'Z'.
    ls_dd07v-ddtext = 'Range B-Z'.
    ls_dd07v-ddlanguage = ls_dd01v_exp-ddlanguage.
    APPEND ls_dd07v TO lt_dd07v_exp.

    lv_json = lcl_aff_metadata_handler=>serialize(
      is_dd01v = ls_dd01v_exp
      it_dd07v = lt_dd07v_exp ).

    cl_abap_unit_assert=>assert_not_initial( lv_json ).

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json        = lv_json
        iv_object_name = 'zabapgit_test_doma'
      IMPORTING
        es_dd01v       = ls_dd01v_act
        et_dd07v       = lt_dd07v_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act
      exp = ls_dd01v_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_dd07v_act
      exp = lt_dd07v_exp ).
  ENDMETHOD.

ENDCLASS.