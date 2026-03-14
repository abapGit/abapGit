CLASS zcl_abapgit_fm_ddif_doma_get DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
ENDCLASS.

CLASS zcl_abapgit_fm_ddif_doma_get IMPLEMENTATION.
  METHOD if_ftd_invocation_answer~answer.

    DATA ls_dd01v  TYPE dd01v.
    DATA lt_dd07v  TYPE dd07v_tab.
    DATA lv_state  TYPE ddgotstate.
    DATA li_config TYPE REF TO if_ftd_output_configuration.

    lv_state = 'A'.

    IF zcl_abapgit_fm_ddif_doma_put=>gs_dd01v IS NOT INITIAL.
      ls_dd01v = zcl_abapgit_fm_ddif_doma_put=>gs_dd01v.
      lt_dd07v = zcl_abapgit_fm_ddif_doma_put=>gt_dd07v.
    ELSE.
      ls_dd01v-domname    = 'ZABAPGIT_TEST_DOMA'.
      ls_dd01v-ddlanguage = 'E'.
      ls_dd01v-datatype   = 'CHAR'.
      ls_dd01v-leng       = 1.
      ls_dd01v-outputlen  = 1.
      ls_dd01v-ddtext     = 'Testing'.
    ENDIF.

    li_config = result->get_output_configuration( ).
    li_config->set_exporting_parameter(
      name  = 'GOTSTATE'
      value = lv_state ).
    li_config->set_exporting_parameter(
      name  = 'DD01V_WA'
      value = ls_dd01v ).
    li_config->set_table_parameter(
      name  = 'DD07V_TAB'
      value = lt_dd07v ).

  ENDMETHOD.
ENDCLASS.
