CLASS ltcl_split_parameters DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_object_tran.
    METHODS:
      setup,

      parmeter_transaction FOR TESTING RAISING cx_static_check,

      given_parameter_transaction
        IMPORTING
          iv_tcode TYPE csequence
          iv_param TYPE csequence,

      when_parameter_split,

      then_st_tcode_shd_be
        IMPORTING
          iv_exp_st_tcode TYPE eusel_tcod,

      then_st_skip_1_shd_be
        IMPORTING
          iv_st_skip TYPE eusel_skip,

      then_call_tcode_shd_be
        IMPORTING
          iv_call_tcode TYPE tcode,
      then_param_shd_be
        IMPORTING
          iv_line  TYPE i
          iv_field TYPE string
          iv_value TYPE string.

    DATA:
      ms_tstcp   TYPE tstcp,
      mt_rsparam TYPE s_param,
      ms_rsstcd  TYPE rsstcd,
      ms_tstc    TYPE tstc.

ENDCLASS.

CLASS zcl_abapgit_object_tran DEFINITION LOCAL FRIENDS ltcl_split_parameters.

CLASS ltcl_split_parameters IMPLEMENTATION.

  METHOD setup.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_name = 'ZDUMMY'.
    ls_item-obj_type = 'TRAN'.

    CREATE OBJECT mo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

  ENDMETHOD.

  METHOD parmeter_transaction.

    "Parameter transaction ISSUE_1276

    given_parameter_transaction(
      iv_tcode = 'ZMM_BF_SPEZ_ROLE'
      iv_param = '/*SM30 VIEWNAME=ZMM_BF_SPEZ_ROLE;UPDATE=X;' ).

    when_parameter_split( ).

    then_st_tcode_shd_be( 'X' ).
    then_st_skip_1_shd_be( 'X' ).
    then_call_tcode_shd_be( 'SM30' ).

    then_param_shd_be(
      iv_line  = 1
      iv_field = 'VIEWNAME'
      iv_value ='ZMM_BF_SPEZ_ROLE' ).

    then_param_shd_be(
      iv_line  = 2
      iv_field = 'UPDATE'
      iv_value = 'X' ).

  ENDMETHOD.


  METHOD given_parameter_transaction.

    ms_tstcp-tcode = iv_tcode.
    ms_tstcp-param = iv_param.

  ENDMETHOD.


  METHOD when_parameter_split.

    mo_cut->split_parameters(
      CHANGING
        ct_rsparam = mt_rsparam
        cs_rsstcd  = ms_rsstcd
        cs_tstcp   = ms_tstcp
        cs_tstc    = ms_tstc ).

  ENDMETHOD.


  METHOD then_st_tcode_shd_be.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp_st_tcode
      act = ms_rsstcd-st_tcode ).

  ENDMETHOD.


  METHOD then_st_skip_1_shd_be.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_st_skip
      act = ms_rsstcd-st_skip_1 ).

  ENDMETHOD.


  METHOD then_call_tcode_shd_be.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_call_tcode
      act = ms_rsstcd-call_tcode ).

  ENDMETHOD.


  METHOD then_param_shd_be.

    DATA: ls_exp_rsparam LIKE LINE OF mt_rsparam,
          ls_act_rsparam TYPE rsparam.

    ls_exp_rsparam-field = iv_field.
    ls_exp_rsparam-value = iv_value.

    READ TABLE mt_rsparam INDEX iv_line
                          INTO ls_act_rsparam.

    cl_abap_unit_assert=>assert_equals(
      exp = ls_exp_rsparam
      act = ls_act_rsparam ).

  ENDMETHOD.

ENDCLASS.
