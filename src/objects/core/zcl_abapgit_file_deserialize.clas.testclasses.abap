CLASS ltcl_filter_files_to_deser DEFINITION DEFERRED.
CLASS ltcl_prio_deserialization DEFINITION DEFERRED.

CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS ltcl_filter_files_to_deser.
CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS ltcl_prio_deserialization.

CLASS ltcl_filter_files_to_deser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_objects TYPE REF TO zcl_abapgit_file_deserialize,
      mt_result  TYPE zif_abapgit_definitions=>ty_results_tt.

    METHODS:
      setup,
      filter_duplicates FOR TESTING RAISING cx_static_check,
      filter_duplicates_rstate FOR TESTING RAISING cx_static_check,
      filter_duplicates_lstate FOR TESTING RAISING cx_static_check,
      filter_duplicates_match FOR TESTING RAISING cx_static_check,
      filter_duplicates_init_objtype FOR TESTING RAISING cx_static_check,
      filter_duplicates_changes_01 FOR TESTING RAISING cx_static_check,
      filter_duplicates_changes_02 FOR TESTING RAISING cx_static_check,
      filter_duplicates_deleted FOR TESTING RAISING cx_static_check,

      given_result
        IMPORTING
          iv_result_line TYPE string,

      when_filter_is_applied.

ENDCLASS.

CLASS ltcl_filter_files_to_deser IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_objects.

  ENDMETHOD.

  METHOD filter_duplicates.

    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.abap;;;;| ).
    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.xml;;;;| ).

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_result ) ).

  ENDMETHOD.


  METHOD filter_duplicates_rstate.

    DATA: ls_exp LIKE LINE OF mt_result,
          ls_act LIKE LINE OF mt_result.

    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.abap;;;;| ).
    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.xml;;;;A| ).

    READ TABLE mt_result INDEX 2 INTO ls_exp.

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_result ) ).

    READ TABLE mt_result INDEX 1 INTO ls_act.

    cl_abap_unit_assert=>assert_equals(
      exp = ls_exp
      act = ls_act ).

  ENDMETHOD.


  METHOD filter_duplicates_lstate.

    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.abap;;;A;| ).
    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.xml;;;A;| ).

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_result ) ).

  ENDMETHOD.


  METHOD filter_duplicates_match.

    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.abap;;X;;| ).
    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.xml;;X;;| ).

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_result ) ).

  ENDMETHOD.


  METHOD filter_duplicates_init_objtype.

    given_result( |;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.abap;;;;| ).
    given_result( |;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.xml;;;;| ).

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_result ) ).

  ENDMETHOD.

  METHOD filter_duplicates_changes_01.

    DATA: ls_exp LIKE LINE OF mt_result,
          ls_act LIKE LINE OF mt_result.

    " test different order since SORT object,obj_name is non-deterministic
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;M;M| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.testclasses.abap;;;;M| ).

    READ TABLE mt_result INDEX 1 INTO ls_exp.

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_result ) ).

    READ TABLE mt_result INDEX 1 INTO ls_act.

    " expect M,M
    cl_abap_unit_assert=>assert_equals(
      exp = ls_exp
      act = ls_act ).

  ENDMETHOD.

  METHOD filter_duplicates_changes_02.

    DATA: ls_exp LIKE LINE OF mt_result,
          ls_act LIKE LINE OF mt_result.

    " test different order since SORT object,obj_name is non-deterministic
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;M| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.testclasses.abap;;;M;M| ).

    READ TABLE mt_result INDEX 2 INTO ls_exp.

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_result ) ).

    READ TABLE mt_result INDEX 1 INTO ls_act.

    " expect M,M
    cl_abap_unit_assert=>assert_equals(
      exp = ls_exp
      act = ls_act ).

  ENDMETHOD.

  METHOD filter_duplicates_deleted.

    given_result( |PROG;ZAG_UNIT_TEST;;/src/;zag_unit_test.prog.xml;;;;D| ).

    when_filter_is_applied( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_result ) ).

  ENDMETHOD.

  METHOD given_result.

    DATA: ls_result LIKE LINE OF mt_result.

    SPLIT iv_result_line
      AT ';'
      INTO ls_result-obj_type
           ls_result-obj_name
           ls_result-inactive
           ls_result-path
           ls_result-filename
           ls_result-package
           ls_result-match
           ls_result-lstate
           ls_result-rstate.

    INSERT ls_result INTO TABLE mt_result.

  ENDMETHOD.


  METHOD when_filter_is_applied.

    mt_result = mo_objects->filter_files_to_deserialize( mt_result ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_prio_deserialization DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      ddls_before_dcls FOR TESTING RAISING cx_static_check,

      given
        IMPORTING
          iv_object_type TYPE trobjtype,
      when_deser_is_priorized,
      then
        IMPORTING
          iv_exp_object_type TYPE trobjtype.

    DATA:
      mo_objects          TYPE REF TO zcl_abapgit_file_deserialize,
      mt_input            TYPE zif_abapgit_definitions=>ty_results_tt,
      mt_output           TYPE zif_abapgit_definitions=>ty_results_tt,
      mv_exp_output_tabix TYPE i.

ENDCLASS.

CLASS ltcl_prio_deserialization IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_objects.
    mv_exp_output_tabix = 0.

  ENDMETHOD.

  METHOD ddls_before_dcls.

    given( 'DCLS' ).
    given( 'DDLS' ).
    given( 'DCLS' ).
    given( 'DDLS' ).

    when_deser_is_priorized( ).

    then( 'DDLS' ).
    then( 'DDLS' ).
    then( 'DCLS' ).
    then( 'DCLS' ).

  ENDMETHOD.


  METHOD given.

    DATA: ls_input LIKE LINE OF mt_input.

    ls_input-obj_type = iv_object_type.
    INSERT ls_input INTO TABLE mt_input.

  ENDMETHOD.


  METHOD when_deser_is_priorized.

    mt_output = mo_objects->prioritize_deser( mt_input ).

  ENDMETHOD.


  METHOD then.

    DATA: ls_output LIKE LINE OF mt_output.

    mv_exp_output_tabix = mv_exp_output_tabix + 1.

    READ TABLE mt_output INTO ls_output INDEX mv_exp_output_tabix.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp_object_type
      act = ls_output-obj_type ).

  ENDMETHOD.

ENDCLASS.
