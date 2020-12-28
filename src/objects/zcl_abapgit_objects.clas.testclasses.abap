CLASS ltcl_object_types DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      not_exist FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_object_types IMPLEMENTATION.

  METHOD not_exist.

    DATA: ls_item   TYPE zif_abapgit_definitions=>ty_item,
          lv_exists TYPE abap_bool,
          lt_types  TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = zcl_abapgit_objects=>supported_list( ).

    cl_abap_unit_assert=>assert_not_initial( lt_types ).

    LOOP AT lt_types ASSIGNING <lv_type>.
      CLEAR ls_item.
      ls_item-obj_name = 'ZABAPGIT_FOOBAR'.
      ls_item-obj_type = <lv_type>.
      lv_exists = zcl_abapgit_objects=>exists( ls_item ).

      cl_abap_unit_assert=>assert_equals(
        act  = lv_exists
        exp  = abap_false
        msg  = ls_item-obj_type
        quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      check
        IMPORTING VALUE(is_item) TYPE zif_abapgit_definitions=>ty_item
        RAISING   zcx_abapgit_exception,
      serialize_tabl FOR TESTING RAISING zcx_abapgit_exception,
      serialize_shlp FOR TESTING RAISING zcx_abapgit_exception,
      serialize_view FOR TESTING RAISING zcx_abapgit_exception,
      serialize_auth FOR TESTING RAISING zcx_abapgit_exception,
      serialize_clas FOR TESTING RAISING zcx_abapgit_exception,
      serialize_doma FOR TESTING RAISING zcx_abapgit_exception,
      serialize_dtel FOR TESTING RAISING zcx_abapgit_exception,
      serialize_fugr FOR TESTING RAISING zcx_abapgit_exception,
      serialize_msag FOR TESTING RAISING zcx_abapgit_exception,
      serialize_prog FOR TESTING RAISING zcx_abapgit_exception,
      serialize_tran FOR TESTING RAISING zcx_abapgit_exception,
      serialize_ttyp FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_shlp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_view.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_tabl.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_auth.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'AUTH'.
    ls_item-obj_name = 'AREA'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_clas.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_GUI_FRONTEND_SERVICES'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_doma.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_dtel.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_fugr.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SRFC'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_msag.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'MSAG'.
    ls_item-obj_name = '00'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_prog.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPLWBABAP'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_tran.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TRAN'.
    ls_item-obj_name = 'SE38'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_ttyp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TTYP'.
    ls_item-obj_name = 'ABAPPROG'.

    check( ls_item ).

  ENDMETHOD.

  METHOD check.

    DATA: ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    ls_files_item = zcl_abapgit_objects=>serialize( is_item     = is_item
                                                    iv_language = zif_abapgit_definitions=>c_english ).

    cl_abap_unit_assert=>assert_not_initial( ls_files_item-files ).
    cl_abap_unit_assert=>assert_equals( act = ls_files_item-item
                                        exp = is_item ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_object_ddls_mock DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PRIVATE SECTION.
    DATA ms_item TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.

CLASS ltcl_object_ddls_mock IMPLEMENTATION.

  METHOD constructor.

    ms_item = is_item.

* dummy use of variable
    IF iv_language = 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    CASE ms_item-obj_name.
      WHEN 'Z_TEST_DDLS'.

        rv_is_locked = abap_true.

      WHEN 'Z_TEST_DDLS2'.

        rv_is_locked = abap_false.

    ENDCASE.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~get_comparator. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~delete. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~exists. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~jump. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~is_active. "##needed

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_check_objects_locked DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mt_given_items    TYPE zif_abapgit_definitions=>ty_items_tt,
      mv_exception_text TYPE string.

    METHODS:
      throw_excp_if_object_is_locked FOR TESTING RAISING cx_static_check,
      no_excp_if_obj_is_not_locked FOR TESTING RAISING cx_static_check,
      given_locked_object,
      when_check_objects_locked,
      then_exception_shd_be_raised,
      given_not_locked_object,
      then_no_exception_shd_occur,
      given_object
        IMPORTING
          iv_object_name TYPE string.

ENDCLASS.

CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS ltcl_check_objects_locked.

CLASS ltcl_check_objects_locked IMPLEMENTATION.

  METHOD throw_excp_if_object_is_locked.

    given_locked_object( ).
    when_check_objects_locked( ).
    then_exception_shd_be_raised( ).

  ENDMETHOD.

  METHOD no_excp_if_obj_is_not_locked.

    given_not_locked_object( ).
    when_check_objects_locked( ).
    then_no_exception_shd_occur( ).

  ENDMETHOD.

  METHOD given_locked_object.

    given_object( 'Z_TEST_DDLS' ).

  ENDMETHOD.


  METHOD when_check_objects_locked.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_objects=>check_objects_locked( iv_language = 'E'
                                                   it_items    = mt_given_items ).

      CATCH zcx_abapgit_exception INTO lx_error.
        mv_exception_text = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD then_exception_shd_be_raised.

    cl_abap_unit_assert=>assert_equals(
      exp = |Object DDLS Z_TEST_DDLS is locked. Action not possible.|
      act = mv_exception_text ).

  ENDMETHOD.


  METHOD given_not_locked_object.

    given_object( 'Z_TEST_DDLS2' ).

  ENDMETHOD.


  METHOD then_no_exception_shd_occur.

    cl_abap_unit_assert=>assert_initial( mv_exception_text ).

  ENDMETHOD.


  METHOD given_object.

    CONSTANTS:
      lc_obj_type TYPE string VALUE 'DDLS'.

    DATA:
      ls_item               LIKE LINE OF mt_given_items,
      ls_obj_serializer_map LIKE LINE OF zcl_abapgit_objects=>gt_obj_serializer_map.

    ls_item-obj_type = lc_obj_type.
    ls_item-obj_name = iv_object_name.
    INSERT ls_item INTO TABLE mt_given_items.

    ls_obj_serializer_map-item-obj_type = lc_obj_type.
    ls_obj_serializer_map-item-obj_name = iv_object_name.
    ls_obj_serializer_map-metadata-class = '\CLASS-POOL=ZCL_ABAPGIT_OBJECTS\CLASS=LTCL_OBJECT_DDLS_MOCK'.
    INSERT ls_obj_serializer_map INTO TABLE zcl_abapgit_objects=>gt_obj_serializer_map.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_filter_files_to_deser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_objects TYPE REF TO zcl_abapgit_objects,
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

CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS ltcl_filter_files_to_deser.

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

CLASS ltcl_adjust_namespaces DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      adjust_namespaces FOR TESTING RAISING cx_static_check.

    DATA:
      mo_objects TYPE REF TO zcl_abapgit_objects.

ENDCLASS.

CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS ltcl_adjust_namespaces.

CLASS ltcl_adjust_namespaces IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_objects.

  ENDMETHOD.

  METHOD adjust_namespaces.

    DATA: lt_input  TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_ouptut TYPE zif_abapgit_definitions=>ty_results_tt,
          ls_result LIKE LINE OF lt_input.

    ls_result-obj_name = |#SAP#ZTEST|.
    INSERT ls_result INTO TABLE lt_input.

    ls_result-obj_name = |ZTEST|.
    INSERT ls_result INTO TABLE lt_input.

    lt_ouptut = mo_objects->adjust_namespaces( lt_input ).

    READ TABLE lt_ouptut INTO ls_result INDEX 1.

    cl_abap_unit_assert=>assert_equals(
      exp = |/SAP/ZTEST|
      act = ls_result-obj_name ).

    READ TABLE lt_ouptut INTO ls_result INDEX 2.
    cl_abap_unit_assert=>assert_equals(
      exp = |ZTEST|
      act = ls_result-obj_name ).

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
      mo_objects          TYPE REF TO zcl_abapgit_objects,
      mt_input            TYPE zif_abapgit_definitions=>ty_results_tt,
      mt_output           TYPE zif_abapgit_definitions=>ty_results_tt,
      mv_exp_output_tabix TYPE i.

ENDCLASS.

CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS ltcl_prio_deserialization.

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

CLASS ltcl_warning_overwrite_find DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_objects   TYPE REF TO zcl_abapgit_objects,
      mt_result    TYPE zif_abapgit_definitions=>ty_results_tt,
      mt_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt.

    METHODS:
      setup,
      warning_overwrite_find_01 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_02 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_03 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_04 FOR TESTING RAISING cx_static_check,

      given_result
        IMPORTING
          iv_result_line TYPE string,

      when_warning_overwrite_find.

ENDCLASS.

CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS ltcl_warning_overwrite_find.

CLASS ltcl_warning_overwrite_find IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_objects.

  ENDMETHOD.

  METHOD warning_overwrite_find_01.

    " change remote but not local -> no overwrite
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;M| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_02.

    " change remote and local -> overwrite
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;M;M| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_03.

    " delete local -> overwrite (since object will be created again from remote)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;D;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_04.

    " exists local but not remote -> no overwrite
    " (object will be in delete confirmation popup: see ZCL_ABAPGIT_SERVICES_GIT->GET_UNNECESSARY_LOCAL_OBJS)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;A;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_overwrite ) ).

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


  METHOD when_warning_overwrite_find.

    mt_overwrite = mo_objects->warning_overwrite_find( mt_result ).

  ENDMETHOD.

ENDCLASS.
