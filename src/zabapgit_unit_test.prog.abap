*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UNIT_TEST
*&---------------------------------------------------------------------*

* todo, should the tests be in the same include as the classes
* they are testing?

CLASS ltcl_critical_tests DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_run_permission.
ENDCLASS.

CLASS ltcl_critical_tests IMPLEMENTATION.

  METHOD check_run_permission.
    DATA: lo_settings TYPE REF TO zcl_abapgit_settings.

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    "Objects will be created and deleted, do not run in customer system!
    "These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)

    IF lo_settings->get_run_critical_tests( ) = abap_false.
      cl_abap_unit_assert=>fail(
        msg   = 'Cancelled. You can enable these tests at the Settings page'
        level = if_aunit_constants=>tolerable ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION LONG FINAL.
* if this test class does not run, parameters in transaction SAUNIT_CLIENT_SETUP
* might need to be adjusted

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup.

    METHODS:
      run FOR TESTING
        RAISING zcx_abapgit_exception.

    CONSTANTS: c_package TYPE devclass VALUE '$ABAPGIT_UNIT_TEST'.

ENDCLASS.                    "ltcl_dangerous DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous IMPLEMENTATION.

  METHOD class_setup.
    ltcl_critical_tests=>check_run_permission( ).
  ENDMETHOD.                    "class_setup

  METHOD run.

    DATA: lo_repo    TYPE REF TO zcl_abapgit_repo_online,
          lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_msg     TYPE string,
          lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_types   TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <lv_type>   LIKE LINE OF lt_types.


    zcl_abapgit_sap_package=>create_local( c_package ).

    lt_types = zcl_abapgit_objects=>supported_list( ).

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = 'https://github.com/larshp/abapGit-Unit-Test.git'
      iv_branch_name = 'refs/heads/master'
      iv_package     = c_package ).
    lo_repo->status( ).
    lo_repo->deserialize( ).

    lt_tadir = zcl_abapgit_tadir=>read( c_package ).
    LOOP AT lt_types ASSIGNING <lv_type>.
      READ TABLE lt_tadir WITH KEY object = <lv_type> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        lv_msg = |Missing object type { <lv_type> }|.
        cl_abap_unit_assert=>fail(
            msg   = lv_msg
            level = if_aunit_constants=>tolerable
            quit  = if_aunit_constants=>no ).
      ENDIF.
    ENDLOOP.

    lt_results = lo_repo->status( ).
    LOOP AT lt_results ASSIGNING <ls_result> WHERE match = abap_false.
      lv_msg = |Does not match { <ls_result>-obj_type } { <ls_result>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    zcl_abapgit_objects=>delete( lt_tadir ).
    lt_tadir = zcl_abapgit_tadir=>read( c_package ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lv_msg = |Not deleted properly { <ls_tadir>-object } { <ls_tadir>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "run

ENDCLASS.                    "ltcl_dangerous IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      is_supported FOR TESTING,
      not_exist FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.                    "ltcl_object_types DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types IMPLEMENTATION.

  METHOD is_supported.

    DATA: ls_item      TYPE zif_abapgit_definitions=>ty_item,
          lv_supported TYPE abap_bool,
          lt_types     TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = zcl_abapgit_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.

      CLEAR ls_item.
      ls_item-obj_type = <lv_type>.
      lv_supported = zcl_abapgit_objects=>is_supported( ls_item ).

      cl_abap_unit_assert=>assert_equals(
          act  = lv_supported
          exp  = abap_true
          msg  = ls_item-obj_type
          quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.                    "is_supported

  METHOD not_exist.

    DATA: ls_item   TYPE zif_abapgit_definitions=>ty_item,
          lv_exists TYPE abap_bool,
          lt_types  TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = zcl_abapgit_objects=>supported_list( ).

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

  ENDMETHOD.                    "not_exist

ENDCLASS.                    "ltcl_object_types IMPLEMENTATION


CLASS ltcl_persistence_settings DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      clear_settings_database,
      "Proxy
      modify_settings_proxy_url     FOR TESTING RAISING cx_static_check,
      modify_settings_proxy_port    FOR TESTING RAISING cx_static_check,
      read_proxy_settings           FOR TESTING RAISING cx_static_check,
      read_not_found_url            FOR TESTING RAISING cx_static_check,
      read_not_found_port           FOR TESTING RAISING cx_static_check,
      "Run critical tests
      modify_run_critical_tests     FOR TESTING RAISING cx_static_check,
      read_run_critical_tests       FOR TESTING RAISING cx_static_check,
      read_not_found_critical_tests FOR TESTING RAISING cx_static_check.
    DATA:
      mo_persistence_settings TYPE REF TO zcl_abapgit_persist_settings,
      mo_settings             TYPE REF TO zcl_abapgit_settings.
ENDCLASS.

CLASS ltcl_persistence_settings IMPLEMENTATION.

  METHOD class_setup.
    ltcl_critical_tests=>check_run_permission( ).
  ENDMETHOD.

  METHOD setup.
    mo_persistence_settings = zcl_abapgit_persist_settings=>get_instance( ).
    CREATE OBJECT mo_settings.
    clear_settings_database( ).
  ENDMETHOD.

  METHOD modify_settings_proxy_url.
    DATA lv_proxy_url TYPE string.

    mo_settings->set_proxy_url( 'http://proxy' ).

    mo_persistence_settings->modify( mo_settings ).

    lv_proxy_url = zcl_abapgit_persistence_db=>get_instance( )->read(
      iv_type  = 'SETTINGS'
      iv_value = 'PROXY_URL' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_proxy_url
      exp = 'http://proxy' ).
  ENDMETHOD.

  METHOD modify_settings_proxy_port.
    DATA lv_proxy_port TYPE string.
    mo_settings->set_proxy_port( '8080' ).

    mo_persistence_settings->modify( mo_settings ).

    lv_proxy_port = zcl_abapgit_persistence_db=>get_instance( )->read(
      iv_type  = 'SETTINGS'
      iv_value = 'PROXY_PORT' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_proxy_port
      exp = '8080' ).
  ENDMETHOD.

  METHOD read_proxy_settings.
    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = 'SETTINGS'
      iv_value      = 'PROXY_URL'
      iv_data       = 'A_URL' ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = 'SETTINGS'
      iv_value      = 'PROXY_PORT'
      iv_data       = '1000' ).

    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_url( )
      exp = 'A_URL' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_port( )
      exp = '1000' ).
  ENDMETHOD.

  METHOD read_not_found_port.
    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_port( )
      exp = '' ).
  ENDMETHOD.

  METHOD read_not_found_url.

    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_proxy_url( )
      exp = '' ).
  ENDMETHOD.

  METHOD modify_run_critical_tests.
    DATA lv_run_critical_tests TYPE abap_bool.
    mo_settings->set_run_critical_tests( abap_true ).

    mo_persistence_settings->modify( mo_settings ).

    lv_run_critical_tests = zcl_abapgit_persistence_db=>get_instance( )->read(
      iv_type  = 'SETTINGS'
      iv_value = 'CRIT_TESTS' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_run_critical_tests
      exp = abap_true ).
  ENDMETHOD.

  METHOD read_run_critical_tests.
    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = 'SETTINGS'
      iv_value      = 'CRIT_TESTS'
      iv_data       = 'X' ).
    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_run_critical_tests( )
      exp = abap_true ).
  ENDMETHOD.
  METHOD read_not_found_critical_tests.
    mo_settings = mo_persistence_settings->read( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_settings->get_run_critical_tests( )
      exp = abap_false ).
  ENDMETHOD.


  METHOD clear_settings_database.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type       = 'SETTINGS'
          iv_value      = 'PROXY_URL' ).
      CATCH cx_static_check ##NO_HANDLER.
        "If entry didn't exist, that's okay
    ENDTRY.
    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type       = 'SETTINGS'
          iv_value      = 'PROXY_PORT' ).
      CATCH cx_static_check ##NO_HANDLER.
        "If entry didn't exist, that's okay
    ENDTRY.
    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type       = 'SETTINGS'
          iv_value      = 'CRIT_TESTS' ).
      CATCH cx_static_check ##NO_HANDLER.
        "If entry didn't exist, that's okay
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*CLASS ltcl_oo_serialize DEFINITION FINAL FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    METHODS:
*      setup,
*      empty_include FOR TESTING RAISING cx_static_check,
*      one_line_include FOR TESTING RAISING cx_static_check,
*      one_line_include_2 FOR TESTING RAISING cx_static_check,
*      one_line_include_3 FOR TESTING RAISING cx_static_check,
*      two_line_include FOR TESTING RAISING cx_static_check,
*      two_line_include_2 FOR TESTING RAISING cx_static_check,
*      two_line_include_3 FOR TESTING RAISING cx_static_check,
*      more_than_two_lines FOR TESTING RAISING cx_static_check,
*
*      _given_source_is
*        IMPORTING
*          i_source TYPE LINE OF zif_abapgit_definitions=>ty_string_tt,
*      _given_empty_test_include,
*      _when_skip_is_calculated,
*      _then_should_be_skipped,
*      _then_should_not_be_skipped.
*
*    DATA: mo_oo_serializer  TYPE REF TO lcl_oo_serializer,
*          mt_source         TYPE zif_abapgit_definitions=>ty_string_tt,
*          mv_skip_testclass TYPE abap_bool.
*
*ENDCLASS.
*
*
*CLASS ltcl_oo_serialize IMPLEMENTATION.
*
*  METHOD setup.
*
*    CREATE OBJECT mo_oo_serializer.
*
*  ENDMETHOD.
*
*  METHOD empty_include.
*
*    _given_empty_test_include( ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD one_line_include.
*
*    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD one_line_include_2.
*
*    _given_source_is( `*` ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD one_line_include_3.
*
*    _given_source_is( `write: 'This is ABAP'.` ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_not_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD two_line_include.
*
*    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
*    _given_source_is( ``                                                         ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD two_line_include_2.
*
*    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
*    _given_source_is( `write: 'This is ABAP'.`                                   ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_not_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD two_line_include_3.
*
*    _given_source_is( ` `                                                        ).
*    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_not_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD more_than_two_lines.
*
*    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
*    _given_source_is( `CLASS ltcl_test DEFINITION FINAL FOR TESTING`             ).
*    _given_source_is( `  DURATION SHORT`                                         ).
*    _given_source_is( `  RISK LEVEL HARMLESS.`                                   ).
*    _given_source_is( ` `                                                        ).
*    _given_source_is( `  PRIVATE SECTION.`                                       ).
*    _given_source_is( `    METHODS:`                                             ).
*    _given_source_is( `      first_test FOR TESTING RAISING cx_static_check.`    ).
*    _given_source_is( `ENDCLASS.`                                                ).
*    _given_source_is( ` `                                                        ).
*    _given_source_is( `CLASS ltcl_test IMPLEMENTATION.`                          ).
*    _given_source_is( ` `                                                        ).
*    _given_source_is( `  METHOD first_test.`                                     ).
*    _given_source_is( `    cl_abap_unit_assert=>fail( 'This is a real test' ).`  ).
*    _given_source_is( `  ENDMETHOD.`                                             ).
*    _given_source_is( ` `                                                        ).
*    _given_source_is( `ENDCLASS.`                                                ).
*
*    _when_skip_is_calculated( ).
*
*    _then_should_not_be_skipped( ).
*
*  ENDMETHOD.
*
*  METHOD _given_source_is.
*
*    INSERT i_source INTO TABLE mt_source.
*
*  ENDMETHOD.
*
*  METHOD _given_empty_test_include.
*
*  ENDMETHOD.
*
*  METHOD _when_skip_is_calculated.
*
*    mv_skip_testclass = mo_oo_serializer->calculate_skip_testclass( mt_source ).
*
*  ENDMETHOD.
*
*  METHOD _then_should_be_skipped.
*
*    cl_abap_unit_assert=>assert_equals(
*      act = mv_skip_testclass
*      exp = abap_true
*      msg = |Testclass should be skipped| ).
*
*  ENDMETHOD.
*
*
*  METHOD _then_should_not_be_skipped.
*
*    cl_abap_unit_assert=>assert_equals(
*      act = mv_skip_testclass
*      exp = abap_false
*      msg = |Testclass should not be skipped| ).
*
*  ENDMETHOD.
*
*ENDCLASS.

INCLUDE zabapgit_unit_test_clas_intf.
