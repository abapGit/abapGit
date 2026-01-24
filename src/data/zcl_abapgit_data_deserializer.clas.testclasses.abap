CLASS ltcl_test DEFINITION DEFERRED.
CLASS lcl_cts_api_spy DEFINITION DEFERRED.
CLASS lcl_tadir_mock DEFINITION DEFERRED.
CLASS lcl_sap_package_mock DEFINITION DEFERRED.
CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS ltcl_test.


" ======================================================================
" Mock classes for testing
" ======================================================================

CLASS lcl_cts_api_spy DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_cts_api.
    DATA mv_create_transport_entries_called TYPE abap_bool.
ENDCLASS.


CLASS lcl_tadir_mock DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_tadir.
    METHODS constructor IMPORTING iv_package TYPE devclass.
  PRIVATE SECTION.
    DATA mv_package TYPE devclass.
ENDCLASS.


CLASS lcl_sap_package_mock DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_sap_package.
    METHODS constructor IMPORTING iv_is_transported TYPE abap_bool.
  PRIVATE SECTION.
    DATA mv_is_transported TYPE abap_bool.
ENDCLASS.


CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS preview_database_changes_ins FOR TESTING RAISING cx_static_check.
    METHODS preview_database_changes_upd FOR TESTING RAISING cx_static_check.
    METHODS preview_database_changes_del FOR TESTING RAISING cx_static_check.
    METHODS actualize_local_package FOR TESTING RAISING cx_static_check.
    METHODS actualize_transported_package FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test1.

    DATA li_cut TYPE REF TO zif_abapgit_data_deserializer.
    DATA li_config TYPE REF TO zif_abapgit_data_config.
    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.
    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = 'T100'.

    li_config->add_config( ls_config ).

    " this does not change the database. it just gives a preview of changes
    li_cut->deserialize(
      iv_package = ''
      ii_config  = li_config
      it_files   = lt_files ).

  ENDMETHOD.


  METHOD preview_database_changes_ins.

    CONSTANTS: lc_msgnr TYPE c LENGTH 3 VALUE '999'.

    DATA: li_cut     TYPE REF TO zcl_abapgit_data_deserializer,
          ls_config  TYPE zif_abapgit_data_config=>ty_config,
          lr_db_data TYPE REF TO data,
          lr_lc_data TYPE REF TO data,
          ls_t100    TYPE t100,
          ls_result  TYPE zif_abapgit_data_deserializer=>ty_result.

    FIELD-SYMBOLS: <lt_db_data> TYPE ANY TABLE,
                   <lt_lc_data> TYPE ANY TABLE,
                   <lg_ins>     TYPE ANY TABLE,
                   <ls_ins>     TYPE t100,
                   <lg_upd>     TYPE ANY TABLE,
                   <lg_del>     TYPE ANY TABLE.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = 'T100'.

    lr_db_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
    ASSIGN lr_db_data->* TO <lt_db_data>.
    lr_lc_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
    ASSIGN lr_lc_data->* TO <lt_lc_data>.

    " Create test data for INSERT
    ls_t100-sprsl = sy-langu.
    ls_t100-arbgb = 'AUNIT_ABAPGIT'.
    ls_t100-msgnr = lc_msgnr.
    ls_t100-text  = |abapGit aunit test|.
    INSERT ls_t100 INTO TABLE <lt_lc_data>.

    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.
    ls_result = li_cut->preview_database_changes(
      iv_name    = ls_config-name
      ir_db_data = lr_db_data
      ir_lc_data = lr_lc_data ).

    ASSIGN ls_result-inserts->* TO <lg_ins>.
    ASSIGN ls_result-updates->* TO <lg_upd>.
    ASSIGN ls_result-deletes->* TO <lg_del>.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( <lg_ins> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( <lg_upd> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( <lg_del> ) ).

    LOOP AT <lg_ins> ASSIGNING <ls_ins>.
      cl_abap_unit_assert=>assert_equals(
        exp = lc_msgnr
        act = <ls_ins>-msgnr ).
    ENDLOOP.

  ENDMETHOD.


  METHOD preview_database_changes_upd.

    CONSTANTS: lc_msgnr TYPE c LENGTH 3 VALUE '999'.

    DATA: li_cut     TYPE REF TO zcl_abapgit_data_deserializer,
          ls_config  TYPE zif_abapgit_data_config=>ty_config,
          lr_db_data TYPE REF TO data,
          lr_lc_data TYPE REF TO data,
          ls_t100    TYPE t100,
          ls_result  TYPE zif_abapgit_data_deserializer=>ty_result.

    FIELD-SYMBOLS: <lt_db_data> TYPE ANY TABLE,
                   <lt_lc_data> TYPE ANY TABLE,
                   <lg_ins>     TYPE ANY TABLE,
                   <lg_upd>     TYPE ANY TABLE,
                   <ls_upd>     TYPE t100,
                   <lg_del>     TYPE ANY TABLE.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = 'T100'.

    lr_db_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
    ASSIGN lr_db_data->* TO <lt_db_data>.
    lr_lc_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
    ASSIGN lr_lc_data->* TO <lt_lc_data>.

    " Create test data for UPDATE
    ls_t100-sprsl = sy-langu.
    ls_t100-arbgb = |AUNIT_ABAPGIT|.
    ls_t100-msgnr = lc_msgnr.
    ls_t100-text  = |abapGit aunit test|.
    INSERT ls_t100 INTO TABLE <lt_db_data>.

    ls_t100-sprsl = sy-langu.
    ls_t100-arbgb = 'AUNIT_ABAPGIT'.
    ls_t100-msgnr = lc_msgnr.
    ls_t100-text  = |abapGit aunit test UPDATE|.
    INSERT ls_t100 INTO TABLE <lt_lc_data>.

    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.
    ls_result = li_cut->preview_database_changes(
      iv_name    = ls_config-name
      ir_db_data = lr_db_data
      ir_lc_data = lr_lc_data ).

    ASSIGN ls_result-inserts->* TO <lg_ins>.
    ASSIGN ls_result-updates->* TO <lg_upd>.
    ASSIGN ls_result-deletes->* TO <lg_del>.

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( <lg_ins> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( <lg_upd> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( <lg_del> ) ).

    LOOP AT <lg_upd> ASSIGNING <ls_upd>.
      cl_abap_unit_assert=>assert_equals(
        exp = lc_msgnr
        act = <ls_upd>-msgnr ).
    ENDLOOP.

  ENDMETHOD.


  METHOD preview_database_changes_del.

    CONSTANTS: lc_msgnr TYPE c LENGTH 3 VALUE '999'.

    DATA: li_cut     TYPE REF TO zcl_abapgit_data_deserializer,
          ls_config  TYPE zif_abapgit_data_config=>ty_config,
          lr_db_data TYPE REF TO data,
          lr_lc_data TYPE REF TO data,
          ls_t100    TYPE t100,
          ls_result  TYPE zif_abapgit_data_deserializer=>ty_result.

    FIELD-SYMBOLS: <lt_db_data> TYPE ANY TABLE,
                   <lt_lc_data> TYPE ANY TABLE,
                   <lg_ins>     TYPE ANY TABLE,
                   <lg_upd>     TYPE ANY TABLE,
                   <lg_del>     TYPE ANY TABLE,
                   <ls_del>     TYPE t100.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = 'T100'.

    lr_db_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
    ASSIGN lr_db_data->* TO <lt_db_data>.
    lr_lc_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
    ASSIGN lr_lc_data->* TO <lt_lc_data>.

    " Create test data for DELETE
    ls_t100-sprsl = sy-langu.
    ls_t100-arbgb = 'AUNIT_ABAPGIT'.
    ls_t100-msgnr = lc_msgnr.
    ls_t100-text  = |abapGit aunit test DELETE|.
    INSERT ls_t100 INTO TABLE <lt_db_data>.

    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.
    ls_result = li_cut->preview_database_changes(
      iv_name    = ls_config-name
      ir_db_data = lr_db_data
      ir_lc_data = lr_lc_data ).

    ASSIGN ls_result-inserts->* TO <lg_ins>.
    ASSIGN ls_result-updates->* TO <lg_upd>.
    ASSIGN ls_result-deletes->* TO <lg_del>.

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( <lg_ins> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( <lg_upd> ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( <lg_del> ) ).

    LOOP AT <lg_del> ASSIGNING <ls_del>.
      cl_abap_unit_assert=>assert_equals(
        exp = lc_msgnr
        act = <ls_del>-msgnr ).
    ENDLOOP.

  ENDMETHOD.


  METHOD actualize_local_package.
    " Test that actualize does NOT create transport entries for tables in local packages

    CONSTANTS: lc_tabname TYPE tabname VALUE 'ZTESTLOCAL',
               lc_package TYPE devclass VALUE '$TMP'.

    DATA: li_cut              TYPE REF TO zcl_abapgit_data_deserializer,
          li_cts_api_spy      TYPE REF TO lcl_cts_api_spy,
          li_tadir_mock       TYPE REF TO lcl_tadir_mock,
          li_sap_package_mock TYPE REF TO zif_abapgit_sap_package,
          ls_checks           TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lt_result           TYPE zif_abapgit_data_deserializer=>ty_results,
          ls_result           TYPE zif_abapgit_data_deserializer=>ty_result,
          ls_overwrite        TYPE zif_abapgit_definitions=>ty_overwrite,
          lr_data             TYPE REF TO data,
          li_cts_api_clear    TYPE REF TO zif_abapgit_cts_api,
          li_tadir_clear      TYPE REF TO zif_abapgit_tadir,
          li_sap_package_clear TYPE REF TO zif_abapgit_sap_package.

    " Setup mocks
    CREATE OBJECT li_cts_api_spy.
    CREATE OBJECT li_tadir_mock EXPORTING iv_package = lc_package.
    CREATE OBJECT li_sap_package_mock TYPE lcl_sap_package_mock EXPORTING iv_is_transported = abap_false.

    " Inject mocks
    zcl_abapgit_injector=>set_cts_api( li_cts_api_spy ).
    zcl_abapgit_injector=>set_tadir( li_tadir_mock ).
    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lc_package
      ii_sap_package = li_sap_package_mock ).

    " Setup test data
    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.

    " Create dummy data for the result
    lr_data = zcl_abapgit_data_utils=>build_table_itab( lc_tabname ).

    ls_result-type    = zif_abapgit_data_config=>c_data_type-tabu.
    ls_result-name    = lc_tabname.
    ls_result-inserts = lr_data.
    ls_result-updates = lr_data.
    ls_result-deletes = lr_data.
    INSERT ls_result INTO TABLE lt_result.

    " Mark object for update
    ls_overwrite-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_overwrite-obj_name = lc_tabname.
    ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
    INSERT ls_overwrite INTO TABLE ls_checks-overwrite.

    ls_checks-transport-transport = 'DEVK000001'.

    " Act - call actualize
    TRY.
        li_cut->zif_abapgit_data_deserializer~actualize(
          is_checks = ls_checks
          it_result = lt_result ).
      CATCH zcx_abapgit_exception.
        " Ignore exceptions for this test - we're only checking if CTS was called
    ENDTRY.

    " Assert - verify create_transport_entries was NOT called
    cl_abap_unit_assert=>assert_equals(
      act = li_cts_api_spy->mv_create_transport_entries_called
      exp = abap_false
      msg = 'create_transport_entries should not be called for local packages' ).

    " Cleanup
    zcl_abapgit_injector=>set_cts_api( li_cts_api_clear ).
    zcl_abapgit_injector=>set_tadir( li_tadir_clear ).
    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lc_package
      ii_sap_package = li_sap_package_clear ).

  ENDMETHOD.


  METHOD actualize_transported_package.
    " Test that actualize DOES create transport entries for tables in transportable packages

    CONSTANTS: lc_tabname TYPE tabname VALUE 'ZTESTTRANS',
               lc_package TYPE devclass VALUE 'ZTEST'.

    DATA: li_cut              TYPE REF TO zcl_abapgit_data_deserializer,
          li_cts_api_spy      TYPE REF TO lcl_cts_api_spy,
          li_tadir_mock       TYPE REF TO lcl_tadir_mock,
          li_sap_package_mock TYPE REF TO zif_abapgit_sap_package,
          ls_checks           TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lt_result           TYPE zif_abapgit_data_deserializer=>ty_results,
          ls_result           TYPE zif_abapgit_data_deserializer=>ty_result,
          ls_overwrite        TYPE zif_abapgit_definitions=>ty_overwrite,
          lr_data             TYPE REF TO data,
          li_cts_api_clear    TYPE REF TO zif_abapgit_cts_api,
          li_tadir_clear      TYPE REF TO zif_abapgit_tadir,
          li_sap_package_clear TYPE REF TO zif_abapgit_sap_package.

    " Setup mocks
    CREATE OBJECT li_cts_api_spy.
    CREATE OBJECT li_tadir_mock EXPORTING iv_package = lc_package.
    CREATE OBJECT li_sap_package_mock TYPE lcl_sap_package_mock EXPORTING iv_is_transported = abap_true.

    " Inject mocks
    zcl_abapgit_injector=>set_cts_api( li_cts_api_spy ).
    zcl_abapgit_injector=>set_tadir( li_tadir_mock ).
    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lc_package
      ii_sap_package = li_sap_package_mock ).

    " Setup test data
    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.

    " Create dummy data for the result
    lr_data = zcl_abapgit_data_utils=>build_table_itab( lc_tabname ).

    ls_result-type    = zif_abapgit_data_config=>c_data_type-tabu.
    ls_result-name    = lc_tabname.
    ls_result-inserts = lr_data.
    ls_result-updates = lr_data.
    ls_result-deletes = lr_data.
    INSERT ls_result INTO TABLE lt_result.

    " Mark object for update
    ls_overwrite-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_overwrite-obj_name = lc_tabname.
    ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
    INSERT ls_overwrite INTO TABLE ls_checks-overwrite.

    ls_checks-transport-transport = 'DEVK000001'.

    " Act - call actualize
    TRY.
        li_cut->zif_abapgit_data_deserializer~actualize(
          is_checks = ls_checks
          it_result = lt_result ).
      CATCH zcx_abapgit_exception.
        " Ignore exceptions for this test - we're only checking if CTS was called
    ENDTRY.

    " Assert - verify create_transport_entries WAS called
    cl_abap_unit_assert=>assert_equals(
      act = li_cts_api_spy->mv_create_transport_entries_called
      exp = abap_true
      msg = 'create_transport_entries should be called for transportable packages' ).

    " Cleanup
    zcl_abapgit_injector=>set_cts_api( li_cts_api_clear ).
    zcl_abapgit_injector=>set_tadir( li_tadir_clear ).
    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lc_package
      ii_sap_package = li_sap_package_clear ).

  ENDMETHOD.

ENDCLASS.


" ======================================================================
" Mock class implementations
" ======================================================================

CLASS lcl_cts_api_spy IMPLEMENTATION.
  METHOD zif_abapgit_cts_api~create_transport_entries.
    mv_create_transport_entries_called = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~confirm_transport_messages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_r3tr_obj_for_limu_obj.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_transports_for_list.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_transport_for_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~insert_transport_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~is_chrec_possible_for_package.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~list_open_requests.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~list_r3tr_by_request.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_description.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_user.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~validate_transport_request.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~change_transport_type.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_request_and_tasks.
    RETURN.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_tadir_mock IMPLEMENTATION.
  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.

  METHOD zif_abapgit_tadir~get_object_package.
    rv_devclass = mv_package.
  ENDMETHOD.

  METHOD zif_abapgit_tadir~read.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_tadir~read_single.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_tadir~insert_single.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_tadir~delete_single.
    RETURN.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_sap_package_mock IMPLEMENTATION.
  METHOD constructor.
    mv_is_transported = iv_is_transported.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.
    rv_are_changes_rec_in_tr_req = mv_is_transported.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_type.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_subpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_child.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_local.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~exists.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_description.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_responsible.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~validate_name.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_default_transport_layer.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~check_object_type.
    RETURN.
  ENDMETHOD.
ENDCLASS.
