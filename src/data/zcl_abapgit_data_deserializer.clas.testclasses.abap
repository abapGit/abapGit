CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS preview_database_changes_ins FOR TESTING RAISING cx_static_check.
    METHODS preview_database_changes_upd FOR TESTING RAISING cx_static_check.
    METHODS preview_database_changes_del FOR TESTING RAISING cx_static_check.

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
      ii_config = li_config
      it_files  = lt_files ).

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

ENDCLASS.
