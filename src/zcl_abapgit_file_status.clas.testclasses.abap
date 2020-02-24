CLASS ltcl_run_checks DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_run_checks.

CLASS ltcl_run_checks DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          mo_dot     TYPE REF TO zcl_abapgit_dot_abapgit,
          mi_log     TYPE REF TO zif_abapgit_log.

    METHODS:
      append_result IMPORTING iv_obj_type TYPE trobjtype
                              iv_obj_name TYPE sobj_name
                              iv_match    TYPE abap_bool
                              iv_lstate   TYPE char1
                              iv_rstate   TYPE char1
                              iv_package  TYPE devclass
                              iv_path     TYPE string
                              iv_filename TYPE string,
      setup,
      positive FOR TESTING RAISING zcx_abapgit_exception,
      neg_diff_path_for_same_obj FOR TESTING RAISING zcx_abapgit_exception,
      neg_incorrect_path_vs_pack FOR TESTING RAISING zcx_abapgit_exception,
      neg_similar_filenames FOR TESTING RAISING zcx_abapgit_exception,
      neg_empty_filenames FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_run_checks IMPLEMENTATION.

  METHOD append_result.

    DATA ls_result LIKE LINE OF mt_results.

    ls_result-inactive = abap_false.

    ls_result-obj_type = iv_obj_type.
    ls_result-obj_name = iv_obj_name.
    ls_result-match    = iv_match.
    ls_result-lstate   = iv_lstate.
    ls_result-rstate   = iv_rstate.
    ls_result-package  = iv_package.
    ls_result-path     = iv_path.
    ls_result-filename = iv_filename.

    APPEND ls_result TO mt_results.

  ENDMETHOD.

  METHOD setup.

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
    mo_dot->set_starting_folder( '/' ).

  ENDMETHOD.

  METHOD positive.

    " 0 Positive
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zclass1.clas.abap' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zclass1.clas.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = 'ZDOMA1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zdoma1.doma.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = 'ZDOMA2'
                   iv_match    = ' '
                   iv_lstate   = 'M'
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zdoma2.doma.xml' ).


    zcl_abapgit_file_status=>run_checks(
      ii_log     = mi_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 0 ).

  ENDMETHOD.

  METHOD neg_diff_path_for_same_obj.

    " 1 Negative, different path for same object
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zclass1.clas.abap' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/sub'
                   iv_filename = 'zclass1.clas.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = 'ZDOMA1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zdoma1.doma.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = 'ZDOMA2'
                   iv_match    = ' '
                   iv_lstate   = 'M'
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zdoma2.doma.xml' ).

    zcl_abapgit_file_status=>run_checks(
      ii_log     = mi_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    " This one is not pure - incorrect path also triggers path vs package check
    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->has_rc( '1' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD neg_incorrect_path_vs_pack.

    " 2 Negative, incorrect path vs package
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '$$ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zclass1.clas.abap' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '$$ZCLASS1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zclass1.clas.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = '$$ZDOMA1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/sub'
                   iv_filename = '$$zdoma1.doma.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = '$$ZDOMA2'
                   iv_match    = ' '
                   iv_lstate   = 'M'
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zdoma2.doma.xml' ).

    zcl_abapgit_file_status=>run_checks(
      ii_log     = mi_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->has_rc( '2' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD neg_similar_filenames.

    " 3 Negative, similar filenames
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '$$ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zclass1.clas.abap' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '$$ZCLASS1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zclass1.clas.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = '$$ZDOMA1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zdoma1.doma.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = '$$ZDOMA2'
                   iv_match    = ' '
                   iv_lstate   = 'M'
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zdoma1.doma.xml' ).

    zcl_abapgit_file_status=>run_checks(
      ii_log     = mi_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->has_rc( '3' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD neg_empty_filenames.

    " 4 Negative, empty filenames
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '$$ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zclass1.clas.abap' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '$$ZCLASS1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '$$zclass1.clas.xml' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = '$$ZDOMA1'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = '' ).

    zcl_abapgit_file_status=>run_checks(
      ii_log     = mi_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->has_rc( '4' )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_status_result DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_results TYPE zif_abapgit_definitions=>ty_results_tt,
      get_line
        IMPORTING
          iv_line        TYPE i
        RETURNING
          VALUE(rs_data) TYPE zif_abapgit_definitions=>ty_result,
      assert_lines
        IMPORTING
          iv_lines TYPE i.

  PRIVATE SECTION.
    DATA: mt_results TYPE zif_abapgit_definitions=>ty_results_tt.

ENDCLASS.

CLASS lcl_status_result IMPLEMENTATION.

  METHOD constructor.

    mt_results = it_results.

  ENDMETHOD.

  METHOD get_line.

    READ TABLE mt_results INDEX iv_line INTO rs_data.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD assert_lines.

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_results )
      exp = iv_lines ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_status_helper DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_status_helper.

CLASS ltcl_status_helper DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_tadir.

    METHODS:
      constructor,
      add_tadir
        IMPORTING
          iv_obj_type TYPE tadir-object
          iv_obj_name TYPE tadir-obj_name
          iv_devclass TYPE tadir-devclass,
      add_remote
        IMPORTING
          iv_path     TYPE string DEFAULT '/'
          iv_filename TYPE string
          iv_sha1     TYPE zif_abapgit_definitions=>ty_sha1,
      add_local
        IMPORTING
          iv_path     TYPE string DEFAULT '/'
          iv_filename TYPE string
          iv_sha1     TYPE zif_abapgit_definitions=>ty_sha1
          iv_obj_type TYPE tadir-object
          iv_obj_name TYPE tadir-obj_name
          iv_devclass TYPE devclass DEFAULT '$Z$',
      add_state,
      run
        IMPORTING
          iv_devclass      TYPE devclass DEFAULT '$Z$'
        RETURNING
          VALUE(ro_result) TYPE REF TO lcl_status_result
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_tadir,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_tadir.

    DATA:
      mt_tadir  TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY,
      mt_local  TYPE zif_abapgit_definitions=>ty_files_item_tt,
      mt_remote TYPE zif_abapgit_definitions=>ty_files_tt,
      mt_state  TYPE zif_abapgit_definitions=>ty_file_signatures_tt.

ENDCLASS.

CLASS ltcl_status_helper IMPLEMENTATION.

  METHOD constructor.

    zcl_abapgit_injector=>set_tadir( me ).

  ENDMETHOD.

  METHOD add_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF mt_tadir.

    APPEND INITIAL LINE TO mt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-obj_type = iv_obj_type.
    <ls_tadir>-obj_name = iv_obj_name.
    <ls_tadir>-devclass = iv_devclass.

  ENDMETHOD.

  METHOD zif_abapgit_tadir~get_object_package.

    DATA: ls_tadir LIKE LINE OF mt_tadir.

    IF lines( mt_tadir ) > 0.
      READ TABLE mt_tadir INTO ls_tadir WITH KEY
        obj_type = iv_object
        obj_name = iv_obj_name.
      cl_abap_unit_assert=>assert_subrc( ).

      rv_devclass = ls_tadir-devclass.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_tadir~read.
    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD zif_abapgit_tadir~read_single.
    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD add_remote.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    APPEND INITIAL LINE TO mt_remote ASSIGNING <ls_remote>.
    <ls_remote>-path     = iv_path.
    <ls_remote>-filename = iv_filename.
    <ls_remote>-sha1     = iv_sha1.

  ENDMETHOD.

  METHOD add_local.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF mt_local.

    APPEND INITIAL LINE TO mt_local ASSIGNING <ls_local>.
    <ls_local>-item-obj_type = iv_obj_type.
    <ls_local>-item-obj_name = iv_obj_name.
    <ls_local>-item-devclass = iv_devclass.
    <ls_local>-file-path     = iv_path.
    <ls_local>-file-filename = iv_filename.
    <ls_local>-file-sha1     = iv_sha1.

  ENDMETHOD.

  METHOD add_state.

* todo
  ENDMETHOD.

  METHOD run.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_dot     TYPE REF TO zcl_abapgit_dot_abapgit.


    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    lt_results = zcl_abapgit_file_status=>calculate_status(
      iv_devclass  = iv_devclass
      io_dot       = lo_dot
      it_local     = mt_local
      it_remote    = mt_remote
      it_cur_state = mt_state ).

    CREATE OBJECT ro_result
      EXPORTING
        it_results = lt_results.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_calculate_status DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_calculate_status.

CLASS ltcl_calculate_status DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA:
      mo_helper TYPE REF TO ltcl_status_helper,
      mo_result TYPE REF TO lcl_status_result.

    METHODS:
      setup,
      only_remote FOR TESTING RAISING zcx_abapgit_exception,
      only_local FOR TESTING RAISING zcx_abapgit_exception,
      match FOR TESTING RAISING zcx_abapgit_exception,
      diff FOR TESTING RAISING zcx_abapgit_exception,
      local_outside_main FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_calculate_status IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_helper.

  ENDMETHOD.

  METHOD only_remote.

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'D1' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-rstate
      exp = zif_abapgit_definitions=>c_state-added ).

  ENDMETHOD.

  METHOD only_local.

    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'D1' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-lstate
      exp = zif_abapgit_definitions=>c_state-added ).

  ENDMETHOD.

  METHOD match.

    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'D1' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'D1' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-match
      exp = abap_true ).

  ENDMETHOD.

  METHOD diff.

    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '12345' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-lstate
      exp = zif_abapgit_definitions=>c_state-modified ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-rstate
      exp = zif_abapgit_definitions=>c_state-modified ).

  ENDMETHOD.

  METHOD local_outside_main.

    mo_helper->add_tadir(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_devclass = '$OUTSIDE$' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    " it should appear as not existing locally
    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-rstate
      exp = zif_abapgit_definitions=>c_state-added ).

  ENDMETHOD.

ENDCLASS.
