CLASS ltcl_run_checks DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_run_checks.

CLASS ltcl_util DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    CLASS-METHODS check_contains
      IMPORTING
        ii_log     TYPE REF TO zif_abapgit_log
        iv_pattern TYPE string.
ENDCLASS.

CLASS ltcl_util IMPLEMENTATION.
  METHOD check_contains.
    DATA lt_messages TYPE zif_abapgit_log=>ty_log_outs.
    DATA ls_message LIKE LINE OF lt_messages.
    DATA lv_contains TYPE abap_bool.

    lt_messages = ii_log->get_messages( ).
    LOOP AT lt_messages INTO ls_message.
      IF ls_message-text CP iv_pattern.
        lv_contains = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = lv_contains
      exp = abap_true ).

  ENDMETHOD.
ENDCLASS.

CLASS ltcl_run_checks DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_sap_package.
    INTERFACES zif_abapgit_sap_namespace.

  PRIVATE SECTION.
    DATA: mt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
          mo_instance TYPE REF TO lcl_status_consistency_checks,
          mo_dot      TYPE REF TO zcl_abapgit_dot_abapgit,
          mi_log      TYPE REF TO zif_abapgit_log.

    METHODS:
      append_result IMPORTING iv_obj_type TYPE trobjtype
                              iv_obj_name TYPE sobj_name
                              iv_match    TYPE abap_bool
                              iv_lstate   TYPE char1
                              iv_rstate   TYPE char1
                              iv_package  TYPE devclass
                              iv_path     TYPE string
                              iv_filename TYPE string
                              iv_packmove TYPE abap_bool OPTIONAL,
      setup,
      positive FOR TESTING RAISING zcx_abapgit_exception,
      neg_diff_path_for_same_obj FOR TESTING RAISING zcx_abapgit_exception,
      neg_incorrect_path_vs_pack FOR TESTING RAISING zcx_abapgit_exception,
      neg_similar_filenames FOR TESTING RAISING zcx_abapgit_exception,
      neg_empty_filenames FOR TESTING RAISING zcx_abapgit_exception,
      package_move FOR TESTING RAISING zcx_abapgit_exception,
      check_namespace FOR TESTING RAISING zcx_abapgit_exception,
      check_namespace_aff FOR TESTING RAISING zcx_abapgit_exception,
      check_sub_package FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_run_checks IMPLEMENTATION.

  METHOD zif_abapgit_sap_package~validate_name.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_responsible.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_description.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_subpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent.
    rv_parentcl = '$MAIN'.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_child.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~exists.
    rv_bool = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_type.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_layer.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_local.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_namespace~exists.
    rv_yes = boolc( iv_namespace <> 'NOTEXIST' ).
  ENDMETHOD.

  METHOD zif_abapgit_sap_namespace~is_editable.
  ENDMETHOD.

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
    ls_result-packmove = iv_packmove.

    APPEND ls_result TO mt_results.

  ENDMETHOD.

  METHOD setup.

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
    mo_dot->set_starting_folder( '/' ).  " assumed by unit tests

    zcl_abapgit_injector=>set_sap_package( iv_package     = '$MAIN'
                                           ii_sap_package = me ).

    zcl_abapgit_injector=>set_sap_package( iv_package     = '$MAIN_SUB'
                                           ii_sap_package = me ).

    zcl_abapgit_injector=>set_sap_namespace( me ).

    CREATE OBJECT mo_instance
      EXPORTING
        iv_root_package = '$Z$'
        io_dot          = mo_dot.

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

    mi_log = mo_instance->run_checks( mt_results ).

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

    mi_log = mo_instance->run_checks( mt_results ).

    " This one is not pure - incorrect path also triggers path vs package check
    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 2 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Files for object *| ).

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

    mi_log = mo_instance->run_checks( mt_results ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Package and path do not match for object*| ).

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

    mi_log = mo_instance->run_checks( mt_results ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Multiple files with same filename, *| ).

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

    mi_log = mo_instance->run_checks( mt_results ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Filename is empty for object *| ).

  ENDMETHOD.

  METHOD package_move.

    " 5 Changed package assignment
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zclass1.clas.abap'
                   iv_packmove = 'X' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = 'A'
                   iv_rstate   = ' '
                   iv_package  = '$Z$SUB'
                   iv_path     = '/sub'
                   iv_filename = 'zclass1.clas.abap'
                   iv_packmove = 'X' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zclass1.clas.xml'
                   iv_packmove = 'X' ).

    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = 'ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = 'A'
                   iv_rstate   = ' '
                   iv_package  = '$Z$SUB'
                   iv_path     = '/sub'
                   iv_filename = 'zclass1.clas.xml'
                   iv_packmove = 'X' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = 'ZDOMA1'
                   iv_match    = ' '
                   iv_lstate   = 'A'
                   iv_rstate   = ' '
                   iv_package  = '$Z$'
                   iv_path     = '/'
                   iv_filename = 'zdoma1.doma.xml'
                   iv_packmove = 'X' ).

    append_result( iv_obj_type = 'DOMA'
                   iv_obj_name = 'ZDOMA2'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '$Z$SUB'
                   iv_path     = '/sub'
                   iv_filename = 'zdoma1.doma.xml'
                   iv_packmove = 'X' ).

    mi_log = mo_instance->run_checks( mt_results ).

    " Three files, but only two msg (for two changed objects)
    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 2 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Changed package assignment for object*| ).

  ENDMETHOD.

  METHOD check_namespace.

    " 6 Missing namespace
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '/NOTEXIST/ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '/NOTEXIST/Z'
                   iv_path     = '/'
                   iv_filename = '#notexist#zclass1.clas.xml' ).

    CREATE OBJECT mo_instance
      EXPORTING
        iv_root_package = '/NOTEXIST/Z'
        io_dot          = mo_dot.

    mi_log = mo_instance->run_checks( mt_results ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Namespace *| ).

  ENDMETHOD.

  METHOD check_namespace_aff.

    " 6 Missing namespace
    append_result( iv_obj_type = 'CLAS'
                   iv_obj_name = '/NOTEXIST/ZCLASS1'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'A'
                   iv_package  = '/NOTEXIST/Z'
                   iv_path     = '/'
                   iv_filename = '(notexist)zclass1.clas.json' ).

    CREATE OBJECT mo_instance
      EXPORTING
        iv_root_package = '/NOTEXIST/Z'
        io_dot          = mo_dot.

    mi_log = mo_instance->run_checks( mt_results ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Namespace *| ).

  ENDMETHOD.

  METHOD check_sub_package.

    append_result( iv_obj_type = 'DEVC'
                   iv_obj_name = '$MAIN'
                   iv_match    = 'X'
                   iv_lstate   = ' '
                   iv_rstate   = ' '
                   iv_package  = '$MAIN'
                   iv_path     = '/'
                   iv_filename = 'package.devc.xml' ).

    append_result( iv_obj_type = 'DEVC'
                   iv_obj_name = '$MAIN_SUB'
                   iv_match    = ' '
                   iv_lstate   = ' '
                   iv_rstate   = 'X'
                   iv_package  = ''
                   iv_path     = ''
                   iv_filename = 'package.devc.xml' ).

    CREATE OBJECT mo_instance
      EXPORTING
        iv_root_package = '$MAIN'
        io_dot          = mo_dot.

    mi_log = mo_instance->run_checks( mt_results ).

    cl_abap_unit_assert=>assert_equals(
      act = mi_log->count( )
      exp = 1 ).

    ltcl_util=>check_contains(
      ii_log     = mi_log
      iv_pattern = |Package $MAIN_SUB already exists but is not a sub-package of $MAIN*| ).

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
          iv_lines TYPE i
          iv_msg   TYPE csequence OPTIONAL.

  PRIVATE SECTION.
    DATA: mt_results TYPE zif_abapgit_definitions=>ty_results_tt.

ENDCLASS.

CLASS lcl_status_result IMPLEMENTATION.

  METHOD constructor.

    mt_results = it_results.
    SORT mt_results BY path filename.

  ENDMETHOD.

  METHOD get_line.

    READ TABLE mt_results INDEX iv_line INTO rs_data.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD assert_lines.

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_results )
      exp = iv_lines
      msg = iv_msg ).

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
          iv_sha1     TYPE zif_abapgit_git_definitions=>ty_sha1,
      add_local
        IMPORTING
          iv_path     TYPE string DEFAULT '/'
          iv_filename TYPE string
          iv_sha1     TYPE zif_abapgit_git_definitions=>ty_sha1
          iv_obj_type TYPE tadir-object OPTIONAL
          iv_obj_name TYPE tadir-obj_name OPTIONAL
          iv_devclass TYPE devclass DEFAULT '$Z$'
          iv_inactive TYPE abap_bool DEFAULT abap_false,
      add_state
        IMPORTING
          iv_path     TYPE string DEFAULT '/'
          iv_filename TYPE string
          iv_sha1     TYPE zif_abapgit_git_definitions=>ty_sha1,
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
      mt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt,
      mt_state  TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt.

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
    <ls_local>-item-inactive = iv_inactive.
    <ls_local>-file-path     = iv_path.
    <ls_local>-file-filename = iv_filename.
    <ls_local>-file-sha1     = iv_sha1.

  ENDMETHOD.

  METHOD add_state.

    FIELD-SYMBOLS: <ls_state> LIKE LINE OF mt_state.

    APPEND INITIAL LINE TO mt_state ASSIGNING <ls_state>.
    <ls_state>-path     = iv_path.
    <ls_state>-filename = iv_filename.
    <ls_state>-sha1     = iv_sha1.

  ENDMETHOD.

  METHOD run.

    DATA: lt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_instance TYPE REF TO zcl_abapgit_file_status,
          lo_dot      TYPE REF TO zcl_abapgit_dot_abapgit.

    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot->set_starting_folder( '/' ). " assumed by unit tests

    CREATE OBJECT lo_instance
      EXPORTING
        iv_root_package = iv_devclass
        io_dot          = lo_dot.

    lt_results = lo_instance->calculate_status(
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
      complete_local,
      complete_remote,
      complete_state,
      only_remote FOR TESTING RAISING zcx_abapgit_exception,
      deleted_remote FOR TESTING RAISING zcx_abapgit_exception,
      only_local FOR TESTING RAISING zcx_abapgit_exception,
      match_file FOR TESTING RAISING zcx_abapgit_exception,
      diff FOR TESTING RAISING zcx_abapgit_exception,
      moved FOR TESTING RAISING zcx_abapgit_exception,
      inactive FOR TESTING RAISING zcx_abapgit_exception,
      local_outside_main FOR TESTING RAISING zcx_abapgit_exception,
      complete FOR TESTING RAISING zcx_abapgit_exception,
      only_local2 FOR TESTING RAISING zcx_abapgit_exception,
      only_remote2 FOR TESTING RAISING zcx_abapgit_exception,
      only_remote3 FOR TESTING RAISING zcx_abapgit_exception.
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

  METHOD deleted_remote.

    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotel.prog.xml'
      iv_sha1     = '1011' ).

* this remote has to be there, even tho its not related
* SUBRC = 4 vs SUBRC = 8 during READ TABLE
    mo_helper->add_remote(
      iv_path     = '/'
      iv_filename = 'zzz.xml'
      iv_sha1     = '1017' ).

    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotel.prog.xml'
      iv_sha1     = '1011' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 2 )-rstate
      exp = zif_abapgit_definitions=>c_state-deleted ).

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

  METHOD match_file.

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

    " Modified both
    CREATE OBJECT mo_helper.
    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '12345' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_helper->add_state(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'xxx' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-lstate
      exp = zif_abapgit_definitions=>c_state-modified ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-rstate
      exp = zif_abapgit_definitions=>c_state-modified ).

    " Modified local only
    CREATE OBJECT mo_helper.
    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '12345' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_helper->add_state(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-lstate
      exp = zif_abapgit_definitions=>c_state-modified ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-rstate
      exp = zif_abapgit_definitions=>c_state-unchanged ).

    " Modified remote only
    CREATE OBJECT mo_helper.
    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '12345' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_helper->add_state(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '12345' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-lstate
      exp = zif_abapgit_definitions=>c_state-unchanged ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-rstate
      exp = zif_abapgit_definitions=>c_state-modified ).

  ENDMETHOD.

  METHOD moved.

    mo_helper->add_local(
     iv_obj_type = 'DOMA'
     iv_obj_name = '$$ZDOMA1'
     iv_filename = '$$zdoma1.doma.xml'
     iv_path     = '/foo/'
     iv_devclass = 'FOO'
     iv_sha1     = 'D1' ).

    mo_helper->add_remote(
     iv_filename = '$$zdoma1.doma.xml'
     iv_path     = '/bar/'
     iv_sha1     = 'D1' ).

    mo_helper->add_tadir(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_devclass = 'FOO' ).

    mo_result = mo_helper->run( iv_devclass = 'FOO' ).

    mo_result->assert_lines(
      iv_lines = 2
      iv_msg   = 'there must be a status calculated for both files, they are in different folders' ).

  ENDMETHOD.

  METHOD inactive.

    mo_helper->add_local(
      iv_obj_type = 'DOMA'
      iv_obj_name = '$$ZDOMA1'
      iv_inactive = abap_true
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '12345' ).

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = '54321' ).

    mo_helper->add_state(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'xxx' ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-inactive
      exp = abap_true ).

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

  METHOD complete_local.

    mo_helper->add_local(
      iv_path     = '/'
      iv_filename = '.abapgit.xml'
      iv_sha1     = '1017' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_created_locally.prog.abap'
      iv_sha1     = '1001' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_created_locally.prog.xml'
      iv_sha1     = '1022' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotely.prog.abap'
      iv_sha1     = '1016' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotely.prog.xml'
      iv_sha1     = '1003' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_both.prog.abap'
      iv_sha1     = '1028' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_both.prog.xml'
      iv_sha1     = '1032' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_locally.prog.abap'
      iv_sha1     = '1023' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_locally.prog.xml'
      iv_sha1     = '1033' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_remotely.prog.abap'
      iv_sha1     = '1018' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_remotely.prog.xml'
      iv_sha1     = '1011' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_mod_del.prog.abap'
      iv_sha1     = '1012' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_mod_del.prog.xml'
      iv_sha1     = '1006' ).
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '1027' ).
    mo_helper->add_local(
      iv_path     = '/src/sub/'
      iv_filename = 'ztest_move_package.prog.xml'
      iv_sha1     = '1040' ).
    mo_helper->add_local(
      iv_path     = '/src/sub/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '1041' ).
    mo_helper->add_local(
      iv_path     = '/src/sub/'
      iv_filename = 'ztest_move_package_w_change.prog.xml'
      iv_sha1     = '1042' ).

  ENDMETHOD.

  METHOD complete_remote.

    mo_helper->add_remote(
      iv_path     = '/'
      iv_filename = '.abapgit.xml'
      iv_sha1     = '1017' ).
    mo_helper->add_remote(
      iv_path     = '/'
      iv_filename = 'README.md'
      iv_sha1     = '1007' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '1027' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_created_remotely.prog.abap'
      iv_sha1     = '1025' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_created_remotely.prog.xml'
      iv_sha1     = '1015' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_del_mod.prog.abap'
      iv_sha1     = '1024' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_del_mod.prog.xml'
      iv_sha1     = '1013' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_locally.prog.abap'
      iv_sha1     = '1008' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_locally.prog.xml'
      iv_sha1     = '1009' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_both.prog.abap'
      iv_sha1     = '1002' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_both.prog.xml'
      iv_sha1     = '1030' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_locally.prog.abap'
      iv_sha1     = '1026' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_locally.prog.xml'
      iv_sha1     = '1021' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_remotely.prog.abap'
      iv_sha1     = '1019' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_remotely.prog.xml'
      iv_sha1     = '1031' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_move_package.prog.xml'
      iv_sha1     = '1040' ).
    mo_helper->add_remote(
      iv_path     = '/src/sub/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '1041' ).
    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'ztest_move_package_w_change.prog.xml'
      iv_sha1     = '2042' ).

  ENDMETHOD.

  METHOD complete_state.

    mo_helper->add_state(
      iv_path     = '/'
      iv_filename = '.abapgit.xml'
      iv_sha1     = '1017' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '1027' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_locally.prog.abap'
      iv_sha1     = '1008' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_locally.prog.xml'
      iv_sha1     = '1009' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotely.prog.abap'
      iv_sha1     = '1016' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotely.prog.xml'
      iv_sha1     = '1003' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_del_mod.prog.abap'
      iv_sha1     = '1020' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_del_mod.prog.xml'
      iv_sha1     = '1029' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_both.prog.abap'
      iv_sha1     = '1010' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_both.prog.xml'
      iv_sha1     = '1004' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_locally.prog.abap'
      iv_sha1     = '1026' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_locally.prog.xml'
      iv_sha1     = '1021' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_remotely.prog.abap'
      iv_sha1     = '1018' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_modified_remotely.prog.xml'
      iv_sha1     = '1011' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_mod_del.prog.abap'
      iv_sha1     = '1014' ).
    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_mod_del.prog.xml'
      iv_sha1     = '1005' ).
    mo_helper->add_state(
      iv_path     = '/src/sub/'
      iv_filename = 'ztest_move_package.prog.xml'
      iv_sha1     = '1040' ).
    mo_helper->add_state(
      iv_path     = '/src/sub/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '1041' ).
    mo_helper->add_state(
      iv_path     = '/src/sub/'
      iv_filename = 'ztest_move_package_w_change.prog.xml'
      iv_sha1     = '1042' ).

  ENDMETHOD.

  METHOD complete.

    DATA:
      ls_line TYPE zif_abapgit_definitions=>ty_result,
      lv_act  TYPE c LENGTH 4,
      lv_exp  TYPE c LENGTH 4.

    complete_local( ).
    complete_remote( ).
    complete_state( ).

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 26 ).

    DO 26 TIMES.
      ls_line = mo_result->get_line( sy-index ).
      lv_act+0(1) = ls_line-match.
      lv_act+1(1) = ls_line-lstate.
      lv_act+2(1) = ls_line-rstate.
      lv_act+3(1) = ls_line-packmove.
      CASE sy-index.
        WHEN 1.
          lv_exp = 'X  '. " no changes
        WHEN 2.
          lv_exp = '  A'. " add remote
        WHEN 3.
          lv_exp = 'X  '. " no change
        WHEN 4 OR 5.
          lv_exp = ' A '. " add local
        WHEN 6 OR 7.
          lv_exp = '  A'. " add remote
        WHEN 8 OR 9.
          lv_exp = ' DM'. " delete local, modify remote
        WHEN 10 OR 11.
          lv_exp = ' D '. " delete local
        WHEN 12 OR 13.
          lv_exp = '  D'. " delete remote
        WHEN 14 OR 15.
          lv_exp = ' MD'. " modify local, delete remote
        WHEN 16 OR 17.
          lv_exp = ' MM'. " modify both sides
        WHEN 18 OR 19.
          lv_exp = ' M '. " modify local
        WHEN 20 OR 21.
          lv_exp = '  M'. " modify remote
        WHEN 22.
          lv_exp = ' D X'. " package move (no change)
        WHEN 23.
          lv_exp = ' D  '. " package move with change
        WHEN 24.
          lv_exp = 'X   '. " no chagen
        WHEN 25.
          lv_exp = ' A X'. " package move (no change)
        WHEN 26.
          lv_exp = ' A  '. " package move with change
      ENDCASE.

      cl_abap_unit_assert=>assert_equals(
        act = lv_act
        exp = lv_exp
        msg = |Line { sy-index }: { ls_line-filename }| ).
    ENDDO.

  ENDMETHOD.

  METHOD only_local2.

    mo_helper->add_local(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotely.prog.abap'
      iv_sha1     = '1016' ).

    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'ztest_deleted_remotely.prog.abap'
      iv_sha1     = '2016' ). " different checksum

    mo_result = mo_helper->run( ).

    mo_result->assert_lines( 1 ).

    " it should appear as deleted remotely
    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-lstate
      exp = zif_abapgit_definitions=>c_state-added ).

  ENDMETHOD.

  METHOD only_remote2.

    " Add local class implementation
    mo_helper->add_local(
      iv_path     = '/src/sub/'
      iv_obj_name = 'ZCL_CLAS'
      iv_obj_type = 'CLAS'
      iv_filename = 'zcl_clas.clas.abap'
      iv_sha1     = '112233' ).

    mo_helper->add_remote(
      iv_path     = '/src/sub/'
      iv_filename = 'zcl_clas.clas.abap'
      iv_sha1     = '332211' ).

    mo_helper->add_remote(
      iv_path     = '/src/sub/'
      iv_filename = 'zcl_clas.clas.locals_imp.abap'
      iv_sha1     = '1111' ).

    mo_helper->add_state(
      iv_path     = '/src/sub/'
      iv_filename = 'zcl_clas.clas.locals_imp.abap'
      iv_sha1     = '1111' ).

    mo_result = mo_helper->run( iv_devclass = '$DIFFERENT' ).

    mo_result->assert_lines( 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 2 )-match
      exp = abap_false ).

  ENDMETHOD.

  METHOD only_remote3.

    " Add subpackage remotely
    mo_helper->add_local(
      iv_path     = '/src/'
      iv_obj_name = '$DIFFERENT'
      iv_obj_type = 'DEVC'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '112233' ).

    mo_helper->add_remote(
      iv_path     = '/src/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '112233' ).

    mo_helper->add_remote(
      iv_path     = '/src/sub/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '332211' ).

    mo_helper->add_state(
      iv_path     = '/src/'
      iv_filename = 'package.devc.xml'
      iv_sha1     = '112233' ).

    mo_result = mo_helper->run( iv_devclass = '$DIFFERENT' ).

    mo_result->assert_lines( 2 ).

    " main package matches
    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 1 )-match
      exp = abap_true ).

    " subpackage should appear as added remotely
    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 2 )-lstate
      exp = zif_abapgit_definitions=>c_state-unchanged ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_result->get_line( 2 )-rstate
      exp = zif_abapgit_definitions=>c_state-added ).

  ENDMETHOD.

ENDCLASS.
