CLASS ltcl_run_checks DEFINITION DEFERRED.
CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS ltcl_run_checks.

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
      teardown,
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

  METHOD zif_abapgit_sap_package~get_default_transport_layer.
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

  METHOD zif_abapgit_sap_package~get.
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

  METHOD zif_abapgit_sap_namespace~split_by_name.
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

  METHOD teardown.
    DATA li_empty TYPE REF TO zif_abapgit_sap_namespace.
    zcl_abapgit_injector=>set_sap_namespace( li_empty ).
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
