CLASS ltcl_file_status DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_file_status.

DEFINE _append_state.
  APPEND INITIAL LINE TO lt_state ASSIGNING <ls_state>.
  <ls_state>-path     = '/'.
  <ls_state>-filename = &1.
  <ls_state>-sha1     = &2.
END-OF-DEFINITION.

DEFINE _append_local.
  APPEND INITIAL LINE TO lt_local ASSIGNING <ls_local>.
  <ls_local>-item-obj_type = &1.
  <ls_local>-item-obj_name = &2.
  <ls_local>-item-devclass = '$Z$'.
  <ls_local>-file-path     = '/'.
  <ls_local>-file-filename = &3.
  <ls_local>-file-sha1     = &4.
END-OF-DEFINITION.

DEFINE _append_remote.
  APPEND INITIAL LINE TO lt_remote ASSIGNING <ls_remote>.
  <ls_remote>-path     = '/'.
  <ls_remote>-filename = &1.
  <ls_remote>-sha1     = &2.
END-OF-DEFINITION.

DEFINE _append_result.
  APPEND INITIAL LINE TO mt_results ASSIGNING <ls_result>.
  <ls_result>-obj_type = &1.
  <ls_result>-obj_name = &2.
  <ls_result>-match    = &3.
  <ls_result>-lstate   = &4.
  <ls_result>-rstate   = &5.
  <ls_result>-package  = &6.
  <ls_result>-path     = &7.
  <ls_result>-filename = &8.
END-OF-DEFINITION.

CLASS ltcl_file_status DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_results TYPE zif_abapgit_definitions=>ty_results_tt.

    METHODS: calculate_status FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_file_status IMPLEMENTATION.

  METHOD calculate_status.

    DATA: lt_local       TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_remote      TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_state       TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lt_results_exp TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_dot         TYPE REF TO zcl_abapgit_dot_abapgit.

    FIELD-SYMBOLS: <ls_local>  LIKE LINE OF lt_local,
                   <ls_remote> LIKE LINE OF lt_remote,
                   <ls_result> LIKE LINE OF mt_results,
                   <ls_state>  LIKE LINE OF lt_state.


    "STATE         FILE                              SHA1
    _append_state '$$zclass1.clas.xml'              'C1_F1'.
    " class1 testclasses is new locally, abap is new remotely
    " class2 is completely new remotely
    _append_state '$$zdoma1.doma.xml'               'D1'.
    _append_state '$$zdoma2.doma.xml'               'D2'.
    _append_state '$$zdoma3.doma.xml'               'D3'.
    " doma4 is new locally
    " doma5 is new remotely
    _append_state '$$zdoma6.doma.xml'               'D6'.
    " doma7 is not in state - emulate brocken cache
    " doma8 is not in state - emulate brocken cache
    _append_state '$$zdoma9.doma.xml'               'D9'.
    _append_state 'num01.doma.xml'                  'NUM01'. " another from different package
    _append_state 'xfeld.doma.xml'                  'XFELD'. " from different package

    "LOCAL         TYPE   NAME        FILE                              SHA1
    _append_local 'CLAS' '$$ZCLASS1' '$$zclass1.clas.testclasses.abap' 'C1_F3'.
    _append_local 'CLAS' '$$ZCLASS1' '$$zclass1.clas.xml'              'C1_F1'.
    _append_local 'DOMA' '$$ZDOMA1'  '$$zdoma1.doma.xml'               'D1'.
    _append_local 'DOMA' '$$ZDOMA2'  '$$zdoma2.doma.xml'               'D2_CHANGED_L'.
    _append_local 'DOMA' '$$ZDOMA3'  '$$zdoma3.doma.xml'               'D3'.
    _append_local 'DOMA' '$$ZDOMA4'  '$$zdoma4.doma.xml'               'D4'.
    _append_local 'DOMA' '$$ZDOMA6'  '$$zdoma6.doma.xml'               'D6_CHANGED_L'.
    _append_local 'DOMA' '$$ZDOMA7'  '$$zdoma7.doma.xml'               'D7'.
    _append_local 'DOMA' '$$ZDOMA8'  '$$zdoma8.doma.xml'               'D8'.
    " dome9 was deleted from local system. Can be found by existing state

    "REMOTE         FILE                  SHA1
    _append_remote 'textfile.txt'        'T1'.
    _append_remote '$$zclass1.clas.abap' 'C1_F2'. " Must be before xml for tougher test
    _append_remote '$$zclass1.clas.xml'  'C1_F1'.
    _append_remote '$$zclass2.clas.abap' 'C1_F2'. " Must be before xml for tougher test
    _append_remote '$$zclass2.clas.xml'  'C1_F1'.
    _append_remote '$$zdoma1.doma.xml'   'D1'.
    _append_remote '$$zdoma2.doma.xml'   'D2'.
    _append_remote '$$zdoma3.doma.xml'   'D3_CHANGED_R'.
    _append_remote '$$zdoma5.doma.xml'   'D5'.
    _append_remote '$$zdoma6.doma.xml'   'D6_CHANGED_R'.
    _append_remote '$$zdoma7.doma.xml'   'D7'.
    _append_remote '$$zdoma8.doma.xml'   'D8_CHANGED_R'.  " This one is changed
    _append_remote '$$zdoma9.doma.xml'   'D9'.            " This one is deleted locally
    _append_remote 'xfeld.doma.xml'      'XFELD'.         " Object from different package
    _append_remote 'num01.doma.xml'      'NUM01_CHANGED'. " Changed object from different package

    "EXP RESULT    TYPE   NAME        MATCH LST   RST  PKG    PATH FILE
    _append_result ''     ''          ' '   ' '   'A'  ''     '/'  'textfile.txt'.
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'  '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' ' '   'A'   ' '  '$Z$'  '/'  '$$zclass1.clas.testclasses.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'  '$$zclass1.clas.xml'.
    _append_result 'CLAS' '$$ZCLASS2' ' '   ' '   'A'  ''     '/'  '$$zclass2.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS2' ' '   ' '   'A'  ''     '/'  '$$zclass2.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'  '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'  '$$zdoma2.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA3'  ' '   ' '   'M'  '$Z$'  '/'  '$$zdoma3.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA4'  ' '   'A'   ' '  '$Z$'  '/'  '$$zdoma4.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA5'  ' '   ' '   'A'  ''     '/'  '$$zdoma5.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA6'  ' '   'M'   'M'  '$Z$'  '/'  '$$zdoma6.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA7'  'X'   ' '   ' '  '$Z$'  '/'  '$$zdoma7.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA8'  ' '   'M'   'M'  '$Z$'  '/'  '$$zdoma8.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA9'  ' '   'D'   ' '  ''     '/'  '$$zdoma9.doma.xml'.
    _append_result 'DOMA' 'NUM01'     ' '   ' '   'M'  'SUTI' '/'  'num01.doma.xml'.
    _append_result 'DOMA' 'XFELD'     'X'   ' '   ' '  'SUTI' '/'  'xfeld.doma.xml'.
    lt_results_exp = mt_results.

    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    mt_results = zcl_abapgit_file_status=>calculate_status(
      iv_devclass        = '$Z$'
      io_dot             = lo_dot
      it_local           = lt_local
      it_remote          = lt_remote
      it_cur_state       = lt_state ).

    cl_abap_unit_assert=>assert_equals(
      act = mt_results
      exp = lt_results_exp ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_run_checks DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_run_checks.

CLASS ltcl_run_checks DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          mo_dot     TYPE REF TO zcl_abapgit_dot_abapgit,
          mo_log     TYPE REF TO zcl_abapgit_log.

    METHODS:
      setup,
      positive FOR TESTING RAISING zcx_abapgit_exception,
      neg_diff_path_for_same_obj FOR TESTING RAISING zcx_abapgit_exception,
      neg_incorrect_path_vs_pack FOR TESTING RAISING zcx_abapgit_exception,
      neg_similar_filenames FOR TESTING RAISING zcx_abapgit_exception,
      neg_empty_filenames FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_run_checks IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_log.

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
    mo_dot->set_starting_folder( '/' ).

  ENDMETHOD.

  METHOD positive.

*** 0 Positive

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_results.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH FILE
    _append_result 'CLAS' 'ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'  'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'  'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'  'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'  'zdoma2.doma.xml'.

    zcl_abapgit_file_status=>run_checks(
      io_log     = mo_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->count( )
      exp = 0 ).

  ENDMETHOD.

  METHOD neg_diff_path_for_same_obj.

*** 1 Negative, different path for same object

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_results.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' 'ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/sub' 'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    'zdoma2.doma.xml'.

    zcl_abapgit_file_status=>run_checks(
      io_log     = mo_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    " This one is not pure - incorrect path also triggers path vs package check
    cl_abap_unit_assert=>assert_equals(
      act = mo_log->count( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->has_rc( '1' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD neg_incorrect_path_vs_pack.

*** 2 Negative, incorrect path vs package

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_results.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/sub' '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    '$$zdoma2.doma.xml'.

    zcl_abapgit_file_status=>run_checks(
      io_log     = mo_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->has_rc( '2' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD neg_similar_filenames.

*** 3 Negative, similar filenames

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_results.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    '$$zdoma1.doma.xml'.

    zcl_abapgit_file_status=>run_checks(
      io_log     = mo_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->has_rc( '3' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD neg_empty_filenames.

*** 4 Negative, empty filenames

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_results.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    ''.

    zcl_abapgit_file_status=>run_checks(
      io_log     = mo_log
      it_results = mt_results
      io_dot     = mo_dot
      iv_top     = '$Z$' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_log->has_rc( '4' )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_status_helper DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_status_helper.

CLASS ltcl_status_helper DEFINITION.

  PUBLIC SECTION.
    METHODS:
      add_remote
        IMPORTING
          iv_path     TYPE string DEFAULT '/'
          iv_filename TYPE string
          iv_sha1     TYPE zif_abapgit_definitions=>ty_sha1,
      add_local,
      add_state,
      run
        RETURNING VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.

    DATA: mt_local  TYPE zif_abapgit_definitions=>ty_files_item_tt,
          mt_remote TYPE zif_abapgit_definitions=>ty_files_tt,
          mt_state  TYPE zif_abapgit_definitions=>ty_file_signatures_tt.

ENDCLASS.

CLASS ltcl_status_helper IMPLEMENTATION.

  METHOD add_remote.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    APPEND INITIAL LINE TO mt_remote ASSIGNING <ls_remote>.
    <ls_remote>-path     = iv_path.
    <ls_remote>-filename = iv_filename.
    <ls_remote>-sha1     = iv_sha1.

  ENDMETHOD.

  METHOD add_local.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF mt_local.

* todo
  ENDMETHOD.

  METHOD add_state.

    FIELD-SYMBOLS:  <ls_state> LIKE LINE OF mt_state.
* todo
  ENDMETHOD.

  METHOD run.

    DATA: lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.

    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    rt_results = zcl_abapgit_file_status=>calculate_status(
      iv_devclass  = '$Z$'
      io_dot       = lo_dot
      it_local     = mt_local
      it_remote    = mt_remote
      it_cur_state = mt_state ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_calculate_status DEFINITION DEFERRED.
CLASS zcl_abapgit_file_status DEFINITION LOCAL FRIENDS ltcl_calculate_status.

CLASS ltcl_calculate_status DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA:
      mt_results TYPE zif_abapgit_definitions=>ty_results_tt,
      ms_result  LIKE LINE OF mt_results,
      mo_helper  TYPE REF TO ltcl_status_helper.

    METHODS:
      setup,
      test1 FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_calculate_status IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_helper.

  ENDMETHOD.

  METHOD test1.

    mo_helper->add_remote(
      iv_filename = '$$zdoma1.doma.xml'
      iv_sha1     = 'D1' ).

    mt_results = mo_helper->run( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_results )
      exp = 1 ).

    READ TABLE mt_results INDEX 1 INTO ms_result.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-rstate
      exp = zif_abapgit_definitions=>gc_state-added ).

  ENDMETHOD.

ENDCLASS.
