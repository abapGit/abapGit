*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UNIT_TEST
*&---------------------------------------------------------------------*

DEFINE _append_local.
  APPEND INITIAL LINE TO lt_local ASSIGNING <local>.
  <local>-item-obj_type = &1.
  <local>-item-obj_name = &2.
  <local>-item-devclass = '$Z$'.
  <local>-file-path     = '/'.
  <local>-file-filename = &3.
  <local>-file-sha1     = &4.
END-OF-DEFINITION.

DEFINE _append_remote.
  APPEND INITIAL LINE TO lt_remote ASSIGNING <remote>.
  <remote>-path     = '/'.
  <remote>-filename = &1.
  <remote>-sha1     = &2.
END-OF-DEFINITION.

DEFINE _append_state.
  APPEND INITIAL LINE TO lt_state ASSIGNING <state>.
  <state>-path     = '/'.
  <state>-filename = &1.
  <state>-sha1     = &2.
END-OF-DEFINITION.

DEFINE _append_result.
  APPEND INITIAL LINE TO lt_results ASSIGNING <result>.
  <result>-obj_type = &1.
  <result>-obj_name = &2.
  <result>-match    = &3.
  <result>-lstate   = &4.
  <result>-rstate   = &5.
  <result>-package  = &6.
  <result>-path     = &7.
  <result>-filename = &8.
END-OF-DEFINITION.

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

    DATA: lo_repo    TYPE REF TO lcl_repo_online,
          lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_msg     TYPE string,
          lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_types   TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <lv_type>   LIKE LINE OF lt_types.


    zcl_abapgit_sap_package=>create_local( c_package ).

    lt_types = lcl_objects=>supported_list( ).

    lo_repo = lcl_repo_srv=>get_instance( )->new_online(
      iv_url         = 'https://github.com/larshp/abapGit-Unit-Test.git'
      iv_branch_name = 'refs/heads/master'
      iv_package     = c_package ).
    lo_repo->status( ).
    lo_repo->deserialize( ).

    lt_tadir = lcl_tadir=>read( c_package ).
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

    lcl_objects=>delete( lt_tadir ).
    lt_tadir = lcl_tadir=>read( c_package ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lv_msg = |Not deleted properly { <ls_tadir>-object } { <ls_tadir>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    lcl_repo_srv=>get_instance( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "run

ENDCLASS.                    "ltcl_dangerous IMPLEMENTATION

CLASS ltcl_git_porcelain DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      append
        IMPORTING iv_path TYPE string
                  iv_name TYPE string,
      single_file FOR TESTING
        RAISING zcx_abapgit_exception,
      two_files_same_path FOR TESTING
        RAISING zcx_abapgit_exception,
      root_empty FOR TESTING
        RAISING zcx_abapgit_exception,
      namespaces FOR TESTING
        RAISING zcx_abapgit_exception,
      more_sub FOR TESTING
        RAISING zcx_abapgit_exception,
      sub FOR TESTING
        RAISING zcx_abapgit_exception.

    DATA: mt_expanded TYPE lcl_git_porcelain=>ty_expanded_tt,
          mt_trees    TYPE lcl_git_porcelain=>ty_trees_tt.

ENDCLASS.

CLASS ltcl_git_porcelain IMPLEMENTATION.

  METHOD setup.
    CLEAR mt_expanded.
    CLEAR mt_trees.
  ENDMETHOD.

  METHOD append.

    FIELD-SYMBOLS: <ls_expanded> LIKE LINE OF mt_expanded.


    APPEND INITIAL LINE TO mt_expanded ASSIGNING <ls_expanded>.
    <ls_expanded>-path  = iv_path.
    <ls_expanded>-name  = iv_name.
    <ls_expanded>-sha1  = 'a'.
    <ls_expanded>-chmod = zif_abapgit_definitions=>gc_chmod-file.

  ENDMETHOD.

  METHOD single_file.

    append( iv_path = '/'
            iv_name = 'foobar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD two_files_same_path.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD sub.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

  METHOD more_sub.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF mt_trees.

    append( iv_path = '/src/foo_a/foo_a1/'
            iv_name = 'a1.txt' ).

    append( iv_path = '/src/foo_a/foo_a2/'
            iv_name = 'a2.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 5 ).

    LOOP AT mt_trees ASSIGNING <ls_tree>.
      cl_abap_unit_assert=>assert_not_initial( <ls_tree>-data ).
    ENDLOOP.

  ENDMETHOD.

  METHOD namespaces.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF mt_trees.

    append( iv_path = '/src/#foo#a/#foo#a1/'
            iv_name = 'a1.txt' ).

    append( iv_path = '/src/#foo#a/#foo#a2/'
            iv_name = 'a2.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 5 ).

    LOOP AT mt_trees ASSIGNING <ls_tree>.
      cl_abap_unit_assert=>assert_not_initial( <ls_tree>-data ).
    ENDLOOP.

  ENDMETHOD.

  METHOD root_empty.

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

* so 2 total trees are expected: '/' and '/sub/'
    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.

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
          lt_types     TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = lcl_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.

      CLEAR ls_item.
      ls_item-obj_type = <lv_type>.
      lv_supported = lcl_objects=>is_supported( ls_item ).

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
          lt_types  TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = lcl_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.
      CLEAR ls_item.
      ls_item-obj_name = 'ZABAPGIT_FOOBAR'.
      ls_item-obj_type = <lv_type>.
      lv_exists = lcl_objects=>exists( ls_item ).

      cl_abap_unit_assert=>assert_equals(
          act  = lv_exists
          exp  = abap_false
          msg  = ls_item-obj_type
          quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.                    "not_exist

ENDCLASS.                    "ltcl_object_types IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      check
        IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
        RAISING   zcx_abapgit_exception,
      serialize_tabl FOR TESTING RAISING zcx_abapgit_exception,
      serialize_enqu FOR TESTING RAISING zcx_abapgit_exception,
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

ENDCLASS.                    "ltcl_serialize DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_table

  METHOD serialize_auth.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'AUTH'.
    ls_item-obj_name = 'AREA'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_auth

  METHOD serialize_clas.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_GUI_FRONTEND_SERVICES'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_clas

  METHOD serialize_doma.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_doma

  METHOD serialize_dtel.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_dtel

  METHOD serialize_fugr.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SRFC'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_fugr

  METHOD serialize_msag.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'MSAG'.
    ls_item-obj_name = '00'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_msag

  METHOD serialize_prog.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPLWBABAP'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_prog

  METHOD serialize_tran.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TRAN'.
    ls_item-obj_name = 'SE38'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_tran

  METHOD serialize_ttyp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TTYP'.
    ls_item-obj_name = 'ABAPPROG'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_ttyp

  METHOD check.

    DATA: lt_files TYPE zif_abapgit_definitions=>ty_files_tt.

    lt_files = lcl_objects=>serialize( is_item     = is_item
                                       iv_language = zif_abapgit_definitions=>gc_english ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "check

ENDCLASS.                    "ltcl_serialize IMPLEMENTATION

CLASS ltcl_file_status DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL
  INHERITING FROM cl_aunit_assert.

  PUBLIC SECTION.
    METHODS calculate_status FOR TESTING
      RAISING zcx_abapgit_exception.

ENDCLASS.   "ltcl_file_status

CLASS ltcl_file_status IMPLEMENTATION.

  METHOD calculate_status.

    DATA: lt_local       TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_remote      TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_state       TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lt_results     TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_results_exp TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_dot         TYPE REF TO zcl_abapgit_dot_abapgit.

    FIELD-SYMBOLS: <local>  LIKE LINE OF lt_local,
                   <remote> LIKE LINE OF lt_remote,
                   <result> LIKE LINE OF lt_results,
                   <state>  LIKE LINE OF lt_state.

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
    lt_results_exp = lt_results.

    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
*    lo_dot->set_starting_folder( 'SRC' ).
*    lo_dot->set_folder_logic( lcl_dot_abapgit=>c_folder_logic-prefix ).

    lt_results = lcl_file_status=>calculate_status(
      iv_devclass        = '$Z$'
      io_dot             = lo_dot
      it_local           = lt_local
      it_remote          = lt_remote
      it_cur_state       = lt_state ).

    assert_equals( act = lt_results exp = lt_results_exp ).

  ENDMETHOD.  "calculate_status

ENDCLASS.   "ltcl_file_status

CLASS ltcl_file_status2 DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL
  INHERITING FROM cl_aunit_assert.

  PUBLIC SECTION.
    METHODS check FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.   "ltcl_sap_package

CLASS ltcl_file_status2 IMPLEMENTATION.

  METHOD check.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_log     TYPE REF TO zcl_abapgit_log.

    FIELD-SYMBOLS: <result> LIKE LINE OF lt_results.

*** 0 Positive

    CLEAR lt_results.
    CREATE OBJECT lo_log.
    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH FILE
    _append_result 'CLAS' 'ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'  'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'  'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'  'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'  'zdoma2.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( ) exp = 0 ).

*** 1 Negative, different path for same object

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' 'ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    'zclass1.clas.abap'.
    _append_result 'CLAS' 'ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/sub' 'zclass1.clas.xml'.
    _append_result 'DOMA' 'ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    'zdoma1.doma.xml'.
    _append_result 'DOMA' 'ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    'zdoma2.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    " This one is not pure - incorrect path also triggers path vs package check
    assert_equals( act = lo_log->count( ) exp = 2 ).
    assert_equals( act = lo_log->has_rc( '1' ) exp = abap_true ).

*** 2 Negative, incorrect path vs package

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/sub' '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    '$$zdoma2.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( ) exp = 1 ).
    assert_equals( act = lo_log->has_rc( '2' ) exp = abap_true ).

*** 3 Negative, similar filenames

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    '$$zdoma1.doma.xml'.
    _append_result 'DOMA' '$$ZDOMA2'  ' '   'M'   ' '  '$Z$'  '/'    '$$zdoma1.doma.xml'.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( ) exp = 1 ).
    assert_equals( act = lo_log->has_rc( '3' ) exp = abap_true ).

*** 4 Negative, empty filenames

    CLEAR lt_results.
    CREATE OBJECT lo_log.

    "EXP RESULT    TYPE   NAME      MATCH LST   RST  PKG    PATH   FILE
    _append_result 'CLAS' '$$ZCLASS1' ' '   ' '   'A'  '$Z$'  '/'    '$$zclass1.clas.abap'.
    _append_result 'CLAS' '$$ZCLASS1' 'X'   ' '   ' '  '$Z$'  '/'    '$$zclass1.clas.xml'.
    _append_result 'DOMA' '$$ZDOMA1'  'X'   ' '   ' '  '$Z$'  '/'    ''.

    lcl_file_status=>run_checks( io_log     = lo_log
                                 it_results = lt_results
                                 io_dot     = zcl_abapgit_dot_abapgit=>build_default( )
                                 iv_top     = '$Z$' ).

    assert_equals( act = lo_log->count( )
                   exp = 1 ).

    assert_equals( act = lo_log->has_rc( '4' )
                   exp = abap_true ).

  ENDMETHOD.  " check.

ENDCLASS. "ltcl_sap_package

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
