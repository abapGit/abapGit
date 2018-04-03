*"* use this source file for your ABAP unit test classes

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
    "Objects will be created and deleted, do not run in customer system!
    "These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)
    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_run_critical_tests( ) = abap_false.
      cl_abap_unit_assert=>fail(
        msg   = 'Cancelled. You can enable these tests at the Settings page'
        level = if_aunit_constants=>tolerable ).
    ENDIF.
  ENDMETHOD.                    "class_setup

  METHOD run.

    DATA: lo_repo    TYPE REF TO zcl_abapgit_repo_online,
          lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_msg     TYPE string,
          lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          ls_checks  TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lt_types   TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <lv_type>   LIKE LINE OF lt_types.


    zcl_abapgit_sap_package=>create_local( c_package ).

    lt_types = zcl_abapgit_objects=>supported_list( ).

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = 'https://github.com/abapGit/Test-Objects.git'
      iv_branch_name = 'refs/heads/master'
      iv_package     = c_package ).
    lo_repo->status( ).
    lo_repo->deserialize( ls_checks ).

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
      not_exist FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.                    "ltcl_object_types DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

    lt_files = zcl_abapgit_objects=>serialize( is_item     = is_item
                                       iv_language = zif_abapgit_definitions=>gc_english ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "check

ENDCLASS.                    "ltcl_serialize IMPLEMENTATION
