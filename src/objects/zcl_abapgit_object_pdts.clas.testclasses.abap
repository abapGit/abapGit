
CLASS ltc_ci DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.
    METHODS run_ci FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_ci IMPLEMENTATION.

  METHOD run_ci.

    DATA lv_repo_url TYPE string.

    "Use STVARV to optionally override repo in local system
    SELECT SINGLE low
      INTO lv_repo_url
      FROM tvarvc
      WHERE name = 'ABAPGIT_TEST_URL_PDTS'  ##WARN_OK.

    zcl_abapgit_objects_ci_tests=>run(
        iv_object = 'PDTS'
        iv_url  = lv_repo_url ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_lock DEFINITION
  FINAL
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS c_ts TYPE hr_sotype VALUE 'TS'.

    METHODS enqueue_is_detected FOR TESTING RAISING cx_static_check.
    METHODS get_any_task RETURNING VALUE(rv_taskid) TYPE hrobjid.
    METHODS lock_task IMPORTING iv_taskid TYPE hrobjid.

ENDCLASS.


CLASS ltc_lock IMPLEMENTATION.

  METHOD enqueue_is_detected.

    DATA: lv_taskid TYPE hrobjid,
          lo_cut    TYPE REF TO zif_abapgit_object,
          ls_item   TYPE zif_abapgit_definitions=>ty_item.

    lv_taskid = get_any_task( ).
    lock_task( lv_taskid ).

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = 'TS' && lv_taskid.

    CREATE OBJECT lo_cut TYPE zcl_abapgit_object_pdts
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    cl_abap_unit_assert=>assert_equals( act = lo_cut->is_locked( )
                                        exp = abap_true ).

    CALL FUNCTION 'DEQUEUE_HRSOBJECT'
      EXPORTING
        objid   = lv_taskid
        otype   = c_ts
        x_objid = ' '
        x_otype = ' '
        _scope  = '2'.

  ENDMETHOD.


  METHOD get_any_task.

    SELECT SINGLE objid
           INTO rv_taskid
           FROM hrs1000
           WHERE otype = c_ts  ##WARN_OK. "#EC CI_NOORDER #EC CI_SGLSELECT

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc ).

  ENDMETHOD.


  METHOD lock_task.

    CALL FUNCTION 'ENQUEUE_HRSOBJECT'
      EXPORTING
        objid          = iv_taskid
        otype          = c_ts
        x_objid        = ' '
        x_otype        = ' '
        _scope         = '2'
        _wait          = ' '
      EXCEPTIONS
        foreign_lock   = 01
        system_failure = 02.

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_smoke_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO zif_abapgit_object.

    METHODS setup.
    METHODS run_simple_methods FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_smoke_test IMPLEMENTATION.

  METHOD setup.
    DATA  ls_item   TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = '99999999'.

    TRY.
        CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdts
          EXPORTING
            is_item     = ls_item
            iv_language = sy-langu.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD run_simple_methods.
    mo_cut->get_comparator( ).
    mo_cut->get_deserialize_steps( ).
    mo_cut->get_metadata( ).
    mo_cut->is_active( ).
  ENDMETHOD.

ENDCLASS.
