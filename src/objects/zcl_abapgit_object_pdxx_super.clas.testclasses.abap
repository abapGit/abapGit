CLASS lth_object_pdxx DEFINITION INHERITING FROM zcl_abapgit_object_pdxx_super
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS get_objkey RETURNING VALUE(rv_result) TYPE hrsobject.
ENDCLASS.

CLASS lth_object_pdxx IMPLEMENTATION.

  METHOD get_objkey.
    rv_result = ms_objkey.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_lock DEFINITION
  FINAL
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS c_dummy_otype TYPE hr_sotype VALUE 'XX'.

    METHODS enqueue_is_detected FOR TESTING RAISING cx_static_check.
    METHODS lock_object IMPORTING iv_taskid TYPE hrobjid.

ENDCLASS.


CLASS ltc_lock IMPLEMENTATION.

  METHOD enqueue_is_detected.

    CONSTANTS lc_dummy TYPE hrobjid VALUE '99999999'.

    DATA: lo_cut  TYPE REF TO zif_abapgit_object,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

    lock_object( lc_dummy ).

    ls_item-obj_type = 'PDXX'.
    ls_item-obj_name = 'XX' && lc_dummy.

    CREATE OBJECT lo_cut TYPE lth_object_pdxx
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    cl_abap_unit_assert=>assert_equals( act = lo_cut->is_locked( )
                                        exp = abap_true ).

    CALL FUNCTION 'DEQUEUE_HRSOBJECT'
      EXPORTING
        objid   = lc_dummy
        otype   = c_dummy_otype
        x_objid = ' '
        x_otype = ' '
        _scope  = '2'.

  ENDMETHOD.


  METHOD lock_object.

    CALL FUNCTION 'ENQUEUE_HRSOBJECT'
      EXPORTING
        objid          = iv_taskid
        otype          = c_dummy_otype
        x_objid        = ' '
        x_otype        = ' '
        _scope         = '2'
        _wait          = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_general_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      hrobj_derived_from_otype FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_general_tests IMPLEMENTATION.

  METHOD hrobj_derived_from_otype.
    DATA: lo_cut  TYPE REF TO lth_object_pdxx,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDZZ'.
    ls_item-obj_name = 'ZZ99999999'.

    CREATE OBJECT lo_cut TYPE lth_object_pdxx
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    cl_abap_unit_assert=>assert_equals( act = lo_cut->get_objkey( )-otype
                                        exp = 'ZZ' ).
  ENDMETHOD.

ENDCLASS.
