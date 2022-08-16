CLASS zcl_abapgit_object_evtb DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION .


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
    co_table_name TYPE tabname VALUE 'EVTB_HEADER'.
ENDCLASS.


CLASS zcl_abapgit_object_evtb IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.

    DATA: lv_user                TYPE string,
          lx_error               TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (co_table_name)
            WHERE evtb_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (co_table_name)
            WHERE evtb_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

