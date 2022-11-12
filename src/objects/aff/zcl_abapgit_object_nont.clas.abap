CLASS zcl_abapgit_object_nont DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS co_table_name TYPE tabname VALUE 'NONT_HEADER'.
ENDCLASS.



CLASS zcl_abapgit_object_nont IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.
    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (co_table_name)
            WHERE nont_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (co_table_name)
            WHERE nont_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
