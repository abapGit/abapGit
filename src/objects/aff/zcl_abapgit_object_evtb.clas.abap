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
          lx_error               TYPE REF TO cx_root,
          lv_where_expr_active   TYPE string,
          lv_where_expr_inactive TYPE string,
          lv_select              TYPE string.

    TRY.

        lv_where_expr_active      = 'EVTB_NAME = '   &&  cl_abap_dyn_prg=>quote( ms_item-obj_name )  &&
                                 ' AND VERSION = '   &&  cl_abap_dyn_prg=>quote( 'A' ).
        lv_where_expr_inactive    = 'EVTB_NAME = '   &&  cl_abap_dyn_prg=>quote( ms_item-obj_name ) &&
                                 ' AND VERSION = '   &&  cl_abap_dyn_prg=>quote( 'I' ).

        lv_select                 = 'CHANGED_BY'.


        SELECT SINGLE (lv_select) INTO lv_user
            FROM (co_table_name)
            WHERE (lv_where_expr_inactive).


        IF lv_user IS INITIAL.
          SELECT SINGLE (lv_select) INTO lv_user
            FROM (co_table_name)
            WHERE (lv_where_expr_active).
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.

