CLASS zcl_abapgit_object_cdbo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
    IMPORTING
      !is_item TYPE zif_abapgit_definitions=>ty_item
      !iv_language TYPE spras
    RAISING
      zcx_abapgit_exception .

    METHODS zif_abapgit_object~changed_by
    REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_table_name TYPE tabname VALUE 'CDB_OBJH' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CDBO IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.
        SELECT SINGLE changed_by FROM (c_table_name) INTO lv_user WHERE obj_name = ms_item-obj_name.
        IF lv_user IS INITIAL.
          SELECT SINGLE created_by FROM (c_table_name) INTO lv_user WHERE obj_name = ms_item-obj_name.
        ENDIF.
        rv_user = lv_user.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
