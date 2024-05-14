class ZCL_ABAPGIT_OBJECT_CDBO definition
  public
  inheriting from ZCL_ABAPGIT_OBJECT_COMMON_AFF
  final
  create public .

  public section.

    methods CONSTRUCTOR
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
      !IV_LANGUAGE type SPRAS .

    methods ZIF_ABAPGIT_OBJECT~CHANGED_BY
    redefinition .
  protected section.
  private section.

    constants C_TABLE_NAME type TABNAME value 'CDB_OBJH' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CDBO IMPLEMENTATION.


  method CONSTRUCTOR.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).
  endmethod.


  method ZIF_ABAPGIT_OBJECT~CHANGED_BY.

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

  endmethod.
ENDCLASS.
