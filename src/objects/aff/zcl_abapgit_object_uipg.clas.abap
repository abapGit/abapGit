CLASS zcl_abapgit_object_uipg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_object~changed_by
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_UIPG IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
    TRY.
        DATA(lo_api) = NEW /ui2/cl_uist_sval_sql( ).
        DATA(ls_metadata) = /ui2/cl_uipg_db_access=>get_instance( )->read_wb_metadata( iv_page_id  = CONV #( ms_item-obj_name )
                                                                                       iv_version  = 'A'
                                                                                       iv_language = sy-langu ).
        rv_user = ls_metadata-changed_by.

      CATCH /ui2/cx_uipg INTO DATA(lx_error).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.