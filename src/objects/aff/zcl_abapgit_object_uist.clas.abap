CLASS zcl_abapgit_object_uist DEFINITION
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



CLASS ZCL_ABAPGIT_OBJECT_UIST IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
    TRY.

        DATA(lo_api) = NEW /ui2/cl_uist_sval_sql( ).
        DATA(ls_metadata) = lo_api->/ui2/if_uist_sval~get_metadata( object_name = CONV #( ms_item-obj_name )
                                                                    version     = 'A'
                                                                    language    = sy-langu ).
        rv_user = ls_metadata-changed_by.

      CATCH /ui2/cx_fdm INTO DATA(lx_error).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
