CLASS zcl_abapgit_object_cota DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_cota IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    DATA lx_error TYPE REF TO cx_root.
    TRY.
        SELECT SINGLE changedby FROM ('sapcontargethead') INTO rv_user
          WHERE id = ms_item-obj_name AND version = 'I'.

        IF rv_user IS INITIAL.
          SELECT SINGLE changedby FROM ('sapcontargethead') INTO rv_user
            WHERE id = ms_item-obj_name AND version = 'A'.
        ENDIF.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
