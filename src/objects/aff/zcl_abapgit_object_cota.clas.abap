CLASS zcl_abapgit_object_cota DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  public section.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
  protected section.
  private section.
ENDCLASS.



CLASS zcl_abapgit_object_cota IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE changedby FROM ('sapcontargethead')
      WHERE id = @ms_item-obj_name AND version = 'I'
      INTO @rv_user.

    IF rv_user IS INITIAL.
      SELECT SINGLE changedby FROM ('sapcontargethead')
        WHERE id = @ms_item-obj_name AND version = 'A'
        INTO @rv_user.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
