CLASS zcl_abapgit_object_aplo DEFINITION
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



CLASS ZCL_ABAPGIT_OBJECT_APLO IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
    CLEAR rv_user.
  ENDMETHOD.
ENDCLASS.
