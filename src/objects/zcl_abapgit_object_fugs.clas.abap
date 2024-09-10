CLASS zcl_abapgit_object_fugs DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_fugr
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_object~delete REDEFINITION.
    METHODS zif_abapgit_object~deserialize REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_FUGS IMPLEMENTATION.


  METHOD zif_abapgit_object~delete.
    zcx_abapgit_exception=>raise( 'FUGS delete not supported' ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    zcx_abapgit_exception=>raise( 'FUGS deserialize not supported' ).
  ENDMETHOD.
ENDCLASS.
