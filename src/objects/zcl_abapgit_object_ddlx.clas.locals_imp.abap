CLASS lcl_tadir DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES lif_tadir.
    METHODS:
      constructor
        IMPORTING
          io_outer TYPE REF TO zcl_abapgit_object_ddlx.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_outer TYPE REF TO zcl_abapgit_object_ddlx.

ENDCLASS.

CLASS zcl_abapgit_object_ddlx DEFINITION LOCAL FRIENDS lcl_tadir.

CLASS lcl_tadir IMPLEMENTATION.

  METHOD lif_tadir~tadir_insert.
    mo_outer->tadir_insert( iv_package = iv_package ).
  ENDMETHOD.

  METHOD constructor.
    mo_outer = io_outer.
  ENDMETHOD.

ENDCLASS.
