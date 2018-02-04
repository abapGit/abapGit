CLASS zcl_abapgit_oo_factory DEFINITION PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      make
        IMPORTING
          iv_object_type                   TYPE tadir-object
        RETURNING
          VALUE(ro_object_oriented_object) TYPE REF TO zif_abapgit_oo_object_fnc.
  PRIVATE SECTION.
    CLASS-DATA:
        go_object_oriented_object TYPE REF TO zif_abapgit_oo_object_fnc.
ENDCLASS.

CLASS zcl_abapgit_oo_factory IMPLEMENTATION.
  METHOD make.
    IF go_object_oriented_object IS BOUND.
      ro_object_oriented_object = go_object_oriented_object.
      RETURN.
    ENDIF.
    IF iv_object_type = 'CLAS'.
      CREATE OBJECT ro_object_oriented_object TYPE zcl_abapgit_oo_class.
    ELSEIF iv_object_type = 'INTF'.
      CREATE OBJECT ro_object_oriented_object TYPE zcl_abapgit_oo_interface.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
