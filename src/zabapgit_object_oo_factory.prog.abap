*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_oo_factory
*&---------------------------------------------------------------------*
CLASS lcl_object_oriented_factory IMPLEMENTATION.
  METHOD make.
    IF go_object_oriented_object IS BOUND.
      ro_object_oriented_object = go_object_oriented_object.
      RETURN.
    ENDIF.
    IF iv_object_type = 'CLAS'.
      CREATE OBJECT ro_object_oriented_object TYPE lcl_object_oriented_class.
    ELSEIF iv_object_type = 'INTF'.
      CREATE OBJECT ro_object_oriented_object TYPE lcl_object_oriented_interface.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lth_oo_factory_injector DEFINITION FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      inject
        IMPORTING
          io_object_oriented_object TYPE REF TO lif_object_oriented_object_fnc.
ENDCLASS.
CLASS lth_oo_factory_injector IMPLEMENTATION.
  METHOD inject.
    lcl_object_oriented_factory=>go_object_oriented_object = io_object_oriented_object.
  ENDMETHOD.
ENDCLASS.
