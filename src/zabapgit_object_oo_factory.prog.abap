*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_oo_factory
*&---------------------------------------------------------------------*
*CLASS lcl_oo_factory IMPLEMENTATION.
*  METHOD make.
*    IF go_object_oriented_object IS BOUND.
*      ro_object_oriented_object = go_object_oriented_object.
*      RETURN.
*    ENDIF.
*    IF iv_object_type = 'CLAS'.
*      CREATE OBJECT ro_object_oriented_object TYPE lcl_oo_class.
*    ELSEIF iv_object_type = 'INTF'.
*      CREATE OBJECT ro_object_oriented_object TYPE lcl_oo_interface.
*    ENDIF.
*  ENDMETHOD.
*ENDCLASS.



*CLASS ltcl_oo_factory_injector DEFINITION FOR TESTING.
*  PUBLIC SECTION.
*    CLASS-METHODS:
*      inject
*        IMPORTING
*          io_object_oriented_object TYPE REF TO zif_abapgit_oo_object_fnc.
*ENDCLASS.
*
*CLASS ltcl_oo_factory_injector IMPLEMENTATION.
*  METHOD inject.
*    lcl_oo_factory=>go_object_oriented_object = io_object_oriented_object.
*  ENDMETHOD.
*ENDCLASS.
