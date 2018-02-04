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
