*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_SAMC
*&---------------------------------------------------------------------*

*CLASS lcl_object_samc DEFINITION INHERITING FROM lcl_objects_saxx_super FINAL.
*
*  PROTECTED SECTION.
*    METHODS:
*      get_persistence_class_name REDEFINITION,
*      get_data_class_name REDEFINITION,
*      get_data_structure_name REDEFINITION.
*
*ENDCLASS.                    "lcl_object_samc DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS lcl_object_samc IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS lcl_object_samc IMPLEMENTATION.
*
*  METHOD get_data_class_name.
*
*    r_data_class_name = 'CL_AMC_APPLICATION_OBJ_DATA'.
*
*  ENDMETHOD.
*
*  METHOD get_data_structure_name.
*
*    r_data_structure_name = 'AMC_APPLICATION_COMPLETE'.
*
*  ENDMETHOD.
*
*  METHOD get_persistence_class_name.
*
*    r_persistence_class_name = 'CL_AMC_APPLICATION_OBJ_PERS'.
*
*  ENDMETHOD.
*
*ENDCLASS.                    "lcl_object_samc IMPLEMENTATION
