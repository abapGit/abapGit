CLASS zcl_abapgit_object_sapc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_saxx_super FINAL.

  PROTECTED SECTION.
    METHODS:
      get_persistence_class_name REDEFINITION,
      get_data_class_name REDEFINITION,
      get_data_structure_name REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_object_sapc IMPLEMENTATION.

  METHOD get_data_class_name.

    r_data_class_name = 'CL_APC_APPLICATION_OBJ_DATA'.

  ENDMETHOD.

  METHOD get_data_structure_name.

    r_data_structure_name = 'APC_APPLICATION_COMPLETE'.

  ENDMETHOD.

  METHOD get_persistence_class_name.

    r_persistence_class_name = 'CL_APC_APPLICATION_OBJ_PERS'.

  ENDMETHOD.

ENDCLASS.
