CLASS zcl_abapgit_object_zag1 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    CLASS-METHODS create
      IMPORTING
        iv_name  TYPE zag1-name
        iv_value TYPE zag1-value.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_abapgit_object_zag1 IMPLEMENTATION.

  METHOD create.
    DATA ls_data TYPE zag1.

    ASSERT sy-sysid = 'ABC'.

    ls_data-name = iv_name.
    ls_data-value = iv_value.
    MODIFY zag1 FROM ls_data.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~jump.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~get_comparator.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN. " todo, implement method
  ENDMETHOD.

ENDCLASS.
