CLASS zcl_abapgit_object_bdef DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_abapgit_object_bdef IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    " Deletes object instance
  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.
    " Restores RAP Behavior Definition from repository XML
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.
    rv_bool = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_object~jump.
    " Jump to ADT editor
  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.
    IF ms_item-obj_name IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add(
      iv_name = 'BDEF'
      ig_data = ms_item-obj_name ).
  ENDMETHOD.

ENDCLASS.
