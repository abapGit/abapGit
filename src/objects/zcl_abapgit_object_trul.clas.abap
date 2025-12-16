CLASS zcl_abapgit_object_trul DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_object_trul IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE chuser FROM /ltb/tr_hdr INTO rv_user WHERE id = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

* TODO

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* TODO

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_id TYPE /ltb/tr_hdr-id.

    SELECT SINGLE id FROM /ltb/tr_hdr INTO lv_id WHERE id = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

* TODO

    DATA(instance) = /ltb/cl_tr_standard_rule=>create( ms_item-obj_name ).
    instance->is_inactive( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = '/LTB/E_TR_HDR'
      iv_argument    = ms_item-obj_name ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

* TODO

  ENDMETHOD.
ENDCLASS.
