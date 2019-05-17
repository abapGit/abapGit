CLASS zcl_abapgit_object_sucu DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SUCU IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
    rv_user = zcl_abapgit_objects_super=>c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    lo_generic->delete( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    lo_generic->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    rv_bool = lo_generic->exists( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    zcx_abapgit_exception=>raise( |TODO: Jump| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    lo_generic->serialize( io_xml ).

  ENDMETHOD.
ENDCLASS.
