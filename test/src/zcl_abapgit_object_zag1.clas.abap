CLASS zcl_abapgit_object_zag1 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    CLASS-METHODS upsert
      IMPORTING
        iv_name    TYPE zag1-name
        iv_value   TYPE zag1-value
        iv_package TYPE tadir-devclass.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_type TYPE tadir-object VALUE 'ZAG1'.
ENDCLASS.

CLASS zcl_abapgit_object_zag1 IMPLEMENTATION.

  METHOD upsert.
    DATA ls_data  TYPE zag1.
    DATA ls_tadir TYPE tadir.

    ASSERT sy-sysid = 'ABC'.

    ls_data-name = iv_name.
    ls_data-value = iv_value.
    MODIFY zag1 FROM ls_data.
    ASSERT sy-subrc = 0.

    ls_tadir-pgmid = 'R3TR'.
    ls_tadir-object = c_type.
    ls_tadir-obj_name = condense( to_upper( iv_name ) ).
    ls_tadir-devclass = iv_package.
    MODIFY tadir FROM ls_tadir.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA ls_data TYPE zag1.

    SELECT SINGLE * FROM zag1 INTO ls_data WHERE name = ms_item-obj_name.
    ASSERT sy-subrc = 0.

    io_xml->add( iv_name = 'DATA'
                 ig_data = ls_data ).

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA ls_data TYPE zag1.

    io_xml->read( EXPORTING iv_name = 'DATA'
                  CHANGING  cg_data = ls_data ).

    upsert(
      iv_name    = ls_data-name
      iv_value   = ls_data-value
      iv_package = iv_package ).

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    DELETE FROM zag1 WHERE name = ms_item-obj_name.
    DELETE FROM tadir WHERE pgmid = 'R3TR' AND object = c_type AND name = ms_item-obj_name.
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.
    DATA lv_name TYPE zag1-name.
    SELECT SINGLE name FROM zag1 INTO lv_name WHERE name = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.
    rv_user = sy-uname.
  ENDMETHOD.

  METHOD zif_abapgit_object~jump.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.

ENDCLASS.
