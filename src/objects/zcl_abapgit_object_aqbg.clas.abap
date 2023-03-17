CLASS zcl_abapgit_object_aqbg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception .

    METHODS get_field_rules
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abapgit_field_rules.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_AQBG IMPLEMENTATION.


  METHOD get_field_rules.

    ro_result = zcl_abapgit_field_rules=>create( ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGCNAM'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGUNAM'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGCDAT'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGUDAT'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'DEVC'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-package ).

  ENDMETHOD.


  METHOD get_generic.
* transaction SQ03
    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        io_field_rules = get_field_rules( )
        iv_language    = mv_language.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE bgunam FROM aqgdbbg INTO rv_user WHERE num = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = zcl_abapgit_objects_super=>c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    set_default_transport( iv_transport ).

    get_generic( )->delete( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    set_default_transport( iv_transport ).

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
ENDCLASS.
