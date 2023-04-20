CLASS zcl_abapgit_object_g4ba DEFINITION
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



CLASS zcl_abapgit_object_g4ba IMPLEMENTATION.


  METHOD get_field_rules.

    ro_result = zcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CREATED_TS'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CHANGED_TS'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CREATED_TS'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CHANGED_TS'
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-timestamp ).

  ENDMETHOD.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA lv_created TYPE sy-uname.
    DATA lv_changed TYPE sy-uname.

    SELECT SINGLE created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_V4_MSGR')
      WHERE group_id = ms_item-obj_name.

    rv_user = lv_changed.
    IF lv_changed IS INITIAL.
      rv_user = lv_created.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    get_generic( )->delete( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

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
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
ENDCLASS.
