CLASS zcl_abapgit_object_sldd DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_sldd IMPLEMENTATION.
  METHOD get_generic.
    DATA lo_field_rules TYPE REF TO zif_abapgit_field_rules.

    lo_field_rules = zcl_abapgit_field_rules=>create( ).
    lo_field_rules->add( iv_table     = 'SLDW_HEADER'
                         iv_field     = 'MODIFIER'
                         iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user ).
    lo_field_rules->add( iv_table     = 'SLDW_HEADER'
                         iv_field     = 'MODDATE'
                         iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date ).
    lo_field_rules->add( iv_table     = 'SLDW_HEADER'
                         iv_field     = 'MODTIME'
                         iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time ).
    lo_field_rules->add( iv_table     = 'SLDW_ELEMENTS'
                         iv_field     = 'MODIFIER'
                         iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user ).
    lo_field_rules->add( iv_table     = 'SLDW_ELEMENTS'
                         iv_field     = 'MODDATE'
                         iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date ).
    lo_field_rules->add( iv_table     = 'SLDW_ELEMENTS'
                         iv_field     = 'MODTIME'
                         iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time ).

    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        iv_language    = mv_language
        io_field_rules = lo_field_rules.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    get_generic( )->delete( iv_package   = iv_package
                            iv_transport = iv_transport ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    get_generic( )->deserialize( iv_package   = iv_package
                                 io_xml       = io_xml
                                 iv_transport = iv_transport ).
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SLDW'
                                            iv_argument    = ms_item-obj_name ).
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
