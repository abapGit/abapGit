CLASS zcl_abapgit_object_iwsg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.

    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    METHODS get_rule
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abapgit_rule.
ENDCLASS.



CLASS zcl_abapgit_object_iwsg IMPLEMENTATION.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_rule     = get_rule( )
        is_item     = ms_item
        iv_language = mv_language.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = zcl_abapgit_objects_super=>c_user_unknown.
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
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.

  METHOD get_rule.
    DATA ls_record TYPE /iwfnd/i_med_srh.
    DATA ls_item TYPE zif_abapgit_rule=>ty_item.
    DATA lt_item TYPE zif_abapgit_rule=>ty_items.

    SELECT SINGLE *
      FROM /iwfnd/i_med_srh
      INTO ls_record
     WHERE srv_identifier = ms_item-obj_name
       AND is_active      = 'A'.

    ls_item-tabname     = '/IWFND/I_MED_SRH'.
    ls_item-fieldname   = 'CREATED_BY'.
    ls_item-clear_field = abap_true.
    IF ls_record-created_by IS INITIAL.
      ls_item-fill_rule   = zif_abapgit_rule=>c_fill_rule-user.
    ENDIF.
    INSERT ls_item INTO TABLE lt_item.

    ls_item-tabname     = '/IWFND/I_MED_SRH'.
    ls_item-fieldname   = 'CREATED_TIMESTMP'.
    ls_item-clear_field = abap_true.
    IF ls_record-created_timestmp IS INITIAL.
      ls_item-fill_rule   = zif_abapgit_rule=>c_fill_rule-timestamp.
    ENDIF.
    INSERT ls_item INTO TABLE lt_item.

    ls_item-tabname     = '/IWFND/I_MED_SRH'.
    ls_item-fieldname   = 'CHANGED_BY'.
    ls_item-clear_field = abap_true.
    ls_item-fill_rule   = zif_abapgit_rule=>c_fill_rule-user.
    INSERT ls_item INTO TABLE lt_item.

    ls_item-tabname     = '/IWFND/I_MED_SRH'.
    ls_item-fieldname   = 'CHANGED_TIMESTMP'.
    ls_item-clear_field = abap_true.
    ls_item-fill_rule   = zif_abapgit_rule=>c_fill_rule-timestamp.
    INSERT ls_item INTO TABLE lt_item.

    ro_result = zcl_abapgit_rule=>create( ).
    ro_result->add_items( lt_item ).
  ENDMETHOD.

ENDCLASS.
