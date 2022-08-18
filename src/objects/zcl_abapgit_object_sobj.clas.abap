CLASS zcl_abapgit_object_sobj DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
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
    METHODS get_field_rules
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abapgit_field_rules.
ENDCLASS.



CLASS zcl_abapgit_object_sobj IMPLEMENTATION.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE crea_user, chan_user INTO @DATA(ls_userinfo) FROM tojtb WHERE name = @ms_item-obj_name.
    IF ls_userinfo-chan_user IS INITIAL.
      ls_userinfo-chan_user = ls_userinfo-crea_user.
    ENDIF.
    rv_user = ls_userinfo-chan_user.

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
    "No need as GENERIC class already handles it
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.

  METHOD get_field_rules.

    ro_result = zcl_abapgit_field_rules=>create( ).
    ro_result->add(
        iv_table     = 'TOJTB'
        iv_field     = 'CREA_USER'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'CREA_DATE'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'CREA_TIME'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time
*    )->add(
*        iv_table     = 'TOJTB'
*        iv_field     = 'CREA_REL'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'CHAN_USER'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'CHAN_DATE'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'CHAN_TIME'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time
*    )->add(
*        iv_table     = 'TOJTB'
*        iv_field     = 'CHAN_REL'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
*    )->add(
*        iv_table     = 'TOJTB'
*        iv_field     = 'VERSION'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'ACTV_USER'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'ACTV_DATE'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'ACTV_TIME'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'REL_USER'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'REL_DATE'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
    )->add(
        iv_table     = 'TOJTB'
        iv_field     = 'REL_TIME'
        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time
     ).
    ")->add(
*        iv_table     = 'TOJTB'
*        iv_field     = 'REL_REL'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
*    )->add(
*        iv_table     = 'SWOTD'
*        iv_field     = 'CREA_REL'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
*     )->add(
*        iv_table     = 'SWOTDQ'
*        iv_field     = 'CREA_REL'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
*     )->add(
*        iv_table     = 'SWOTDI'
*        iv_field     = 'CREA_REL'
*        iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-???
*     ).

  ENDMETHOD.
ENDCLASS.
