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
    TYPES: BEGIN OF ty_last_change,
             changed_on TYPE d,
             changed_at TYPE t,
             changed_by TYPE syuname,
           END OF ty_last_change.
    DATA: ls_header_change  TYPE ty_last_change,
          ls_element_change TYPE ty_last_change.

    SELECT SINGLE moddate modtime modifier
      FROM ('SLDW_HEADER')
      INTO ls_header_change
      WHERE name = ms_item-obj_name.

    SELECT moddate modtime modifier
      UP TO 1 ROWS
      FROM ('SLDW_ELEMENTS')
      INTO ls_element_change
      WHERE name = ms_item-obj_name
      ORDER BY moddate DESCENDING modtime DESCENDING.
    ENDSELECT.
    IF ls_element_change-changed_by IS NOT INITIAL
    AND ls_element_change            >= ls_header_change.
      rv_user = ls_element_change-changed_by.
    ELSEIF ls_header_change-changed_by IS NOT INITIAL.
      rv_user = ls_header_change-changed_by.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.
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
    DATA: lr_key      TYPE REF TO data,
          lv_funcname TYPE rs38l_fnam VALUE 'SLDW_MAINTAIN'.
    FIELD-SYMBOLS: <lt_key>  TYPE STANDARD TABLE,
                   <ls_key>  TYPE any,
                   <lv_name> TYPE any.

    TRY.
        CREATE DATA lr_key TYPE STANDARD TABLE OF ('SLDW_S_KEY').
        ASSIGN lr_key->* TO <lt_key>.
        APPEND INITIAL LINE TO <lt_key> ASSIGNING <ls_key>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_key> TO <lv_name>.
        <lv_name> = ms_item-obj_name.

        CALL FUNCTION lv_funcname
          EXPORTING
            id_area     = 'SLDD'
            id_chg_mode = space
            it_key      = <lt_key>.

        rv_exit = abap_true.
      CATCH cx_sy_create_data_error cx_sy_dyn_call_error.
        " SLDW framework not available on this release - nothing to jump to
    ENDTRY.
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
