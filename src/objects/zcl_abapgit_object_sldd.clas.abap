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
    CREATE OBJECT ro_generic
      EXPORTING
        is_item     = ms_item
        iv_language = mv_language.
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
    DATA: lv_table TYPE objsl-tobj_name,
          lr_data  TYPE REF TO data.
    DATA: lt_tables TYPE STANDARD TABLE OF objsl-tobj_name.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_data>  TYPE any,
                   <lv_field> TYPE any.

    SELECT DISTINCT tobj_name
      FROM objsl
      INTO TABLE lt_tables
      WHERE objectname = ms_item-obj_type
        AND objecttype = 'L'
        AND tobject    = 'TABU'
      ORDER BY tobj_name.
    LOOP AT lt_tables INTO lv_table.
      CREATE DATA lr_data TYPE STANDARD TABLE OF (lv_table).
      ASSIGN lr_data->* TO <lt_data>.
      IF lv_table = 'SLDW_HEADERT'.
        SELECT * FROM (lv_table)
          INTO TABLE <lt_data>
          WHERE spras = mv_language
            AND name  = ms_item-obj_name
          ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM (lv_table)
          INTO TABLE <lt_data>
          WHERE name = ms_item-obj_name
          ORDER BY PRIMARY KEY.
      ENDIF.

      LOOP AT <lt_data> ASSIGNING <ls_data>.
        ASSIGN COMPONENT 'MODIFIER' OF STRUCTURE <ls_data> TO <lv_field>.
        IF sy-subrc = 0.
          CLEAR <lv_field>.
        ENDIF.
        ASSIGN COMPONENT 'MODDATE' OF STRUCTURE <ls_data> TO <lv_field>.
        IF sy-subrc = 0.
          CLEAR <lv_field>.
        ENDIF.
        ASSIGN COMPONENT 'MODTIME' OF STRUCTURE <ls_data> TO <lv_field>.
        IF sy-subrc = 0.
          CLEAR <lv_field>.
        ENDIF.
      ENDLOOP.

      io_xml->add( iv_name = lv_table
                   ig_data = <lt_data> ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
