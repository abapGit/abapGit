CLASS zcl_abapgit_object_stcs DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    METHODS constructor
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_language    TYPE spras
        io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.

  PROTECTED SECTION.
    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS get_field_rules
      RETURNING
        VALUE(ro_rules) TYPE REF TO zif_abapgit_field_rules.
    METHODS lock
      RAISING
        zcx_abapgit_exception.
    METHODS unlock.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_stcs IMPLEMENTATION.
  METHOD constructor.
    DATA lr_data TYPE REF TO data.

    super->constructor( is_item        = is_item
                        iv_language    = iv_language
                        io_files       = io_files
                        io_i18n_params = io_i18n_params ).
    " These tables are not available on all supported Basis releases.
    TRY.
        CREATE DATA lr_data TYPE ('STC_SCN_HDR').
        CREATE DATA lr_data TYPE ('STC_SCN_HDR_T').
        CREATE DATA lr_data TYPE ('STC_SCN_TASKS').
        CREATE DATA lr_data TYPE ('STC_SCN_ATTR').
      CATCH cx_sy_create_data_error.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported
          EXPORTING obj_type = ms_item-obj_type.
    ENDTRY.
  ENDMETHOD.


  METHOD get_field_rules.
    ro_rules = zcl_abapgit_field_rules=>create( ).
    ro_rules->add( iv_table     = 'STC_SCN_HDR'
                   iv_field     = 'CREATED_BY'
                   iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
      )->add( iv_table     = 'STC_SCN_HDR'
              iv_field     = 'CHANGED_BY'
              iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user
      )->add( iv_table     = 'STC_SCN_HDR'
              iv_field     = 'CREATED_ON'
              iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date
      )->add( iv_table     = 'STC_SCN_HDR'
              iv_field     = 'CHANGED_ON'
              iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date ).

  ENDMETHOD.


  METHOD get_generic.
    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        iv_language    = mv_language
        io_field_rules = get_field_rules( ).
  ENDMETHOD.


  METHOD lock.
    DATA lv_key TYPE rstable-varkey.
    lv_key = ms_item-obj_name.
    CALL FUNCTION 'ENQUEUE_E_TABLES'
      EXPORTING
        tabname = 'STC_SCN_HDR'
        varkey  = lv_key
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD unlock.
    DATA lv_key TYPE rstable-varkey.
    lv_key = ms_item-obj_name.
    CALL FUNCTION 'DEQUEUE_E_TABLES'
      EXPORTING
        tabname = 'STC_SCN_HDR'
        varkey  = lv_key.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE changed_by FROM ('STC_SCN_HDR') INTO rv_user
      WHERE scenario_id = ms_item-obj_name.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA lx_error TYPE REF TO cx_root.
    lock( ).
    TRY.
        get_generic( )->delete( iv_package   = iv_package
                                iv_transport = iv_transport ).
      CATCH cx_root INTO lx_error.
        unlock( ).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
    unlock( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA lx_error TYPE REF TO cx_root.
    lock( ).
    TRY.
        get_generic( )->deserialize( iv_package   = iv_package
                                     io_xml       = io_xml
                                     iv_transport = iv_transport ).
      CATCH cx_root INTO lx_error.
        unlock( ).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
    unlock( ).
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
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    DATA lv_table TYPE rstable-tabname VALUE 'STC_SCN_HDR'.
    DATA lv_argument TYPE seqg3-garg.
    CONCATENATE lv_table ms_item-obj_name INTO lv_argument RESPECTING BLANKS.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_TABLES'
                                           iv_argument    = lv_argument ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    RETURN.
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
