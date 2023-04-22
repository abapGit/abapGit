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
        VALUE(ri_rules) TYPE REF TO zif_abapgit_field_rules.
    METHODS is_locked RETURNING VALUE(rv_is_locked) TYPE abap_bool.
    METHODS is_objtype_locked RETURNING VALUE(rv_is_locked) TYPE abap_bool.
    METHODS is_program_locked RETURNING VALUE(rv_is_locked) TYPE abap_bool.
    METHODS get_program RETURNING VALUE(rv_program) TYPE tojtb-progname.
ENDCLASS.



CLASS zcl_abapgit_object_sobj IMPLEMENTATION.


  METHOD get_field_rules.

    ri_rules = zcl_abapgit_field_rules=>create( ).
    ri_rules->add(
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
      iv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time ).

  ENDMETHOD.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.


  METHOD get_program.
    SELECT SINGLE progname INTO rv_program FROM tojtb WHERE name = ms_item-obj_name.
  ENDMETHOD.


  METHOD is_locked.
    rv_is_locked = boolc( is_objtype_locked( ) = abap_true OR is_program_locked(  ) = abap_true ).
  ENDMETHOD.


  METHOD is_objtype_locked.
    CONSTANTS lc_tabname TYPE tabname VALUE 'SWOTBASDAT'.
    DATA lv_varkey TYPE vim_enqkey.

    rv_is_locked = abap_false.
    lv_varkey = ms_item-obj_name.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname      = lc_tabname
        varkey       = lv_varkey
      EXCEPTIONS
        foreign_lock = 1
        OTHERS       = 999.
    IF sy-subrc IS NOT INITIAL.
      rv_is_locked = abap_true.
    ELSE.
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          tabname = lc_tabname
          varkey  = lv_varkey.
    ENDIF.
  ENDMETHOD.


  METHOD is_program_locked.
    CONSTANTS lc_enqueue_exclusive TYPE enqmode VALUE 'X'.
    DATA lv_progname TYPE progname.

    rv_is_locked = abap_false.
    lv_progname = get_program(  ).

    IF lv_progname IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_ESRDIRE'
        EXPORTING
          mode_trdir   = lc_enqueue_exclusive
          name         = lv_progname
        EXCEPTIONS
          foreign_lock = 1
          OTHERS       = 999.
      IF sy-subrc IS NOT INITIAL.
        rv_is_locked = abap_true.
      ELSE.
        CALL FUNCTION 'DEQUEUE_ESRDIRE'
          EXPORTING
            mode_trdir = lc_enqueue_exclusive
            name       = lv_progname.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA: BEGIN OF ls_userinfo,
            crea_user TYPE tojtb-crea_user,
            chan_user TYPE tojtb-chan_user,
          END   OF ls_userinfo.

    SELECT SINGLE
        crea_user
        chan_user
    INTO (ls_userinfo-crea_user, ls_userinfo-chan_user)
    FROM tojtb WHERE name = ms_item-obj_name.

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
    rv_is_locked = is_locked(  ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    "No need as GENERIC class already handles it
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
