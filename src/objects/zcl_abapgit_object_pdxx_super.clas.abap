CLASS zcl_abapgit_object_pdxx_super DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor IMPORTING is_item     TYPE zif_abapgit_definitions=>ty_item
                                  iv_language TYPE spras
                        RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
    DATA ms_objkey TYPE hrsobject.

    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_abapgit_object_pdxx_super IMPLEMENTATION.

  METHOD check_subrc_for.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'RH_READ_OBJECT'
      EXPORTING
        plvar     = '01'
        otype     = ms_objkey-otype
        objid     = ms_objkey-objid
        istat     = '1'
        begda     = sy-datum
        endda     = '99991231'
        ointerval = 'X'
        read_db   = 'X'
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE uname
      INTO rv_user
      FROM hrs1201
      WHERE otype = ms_item-obj_type AND
            objid = ms_item-obj_name.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'RH_HRSOBJECT_DELETE'
      EXPORTING
        act_otype           = ms_objkey-otype
        act_objid           = ms_objkey-objid
        no_confirmation_msg = abap_true
      EXCEPTIONS
        enqueue_failed      = 1
        object_not_deleted  = 2
        object_not_found    = 3
        OTHERS              = 4.       "#EC SUBRC_OK

    check_subrc_for( `RH_HRSOBJECT_DELETE` ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    ASSERT 1 = 2. "Must be redefined
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
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'HRSOBJECT'
                                            iv_argument    = ms_objkey-otype && ms_objkey-objid ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    ASSERT 1 = 2. "Must be redefined
  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    ms_objkey-otype = is_item-obj_type+2(2).
    ms_objkey-objid = ms_item-obj_name.

  ENDMETHOD.

ENDCLASS.
