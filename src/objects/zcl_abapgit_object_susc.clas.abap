CLASS zcl_abapgit_object_susc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.

    CONSTANTS transobjecttype_class TYPE c LENGTH 1 VALUE 'C' ##NO_TEXT.

    METHODS has_authorization
      IMPORTING
        !iv_class    TYPE tobc-oclss
        !iv_activity TYPE activ_auth
      RAISING
        zcx_abapgit_exception .
    METHODS is_used
      IMPORTING
        !iv_auth_object_class TYPE tobc-oclss
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    METHODS delete_class
      IMPORTING
        !iv_auth_object_class TYPE tobc-oclss .
    METHODS put_delete_to_transport
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_susc IMPLEMENTATION.


  METHOD delete_class.

    DELETE FROM tobc  WHERE oclss = iv_auth_object_class.
    DELETE FROM tobct WHERE oclss = iv_auth_object_class.

  ENDMETHOD.


  METHOD has_authorization.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
           ID 'DEVCLASS' DUMMY
           ID 'OBJTYPE' FIELD 'SUSC'
           ID 'OBJNAME' FIELD iv_class
           ID 'P_GROUP' DUMMY
           ID 'ACTVT'   FIELD iv_activity.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '467' ).
    ENDIF.

  ENDMETHOD.


  METHOD is_used.

    DATA: lv_used_auth_object_class TYPE tobc-oclss.

    SELECT SINGLE oclss
      FROM tobj
      INTO lv_used_auth_object_class
      WHERE oclss = iv_auth_object_class ##WARN_OK.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '212'
                                         iv_msgv1 = |{ iv_auth_object_class }| ).
    ENDIF.

  ENDMETHOD.


  METHOD put_delete_to_transport.

    DATA: lv_tr_object_name TYPE e071-obj_name,
          lv_tr_return      TYPE char1,
          ls_package_info   TYPE tdevc,
          lv_tadir_object   TYPE tadir-object,
          lv_tadir_obj_name TYPE tadir-obj_name.


    lv_tr_object_name = ms_item-obj_name.

    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname       = lv_tr_object_name
        transobjecttype  = transobjecttype_class
      IMPORTING
        return_from_korr = lv_tr_return.

    IF lv_tr_return <> 'M'.
      zcx_abapgit_exception=>raise( |error in SUSC delete at SUSR_COMMEDITCHECK| ).
    ENDIF.

    CALL FUNCTION 'TR_DEVCLASS_GET'
      EXPORTING
        iv_devclass = ms_item-devclass
      IMPORTING
        es_tdevc    = ls_package_info
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc = 0 AND ls_package_info-korrflag IS INITIAL.
      lv_tadir_object   = ms_item-obj_type.
      lv_tadir_obj_name = lv_tr_object_name.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry = abap_true
          wi_test_modus         = space
          wi_tadir_pgmid        = 'R3TR'
          wi_tadir_object       = lv_tadir_object
          wi_tadir_obj_name     = lv_tadir_obj_name
        EXCEPTIONS
          OTHERS                = 0.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CONSTANTS lc_activity_delete_06 TYPE activ_auth VALUE '06'.

    DATA: lv_auth_object_class TYPE tobc-oclss.


    lv_auth_object_class = ms_item-obj_name.

    TRY.
        IF zif_abapgit_object~exists( ) = abap_false.
          RETURN.
        ENDIF.
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    has_authorization( iv_class    = lv_auth_object_class
                       iv_activity = lc_activity_delete_06 ).

    is_used( lv_auth_object_class ).

    delete_class( lv_auth_object_class ).

    put_delete_to_transport( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
* see function group SUSA

    DATA: ls_tobc       TYPE tobc,
          lv_objectname TYPE e071-obj_name,
          ls_tobct      TYPE tobct.


    io_xml->read( EXPORTING iv_name = 'TOBC'
                  CHANGING cg_data = ls_tobc ).
    io_xml->read( EXPORTING iv_name = 'TOBCT'
                  CHANGING cg_data = ls_tobct ).

    tadir_insert( iv_package ).

    lv_objectname = ms_item-obj_name.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = transobjecttype_class.

    INSERT tobc FROM ls_tobc.                             "#EC CI_SUBRC
* ignore sy-subrc as all fields are key fields

    MODIFY tobct FROM ls_tobct.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_oclss TYPE tobc-oclss.


    SELECT SINGLE oclss FROM tobc INTO lv_oclss
      WHERE oclss = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

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

    DATA: lv_objclass TYPE tobc-oclss.

    lv_objclass = ms_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_tobc  TYPE tobc,
          ls_tobct TYPE tobct.


    SELECT SINGLE * FROM tobc INTO ls_tobc
      WHERE oclss = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tobct INTO ls_tobct
      WHERE oclss = ms_item-obj_name
      AND langu = mv_language.

    io_xml->add( iv_name = 'TOBC'
                 ig_data = ls_tobc ).
    io_xml->add( iv_name = 'TOBCT'
                 ig_data = ls_tobct ).

  ENDMETHOD.
ENDCLASS.
