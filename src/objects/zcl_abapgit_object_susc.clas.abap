CLASS zcl_abapgit_object_susc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
  PROTECTED SECTION.
    CONSTANTS transobjecttype_class TYPE char1 VALUE 'C' ##NO_TEXT.

    METHODS has_authorization
      IMPORTING i_object_type TYPE seu_objid
                i_class       TYPE tobc-oclss
                i_activity    TYPE activ_auth
      RAISING   zcx_abapgit_exception.
    METHODS is_used
      IMPORTING i_auth_object_class TYPE tobc-oclss
      RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.
    METHODS delete_class
      IMPORTING i_auth_object_class TYPE tobc-oclss.
    METHODS put_delete_to_transport
      IMPORTING i_auth_object_class TYPE tobc-oclss
                i_object_type       TYPE seu_objid
      RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_susc IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lv_oclss TYPE tobc-oclss.


    SELECT SINGLE oclss FROM tobc INTO lv_oclss
      WHERE oclss = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

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
        transobjecttype = zcl_abapgit_object_susc=>transobjecttype_class.

    INSERT tobc FROM ls_tobc.                             "#EC CI_SUBRC
* ignore sy-subrc as all fields are key fields

    MODIFY tobct FROM ls_tobct.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    CONSTANTS activity_delete_06 TYPE activ_auth VALUE '06'.

    DATA: auth_object_class TYPE tobc-oclss.
    DATA: object_type       TYPE seu_objid.
    DATA: tr_object_name    TYPE e071-obj_name.
    DATA: tr_return         TYPE char1.

    auth_object_class = ms_item-obj_name.
    object_type = ms_item-obj_type.

    TRY.
        me->zif_abapgit_object~exists( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    has_authorization( EXPORTING i_object_type = object_type
                                 i_class       = auth_object_class
                                 i_activity    = activity_delete_06 ).

    is_used( auth_object_class ).

    delete_class( auth_object_class ).

    put_delete_to_transport( i_auth_object_class = auth_object_class
                             i_object_type       = object_type ).
  ENDMETHOD.

  METHOD put_delete_to_transport.

    DATA: tr_object_name TYPE e071-obj_name.
    DATA: tr_return      TYPE char1.
    DATA: package_info   TYPE tdevc.

    tr_object_name = i_auth_object_class.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname       = tr_object_name
        transobjecttype  = zcl_abapgit_object_susc=>transobjecttype_class
      IMPORTING
        return_from_korr = tr_return.

    IF tr_return <> 'M'.
      zcx_abapgit_exception=>raise( |error in SUSC delete at SUSR_COMMEDITCHECK| ).
    ENDIF.

    CALL FUNCTION 'TR_DEVCLASS_GET'
      EXPORTING
        iv_devclass = ms_item-devclass
      IMPORTING
        es_tdevc    = package_info
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc = 0 AND package_info-korrflag IS INITIAL.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry = abap_true
          wi_test_modus         = space
          wi_tadir_pgmid        = 'R3TR'
          wi_tadir_object       = i_object_type
          wi_tadir_obj_name     = tr_object_name
        EXCEPTIONS
          OTHERS                = 0.
    ENDIF.

  ENDMETHOD.

  METHOD delete_class.

    DELETE FROM tobc  WHERE oclss = i_auth_object_class.
    DELETE FROM tobct WHERE oclss = i_auth_object_class.

  ENDMETHOD.

  METHOD is_used.

    DATA: used_auth_object_class TYPE tobc-oclss.
    SELECT SINGLE oclss
      FROM tobj
      INTO used_auth_object_class
      WHERE oclss = i_auth_object_class ##WARN_OK.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '212'
                                         iv_msgv1 = |{ used_auth_object_class }| ).
    ENDIF.

  ENDMETHOD.

  METHOD has_authorization.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
           ID 'DEVCLASS' DUMMY
           ID 'OBJTYPE' FIELD i_object_type
           ID 'OBJNAME' FIELD i_class
           ID 'P_GROUP' DUMMY
           ID 'ACTVT'   FIELD i_activity.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '467' ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    DATA: lv_objclass TYPE tobc-oclss.

    lv_objclass = ms_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.

ENDCLASS.
