CLASS zcl_abapgit_object_iobj DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_iobj IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_viobj TYPE rsd_s_viobj,
          lv_objna TYPE rsd_iobjnm.

    lv_objna = ms_item-obj_name.

    CALL FUNCTION 'RSD_IOBJ_GET'
      EXPORTING
        i_iobjnm  = lv_objna
        i_objvers = 'A'
      IMPORTING
        e_s_viobj = ls_viobj.

    rv_user = ls_viobj-tstpnm.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lt_iobjname TYPE rsd_t_c30.

    APPEND ms_item-obj_name TO lt_iobjname.

    CALL FUNCTION 'RSDG_IOBJ_MULTI_DELETE'
      EXPORTING
        i_t_iobjnm = lt_iobjname.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error when deleting iobj | ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_details TYPE bapi6108,
          lt_infoobj TYPE STANDARD TABLE OF bapi6108io,
          ls_return  TYPE bapiret2,
          lt_return  TYPE STANDARD TABLE OF bapiret2.

    io_xml->read( EXPORTING iv_name = 'IOBJ'
                   CHANGING cg_data = ls_details ).

    CALL FUNCTION 'BAPI_IOBJ_CREATE'
      EXPORTING
        details = ls_details
      IMPORTING
        return  = ls_return.

    IF ls_return-type = 'E'.
      zcx_abapgit_exception=>raise( |Error when creating iobj: { ls_return-message }| ).
    ENDIF.

    APPEND ls_details-infoobject TO lt_infoobj.

    CALL FUNCTION 'BAPI_IOBJ_ACTIVATE_MULTIPLE'
      TABLES
        infoobjects = lt_infoobj
        return      = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Error when activating iobj: { ls_return-message }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_iobjnm TYPE rsdiobjnm.

    SELECT SINGLE iobjnm
    FROM rsdiobj
    INTO lv_iobjnm
    WHERE iobjnm = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

    DATA: ls_viobj TYPE rsd_s_viobj,
          lv_objna TYPE rsd_iobjnm.

    lv_objna = ms_item-obj_name.

    CALL FUNCTION 'RSD_IOBJ_GET'
      EXPORTING
        i_iobjnm  = lv_objna
        i_objvers = 'A'
      IMPORTING
        e_s_viobj = ls_viobj.

    IF ls_viobj-objstat = 'ACT'.
      rv_active = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object =  ms_item-obj_name.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_BIW_PROV'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_iobjnam TYPE rsiobjnm,
          ls_return  TYPE bapiret2,
          ls_details TYPE bapi6108.

    lv_iobjnam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
      EXPORTING
        infoobject = lv_iobjnam
      IMPORTING
        details    = ls_details
        return     = ls_return.

    IF ls_return-type = 'E'.
      zcx_abapgit_exception=>raise( |Error when geting getails of iobj: { ls_return-message }| ).
    ENDIF.

    CLEAR: ls_details-tstpnm, ls_details-timestmp, ls_details-dbroutid.

    io_xml->add( iv_name = 'IOBJ'
                 ig_data = ls_details ).

  ENDMETHOD.
ENDCLASS.
