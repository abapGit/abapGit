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

    DATA: lt_iobjname     TYPE rsd_t_c30,
          lv_object       TYPE string,
          lv_object_class TYPE string,
          ls_tadir        TYPE zif_abapgit_definitions=>ty_tadir,
          lv_transp_pkg   TYPE abap_bool.

    ls_tadir = zcl_abapgit_factory=>get_tadir( )->read_single(
                                                   iv_object   = ms_item-obj_type
                                                   iv_obj_name = ms_item-obj_name ).

    lv_transp_pkg =
    zcl_abapgit_factory=>get_sap_package( iv_package = ls_tadir-devclass )->are_changes_recorded_in_tr_req( ).

    APPEND ms_item-obj_name TO lt_iobjname.

    CALL FUNCTION 'RSDG_IOBJ_MULTI_DELETE'
      EXPORTING
        i_t_iobjnm = lt_iobjname.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error when deleting infoObject | ).
    ENDIF.

    IF lv_transp_pkg = abap_true.

      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.

      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object              = lv_object
          object_class        = lv_object_class
          master_language     = mv_language
          global_lock         = abap_true
          mode                = 'D'
          suppress_dialog     = abap_true
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

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

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

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
    rs_metadata-delete_tadir = abap_true.
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
    zcx_abapgit_exception=>raise( |Jump to infoObjects is not yet supported| ).
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
