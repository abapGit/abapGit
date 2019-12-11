CLASS zcl_abapgit_object_iobj DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_iobj IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_objna TYPE c LENGTH 30,
          lr_viobj TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_tstpnm> TYPE any,
      <ls_viobj>  TYPE any.

    lv_objna = ms_item-obj_name.

    TRY.
        CREATE DATA lr_viobj TYPE ('RSD_S_VI_OBJ').
      CATCH cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( |IOBJ is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_viobj->* TO <ls_viobj>.

    CALL FUNCTION 'RSD_IOBJ_GET'
      EXPORTING
        i_iobjnm  = lv_objna
        i_objvers = 'A'
      IMPORTING
        e_s_viobj = <ls_viobj>.

    ASSIGN COMPONENT 'TSTPNM' OF STRUCTURE <ls_viobj> TO <lv_tstpnm>.

    rv_user = <lv_tstpnm>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    TYPES: BEGIN OF t_iobj,
             objnm TYPE c LENGTH 30.
    TYPES END OF t_iobj.

    DATA: lt_iobjname     TYPE STANDARD TABLE OF t_iobj,
          lv_object       TYPE string,
          lv_object_class TYPE string,
          lv_transp_pkg   TYPE abap_bool.

    lv_transp_pkg = zcl_abapgit_factory=>get_sap_package( iv_package
                                      )->are_changes_recorded_in_tr_req( ).

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
    TRY.
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

      CATCH  cx_sy_dyn_call_illegal_func.
        zcx_abapgit_exception=>raise( |Necessary BW function modules not found| ).
    ENDTRY.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_iobjnm TYPE char30.

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

    DATA: lv_objna TYPE c LENGTH 30,
          lr_viobj TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_objstat> TYPE any,
      <ls_viobj>   TYPE any.

    lv_objna = ms_item-obj_name.

    TRY.
        CREATE DATA lr_viobj TYPE ('RSD_S_VI_OBJ').
      CATCH cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( |IOBJ is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_viobj->* TO <ls_viobj>.

    CALL FUNCTION 'RSD_IOBJ_GET'
      EXPORTING
        i_iobjnm  = lv_objna
        i_objvers = 'A'
      IMPORTING
        e_s_viobj = <ls_viobj>.

    ASSIGN COMPONENT 'OBJSTAT' OF STRUCTURE <ls_viobj> TO <lv_objstat>.

    IF <lv_objstat> = 'ACT' AND sy-subrc = 0.
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
