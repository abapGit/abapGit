CLASS zcl_abapgit_object_odso DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_abapgit_object_odso IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_dsonam  TYPE rsobjvers,
          ls_return  TYPE bapiret2,
          ls_details TYPE bapi6116.

    lv_dsonam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
      EXPORTING
        odsobject = lv_dsonam
      IMPORTING
        details   = ls_details
        return    = ls_return.

    IF ls_return-type = 'E'.
      zcx_abapgit_exception=>raise( |Error when geting changed by of ODSO: { ls_return-message }| ).
    ENDIF.

    rv_user = ls_details-tstpnm.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_odso         TYPE rsdodsobject,
          lv_objname      TYPE sobj_name,
          lr_collection   TYPE REF TO cl_rsd_odso_collection.

    lv_odso = ms_item-obj_name.
    lv_objname =  ms_item-obj_name.

    CREATE OBJECT lr_collection.

    TRY.
        lr_collection->add_tlogo(
            i_objnm          = lv_objname
            i_modify         = abap_true
            i_delete         = abap_true  ).

        lr_collection->delete( ).

      CATCH cx_rs_cancelled.
        zcx_abapgit_exception=>raise( |Canceled deletion of ODSO: { ms_item-obj_name }| ).
      CATCH cx_rs_existing.
      CATCH cx_rs_not_found.
        zcx_abapgit_exception=>raise( |ODSO not found: { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_odso        TYPE rsdodsobject,
          ls_details     TYPE bapi6116,
          lt_infoobjects TYPE STANDARD TABLE OF bapi6116io,
          lt_navigation  TYPE STANDARD TABLE OF bapi6116na,
          lt_indexes     TYPE STANDARD TABLE OF bapi6116in,
          lt_index_iobj  TYPE STANDARD TABLE OF bapi6116ii,
          lt_return      TYPE STANDARD TABLE OF bapiret2,
          ls_return      TYPE bapiret2.

    io_xml->read( EXPORTING iv_name = 'ODSO'
                  CHANGING  cg_data = ls_details ).

    io_xml->read( EXPORTING iv_name = 'INFOOBJECTS'
                  CHANGING  cg_data =  lt_infoobjects ).

    io_xml->read( EXPORTING iv_name = 'NAVIGATION'
                  CHANGING  cg_data =  lt_navigation ).

    io_xml->read( EXPORTING iv_name = 'INDEXES'
                  CHANGING  cg_data =  lt_indexes ).

    io_xml->read( EXPORTING iv_name = 'INDEX_IOBJ'
                  CHANGING  cg_data =  lt_index_iobj ).

    CALL FUNCTION 'BAPI_ODSO_CREATE'
      EXPORTING
        details              = ls_details
      IMPORTING
        odsobject            = lv_odso
      TABLES
        infoobjects          = lt_infoobjects
        navigationattributes = lt_navigation
        indexes              = lt_indexes
        indexesinfoobjects   = lt_index_iobj
        return               = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Error when creating ODSO: { ls_return-message }| ).
    ENDIF.

    CALL FUNCTION 'BAPI_ODSO_ACTIVATE'
      EXPORTING
        odsobject = lv_odso
      TABLES
        return    = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Error when activating ODSO: { ls_return-message }| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_iobjnm TYPE rsdodsobject.

    SELECT SINGLE odsobject
    FROM rsdodso
    INTO lv_iobjnm
    WHERE odsobject = ms_item-obj_name.

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

    DATA: lv_dsona TYPE  rsdodsobject.

    lv_dsona = ms_item-obj_name.

    rv_active = cl_rsd_odso=>factory( i_odsobject = lv_dsona )->is_active( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object =  ms_item-obj_name.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'RSD_S_PROV'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    zcx_abapgit_exception=>raise( |Jump to ODSO is not yet supported| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_dsonam      TYPE rsdodsobject,
          ls_return      TYPE bapiret2,
          ls_details     TYPE bapi6116,
          lt_infoobjects TYPE STANDARD TABLE OF bapi6116io,
          lt_navigation  TYPE STANDARD TABLE OF bapi6116na,
          lt_indexes     TYPE STANDARD TABLE OF bapi6116in,
          lt_index_iobj  TYPE STANDARD TABLE OF bapi6116ii.

    lv_dsonam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
      EXPORTING
        odsobject            = lv_dsonam
      IMPORTING
        details              = ls_details
        return               = ls_return
      TABLES
        infoobjects          = lt_infoobjects
        navigationattributes = lt_navigation
        indexes              = lt_indexes
        indexesinfoobjects   = lt_index_iobj.

    IF ls_return-type = 'E'.
      zcx_abapgit_exception=>raise( |Error when geting getails of ODSO: { ls_return-message }| ).
    ENDIF.

    CLEAR: ls_details-tstpnm, ls_details-timestmp, ls_details-tstpnm, ls_details-conttimestmp,ls_details-owner.

    io_xml->add( iv_name = 'ODSO'
                 ig_data = ls_details ).

    io_xml->add( iv_name = 'INFOOBJECTS'
                 ig_data = lt_infoobjects ).

    io_xml->add( iv_name = 'NAVIGATION'
                 ig_data = lt_navigation ).

    io_xml->add( iv_name = 'INDEXES'
                 ig_data = lt_indexes ).

    io_xml->add( iv_name = 'INDEX_IOBJ'
                 ig_data = lt_index_iobj ).

  ENDMETHOD.
ENDCLASS.
