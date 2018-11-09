CLASS zcl_abapgit_object_susc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

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
        transobjecttype = 'C'.

    INSERT tobc FROM ls_tobc.                             "#EC CI_SUBRC
* ignore sy-subrc as all fields are key fields

    MODIFY tobct FROM ls_tobct.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.

    DATA: objclass     TYPE tobc-oclss.
    DATA: message_text TYPE string.

    objclass = ms_item-obj_name.

* Copy of FM 'SUSR_DELETE_OBJECT_CLASS' 740SP08
* I hate to do this, but there is no other way
    DATA:
      lr_susc   TYPE REF TO cl_auth_tools,
      ls_tobct  TYPE tobct,
      ls_tadir  TYPE tadir,
      ls_tdevc  TYPE tdevc,
      ld_dummy  TYPE xuobjclass,
      ld_tr_obj TYPE e071-obj_name,
      ld_korret TYPE char1, "tv_c1,
      ld_answer TYPE char1. "tv_c1.
*-----------------------------------------------------------------------
    CREATE OBJECT lr_susc.

* 1st - existence check, get data for deletion
    lr_susc->susc_get_class( EXPORTING oclss    = objclass
                             IMPORTING ps_tobct = ls_tobct
                                       ps_tadir = ls_tadir ).
    IF ls_tobct IS INITIAL.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '434'
                                         iv_msgv1 = |{ objclass }| ).
    ENDIF.

* 2nd - authority-check
    IF lr_susc->susc_auth_check( oclss = objclass
                                 actvt = '06' ) <> 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '467' ).
    ENDIF.

* 3rd - where used in auth.-objects test
    SELECT SINGLE oclss FROM tobj INTO ld_dummy
      WHERE oclss = objclass     ##WARN_OK.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '212'
                                         iv_msgv1 = |{ ld_dummy }| ).
    ENDIF.

*!!! delete confirmation popup !!!

* 5th - create a transport task
    ld_tr_obj = objclass.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname       = ld_tr_obj
        transobjecttype  = 'C'
      IMPORTING
        return_from_korr = ld_korret
        return_ls_tadir  = ls_tadir.

    IF ld_korret <> 'M'.
      zcx_abapgit_exception=>raise( |error in SUSC delete at SUSR_COMMEDITCHECK| ).
    ENDIF.
* now the real deletion
    DELETE FROM tobc  WHERE oclss = objclass.
    DELETE FROM tobct WHERE oclss = objclass.           "#EC CI_NOFIRST

* follow-up action ... clean up in table tadir
    CALL FUNCTION 'TR_DEVCLASS_GET'
      EXPORTING
        iv_devclass = ls_tadir-devclass
      IMPORTING
        es_tdevc    = ls_tdevc
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc = 0 AND ls_tdevc-korrflag IS INITIAL.
*   object classes are not connected with the CTS
*   -> if a TADIR-entry exists, we have to reorganize it manualy
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry = 'X'
          wi_test_modus         = space
          wi_tadir_pgmid        = ls_tadir-pgmid
          wi_tadir_object       = ls_tadir-object
          wi_tadir_obj_name     = ls_tadir-obj_name
        EXCEPTIONS
          OTHERS                = 0.
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
