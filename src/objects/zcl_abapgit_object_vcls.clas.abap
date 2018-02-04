CLASS zcl_abapgit_object_vcls DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PRIVATE SECTION.
* See include MTOBJCON:
    CONSTANTS: c_cluster_type TYPE c VALUE 'C'.
    CONSTANTS: c_mode_insert  TYPE obj_para-maint_mode VALUE 'I'.

ENDCLASS.

CLASS zcl_abapgit_object_vcls IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~exists.

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

    IF lv_changedate IS INITIAL.
* same logic as in function module VIEWCLUSTER_GET_DEFINITION
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~serialize.

    DATA: lv_vclname      TYPE vcl_name,
          ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_vclname = ms_item-obj_name.

    CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
      EXPORTING
        vclname                = lv_vclname
      IMPORTING
        vcldir_entry           = ls_vcldir_entry
      TABLES
        vclstruc_tab           = lt_vclstruc
        vclstrudep_tab         = lt_vclstrudep
        vclmf_tab              = lt_vclmf
      EXCEPTIONS
        viewcluster_not_found  = 1
        incomplete_viewcluster = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error in VIEWCLUSTER_GET_DEFINITION' ).
    ENDIF.

    CLEAR ls_vcldir_entry-author.

    io_xml->add( iv_name = 'VCLDIR'
                 ig_data = ls_vcldir_entry ).
    io_xml->add( iv_name = 'VLCSTRUC_TAB'
                 ig_data = lt_vclstruc ).
    io_xml->add( iv_name = 'VCLSTRUDEP_TAB'
                 ig_data = lt_vclstrudep ).
    io_xml->add( iv_name = 'VCLMF_TAB'
                 ig_data = lt_vclmf ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf,
          lv_objectname   TYPE ob_object.


    io_xml->read( EXPORTING iv_name = 'VCLDIR'
                  CHANGING cg_data = ls_vcldir_entry ).
    io_xml->read( EXPORTING iv_name = 'VLCSTRUC_TAB'
                  CHANGING cg_data = lt_vclstruc ).
    io_xml->read( EXPORTING iv_name = 'VCLSTRUDEP_TAB'
                  CHANGING cg_data = lt_vclstrudep ).
    io_xml->read( EXPORTING iv_name = 'lt_vclstrudep'
                  CHANGING cg_data = lt_vclmf ).

    ls_vcldir_entry-author = sy-uname.

    CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
      EXPORTING
        vcldir_entry   = ls_vcldir_entry
      TABLES
        vclstruc_tab   = lt_vclstruc
        vclstrudep_tab = lt_vclstrudep
        vclmf_tab      = lt_vclmf.

    lv_objectname = ls_vcldir_entry-vclname.
    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = lv_objectname
        iv_objecttype         = c_cluster_type
        iv_maint_mode         = c_mode_insert
        iv_devclass           = iv_package
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error in OBJ_GENERATE for VCLS' ).
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~delete.
* Do the same as in VIEWCLUSTER_SAVE_DEFINITION
    DATA: lv_vclname TYPE vcl_name.


    lv_vclname = ms_item-obj_name.

    DELETE FROM vcldir WHERE vclname = lv_vclname.        "#EC CI_SUBRC
    DELETE FROM vcldirt WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstruc WHERE vclname = lv_vclname.      "#EC CI_SUBRC
    DELETE FROM vclstruct WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstrudep WHERE vclname = lv_vclname.    "#EC CI_SUBRC
    DELETE FROM vclmf WHERE vclname = lv_vclname.         "#EC CI_SUBRC

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~jump.

    DATA: lv_vclname      TYPE  vcl_name.

    lv_vclname = ms_item-obj_name.
    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
      EXPORTING
        viewcluster_name             = lv_vclname
        maintenance_action           = 'S'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        viewcluster_not_found        = 3
        viewcluster_is_inconsistent  = 4
        missing_generated_function   = 5
        no_upd_auth                  = 6
        no_show_auth                 = 7
        object_not_found             = 8
        no_tvdir_entry               = 9
        no_clientindep_auth          = 10
        invalid_action               = 11
        saving_correction_failed     = 12
        system_failure               = 13
        unknown_field_in_dba_sellist = 14
        missing_corr_number          = 15
        OTHERS                       = 16.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error in VIEWCLUSTER_MAINTENANCE_CALL' ).
    ENDIF.

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_vcls IMPLEMENTATION
