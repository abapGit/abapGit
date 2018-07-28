CLASS zcl_abapgit_object_auth DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

ENDCLASS.



CLASS zcl_abapgit_object_auth IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_fieldname TYPE authx-fieldname.


    lv_fieldname = ms_item-obj_name.

* there is a bug in SAP standard, the TADIR entries are not deleted
* when the AUTH object is deleted in transaction SU20
    CALL FUNCTION 'SUSR_AUTF_DELETE_FIELD'
      EXPORTING
        fieldname           = lv_fieldname
      EXCEPTIONS
        delete_not_possible = 1
        field_in_use        = 2
        not_existing        = 3
        no_authority        = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SUSR_AUTF_DELETE_FIELD' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
* see include LSAUT_FIELDF02

    DATA: ls_authx TYPE authx,
          lo_auth  TYPE REF TO cl_auth_tools.


    io_xml->read( EXPORTING iv_name = 'AUTHX'
                  CHANGING cg_data = ls_authx ).

    CREATE OBJECT lo_auth.

    IF lo_auth->add_afield_to_trkorr( ls_authx-fieldname ) <> 0.
      zcx_abapgit_exception=>raise( 'Error deserializing AUTH' ).
    ENDIF.

    MODIFY authx FROM ls_authx.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error deserializing AUTH' ).
    ENDIF.

    CALL FUNCTION 'DB_COMMIT'.
    lo_auth->set_authfld_info_from_db( ls_authx-fieldname ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_fieldname TYPE authx-fieldname.


    SELECT SINGLE fieldname FROM authx
      INTO lv_fieldname
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lv_field TYPE fieldname.

    lv_field = ms_item-obj_name.

* TODO, this function module does not exist in 702
    CALL FUNCTION 'SU20_MAINTAIN_SNGL'
      EXPORTING
        id_field    = lv_field
        id_wbo_mode = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_authx TYPE authx.


    SELECT SINGLE * FROM authx INTO ls_authx
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'AUTHX'
                 ig_data = ls_authx ).

  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.

ENDCLASS.
