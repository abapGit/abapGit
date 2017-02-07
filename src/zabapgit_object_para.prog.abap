*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_PARA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_para DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_para DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_para DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_para IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_para IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_paramid TYPE tpara-paramid.


    SELECT SINGLE paramid FROM tpara INTO lv_paramid
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tparat INTO ls_tparat
      WHERE paramid = ms_item-obj_name
      AND sprache = mv_language.          "#EC CI_GENBUFF "#EC CI_SUBRC

    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).
    io_xml->add( iv_name = 'TPARAT'
                 ig_data = ls_tparat ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see fm RS_PARAMETER_ADD and RS_PARAMETER_EDIT

    DATA: lv_mode   TYPE c LENGTH 1,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc = 0.
      lv_mode = 'M'.
    ELSE.
      lv_mode = 'I'.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TPARA'
                  CHANGING cg_data = ls_tpara ).
    io_xml->read( EXPORTING iv_name = 'TPARAT'
                  CHANGING cg_data = ls_tparat ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ms_item-obj_name
        object_class        = 'PARA'
        mode                = lv_mode
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = mv_language
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_CORR_INSERT, PARA' ).
    ENDIF.

    MODIFY tpara FROM ls_tpara.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    MODIFY tparat FROM ls_tparat.                         "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_paramid TYPE tpara-paramid.


    lv_paramid = ms_item-obj_name.
    CALL FUNCTION 'RS_PARAMETER_DELETE'
      EXPORTING
        objectname = lv_paramid
      EXCEPTIONS
        cancelled  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_PRAMETER_DELETE' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PARA'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_para IMPLEMENTATION
