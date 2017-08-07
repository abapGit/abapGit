*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_CMPT
*&---------------------------------------------------------------------*

CLASS lcl_object_cmpt DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
ENDCLASS.

CLASS lcl_object_cmpt IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    DATA(template) = CAST cl_cmp_template( cl_cmp_template=>s_create_from_db( i_name     = |{ ms_item-obj_name }|
                                                                              i_version  = 'A' ) ).


    rv_user = template->if_cmp_template_edit~get_change_user( ).

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: name TYPE c LENGTH 30.

    name = ms_item-obj_name.

    rv_bool = cl_cmp_template=>s_template_exists( i_name    = name
                                                  i_version = 'A' ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA(cmp_db) = cl_cmp_template=>s_get_db_access( ).

    DATA(template) = cmp_db->read_template( i_name     = |{ ms_item-obj_name }|
                                            i_version  = 'A' ).

    io_xml->add( iv_name = 'CMPT'
                 ig_data = template ).

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: template TYPE if_cmp_template_db=>typ_template.

    io_xml->read(
      EXPORTING
        iv_name       = 'CMPT'
      CHANGING
        cg_data       = template ).

    DATA(cmp_db) = cl_cmp_template=>s_get_db_access( ).

    cmp_db->save_template( i_template_db = template
                           i_flg_header  = abap_true
                           i_flg_lines   = abap_true ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ms_item-obj_name
        object_class        = ms_item-obj_type
        mode                = 'I'
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = mv_language
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_CORR_INSERT, CMPT' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA(cmp_db) = cl_cmp_template=>s_get_db_access( ).

    DATA(deleted) = cmp_db->delete_template( i_name        = |{ ms_item-obj_name }|
                                             i_version     = 'A'
                                             i_flg_header  = abap_true
                                             i_flg_lines   = abap_true ).

    IF deleted = abap_false.
      lcx_exception=>raise( |Error deleting CMPT { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from RS_TOOL_ACCESS, CMPT| ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

ENDCLASS.
