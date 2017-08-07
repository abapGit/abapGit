*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_CMPT
*&---------------------------------------------------------------------*

CLASS lcl_object_cmpt DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE lif_defs=>ty_item
          iv_language TYPE spras.

    INTERFACES lif_object.

  PRIVATE SECTION.
    DATA: mo_cmp_db TYPE REF TO object.

ENDCLASS.

CLASS lcl_object_cmpt IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_GET_DB_ACCESS')
          RECEIVING
            r_ref_db_access = mo_cmp_db.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    DATA: mi_cmp_template TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_CREATE_FROM_DB')
          EXPORTING
            i_name         = |{ ms_item-obj_name }|
            i_version      = 'A'
          RECEIVING
            r_ref_template = mi_cmp_template.

        CALL METHOD mi_cmp_template->('IF_CMP_TEMPLATE_EDIT~GET_CHANGE_USER')
          RECEIVING
            r_user = rv_user.

      CATCH cx_root.
        lcx_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: name TYPE c LENGTH 30.

    name = ms_item-obj_name.

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_TEMPLATE_EXISTS')
          EXPORTING
            i_name       = name
            i_version    = 'A'
          RECEIVING
            r_flg_exists = rv_bool.

      CATCH cx_root.
        lcx_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lr_template TYPE REF TO data.
    FIELD-SYMBOLS: <template> TYPE any.

    TRY.
        CREATE DATA lr_template TYPE ('IF_CMP_TEMPLATE_DB=>TYP_TEMPLATE').
        ASSIGN lr_template->* TO <template>.

        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~READ_TEMPLATE')
          EXPORTING
            i_name     = |{ ms_item-obj_name }|
            i_version  = 'A'
          RECEIVING
            r_template = <template>.

        io_xml->add( iv_name = 'CMPT'
                     ig_data = <template> ).

      CATCH cx_root.
        lcx_exception=>raise( 'CMPT not supported' ).
    ENDTRY.


  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lr_template TYPE REF TO data.
    FIELD-SYMBOLS: <template> TYPE any.

    TRY.
        CREATE DATA lr_template TYPE ('IF_CMP_TEMPLATE_DB=>TYP_TEMPLATE').
        ASSIGN lr_template->* TO <template>.

        io_xml->read(
          EXPORTING
            iv_name = 'CMPT'
          CHANGING
            cg_data = <template> ).

        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~SAVE_TEMPLATE')
          EXPORTING
            i_template_db = <template>
            i_flg_header  = abap_true
            i_flg_lines   = abap_true.

      CATCH cx_root.
        lcx_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

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

    DATA: deleted TYPE abap_bool.

    TRY.
        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~DELETE_TEMPLATE')
          EXPORTING
            i_name        = |{ ms_item-obj_name }|
            i_version     = 'A'
            i_flg_header  = abap_true
            i_flg_lines   = abap_true
          RECEIVING
            r_flg_deleted = deleted.

      CATCH cx_root.
        lcx_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

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
