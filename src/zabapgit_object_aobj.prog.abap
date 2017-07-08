*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_AOBJ
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_object_aobj DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_object_aobj DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.

    CONSTANTS: c_sql_select TYPE string VALUE 'select' ##NO_TEXT.  "#EC NOTEXT
    CONSTANTS: c_sql_delete TYPE string VALUE 'delete' ##NO_TEXT.  "#EC NOTEXT
    CONSTANTS: c_sql_insert TYPE string VALUE 'insert' ##NO_TEXT.  "#EC NOTEXT

    TYPES: BEGIN OF tys_tables,
             dbtable      TYPE tabname,
             target_field TYPE fieldname,
             xml_name     TYPE string,
             is_table     TYPE flag,
           END OF tys_tables,
           tyt_tables TYPE STANDARD TABLE OF tys_tables WITH DEFAULT KEY.

    TYPES: BEGIN OF tys_aobj_data,
             object_definition   TYPE arch_obj,
             object_customizing  TYPE arch_usr,
             description_texts   TYPE STANDARD TABLE OF arch_txt   WITH DEFAULT KEY,
             used_classes        TYPE STANDARD TABLE OF arch_oclas WITH DEFAULT KEY,
             custom_transactions TYPE STANDARD TABLE OF arch_tcode WITH DEFAULT KEY,
             object_structure    TYPE STANDARD TABLE OF arch_def   WITH DEFAULT KEY,
             tables_to_delete    TYPE STANDARD TABLE OF arch_dele  WITH DEFAULT KEY,
             read_reports        TYPE STANDARD TABLE OF arch_rprg  WITH DEFAULT KEY,
             info_tables         TYPE STANDARD TABLE OF admi_appli WITH DEFAULT KEY,
             generation_events   TYPE STANDARD TABLE OF arch_gener WITH DEFAULT KEY,
             networks            TYPE STANDARD TABLE OF arch_net   WITH DEFAULT KEY,
           END OF tys_aobj_data.

    METHODS _get_serialized_fields
      RETURNING VALUE(et_fieldlist) TYPE lcl_object_aobj=>tyt_tables.
    METHODS _determine_object_description
      IMPORTING is_aobj_data          TYPE lcl_object_aobj=>tys_aobj_data
      RETURNING VALUE(ev_description) TYPE obtxt_tr01.
    METHODS _execute_sql
      IMPORTING iv_arobject_name TYPE lif_defs=>ty_item-obj_name
                iv_statement     TYPE string
      CHANGING  cs_aobj_data     TYPE lcl_object_aobj=>tys_aobj_data.
    METHODS _write_tr_entry
      IMPORTING iv_arobject_name TYPE lif_defs=>ty_item-obj_name
      RAISING   lcx_exception.

    DATA: mt_fields TYPE lcl_object_aobj=>tyt_tables.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_aobj IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_object_aobj IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_arobject_name TYPE arch_obj-object.

    lv_arobject_name = ms_item-obj_name.

    CALL FUNCTION 'ARCHIVE_OBJECT_GET'
      EXPORTING
        object           = lv_arobject_name
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    rv_bool = boolc( sy-subrc = 0  ).

  ENDMETHOD.

  METHOD lif_object~jump.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode    = 'AOBJ'
        mode_val = 'E'
      EXCEPTIONS
        OTHERS   = 1 ##fm_subrc_ok. "#EC CI_SUBRC

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: ls_aobj_data TYPE tys_aobj_data.

    _execute_sql( EXPORTING iv_arobject_name = ms_item-obj_name
                            iv_statement     = c_sql_delete
                  CHANGING  cs_aobj_data     = ls_aobj_data ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: ls_aobj_data           TYPE tys_aobj_data.
    DATA: lt_fields              TYPE lcl_object_aobj=>tyt_tables.
    FIELD-SYMBOLS: <ls_field>    LIKE LINE OF lt_fields.
    FIELD-SYMBOLS: <l_any_field> TYPE any.

    _execute_sql( EXPORTING iv_arobject_name = ms_item-obj_name
                            iv_statement     = c_sql_select
                  CHANGING  cs_aobj_data     = ls_aobj_data ).

    IF ls_aobj_data IS NOT INITIAL.

      lt_fields = _get_serialized_fields( ).

      LOOP AT lt_fields ASSIGNING <ls_field>.

        ASSIGN COMPONENT <ls_field>-target_field
               OF STRUCTURE ls_aobj_data TO <l_any_field>.
        IF sy-subrc = 0.

          io_xml->add( iv_name = <ls_field>-xml_name
                       ig_data = <l_any_field> ).

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: ls_aobj_data               TYPE tys_aobj_data.
    DATA: lt_fields                  TYPE lcl_object_aobj=>tyt_tables.
    FIELD-SYMBOLS: <ls_field>        LIKE LINE OF lt_fields.
    FIELD-SYMBOLS: <l_any_field>     TYPE any.


    lt_fields = _get_serialized_fields( ).

    LOOP AT lt_fields ASSIGNING <ls_field>.

      ASSIGN COMPONENT <ls_field>-target_field OF STRUCTURE ls_aobj_data TO <l_any_field>.
      IF sy-subrc = 0.

        io_xml->read( EXPORTING iv_name  = <ls_field>-xml_name
                      CHANGING  cg_data  = <l_any_field> ).

      ENDIF.

    ENDLOOP.

    IF ls_aobj_data IS NOT INITIAL.

      corr_insert( EXPORTING iv_package     = iv_package
                             iv_global_lock = abap_true ).

      _write_tr_entry( iv_arobject_name = ms_item-obj_name ).

      IF lif_object~exists( ) = abap_true.
        lif_object~delete( ).
      ENDIF.

      _execute_sql( EXPORTING iv_arobject_name = ms_item-obj_name
                              iv_statement     = c_sql_insert
                    CHANGING  cs_aobj_data     = ls_aobj_data ).

      tadir_insert( iv_package          = iv_package
                    iv_generated_object = ls_aobj_data-object_definition-arch_gener ).

    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.


  METHOD _get_serialized_fields.

    DATA: ls_field TYPE lcl_object_aobj=>tys_tables.

    IF mt_fields IS INITIAL.

      ls_field-xml_name     = 'definition'         ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'OBJECT_DEFINITION'  ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_OBJ'           ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_false.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'customizing'         ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'OBJECT_CUSTOMIZING'  ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_USR'            ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_false.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'description'         ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'DESCRIPTION_TEXTS'   ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_TXT'            ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'classes'             ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'USED_CLASSES'        ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_OCLAS'          ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'transactions'        ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'CUSTOM_TRANSACTIONS' ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_TCODE'          ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'object_structure'    ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'OBJECT_STRUCTURE'    ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_DEF'            ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'tables_to_delete'    ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'TABLES_TO_DELETE'    ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_DELE'           ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'read_reports'        ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'READ_REPORTS'        ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_RPRG'           ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'info_tables'         ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'INFO_TABLES'         ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ADMI_APPLI'          ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'generation_events'   ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'GENERATION_EVENTS'   ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_GENER'          ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'networks'            ##NO_TEXT.  "#EC NOTEXT
      ls_field-target_field = 'NETWORKS'            ##NO_TEXT.  "#EC NOTEXT
      ls_field-dbtable      = 'ARCH_NET'            ##NO_TEXT.  "#EC NOTEXT
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

    ENDIF.

    et_fieldlist = mt_fields.

  ENDMETHOD.

  METHOD _determine_object_description.

    FIELD-SYMBOLS: <ls_description_text> LIKE LINE OF is_aobj_data-description_texts.

    READ TABLE is_aobj_data-description_texts ASSIGNING <ls_description_text>
         WITH KEY langu = sy-langu.
    IF sy-subrc <> 0 AND sy-langu <> 'E'.
      READ TABLE is_aobj_data-description_texts ASSIGNING <ls_description_text>
           WITH KEY langu = 'E'.
    ENDIF.
    IF sy-subrc <> 0.
      READ TABLE is_aobj_data-description_texts ASSIGNING <ls_description_text>
           INDEX 1 ##fm_subrc_ok. "#EC CI_SUBRC
    ENDIF.

    IF <ls_description_text> IS ASSIGNED.
      ev_description = <ls_description_text>-objtext.
    ELSE.
      ev_description = is_aobj_data-object_definition-object.
    ENDIF.

  ENDMETHOD.

  METHOD _execute_sql.

    DATA: lt_fields            TYPE lcl_object_aobj=>tyt_tables.
    DATA: lv_arobject_name     TYPE arch_obj-object.
    FIELD-SYMBOLS: <ls_field>  LIKE LINE OF lt_fields.
    FIELD-SYMBOLS: <ls_target> TYPE any.
    FIELD-SYMBOLS: <lt_target> TYPE STANDARD TABLE.

    lv_arobject_name = iv_arobject_name.

    lt_fields = _get_serialized_fields( ).

    LOOP AT lt_fields ASSIGNING <ls_field>.

      IF <ls_field>-is_table IS INITIAL.
        ASSIGN COMPONENT <ls_field>-target_field OF STRUCTURE cs_aobj_data TO <ls_target>.
      ELSE.
        ASSIGN COMPONENT <ls_field>-target_field OF STRUCTURE cs_aobj_data TO <lt_target>.
      ENDIF.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE iv_statement.
        WHEN c_sql_select.

          IF <ls_field>-is_table IS INITIAL.
            SELECT SINGLE * FROM (<ls_field>-dbtable)
                            INTO <ls_target>
                            WHERE object = lv_arobject_name.
          ELSE.
            SELECT * FROM (<ls_field>-dbtable)
                     INTO TABLE <lt_target>
                     WHERE object = lv_arobject_name ORDER BY PRIMARY KEY.
          ENDIF.

        WHEN c_sql_delete.

          DELETE FROM (<ls_field>-dbtable) WHERE object = lv_arobject_name.

        WHEN c_sql_insert.

          IF <ls_field>-is_table IS INITIAL.
            IF <ls_target> IS NOT INITIAL.
              INSERT (<ls_field>-dbtable) FROM <ls_target>.
            ENDIF.
          ELSE.
            IF <lt_target> IS NOT INITIAL.
              INSERT (<ls_field>-dbtable) FROM TABLE <lt_target>.
            ENDIF.
          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD _write_tr_entry.

    DATA: lv_arobject_name TYPE arch_obj-object.
    DATA: lt_tr_objects    TYPE TABLE OF ko200.
    DATA: lt_tr_keys       TYPE TABLE OF e071k.
    DATA: ls_tr_object     TYPE ko200.
    DATA: ls_tr_key        TYPE e071k.
    DATA: lwa_order        TYPE trkorr.                     "#EC NEEDED
    DATA: lwa_task         TYPE trkorr.                     "#EC NEEDED


    lv_arobject_name = iv_arobject_name.

    ls_tr_object-pgmid    = 'R3TR'.
    ls_tr_object-object   = 'CDAT'.
    ls_tr_object-obj_name = 'ARCHIVE'.
    ls_tr_object-objfunc  = 'K'.
    APPEND ls_tr_object TO lt_tr_objects.

    ls_tr_key-pgmid      = 'R3TR'.
    ls_tr_key-object     = 'TABU'.
    ls_tr_key-objname    = 'ARCH_USR'.
    ls_tr_key-mastertype = 'CDAT'.
    ls_tr_key-mastername = 'ARCHIVE'.
    ls_tr_key-viewname   = 'V_ARC_USR'.
    ls_tr_key-tabkey     = lv_arobject_name.
    ls_tr_key-sortflag   = '2'.
    APPEND ls_tr_key TO lt_tr_keys.

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = lt_tr_objects
        wt_e071k                = lt_tr_keys
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      lcx_exception=>raise( 'lcl_object_aobj TR_OBJECTS_CHECK failed: ARCHIVE' ).
    ENDIF.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      IMPORTING
        we_order                = lwa_order
        we_task                 = lwa_task
      TABLES
        wt_ko200                = lt_tr_objects
        wt_e071k                = lt_tr_keys
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      lcx_exception=>raise( 'lcl_object_aobj TR_OBJECTS_INSERT failed: ARCHIVE' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_object_FORM IMPLEMENTATION
