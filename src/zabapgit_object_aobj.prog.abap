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
    CONSTANTS c_sql_select TYPE string VALUE 'select' ##NO_TEXT.
    CONSTANTS c_sql_delete TYPE string VALUE 'delete' ##NO_TEXT.
    CONSTANTS c_sql_insert TYPE string VALUE 'insert' ##NO_TEXT.

    METHODS _get_serialized_fields
      RETURNING
        VALUE(et_fieldlist) TYPE lcl_object_aobj=>tyt_tables.
    METHODS _determine_object_description
      IMPORTING is_aobj_data          TYPE lcl_object_aobj=>tys_aobj_data
      RETURNING VALUE(ev_description) TYPE obtxt_tr01.
    METHODS _execute_sql
      IMPORTING
                iv_arobject_name TYPE lif_defs=>ty_item-obj_name
                iv_statement     TYPE string
      CHANGING  cs_aobj_data     TYPE lcl_object_aobj=>tys_aobj_data.

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

*    DATA: lt_bdcdata TYPE TABLE OF bdcdata.
*
*    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-program  = 'SAPMSSCF' ##NO_TEXT.
*    <ls_bdcdata>-dynpro   = '1102' ##NO_TEXT.
*    <ls_bdcdata>-dynbegin = abap_true.
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-fnam = 'BDC_OKCODE' ##NO_TEXT.
*    <ls_bdcdata>-fval = '=SHOW' ##NO_TEXT.
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-fnam = 'RSSCF-TDFORM' ##NO_TEXT.
*    <ls_bdcdata>-fval = ms_item-obj_name.
*
*    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
*      STARTING NEW TASK 'GIT'
*      EXPORTING
*        tcode     = 'SE71'
*        mode_val  = 'E'
*      TABLES
*        using_tab = lt_bdcdata
*      EXCEPTIONS
*        OTHERS    = 1
*        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

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

        ASSIGN COMPONENT <ls_field>-target_field OF STRUCTURE ls_aobj_data TO <l_any_field>.
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
    DATA: lv_ordernum                TYPE e070-trkorr.
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
                             iv_global_lock = abap_true
                   IMPORTING ev_ordernum    = lv_ordernum ).

      IF lif_object~exists( ) = abap_true.
        lif_object~delete( ).
      ENDIF.

      _execute_sql( EXPORTING iv_arobject_name = ms_item-obj_name
                              iv_statement     = c_sql_insert
                    CHANGING  cs_aobj_data     = ls_aobj_data ).

      tadir_insert( iv_package ).

    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.


  METHOD _get_serialized_fields.

    DATA: ls_field TYPE lcl_object_aobj=>tys_tables.

    IF mt_fields IS INITIAL.

      ls_field-xml_name     = 'definition'.
      ls_field-target_field = 'OBJECT_DEFINITION'.
      ls_field-dbtable      = 'ARCH_OBJ'.
      ls_field-is_table     = abap_false.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'customizing'.
      ls_field-target_field = 'OBJECT_CUSTOMIZING'.
      ls_field-dbtable      = 'ARCH_USR'.
      ls_field-is_table     = abap_false.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'description'.
      ls_field-target_field = 'DESCRIPTION_TEXTS'.
      ls_field-dbtable      = 'ARCH_TXT'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'classes'.
      ls_field-target_field = 'USED_CLASSES'.
      ls_field-dbtable      = 'ARCH_OCLAS'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'transactions'.
      ls_field-target_field = 'CUSTOM_TRANSACTIONS'.
      ls_field-dbtable      = 'ARCH_TCODE'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'object_structure'.
      ls_field-target_field = 'OBJECT_STRUCTURE'.
      ls_field-dbtable      = 'ARCH_DEF'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'tables_to_delete'.
      ls_field-target_field = 'TABLES_TO_DELETE'.
      ls_field-dbtable      = 'ARCH_DELE'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'read_reports'.
      ls_field-target_field = 'READ_REPORTS'.
      ls_field-dbtable      = 'ARCH_RPRG'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'info_tables'.
      ls_field-target_field = 'INFO_TABLES'.
      ls_field-dbtable      = 'ADMI_APPLI'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'generation_events'.
      ls_field-target_field = 'GENERATION_EVENTS'.
      ls_field-dbtable      = 'ARCH_GENER'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

      ls_field-xml_name     = 'networks'.
      ls_field-target_field = 'NETWORKS'.
      ls_field-dbtable      = 'ARCH_NET'.
      ls_field-is_table     = abap_true.
      APPEND ls_field TO mt_fields.

    ENDIF.

    et_fieldlist = mt_fields.

  ENDMETHOD.

  METHOD _determine_object_description.

    FIELD-SYMBOLS: <ls_description_text> LIKE LINE OF is_aobj_data-description_texts.

    READ TABLE is_aobj_data-description_texts ASSIGNING <ls_description_text> WITH KEY langu = sy-langu.
    IF sy-subrc <> 0 AND sy-langu <> 'E'.
      READ TABLE is_aobj_data-description_texts ASSIGNING <ls_description_text> WITH KEY langu = 'E'.
    ENDIF.
    IF sy-subrc <> 0.
      READ TABLE is_aobj_data-description_texts ASSIGNING <ls_description_text> INDEX 1.
    ENDIF.

    IF <ls_description_text> IS ASSIGNED.
      ev_description = <ls_description_text>-objtext.
    ELSE.
      ev_description = is_aobj_data-object_definition-object.
    ENDIF.

  ENDMETHOD.

  METHOD _execute_sql.

    DATA: lt_fields              TYPE lcl_object_aobj=>tyt_tables.
    DATA: lv_arobject_name       TYPE arch_obj-object.
    FIELD-SYMBOLS: <ls_field>   TYPE lcl_object_aobj=>tys_tables.
    FIELD-SYMBOLS: <ls_target>  TYPE any.
    FIELD-SYMBOLS: <lt_target>  TYPE STANDARD TABLE.

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
            SELECT SINGLE * FROM (<ls_field>-dbtable) INTO <ls_target> WHERE object = lv_arobject_name.
          ELSE.
            SELECT * FROM (<ls_field>-dbtable) INTO TABLE <lt_target> WHERE object = lv_arobject_name ORDER BY PRIMARY KEY.
          ENDIF.

*          SELECT SINGLE * FROM arch_obj INTO  rs_aobj_data-object_definition   WHERE object = iv_arobject_name.
*          SELECT SINGLE * FROM arch_usr INTO  rs_aobj_data-object_customizing  WHERE object = iv_arobject_name.
*          SELECT * FROM arch_txt   INTO TABLE rs_aobj_data-description_texts   WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_rprg  INTO TABLE rs_aobj_data-read_reports        WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_oclas INTO TABLE rs_aobj_data-used_classes        WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_tcode INTO TABLE rs_aobj_data-custom_transactions WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_def   INTO TABLE rs_aobj_data-object_structure    WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_dele  INTO TABLE rs_aobj_data-tables_to_delete    WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM admi_appli INTO TABLE rs_aobj_data-info_tables         WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_gener INTO TABLE rs_aobj_data-generation_events   WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
*          SELECT * FROM arch_net   INTO TABLE rs_aobj_data-networks            WHERE object = iv_arobject_name ORDER BY PRIMARY KEY.
        WHEN c_sql_delete.

          DELETE FROM  (<ls_field>-dbtable) WHERE object = lv_arobject_name.

*    DELETE FROM arch_obj   WHERE object = lv_arobject_name.
*    DELETE FROM arch_usr   WHERE object = lv_arobject_name.
*    DELETE FROM arch_txt   WHERE object = lv_arobject_name.
*    DELETE FROM arch_rprg  WHERE object = lv_arobject_name.
*    DELETE FROM arch_oclas WHERE object = lv_arobject_name.
*    DELETE FROM arch_tcode WHERE object = lv_arobject_name.
*    DELETE FROM arch_def   WHERE object = lv_arobject_name.
*    DELETE FROM arch_dele  WHERE object = lv_arobject_name.
*    DELETE FROM admi_appli WHERE object = lv_arobject_name.
*    DELETE FROM arch_gener WHERE object = lv_arobject_name.
*    DELETE FROM arch_net   WHERE object = lv_arobject_name.
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

*        INSERT arch_obj   FROM ls_aobj_data-object_definition.
*        INSERT arch_usr   FROM ls_aobj_data-object_customizing.
*        INSERT arch_txt   FROM TABLE ls_aobj_data-description_texts.
*        INSERT arch_rprg  FROM TABLE ls_aobj_data-read_reports.
*        INSERT arch_oclas FROM TABLE ls_aobj_data-used_classes.
*        INSERT arch_tcode FROM TABLE ls_aobj_data-custom_transactions.
*        INSERT arch_def   FROM TABLE ls_aobj_data-object_structure.
*        INSERT arch_dele  FROM TABLE ls_aobj_data-tables_to_delete.
*        INSERT admi_appli FROM TABLE ls_aobj_data-info_tables.
*        INSERT arch_gener FROM TABLE ls_aobj_data-generation_events.
*        INSERT arch_net   FROM TABLE ls_aobj_data-networks.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.                    "lcl_object_FORM IMPLEMENTATION
