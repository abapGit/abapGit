CLASS zcl_abapgit_object_wdcc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_tadir_entry
      IMPORTING iv_devclass     TYPE tadir-devclass
                iv_obj_name     TYPE tadir-obj_name
      RETURNING VALUE(rs_tadir) TYPE tadir.

    METHODS update_tadir_entry
      IMPORTING
        iv_obj_name     TYPE tadir-obj_name
        iv_devclass     TYPE tadir-devclass
      RETURNING
        VALUE(rs_tadir) TYPE tadir
      RAISING
        zcx_abapgit_exception.

    METHODS delete_tadir_entry
      IMPORTING
        iv_test_mode TYPE trpari-flag
        iv_obj_name  TYPE tadir-obj_name
      RAISING
        zcx_abapgit_exception.

    METHODS create_tadir_entry
      IMPORTING
        iv_obj_name     TYPE tadir-obj_name
        iv_devclass     TYPE tadir-devclass
      RETURNING
        VALUE(rs_tadir) TYPE tadir
      RAISING
        zcx_abapgit_exception.

    METHODS modify_tadir_entry
      IMPORTING
        iv_delete_flag  TYPE tadir-delflag
        iv_obj_name     TYPE tadir-obj_name
        iv_devclass     TYPE tadir-devclass
      RETURNING
        VALUE(rs_tadir) TYPE tadir
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_wdcc IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.

    DATA: lr_outline_struct    TYPE REF TO data,
          lr_config_key_struct TYPE REF TO data.

    FIELD-SYMBOLS: <ls_outline>    TYPE any,
                   <ls_config_key> TYPE any,
                   <lv_component>  TYPE any.

    CREATE DATA lr_outline_struct TYPE ('WDY_CFG_OUTLINE_DATA').
    CREATE DATA lr_config_key_struct TYPE ('WDY_CONFIG_KEY').


    ASSIGN lr_outline_struct->* TO <ls_outline>.
    ASSIGN lr_config_key_struct->* TO <ls_config_key>.

    TRY.
        ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_config_key> TO <lv_component>.
        <lv_component> = ms_item-obj_name.
        ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_config_key> TO <lv_component>.
        <lv_component> = '00'.

        CALL METHOD ('CL_WDR_CFG_PERSISTENCE_UTILS')=>('READ_COMP_CONFIG_FROM_DB')
          EXPORTING
            config_key   = <ls_config_key>
          IMPORTING
            outline_data = <ls_outline>.
      CATCH cx_static_check.
        zcx_abapgit_exception=>raise( 'Error Reading Component Config from DB ' && ms_item-obj_name ).
    ENDTRY.

    ASSIGN COMPONENT 'CHANGEDBY' OF STRUCTURE <ls_outline> TO <lv_component>.
    rv_user = <lv_component>.

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    DATA: lr_config_key_struct TYPE REF TO data,
          lv_subrc             TYPE sysubrc,
          lv_obj_name          TYPE sobj_name,
          ls_tadir             TYPE tadir.

    FIELD-SYMBOLS: <ls_config_key> TYPE any,
                   <lv_component>  TYPE any.

    CREATE DATA lr_config_key_struct TYPE ('WDY_CONFIG_KEY').
    ASSIGN lr_config_key_struct->* TO <ls_config_key>.

    ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_config_key> TO <lv_component>.
    <lv_component> = ms_item-obj_name.
    ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_config_key> TO <lv_component>.
    <lv_component> = '00'.

    CALL METHOD ('CL_WDR_CFG_PERSISTENCE_UTILS')=>('DELETE_CONFIGURATION')
      EXPORTING
        config_key = <ls_config_key>
      RECEIVING
        subrc      = lv_subrc.

    IF lv_subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting WDCC: { ms_item-obj_name }| ).
      RETURN.
    ENDIF.

    "**[HJA]->Check TADIR
    lv_obj_name = ms_item-obj_name.
    ls_tadir = me->get_tadir_entry( iv_devclass = iv_package
                                    iv_obj_name = lv_obj_name ).

    IF ls_tadir IS NOT INITIAL.
      IF ls_tadir-delflag <> abap_true.

        me->modify_tadir_entry( iv_delete_flag = abap_true
                                iv_obj_name    = lv_obj_name
                                iv_devclass    = iv_package ).

      ENDIF.
    ELSE.
      "**[HJA]->Do Nothing
    ENDIF.


  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lr_outline_struct    TYPE REF TO data,
          lr_config_key_struct TYPE REF TO data.

    FIELD-SYMBOLS: <ls_outline>    TYPE any,
                   <ls_config_key> TYPE any,
                   <lv_component>  TYPE any.

    CREATE DATA lr_outline_struct TYPE ('WDY_CFG_OUTLINE_DATA').
    CREATE DATA lr_config_key_struct TYPE ('WDY_CONFIG_KEY').


    ASSIGN lr_outline_struct->* TO <ls_outline>.
    ASSIGN lr_config_key_struct->* TO <ls_config_key>.

    TRY.
        ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_config_key> TO <lv_component>.
        <lv_component> = ms_item-obj_name.
        ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_config_key> TO <lv_component>.
        <lv_component> = '00'.

        CALL METHOD ('CL_WDR_CFG_PERSISTENCE_UTILS')=>('READ_COMP_CONFIG_FROM_DB')
          EXPORTING
            config_key   = <ls_config_key>
          IMPORTING
            outline_data = <ls_outline>.

      CATCH cx_static_check.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

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
    rv_active = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    DATA: lt_enq   TYPE STANDARD TABLE OF seqg3,
          lv_subrc TYPE sysubrc,
          lv_garg  TYPE eqegraarg,
          lv_lines TYPE i.

    lv_garg = ms_item-obj_name.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient               = sy-mandt
        gname                 = 'WDY_CONFIG_DATA'
        garg                  = lv_garg
      IMPORTING
        subrc                 = lv_subrc
      TABLES
        enq                   = lt_enq
      EXCEPTIONS
        communication_failure = 2
        OTHERS                = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error check object lock WDCC: { ms_item-obj_name }| ).
    ENDIF.

    DESCRIBE TABLE lt_enq LINES lv_lines.
    IF lv_lines > 0.
      rv_is_locked = abap_true.
    ELSE.
      rv_is_locked = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'WDCC'
        in_new_window = abap_true.

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA: lv_xml_xstring        TYPE xstring,
          lv_xml_string         TYPE string,
          lo_document           TYPE REF TO if_ixml_document,
          lv_xml_as_string      TYPE string,
          lr_otr_texts_tab      TYPE REF TO data,
          lr_cc_text_tab        TYPE REF TO data,
          lr_orig_config_struct TYPE REF TO data,
          lr_outline_struct     TYPE REF TO data,
          lr_config_key_struct  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_otr_texts>      TYPE ANY TABLE,
                   <lt_cc_text>        TYPE ANY TABLE,
                   <ls_orig_config>    TYPE any,
                   <ls_outline>        TYPE any,
                   <ls_config_key>     TYPE any,
                   <lv_component>      TYPE any,
                   <lv_component_id>   TYPE any,
                   <lv_component_type> TYPE any,
                   <lv_component_var>  TYPE any.

    CREATE DATA lr_otr_texts_tab TYPE TABLE OF ('WDY_CONFIG_COMPT').
    CREATE DATA lr_cc_text_tab TYPE TABLE OF ('WDY_CONFIG_DATT').
    CREATE DATA lr_orig_config_struct TYPE ('WDY_CONFIG_DATA').
    CREATE DATA lr_outline_struct TYPE ('WDY_CFG_OUTLINE_DATA').
    CREATE DATA lr_config_key_struct TYPE ('WDY_CONFIG_KEY').

    ASSIGN lr_otr_texts_tab->* TO <lt_otr_texts>.
    ASSIGN lr_cc_text_tab->* TO <lt_cc_text>.
    ASSIGN lr_orig_config_struct->* TO <ls_orig_config>.
    ASSIGN lr_outline_struct->* TO <ls_outline>.
    ASSIGN lr_config_key_struct->* TO <ls_config_key>.


    io_xml->add( iv_name = 'OBJECT_NAME'
                 ig_data =  ms_item-obj_name ).

    TRY.
        ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_config_key> TO <lv_component>.
        <lv_component> = ms_item-obj_name+0(32).
        ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_config_key> TO <lv_component>.
        <lv_component> = '00'.

        CALL METHOD ('CL_WDR_CFG_PERSISTENCE_UTILS')=>('READ_COMP_CONFIG_FROM_DB')
          EXPORTING
            config_key           = <ls_config_key>
          IMPORTING
            xml_xcontent         = lv_xml_xstring
            original_config_data = <ls_orig_config>
            outline_data         = <ls_outline>.

      CATCH cx_static_check.
        zcx_abapgit_exception=>raise( 'Error Reading Component Config from DB ' && ms_item-obj_name ).
    ENDTRY.

    ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_orig_config> TO <lv_component_id>.
    io_xml->add( iv_name = 'CONFIG_ID'
                 ig_data =  <lv_component_id> ).

    ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_orig_config> TO <lv_component_type>.
    io_xml->add( iv_name = 'CONFIG_TYPE'
                 ig_data =  <lv_component_type> ).

    ASSIGN COMPONENT 'CONFIG_VAR' OF STRUCTURE <ls_orig_config> TO <lv_component_var>.
    io_xml->add( iv_name = 'CONFIG_VAR'
                 ig_data =  <lv_component_var> ).

    ASSIGN COMPONENT 'COMPONENT' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'WDA_COMPONENT'
                 ig_data =  <lv_component> ).

    ASSIGN COMPONENT 'CONFIG_IDPAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'CONFIG_IDPAR'
                 ig_data =  <lv_component> ).

    ASSIGN COMPONENT 'CONFIG_TYPEPAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'CONFIG_TYPEPAR'
                 ig_data =  <lv_component> ).

    ASSIGN COMPONENT 'CONFIG_VARPAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'CONFIG_VARPAR'
                 ig_data =  <lv_component> ).

    ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'PARENT'
                 ig_data =  <lv_component> ).

    ASSIGN COMPONENT 'RELID' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'RELID'
                 ig_data =  <lv_component> ).

    ASSIGN COMPONENT 'FLAVOR_ID' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->add( iv_name = 'FLAVOR_ID'
                 ig_data =  <lv_component> ).

    "**[HJA]->Transform for readable part of xml
    CALL TRANSFORMATION id
           SOURCE XML lv_xml_xstring
           RESULT XML lv_xml_string.

    lo_document  = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml_xstring ).
    lv_xml_as_string  = cl_ixml_80_20=>render_to_string( document      = lo_document
                                                         pretty_print  = 1 ).

    io_xml->add( iv_name = 'COMPONENT_CONFIGURATION_READABLE'
                 ig_data = lv_xml_as_string ).

    io_xml->add( iv_name = 'COMPONENT_CONFIGURATION_RAW'
                 ig_data = lv_xml_xstring ).

    SELECT * FROM wdy_config_compt INTO TABLE <lt_otr_texts> WHERE config_id   = <lv_component_id>
                                                               AND config_type = <lv_component_type>
                                                               AND config_var  = <lv_component_var>.

    IF <lt_otr_texts> IS NOT INITIAL.
      io_xml->add( iv_name = 'OTR_TEXT'
                   ig_data = <lt_otr_texts> ).
    ENDIF.

    SELECT * FROM ('WDY_CONFIG_DATT') INTO TABLE <lt_cc_text> WHERE config_id   = <lv_component_id>
                                                                AND config_type = <lv_component_type>
                                                                AND config_var  = <lv_component_var>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'DESCR_LANG'
                   ig_data = <lt_cc_text> ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA: lv_xml_payload_xstring TYPE xstring,
          lo_translator          TYPE REF TO if_wdr_config_otr,
          lv_config_id           TYPE c LENGTH 32,
          lv_config_type         TYPE n LENGTH 2,
          lv_config_var          TYPE c LENGTH 6,
          lr_otr_texts_tab       TYPE REF TO data,
          lr_orig_config_struct  TYPE REF TO data,
          lr_pers_key_struct     TYPE REF TO data,
          lr_config_datt         TYPE REF TO data,
          lv_obj_name            TYPE sobj_name,
          ls_tadir               TYPE tadir,
          ls_tadir_backup        TYPE tadir.

    FIELD-SYMBOLS: <lt_otr_texts>   TYPE ANY TABLE,
                   <ls_orig_config> TYPE any,
                   <ls_pers_key>    TYPE any,
                   <lv_component>   TYPE any,
                   <lt_config_datt> TYPE ANY TABLE.

    CREATE DATA lr_otr_texts_tab TYPE TABLE OF ('WDY_CONFIG_COMPT').
    CREATE DATA lr_orig_config_struct TYPE ('WDY_CONFIG_DATA').
    CREATE DATA lr_pers_key_struct TYPE ('WDY_PERS_KEY').
    CREATE DATA lr_config_datt TYPE TABLE OF ('WDY_CONFIG_DATT').

    ASSIGN lr_otr_texts_tab->* TO <lt_otr_texts>.
    ASSIGN lr_orig_config_struct->* TO <ls_orig_config>.
    ASSIGN lr_pers_key_struct->* TO <ls_pers_key>.
    ASSIGN lr_config_datt->* TO <lt_config_datt>.

    ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'CONFIG_ID'
                  CHANGING  cg_data = <lv_component>  ).
    lv_config_id = <lv_component>.

    ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'CONFIG_TYPE'
                  CHANGING  cg_data = <lv_component> ).
    lv_config_type = <lv_component>.

    ASSIGN COMPONENT 'CONFIG_VAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'CONFIG_VAR'
                  CHANGING  cg_data = <lv_component> ).
    lv_config_var = <lv_component>.

    ASSIGN COMPONENT 'CONFIG_IDPAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'CONFIG_IDPAR'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'CONFIG_TYPEPAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'CONFIG_TYPEPAR'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'CONFIG_VARPAR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'CONFIG_VARPAR'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'COMPONENT' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'WDA_COMPONENT'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'XCONTENT' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'COMPONENT_CONFIGURATION_RAW'
                  CHANGING  cg_data = lv_xml_payload_xstring  ).
    <lv_component> = lv_xml_payload_xstring.

    ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'PARENT'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'RELID' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'RELID'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'FLAVOR_ID' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    io_xml->read( EXPORTING iv_name = 'FLAVOR_ID'
                  CHANGING  cg_data = <lv_component> ).

    ASSIGN COMPONENT 'AUTHOR' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    <lv_component> = sy-uname.
    ASSIGN COMPONENT 'CHANGEDBY' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    <lv_component> = sy-uname.
    ASSIGN COMPONENT 'CHANGEDON' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    <lv_component> = sy-datum.
    ASSIGN COMPONENT 'CREATEDON' OF STRUCTURE <ls_orig_config> TO <lv_component>.
    <lv_component> = sy-datum.

    CALL FUNCTION 'ENQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = if_wdr_cfg_constants=>c_lock_mode_exclusive
        config_id            = lv_config_id
        config_type          = lv_config_type
        config_var           = lv_config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'
      EXCEPTIONS
        foreign_lock         = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error Enqueueing Component Config ' && ms_item-obj_name ).
      RETURN.
    ENDIF.

    "**[HJA]->Save Component Configuration OTR text  + PLus Object Description in Various Languages
    io_xml->read( EXPORTING iv_name = 'OTR_TEXT'
                  CHANGING  cg_data = <lt_otr_texts> ).

    MODIFY ('WDY_CONFIG_COMPT') FROM TABLE <lt_otr_texts>.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_COMPT for Component Config ' && ms_item-obj_name ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DESCR_LANG'
                  CHANGING  cg_data = <lt_config_datt> ).

    MODIFY ('WDY_CONFIG_DATT') FROM TABLE <lt_config_datt>.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_DATT for Component Config ' && ms_item-obj_name ).
      RETURN.
    ENDIF.

    CALL METHOD ('CL_WDR_CFG_PERSISTENCE_UTILS')=>('SAVE_COMP_CONFIG_TO_DB')
      EXPORTING
        config_data = <ls_orig_config>
        translator  = lo_translator.

    COMMIT WORK.

    CALL FUNCTION 'DEQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = if_wdr_cfg_constants=>c_lock_mode_exclusive
        config_id            = lv_config_id
        config_type          = lv_config_type
        config_var           = lv_config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'.

    io_xml->read( EXPORTING iv_name = 'TADIR'
                  CHANGING  cg_data = ls_tadir_backup ).


    "**[HJA]->Check TADIR
    lv_obj_name = ms_item-obj_name.
    ls_tadir = me->get_tadir_entry( iv_devclass = iv_package
                                    iv_obj_name = lv_obj_name ).

    IF ls_tadir IS NOT INITIAL.

      IF ls_tadir-delflag = abap_true.
        me->modify_tadir_entry( iv_delete_flag = abap_false
                                iv_obj_name    = lv_obj_name
                                iv_devclass    = iv_package ).
      ENDIF.

    ELSE.

      ls_tadir = me->create_tadir_entry(  iv_obj_name = lv_obj_name
                                          iv_devclass = iv_package  ).

    ENDIF.


  ENDMETHOD.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

  ENDMETHOD.

  METHOD create_tadir_entry.

    CALL FUNCTION 'TRINT_TADIR_INSERT'
      EXPORTING
        author               = sy-uname
        masterlang           = sy-langu
        devclass             = iv_devclass
        object               = 'WDCC'
        obj_name             = iv_obj_name
        pgmid                = 'R3TR'
        created_on           = sy-datum
      IMPORTING
        es_tadir             = rs_tadir
      EXCEPTIONS
        object_exists_global = 1
        object_exists_local  = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.

      IF sy-subrc = 1 OR sy-subrc = 2.

        me->modify_tadir_entry( iv_delete_flag = abap_false
                                iv_obj_name    = iv_obj_name
                                iv_devclass    = iv_devclass ).

      ELSE.
        zcx_abapgit_exception=>raise( 'Error Inserting TADIR Entry for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD delete_tadir_entry.

    CALL FUNCTION 'TRINT_TADIR_DELETE'
      EXPORTING
        object                   = 'WDCC'
        obj_name                 = iv_obj_name
        pgmid                    = 'R3TR'
        iv_test_mode             = iv_test_mode
      EXCEPTIONS
        tadir_entry_not_existing = 1
        object_exists            = 2
        object_locked            = 3
        object_distributed       = 4
        OTHERS                   = 5.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error Deleting TADIR Entry for Component Config ' && ms_item-obj_name ).
    ENDIF.

  ENDMETHOD.

  METHOD get_tadir_entry.

    DATA lt_tadir TYPE STANDARD TABLE OF tadir.

    CALL FUNCTION 'TADIR_GET'
      EXPORTING
        devclass = iv_devclass
      TABLES
        iadir    = lt_tadir.

    READ TABLE lt_tadir INTO rs_tadir WITH KEY pgmid = 'R3TR'
                                               object = 'WDCC'
                                               obj_name = iv_obj_name.

  ENDMETHOD.

  METHOD modify_tadir_entry.

    CALL FUNCTION 'TRINT_TADIR_MODIFY'
      EXPORTING
        author               = sy-uname
        devclass             = iv_devclass
        masterlang           = sy-langu
        object               = 'WDCC'
        obj_name             = iv_obj_name
        pgmid                = 'R3TR'
        delflag              = iv_delete_flag
      IMPORTING
        es_tadir             = rs_tadir
      EXCEPTIONS
        object_exists_global = 1
        object_exists_local  = 2
        object_has_no_tadir  = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error Modifying TADIR Entry for Component Config ' && ms_item-obj_name ).
    ENDIF.

  ENDMETHOD.

  METHOD update_tadir_entry.

    CALL FUNCTION 'TRINT_TADIR_UPDATE'
      EXPORTING
        pgmid                = 'R3TR'
        object               = 'WDCC'
        obj_name             = iv_obj_name
        author               = sy-uname
        masterlang           = sy-langu
        devclass             = iv_devclass
      IMPORTING
        es_tadir             = rs_tadir
      EXCEPTIONS
        object_has_no_tadir  = 1
        object_exists_global = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error Updating TADIR Entry for Component Config ' && ms_item-obj_name ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
