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
ENDCLASS.



CLASS zcl_abapgit_object_wdcc IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.

    DATA: ls_outline    TYPE wdy_cfg_outline_data,
          ls_config_key TYPE wdy_config_key.

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = '00'.

    TRY.
        cl_wdr_cfg_persistence_utils=>read_comp_config_from_db(
          EXPORTING
            config_key   = ls_config_key
          IMPORTING
            outline_data = ls_outline ).
      CATCH cx_static_check.
        zcx_abapgit_exception=>raise( 'Error Reading Component Config from DB: ' && ms_item-obj_name ).
    ENDTRY.

    rv_user = ls_outline-changedby.

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.
    DATA: ls_config_key TYPE wdy_config_key,
          lv_subrc      TYPE sysubrc.

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = '00'.

    cl_wdr_cfg_persistence_utils=>delete_configuration(
      EXPORTING
        config_key = ls_config_key
      RECEIVING
        subrc      = lv_subrc ).
    IF lv_subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error deleting WDCC: ' && ms_item-obj_name ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: ls_outline    TYPE wdy_cfg_outline_data,
          ls_config_key TYPE wdy_config_key.

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = '00'.

    TRY.
        cl_wdr_cfg_persistence_utils=>read_comp_config_from_db(
          EXPORTING
            config_key   = ls_config_key
          IMPORTING
            outline_data = ls_outline ).
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

    DATA ls_meta_data TYPE zif_abapgit_definitions=>ty_metadata.

    ls_meta_data = get_metadata( ).
    ls_meta_data-delete_tadir = abap_true.

    rs_metadata = ls_meta_data.

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
      zcx_abapgit_exception=>raise( 'Error check object lock WDCC: ' && ms_item-obj_name ).
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

    DATA: lv_xml_xstring   TYPE xstring,
          lv_xml_string    TYPE string,
          lo_document      TYPE REF TO if_ixml_document,
          lv_xml_as_string TYPE string,
          lt_otr_texts     TYPE TABLE OF wdy_config_compt,
          lt_cc_text       TYPE TABLE OF wdy_config_datt,
          ls_orig_config   TYPE wdy_config_data,
          ls_outline       TYPE wdy_cfg_outline_data,
          ls_config_key    TYPE wdy_config_key.

    io_xml->add( iv_name = 'OBJECT_NAME'
                 ig_data =  ms_item-obj_name ).

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = '00'.

    TRY.
        cl_wdr_cfg_persistence_utils=>read_comp_config_from_db(
          EXPORTING
            config_key           = ls_config_key
          IMPORTING
            xml_xcontent         = lv_xml_xstring
            original_config_data = ls_orig_config
            outline_data         = ls_outline ).

      CATCH cx_static_check.
        zcx_abapgit_exception=>raise( 'Error Reading Component Config from DB: ' && ms_item-obj_name ).
    ENDTRY.

    io_xml->add( iv_name = 'CONFIG_ID'
                 ig_data =  ls_orig_config-config_id ).

    io_xml->add( iv_name = 'CONFIG_TYPE'
                 ig_data =  ls_orig_config-config_type ).

    io_xml->add( iv_name = 'CONFIG_VAR'
                 ig_data =  ls_orig_config-config_var ).

    io_xml->add( iv_name = 'WDA_COMPONENT'
                 ig_data =  ls_orig_config-component ).

    io_xml->add( iv_name = 'CONFIG_IDPAR'
                 ig_data =  ls_orig_config-config_idpar ).

    io_xml->add( iv_name = 'CONFIG_TYPEPAR'
                 ig_data =  ls_orig_config-config_typepar ).

    io_xml->add( iv_name = 'CONFIG_VARPAR'
                 ig_data =  ls_orig_config-config_varpar ).

    io_xml->add( iv_name = 'PARENT'
                 ig_data =  ls_orig_config-parent ).

    io_xml->add( iv_name = 'RELID'
                 ig_data =  ls_orig_config-relid ).

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

    SELECT * FROM wdy_config_compt INTO TABLE lt_otr_texts WHERE config_id   = ls_orig_config-config_id
                                                             AND config_type = ls_orig_config-config_type
                                                             AND config_var  = ls_orig_config-config_var.

    IF lt_otr_texts IS NOT INITIAL.
      io_xml->add( iv_name = 'OTR_TEXT'
                   ig_data = lt_otr_texts ).
    ENDIF.

    SELECT * FROM wdy_config_datt INTO TABLE lt_cc_text WHERE config_id   = ls_orig_config-config_id
                                                          AND config_type = ls_orig_config-config_type
                                                          AND config_var  = ls_orig_config-config_var.
    IF lt_cc_text IS NOT INITIAL.
      io_xml->add( iv_name = 'DESCR_LANG'
                   ig_data = lt_cc_text ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA: lo_translator          TYPE REF TO if_wdr_config_otr,
          lv_config_id           TYPE c LENGTH 32,
          lv_config_type         TYPE n LENGTH 2,
          lv_config_var          TYPE c LENGTH 6,
          lt_otr_texts           TYPE TABLE OF wdy_config_compt,
          ls_orig_config         TYPE wdy_config_data,
          lt_config_datt         TYPE TABLE OF wdy_config_datt.

    io_xml->read( EXPORTING iv_name = 'CONFIG_ID'
                  CHANGING  cg_data = ls_orig_config-config_id  ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_TYPE'
                  CHANGING  cg_data = ls_orig_config-config_type ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_VAR'
                  CHANGING  cg_data = ls_orig_config-config_var ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_IDPAR'
                  CHANGING  cg_data = ls_orig_config-config_idpar ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_TYPEPAR'
                  CHANGING  cg_data = ls_orig_config-config_typepar ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_VARPAR'
                  CHANGING  cg_data = ls_orig_config-config_varpar ).

    io_xml->read( EXPORTING iv_name = 'WDA_COMPONENT'
                  CHANGING  cg_data = ls_orig_config-component ).

    io_xml->read( EXPORTING iv_name = 'COMPONENT_CONFIGURATION_RAW'
                  CHANGING  cg_data = ls_orig_config-xcontent  ).

    io_xml->read( EXPORTING iv_name = 'PARENT'
                  CHANGING  cg_data = ls_orig_config-parent ).

    io_xml->read( EXPORTING iv_name = 'RELID'
                  CHANGING  cg_data = ls_orig_config-relid ).

    ls_orig_config-author = sy-uname.
    ls_orig_config-changedby = sy-uname.
    ls_orig_config-changedon = sy-datum.
    ls_orig_config-createdon = sy-datum.

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
      zcx_abapgit_exception=>raise( 'Error Enqueueing Component Config: ' && ms_item-obj_name ).
    ENDIF.

    "**[HJA]->Save Component Configuration OTR text  + Plus Object Description in Various Languages
    io_xml->read( EXPORTING iv_name = 'OTR_TEXT'
                  CHANGING  cg_data = lt_otr_texts ).

    IF lt_otr_texts IS NOT INITIAL.
      MODIFY wdy_config_compt FROM TABLE lt_otr_texts.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_COMPT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DESCR_LANG'
                  CHANGING  cg_data = lt_config_datt ).

    IF lt_config_datt IS NOT INITIAL.
      MODIFY wdy_config_datt FROM TABLE lt_config_datt.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_DATT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

    cl_wdr_cfg_persistence_utils=>save_comp_config_to_db( config_data = ls_orig_config
                                                          translator  = lo_translator ).

    CALL FUNCTION 'DEQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = if_wdr_cfg_constants=>c_lock_mode_exclusive
        config_id            = lv_config_id
        config_type          = lv_config_type
        config_var           = lv_config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'.

    me->tadir_insert( iv_package = iv_package ).

  ENDMETHOD.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

  ENDMETHOD.

ENDCLASS.
