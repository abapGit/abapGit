CLASS zcl_abapgit_object_wdcc DEFINITION PUBLIC
  INHERITING FROM zcl_abapgit_objects_super.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_WDCC IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

  ENDMETHOD.


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

    TRY.
        " does not exist in 702
        CALL METHOD cl_wdr_cfg_persistence_utils=>('DELETE_CONFIGURATION')
          EXPORTING
            config_key = ls_config_key
          RECEIVING
            subrc      = lv_subrc.
        IF lv_subrc <> 0.
          zcx_abapgit_exception=>raise( 'Error deleting WDCC: ' && ms_item-obj_name ).
        ENDIF.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'Object type WDCC not supported for this release' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_config_id   TYPE c LENGTH 32,
          lv_config_type TYPE n LENGTH 2,
          lv_config_var  TYPE c LENGTH 6,
          lt_otr_texts   TYPE TABLE OF wdy_config_compt,
          ls_orig_config TYPE wdy_config_data,
          lt_config_datt TYPE TABLE OF wdy_config_datt,
          lv_xml_string  TYPE string,
          lv_xml_xstring TYPE xstring.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    io_xml->read( EXPORTING iv_name = 'CONFIG_ID'
                  CHANGING  cg_data = ls_orig_config-config_id  ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_TYPE'
                  CHANGING  cg_data = ls_orig_config-config_type ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_VAR'
                  CHANGING  cg_data = ls_orig_config-config_var ).

    ASSIGN COMPONENT 'CONFIG_IDPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'CONFIG_IDPAR'
                     CHANGING cg_data = <lv_data> ).
    ELSE.
      ii_log->add_error( iv_msg  = |Object type WDCC not supported for this release|
                         is_item = ms_item ).
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_TYPEPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'CONFIG_TYPEPAR'
                     CHANGING cg_data = <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_VARPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'CONFIG_VARPAR'
                     CHANGING cg_data = <lv_data> ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'WDA_COMPONENT'
                  CHANGING  cg_data = ls_orig_config-component ).

    lv_xml_string = mo_files->read_string( iv_extra = 'comp_config'
                                           iv_ext   = 'xml' ).
    TRY.
        lv_xml_string = zcl_abapgit_xml_pretty=>print( iv_xml           = lv_xml_string
                                                       iv_ignore_errors = abap_false
                                                       iv_unpretty      = abap_true ).
      CATCH zcx_abapgit_exception.
        zcx_abapgit_exception=>raise( 'Error Un-Pretty Printing WDCC XML Content: ' && ms_item-obj_name ).
    ENDTRY.

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml_string
      WITH '<?xml version="1.0"?>'.
    ASSERT sy-subrc = 0.

    lv_xml_xstring = zcl_abapgit_convert=>string_to_xstring( iv_str = lv_xml_string ).
    ls_orig_config-xcontent = lv_xml_xstring.

    ASSIGN COMPONENT 'PARENT' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'PARENT'
                     CHANGING cg_data = <lv_data> ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'RELID'
                  CHANGING  cg_data = ls_orig_config-relid ).

    ls_orig_config-author = sy-uname.
    ls_orig_config-changedby = sy-uname.
    ls_orig_config-changedon = sy-datum.
    ls_orig_config-createdon = sy-datum.

    CALL FUNCTION 'ENQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = 'E' "if_wdr_cfg_constants=>c_lock_mode_exclusive
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

    " CL_WDR_CFG_PERSISTENCE_UTILS=>SAVE_COMP_CONFIG_TO_DB does not exist in 702 so we save directly to DB
    DELETE FROM wdy_config_data
      WHERE config_id   = ls_orig_config-config_id
        AND config_type = ls_orig_config-config_type
        AND config_var  = ls_orig_config-config_var.
    MODIFY wdy_config_data FROM ls_orig_config.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_DATA for Component Config ' && ms_item-obj_name ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'OTR_TEXT'
                  CHANGING  cg_data = lt_otr_texts ).

    IF lt_otr_texts IS NOT INITIAL.
      DELETE FROM wdy_config_compt
        WHERE config_id   = ls_orig_config-config_id
          AND config_type = ls_orig_config-config_type
          AND config_var  = ls_orig_config-config_var.
      MODIFY wdy_config_compt FROM TABLE lt_otr_texts.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_COMPT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DESCR_LANG'
                  CHANGING  cg_data = lt_config_datt ).

    IF lt_config_datt IS NOT INITIAL.
      DELETE FROM wdy_config_datt
        WHERE config_id   = ls_orig_config-config_id
          AND config_type = ls_orig_config-config_type
          AND config_var  = ls_orig_config-config_var.
      MODIFY wdy_config_datt FROM TABLE lt_config_datt.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_DATT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = 'E' "if_wdr_cfg_constants=>c_lock_mode_exclusive
        config_id            = lv_config_id
        config_type          = lv_config_type
        config_var           = lv_config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'.

    tadir_insert( iv_package = iv_package ).

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
          lv_garg  TYPE eqegraarg.

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

    rv_is_locked = boolc( lines( lt_enq ) > 0 ).

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

    DATA: lv_xml_xstring TYPE xstring,
          lt_otr_texts   TYPE TABLE OF wdy_config_compt,
          lt_cc_text     TYPE TABLE OF wdy_config_datt,
          ls_orig_config TYPE wdy_config_data,
          ls_outline     TYPE wdy_cfg_outline_data,
          ls_config_key  TYPE wdy_config_key,
          lv_xml_string  TYPE string.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    io_xml->add( iv_name = 'OBJECT_NAME'
                 ig_data =  ms_item-obj_name ).

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = '00'.

    TRY.
        " original_config_data does not exist in 702
        CALL METHOD cl_wdr_cfg_persistence_utils=>('READ_COMP_CONFIG_FROM_DB')
          EXPORTING
            config_key           = ls_config_key
          IMPORTING
            xml_xcontent         = lv_xml_xstring
            original_config_data = ls_orig_config
            outline_data         = ls_outline.

      CATCH cx_static_check.
        zcx_abapgit_exception=>raise( 'Error Reading Component Config from DB: ' && ms_item-obj_name ).
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'Object type WDCC not supported for this release' ).
    ENDTRY.

    io_xml->add( iv_name = 'CONFIG_ID'
                 ig_data =  ls_orig_config-config_id ).

    io_xml->add( iv_name = 'CONFIG_TYPE'
                 ig_data =  ls_orig_config-config_type ).

    io_xml->add( iv_name = 'CONFIG_VAR'
                 ig_data =  ls_orig_config-config_var ).

    io_xml->add( iv_name = 'WDA_COMPONENT'
                 ig_data =  ls_orig_config-component ).

    ASSIGN COMPONENT 'CONFIG_IDPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'CONFIG_IDPAR'
                   ig_data =  <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_TYPEPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'CONFIG_TYPEPAR'
                   ig_data =  <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_VARPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'CONFIG_VARPAR'
                   ig_data =  <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'PARENT' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'PARENT'
                   ig_data =  <lv_data> ).
    ENDIF.

    io_xml->add( iv_name = 'RELID'
                 ig_data =  ls_orig_config-relid ).

    lv_xml_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_data = lv_xml_xstring ).
    TRY.
        lv_xml_string = zcl_abapgit_xml_pretty=>print( iv_xml           = lv_xml_string
                                                       iv_ignore_errors = abap_false ).
      CATCH zcx_abapgit_exception.    "
        zcx_abapgit_exception=>raise( 'Error Pretty Printing WDCC XML Content: ' && ms_item-obj_name ).
    ENDTRY.

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml_string
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

    mo_files->add_string( iv_extra  = 'comp_config'
                          iv_ext    = 'xml'
                          iv_string = lv_xml_string ).

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
ENDCLASS.
