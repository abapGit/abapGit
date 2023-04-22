CLASS zcl_abapgit_object_wdca DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS read
      EXPORTING
        !es_outline TYPE wdy_cfg_outline_data
        !et_data    TYPE wdy_cfg_persist_data_appl_tab
      RAISING
        zcx_abapgit_exception .
    METHODS save
      IMPORTING
        !is_outline   TYPE wdy_cfg_outline_data
        !it_data      TYPE wdy_cfg_persist_data_appl_tab
        !iv_package   TYPE devclass
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_package   TYPE devclass
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS check
      IMPORTING
        !it_messages TYPE cts_messages
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_wdca IMPLEMENTATION.


  METHOD check.

    FIELD-SYMBOLS: <ls_message> TYPE LINE OF cts_messages.

    LOOP AT it_messages ASSIGNING <ls_message> WHERE severity = 'E'.
      zcx_abapgit_exception=>raise( <ls_message>-text ).
    ENDLOOP.

  ENDMETHOD.


  METHOD delete.

    DATA:
      lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
      lx_err       TYPE REF TO cx_wd_configuration,
      lt_messages  TYPE cts_messages,
      ls_key       TYPE wdy_config_key,
      ls_outline   TYPE wdy_cfg_outline_data,
      lv_operation TYPE i,
      lv_name      TYPE wdy_md_object_name,
      lv_exists    TYPE wdy_boolean.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        MOVE-CORRESPONDING ls_key TO ls_outline.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = ls_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).

        IF lv_exists = abap_false.
          RETURN.
        ENDIF.

        lo_cfg->set_transport( trkorr   = iv_transport
                               devclass = iv_package ).

        lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_delete.
        " First call, check, second call, delete
        DO 2 TIMES.
          lo_cfg->do_next_step(
            IMPORTING
              e_messages  = lt_messages
            CHANGING
              c_operation = lv_operation ).
          check( lt_messages ).
        ENDDO.

      CATCH cx_wd_configuration INTO lx_err.
        IF lx_err->textid = cx_wd_configuration=>conf_config_not_exist.
          RETURN.
        ELSE.
          zcx_abapgit_exception=>raise( 'WDCA, delete error:' && lx_err->get_text( ) ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD read.

    DATA:
      lo_cfg    TYPE REF TO cl_wdr_cfg_persistence_appl,
      ls_key    TYPE wdy_config_key,
      lv_exists TYPE abap_bool,
      lx_err    TYPE REF TO cx_wd_configuration,
      lv_name   TYPE wdy_md_object_name.

    FIELD-SYMBOLS:
      <ls_data>        LIKE LINE OF et_data,
      <ls_appl_params> LIKE LINE OF <ls_data>-appl_params.

    CLEAR: es_outline, et_data.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        MOVE-CORRESPONDING ls_key TO es_outline.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = es_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).

        IF lv_exists = abap_false.
          RETURN.
        ENDIF.

        es_outline = lo_cfg->read_outline_data( ).

        CLEAR: es_outline-devclass,
               es_outline-author,
               es_outline-createdon,
               es_outline-changedby,
               es_outline-changedon.

        et_data = lo_cfg->read_data( ).

        " Clear descriptions since they are release and language-specific
        LOOP AT et_data ASSIGNING <ls_data>.
          LOOP AT <ls_data>-appl_params ASSIGNING <ls_appl_params>.
            CLEAR <ls_appl_params>-description.
          ENDLOOP.
        ENDLOOP.

      CATCH cx_wd_configuration INTO lx_err.
        zcx_abapgit_exception=>raise( 'WDCA, read error:' && lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD save.

    DATA:
      lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
      lx_err       TYPE REF TO cx_wd_configuration,
      lt_messages  TYPE cts_messages,
      ls_key       TYPE wdy_config_key,
      ls_data      LIKE LINE OF it_data,
      lv_operation TYPE i,
      lv_name      TYPE wdy_md_object_name,
      lv_exists    TYPE wdy_boolean.

    MOVE-CORRESPONDING is_outline TO ls_key.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        READ TABLE it_data INDEX 1 INTO ls_data.
        ASSERT sy-subrc = 0.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = is_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).

      CATCH cx_wd_configuration ##NO_HANDLER.
        " Ignore
    ENDTRY.

    TRY.
        lo_cfg->set_transport( trkorr   = iv_transport
                               devclass = iv_package ).
        lo_cfg->set_save_data( ls_data ).
        lo_cfg->set_config_description( is_outline ).

        IF lv_exists = abap_false.
          lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_create.
        ELSE.
          lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_save.
        ENDIF.

        " First call, check, second call, create/save
        DO 2 TIMES.
          lo_cfg->do_next_step(
            IMPORTING
              e_messages  = lt_messages
            CHANGING
              c_operation = lv_operation ).
          check( lt_messages ).
        ENDDO.

      CATCH cx_wd_configuration INTO lx_err.
        zcx_abapgit_exception=>raise( 'WDCA, save error:' && lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA ls_key TYPE wdy_config_key.

    ls_key = ms_item-obj_name.

    SELECT SINGLE changedby FROM wdy_config_appl INTO rv_user
      WHERE config_id = ls_key-config_id AND config_type = ls_key-config_type AND config_var = ls_key-config_var.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    delete( iv_package   = iv_package
            iv_transport = iv_transport ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_outline     TYPE wdy_cfg_outline_data,
          lt_data        TYPE wdy_cfg_persist_data_appl_tab,
          lt_config_appt TYPE TABLE OF wdy_config_appt,
          lv_xml_string  TYPE string,
          lv_xml_xstring TYPE xstring.

    io_xml->read( EXPORTING iv_name = 'OUTLINE'
                  CHANGING  cg_data = ls_outline ).
    io_xml->read( EXPORTING iv_name = 'DATA'
                  CHANGING  cg_data = lt_data ).

    save( is_outline   = ls_outline
          it_data      = lt_data
          iv_package   = iv_package
          iv_transport = iv_transport ).

    TRY.
        lv_xml_string = zif_abapgit_object~mo_files->read_string(
          iv_extra = 'appl_config'
          iv_ext   = 'xml' ).

        TRY.
            lv_xml_string = zcl_abapgit_xml_pretty=>print( iv_xml           = lv_xml_string
                                                           iv_ignore_errors = abap_false
                                                           iv_unpretty      = abap_true ).
          CATCH zcx_abapgit_exception.
            zcx_abapgit_exception=>raise( 'Error Un-Pretty Printing WDCA XML Content: ' && ms_item-obj_name ).
        ENDTRY.

        REPLACE FIRST OCCURRENCE
          OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
          IN lv_xml_string
          WITH '<?xml version="1.0"?>'.
        ASSERT sy-subrc = 0.

        lv_xml_xstring = zcl_abapgit_convert=>string_to_xstring( iv_str = lv_xml_string ).
        UPDATE wdy_config_appl
          SET xcontent = lv_xml_xstring
          WHERE config_id   = ls_outline-config_id
            AND config_type = ls_outline-config_type
            AND config_var  = ls_outline-config_var.
      CATCH zcx_abapgit_exception.
        " File not found
    ENDTRY.


    io_xml->read( EXPORTING iv_name = 'DESCR_LANG'
                  CHANGING  cg_data = lt_config_appt ).

    IF lt_config_appt IS NOT INITIAL.
      DELETE FROM wdy_config_appt
        WHERE config_id   = ls_outline-config_id
          AND config_type = ls_outline-config_type
          AND config_var  = ls_outline-config_var.
      MODIFY wdy_config_appt FROM TABLE lt_config_appt.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_APPT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: ls_wdy_config_appl TYPE wdy_config_appl.
    DATA: ls_wdy_config_key TYPE wdy_config_key.

    ls_wdy_config_key = ms_item-obj_name.
    SELECT SINGLE * FROM wdy_config_appl
      INTO ls_wdy_config_appl
      WHERE config_id = ls_wdy_config_key-config_id
        AND config_type = ls_wdy_config_key-config_type
        AND config_var = ls_wdy_config_key-config_var.  "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_outline     TYPE wdy_cfg_outline_data,
          lt_data        TYPE wdy_cfg_persist_data_appl_tab,
          lt_cc_text     TYPE TABLE OF wdy_config_appt,
          lv_xml_xstring TYPE xstring,
          lv_xml_string  TYPE string.

    read( IMPORTING es_outline = ls_outline
                    et_data    = lt_data ).

    IF ls_outline IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'OUTLINE'
                 ig_data = ls_outline ).
    io_xml->add( iv_name = 'DATA'
                 ig_data = lt_data ).


    SELECT SINGLE xcontent
      INTO lv_xml_xstring
      FROM wdy_config_appl
      WHERE config_id = ls_outline-config_id
        AND config_type = ls_outline-config_type
        AND config_var = ls_outline-config_var.
    lv_xml_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_data = lv_xml_xstring ).
    IF lv_xml_string IS NOT INITIAL.
      TRY.
          lv_xml_string = zcl_abapgit_xml_pretty=>print(
            iv_xml           = lv_xml_string
            iv_ignore_errors = abap_false ).
        CATCH zcx_abapgit_exception.
          zcx_abapgit_exception=>raise( 'Error Pretty Printing WDCA XML Content: ' && ms_item-obj_name ).
      ENDTRY.

      REPLACE FIRST OCCURRENCE
        OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
        IN lv_xml_string
        WITH '<?xml version="1.0" encoding="utf-8"?>'.
      ASSERT sy-subrc = 0.
    ENDIF.

    zif_abapgit_object~mo_files->add_string(
      iv_extra  = 'appl_config'
      iv_ext    = 'xml'
      iv_string = lv_xml_string ).

    SELECT * FROM wdy_config_appt INTO TABLE lt_cc_text
      WHERE config_id   = ls_outline-config_id
      AND config_type = ls_outline-config_type
      AND config_var  = ls_outline-config_var
      ORDER BY PRIMARY KEY.
    IF lt_cc_text IS NOT INITIAL.
      io_xml->add( iv_name = 'DESCR_LANG'
                   ig_data = lt_cc_text ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
