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
        !is_outline TYPE wdy_cfg_outline_data
        !it_data    TYPE wdy_cfg_persist_data_appl_tab
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS check
      IMPORTING
        !it_messages TYPE cts_messages
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_WDCA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_OBJECT_WDCA->CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MESSAGES                    TYPE        CTS_MESSAGES
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check.

    FIELD-SYMBOLS: <ls_message> TYPE LINE OF cts_messages.

    LOOP AT it_messages ASSIGNING <ls_message> WHERE severity = 'E'.
      zcx_abapgit_exception=>raise( <ls_message>-text ).
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_OBJECT_WDCA->DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete.

    DATA:
      lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
      lx_err       TYPE REF TO cx_wd_configuration,
      lt_messages  TYPE cts_messages,
      ls_key       TYPE wdy_config_key,
      ls_outline   TYPE wdy_cfg_outline_data,
      lv_operation TYPE i,
      lv_name      TYPE wdy_md_object_name,
      lv_exists    TYPE wdy_boolean,
      lv_transport TYPE trkorr.

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

        lv_transport = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.
        lo_cfg->set_transport( trkorr   = lv_transport
                               devclass = iv_package ).

        lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_delete.
        " First call, check, second call, delete
        DO 2 TIMES.
          lo_cfg->do_next_step(
            IMPORTING
              e_messages = lt_messages
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_OBJECT_WDCA->READ
* +-------------------------------------------------------------------------------------------------+
* | [<---] ES_OUTLINE                     TYPE        WDY_CFG_OUTLINE_DATA
* | [<---] ET_DATA                        TYPE        WDY_CFG_PERSIST_DATA_APPL_TAB
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read.

    DATA:
      lo_cfg    TYPE REF TO cl_wdr_cfg_persistence_appl,
      ls_key    TYPE wdy_config_key,
      lv_exists TYPE abap_bool,
      lx_err    TYPE REF TO cx_wd_configuration,
      lv_name   TYPE wdy_md_object_name.

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

      CATCH cx_wd_configuration INTO lx_err.
        zcx_abapgit_exception=>raise( 'WDCA, read error:' && lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_OBJECT_WDCA->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OUTLINE                     TYPE        WDY_CFG_OUTLINE_DATA
* | [--->] IT_DATA                        TYPE        WDY_CFG_PERSIST_DATA_APPL_TAB
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save.

    DATA:
      lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
      lx_err       TYPE REF TO cx_wd_configuration,
      lt_messages  TYPE cts_messages,
      ls_key       TYPE wdy_config_key,
      ls_data      LIKE LINE OF it_data,
      lv_operation TYPE i,
      lv_name      TYPE wdy_md_object_name,
      lv_exists    TYPE wdy_boolean,
      lv_transport TYPE trkorr.

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
        lv_transport = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.
        lo_cfg->set_transport( trkorr   = lv_transport
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~CHANGED_BY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_USER                        TYPE        XUBNAME
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~changed_by.

    DATA ls_key TYPE wdy_config_key.

    ls_key = ms_item-obj_name.

    SELECT SINGLE changedby FROM wdy_config_appl INTO rv_user
      WHERE config_id = ls_key-config_id AND config_type = ls_key-config_type AND config_var = ls_key-config_var.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~delete.

    delete( iv_package = iv_package ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] IO_XML                         TYPE REF TO ZIF_ABAPGIT_XML_INPUT
* | [--->] IV_STEP                        TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZATION_STEP
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~deserialize.

    DATA: ls_outline TYPE wdy_cfg_outline_data,
          lt_data    TYPE wdy_cfg_persist_data_appl_tab.

    io_xml->read( EXPORTING iv_name = 'OUTLINE'
                  CHANGING  cg_data = ls_outline ).
    io_xml->read( EXPORTING iv_name = 'DATA'
                  CHANGING  cg_data = lt_data ).

    save( is_outline = ls_outline
          it_data    = lt_data
          iv_package = iv_package ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_BOOL                        TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~GET_COMPARATOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RI_COMPARATOR                  TYPE REF TO ZIF_ABAPGIT_COMPARATOR
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_STEPS                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZATION_STEP_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~GET_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_METADATA                    TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_METADATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~IS_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ACTIVE                      TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~IS_LOCKED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_IS_LOCKED                   TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~JUMP
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_WDCA->ZIF_ABAPGIT_OBJECT~SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_XML                         TYPE REF TO ZIF_ABAPGIT_XML_OUTPUT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~serialize.

    DATA: ls_outline TYPE wdy_cfg_outline_data,
          lt_data    TYPE wdy_cfg_persist_data_appl_tab.

    read( IMPORTING es_outline = ls_outline
                    et_data    = lt_data ).

    IF ls_outline IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'OUTLINE'
                 ig_data = ls_outline ).
    io_xml->add( iv_name = 'DATA'
                 ig_data = lt_data ).

  ENDMETHOD.
ENDCLASS.
