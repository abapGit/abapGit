CLASS zcl_abapgit_object_doma DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_dd01_texts,
             ddlanguage TYPE dd01v-ddlanguage,
             ddtext     TYPE dd01v-ddtext,
           END OF ty_dd01_texts,
           BEGIN OF ty_dd07_texts,
             valpos     TYPE dd07v-valpos,
             ddlanguage TYPE dd07v-ddlanguage,
             domvalue_l TYPE dd07v-domvalue_l,
             domvalue_h TYPE dd07v-domvalue_h,
             ddtext     TYPE dd07v-ddtext,
             domval_ld  TYPE dd07v-domval_ld,
             domval_hd  TYPE dd07v-domval_hd,
           END OF ty_dd07_texts,
           tt_dd01_texts TYPE STANDARD TABLE OF ty_dd01_texts,
           tt_dd07_texts TYPE STANDARD TABLE OF ty_dd07_texts.
    CONSTANTS: c_longtext_id_doma TYPE dokil-id VALUE 'DO'.

    METHODS:
      serialize_texts
        IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      deserialize_texts
        IMPORTING io_xml   TYPE REF TO zcl_abapgit_xml_input
                  is_dd01v TYPE dd01v
                  it_dd07v TYPE dd07v_tab
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_DOMA IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_valpos     TYPE valpos,
          ls_dd01v_tmp  TYPE dd01v,
          lt_dd07v_tmp  TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE tt_dd01_texts,
          lt_dd07_texts TYPE tt_dd07_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd07v>     LIKE LINE OF it_dd07v,
                   <ls_dd01_text> LIKE LINE OF lt_dd01_texts,
                   <ls_dd07_text> LIKE LINE OF lt_dd07_texts.

    lv_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    io_xml->read( EXPORTING iv_name = 'DD01_TEXTS'
                  CHANGING  cg_data = lt_dd01_texts ).

    io_xml->read( EXPORTING iv_name = 'DD07_TEXTS'
                  CHANGING  cg_data = lt_dd07_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd07_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Domain description
      ls_dd01v_tmp = is_dd01v.
      READ TABLE lt_dd01_texts ASSIGNING <ls_dd01_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc > 0.
        zcx_abapgit_exception=>raise( |DD01_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd01_text> TO ls_dd01v_tmp.

      " Domain values
      lt_dd07v_tmp = it_dd07v.
      LOOP AT lt_dd07v_tmp ASSIGNING <ls_dd07v>.
        lv_valpos = <ls_dd07v>-valpos.
        READ TABLE lt_dd07_texts ASSIGNING <ls_dd07_text>
          WITH KEY ddlanguage = <lv_lang> valpos = <ls_dd07v>-valpos.
        CHECK sy-subrc = 0. " ! no translation -> master translation remain (maybe not OK)
        MOVE-CORRESPONDING <ls_dd07_text> TO <ls_dd07v>.
        <ls_dd07v>-valpos = lv_valpos.
        DELETE lt_dd07_texts INDEX sy-tabix. " Optimization
      ENDLOOP.

      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = lv_name
          dd01v_wa          = ls_dd01v_tmp
        TABLES
          dd07v_tab         = lt_dd07v_tmp
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from DDIF_DOMA_PUT @TEXTS' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd01v      TYPE dd01v,
          lt_dd07v      TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE tt_dd01_texts,
          lt_dd07_texts TYPE tt_dd07_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd07v>     LIKE LINE OF lt_dd07v,
                   <ls_dd01_text> LIKE LINE OF lt_dd01_texts,
                   <ls_dd07_text> LIKE LINE OF lt_dd07_texts.

    IF io_xml->i18n_params( )-serialize_master_lang_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip master lang - it was serialized already
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd01v
      WHERE domname = lv_name
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd01v_wa      = ls_dd01v
        TABLES
          dd07v_tab     = lt_dd07v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd01v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd01_texts ASSIGNING <ls_dd01_text>.
      MOVE-CORRESPONDING ls_dd01v TO <ls_dd01_text>.

      LOOP AT lt_dd07v ASSIGNING <ls_dd07v> WHERE NOT ddlanguage IS INITIAL.
        APPEND INITIAL LINE TO lt_dd07_texts ASSIGNING <ls_dd07_text>.
        MOVE-CORRESPONDING <ls_dd07v> TO <ls_dd07_text>.
      ENDLOOP.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd01_texts BY ddlanguage ASCENDING.
    SORT lt_dd07_texts BY valpos ASCENDING ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      io_xml->add( iv_name = 'DD01_TEXTS'
                   ig_data = lt_dd01_texts ).

      io_xml->add( iv_name = 'DD07_TEXTS'
                   ig_data = lt_dd07_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd01l INTO rv_user
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
* see class CL_WB_DDIC

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = abap_true
            objname              = lv_objname
            objtype              = 'D'
            no_ask_delete_append = abap_true
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'error from RS_DD_DELETE_OBJ, DOMA' ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = abap_true
            objname              = lv_objname
            objtype              = 'D'
*           no_ask_delete_append = abap_true parameter not available in lower NW versions
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'error from RS_DD_DELETE_OBJ, DOMA' ).
        ENDIF.

    ENDTRY.

    delete_longtexts( c_longtext_id_doma ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: lv_name  TYPE ddobjname,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v.

    FIELD-SYMBOLS <ls_dd07v> TYPE dd07v.

    io_xml->read( EXPORTING iv_name = 'DD01V'
                  CHANGING cg_data = ls_dd01v ).
    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING cg_data = lt_dd07v ).

    corr_insert( iv_package = iv_package iv_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
      <ls_dd07v>-domname = lv_name.
      <ls_dd07v>-valpos = sy-tabix.
    ENDLOOP.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = lv_name
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_DOMA_PUT' ).
    ENDIF.

    deserialize_texts( io_xml   = io_xml
                       is_dd01v = ls_dd01v
                       it_dd07v = lt_dd07v ).

    deserialize_longtexts( io_xml ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_domname TYPE dd01l-domname.


    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( iv_radio = 'RSRD1-DOMA'
               iv_field = 'RSRD1-DOMA_VAL' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name    TYPE ddobjname,
          ls_dd01v   TYPE dd01v,
          lv_masklen TYPE c LENGTH 4,
          lt_dd07v   TYPE TABLE OF dd07v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        dd01v_wa      = ls_dd01v
      TABLES
        dd07v_tab     = lt_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR ls_dd01v IS INITIAL.
      zcx_abapgit_exception=>raise( 'error from DDIF_DOMA_GET' ).
    ENDIF.

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time.

* make sure XML serialization does not dump if the field contains invalid data
* note that this is a N field, so '' is not valid
    IF ls_dd01v-authclass = ''.
      CLEAR ls_dd01v-authclass.
    ENDIF.
    lv_masklen = ls_dd01v-masklen.
    IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
      CLEAR ls_dd01v-masklen.
    ENDIF.

    SORT lt_dd07v BY
      valpos ASCENDING
      ddlanguage ASCENDING.

    io_xml->add( iv_name = 'DD01V'
                 ig_data = ls_dd01v ).
    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v ).

    serialize_texts( io_xml ).

    serialize_longtexts( io_xml         = io_xml
                         iv_longtext_id = c_longtext_id_doma ).

  ENDMETHOD.
ENDCLASS.
