CLASS zcl_abapgit_object_dtel DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_dd04_texts,
             ddlanguage TYPE dd04t-ddlanguage,
             ddtext     TYPE dd04t-ddtext,
             reptext    TYPE dd04t-reptext,
             scrtext_s  TYPE dd04t-scrtext_s,
             scrtext_m  TYPE dd04t-scrtext_m,
             scrtext_l  TYPE dd04t-scrtext_l,
           END OF ty_dd04_texts,
           tt_dd04_texts TYPE STANDARD TABLE OF ty_dd04_texts.
    CONSTANTS: c_longtext_id_dtel TYPE dokil-id VALUE 'DE'.

    METHODS:
      serialize_texts
        IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      deserialize_texts
        IMPORTING io_xml   TYPE REF TO zcl_abapgit_xml_input
                  is_dd04v TYPE dd04v
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_DTEL IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd04v_tmp  TYPE dd04v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd04_texts TYPE tt_dd04_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd04_text> LIKE LINE OF lt_dd04_texts.


    lv_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    io_xml->read( EXPORTING iv_name = 'DD04_TEXTS'
                  CHANGING  cg_data = lt_dd04_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd04_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Data element description
      ls_dd04v_tmp = is_dd04v.
      READ TABLE lt_dd04_texts ASSIGNING <ls_dd04_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc > 0.
        zcx_abapgit_exception=>raise( |DD04_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd04_text> TO ls_dd04v_tmp.
      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING
          name              = lv_name
          dd04v_wa          = ls_dd04v_tmp
        EXCEPTIONS
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |error from DDIF_DTEL_PUT @TEXTS, { sy-subrc }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd04v      TYPE dd04v,
          lt_dd04_texts TYPE tt_dd04_texts,
          lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd04_text> LIKE LINE OF lt_dd04_texts.

    IF io_xml->i18n_params( )-serialize_master_lang_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip master lang - it was serialized already
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd04v
      WHERE rollname = lv_name
      AND ddlanguage <> mv_language.                    "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd04v_wa      = ls_dd04v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd04v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd04_texts ASSIGNING <ls_dd04_text>.
      MOVE-CORRESPONDING ls_dd04v TO <ls_dd04_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd04_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      io_xml->add( iv_name = 'DD04_TEXTS'
                   ig_data = lt_dd04_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd04l INTO rv_user
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.

    lv_objname = ms_item-obj_name.


    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'E'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_DD_DELETE_OBJ, DTEL,' && sy-subrc ).
    ENDIF.

    delete_longtexts( c_longtext_id_dtel ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname.


    io_xml->read( EXPORTING iv_name = 'DD04V'
                  CHANGING cg_data = ls_dd04v ).

    corr_insert( iv_package = iv_package iv_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = lv_name
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from DDIF_DTEL_PUT, { sy-subrc }| ).
    ENDIF.

    deserialize_texts( io_xml   = io_xml
                       is_dd04v = ls_dd04v ).

    deserialize_longtexts( io_xml ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.


    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE rollname = ms_item-obj_name
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

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
* fm DDIF_DTEL_GET bypasses buffer, so SELECTs are
* done directly from here

    DATA: lv_name  TYPE ddobjname,
          ls_dd04v TYPE dd04v.

    lv_name = ms_item-obj_name.


    SELECT SINGLE * FROM dd04l
      INTO CORRESPONDING FIELDS OF ls_dd04v
      WHERE rollname = lv_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
      zcx_abapgit_exception=>raise( 'Not found in DD04L' ) ##NO_TEXT.
    ENDIF.

    SELECT SINGLE * FROM dd04t
      INTO CORRESPONDING FIELDS OF ls_dd04v
      WHERE rollname = lv_name
      AND ddlanguage = mv_language
      AND as4local = 'A'
      AND as4vers = '0000'.

    CLEAR: ls_dd04v-as4user,
           ls_dd04v-as4date,
           ls_dd04v-as4time.

    IF ls_dd04v-refkind = 'D'.
* clear values inherited from domain
      CLEAR: ls_dd04v-datatype,
             ls_dd04v-leng,
             ls_dd04v-decimals,
             ls_dd04v-outputlen,
             ls_dd04v-valexi,
             ls_dd04v-lowercase,
             ls_dd04v-signflag,
             ls_dd04v-convexit,
             ls_dd04v-entitytab.
    ENDIF.

    IF ls_dd04v-routputlen = ''.
* numeric field, make sure it is initial or XML serilization will dump
      CLEAR ls_dd04v-routputlen.
    ENDIF.
    IF ls_dd04v-authclass = ''.
      CLEAR ls_dd04v-authclass.
    ENDIF.

    io_xml->add( iv_name = 'DD04V'
                 ig_data = ls_dd04v ).

    serialize_texts( io_xml ).

    serialize_longtexts( io_xml         = io_xml
                         iv_longtext_id = c_longtext_id_dtel ).

  ENDMETHOD.
ENDCLASS.
