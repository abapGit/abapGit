*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_DTEL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

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

    METHODS:
      serialize_texts
        IMPORTING io_xml TYPE REF TO lcl_xml_output
        RAISING   lcx_exception,
      deserialize_texts
        IMPORTING io_xml   TYPE REF TO lcl_xml_input
                  is_dd04v TYPE dd04v
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    DATA: lv_date TYPE dats,
          lv_time TYPE tims,
          lv_ts   TYPE timestamp.

    SELECT SINGLE as4date as4time FROM dd04l
      INTO (lv_date, lv_time)
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    _object_check_timestamp lv_date lv_time.

  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    SELECT SINGLE as4user FROM dd04l INTO rv_user
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.


    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

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
      lcx_exception=>raise( 'error from RS_DD_DELETE_OBJ, DTEL' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd04v TYPE dd04v,
          ls_tpara TYPE tpara.

    lv_name = ms_item-obj_name.


    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        dd04v_wa      = ls_dd04v
        tpara_wa      = ls_tpara
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
      lcx_exception=>raise( 'Error from DDIF_DTEL_GET' ).
    ENDIF.

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
    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).

    serialize_texts( io_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname,
          ls_tpara TYPE tpara.


    io_xml->read( EXPORTING iv_name = 'DD04V'
                  CHANGING cg_data = ls_dd04v ).
    io_xml->read( EXPORTING iv_name = 'TPARA'
                  CHANGING cg_data = ls_tpara ).

    corr_insert( iv_package ).

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
      lcx_exception=>raise( 'error from DDIF_DTEL_PUT' ).
    ENDIF.

    deserialize_texts( io_xml   = io_xml
                       is_dd04v = ls_dd04v ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd04v      TYPE dd04v,
          lt_dd04_texts TYPE tt_dd04_texts,
          lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS: <lang>      LIKE LINE OF lt_i18n_langs,
                   <dd04_text> TYPE ty_dd04_texts.

    lv_name = ms_item-obj_name.

    " Collect additional languages
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd04v
      WHERE rollname = lv_name
      AND   ddlanguage <> mv_language. " Skip master lang - it was serialized already

    LOOP AT lt_i18n_langs ASSIGNING <lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lang>
        IMPORTING
          dd04v_wa      = ls_dd04v
*         tpara_wa      = ls_tpara
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd04v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd04_texts ASSIGNING <dd04_text>.
      MOVE-CORRESPONDING ls_dd04v TO <dd04_text>.

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

  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd04v_tmp  TYPE dd04v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd04_texts TYPE tt_dd04_texts.
    FIELD-SYMBOLS: <lang>      LIKE LINE OF lt_i18n_langs,
                   <dd04_text> TYPE ty_dd04_texts.

    lv_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    io_xml->read( EXPORTING iv_name = 'DD04_TEXTS'
                  CHANGING  cg_data = lt_dd04_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd04_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lang>.

      " Data element description
      ls_dd04v_tmp = is_dd04v.
      READ TABLE lt_dd04_texts ASSIGNING <dd04_text> WITH KEY ddlanguage = <lang>.
      IF sy-subrc > 0.
        lcx_exception=>raise( |DD04_TEXTS cannot find lang { <lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <dd04_text> TO ls_dd04v_tmp.
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
        lcx_exception=>raise( 'error from DDIF_DTEL_PUT @TEXTS' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_dtel IMPLEMENTATION
