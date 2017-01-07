*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_DOMA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

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

    METHODS:
      serialize_texts
        IMPORTING io_xml TYPE REF TO lcl_xml_output
        RAISING   lcx_exception,
      deserialize_texts
        IMPORTING io_xml   TYPE REF TO lcl_xml_input
                  is_dd01v TYPE dd01v
                  it_dd07v TYPE dd07v_tab
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_doma DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    DATA: lv_date TYPE dats,
          lv_time TYPE tims,
          lv_ts   TYPE timestamp.

    SELECT SINGLE as4date as4time FROM dd01l
      INTO (lv_date, lv_time)
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.

    _object_check_timestamp lv_date lv_time.

  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    SELECT SINGLE as4user FROM dd01l INTO rv_user
      WHERE domname = ms_item-obj_name
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

    DATA: lv_domname TYPE dd01l-domname.


    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DOMA'
               iv_field = 'RSRD1-DOMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.
* see class CL_WB_DDIC

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'D'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_DD_DELETE_OBJ, DOMA' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

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
      lcx_exception=>raise( 'error from DDIF_DOMA_GET' ).
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

    io_xml->add( iv_name = 'DD01V'
                 ig_data = ls_dd01v ).
    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v ).

    serialize_texts( io_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: lv_name  TYPE ddobjname,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v.


    io_xml->read( EXPORTING iv_name = 'DD01V'
                  CHANGING cg_data = ls_dd01v ).
    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING cg_data = lt_dd07v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

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
      lcx_exception=>raise( 'error from DDIF_DOMA_PUT' ).
    ENDIF.

    deserialize_texts( io_xml   = io_xml
                       is_dd01v = ls_dd01v
                       it_dd07v = lt_dd07v ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD serialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_index      TYPE i,
          ls_dd01v      TYPE dd01v,
          lt_dd07v      TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE tt_dd01_texts,
          lt_dd07_texts TYPE tt_dd07_texts.

    FIELD-SYMBOLS: <lang>      LIKE LINE OF lt_i18n_langs,
                   <dd07v>     LIKE LINE OF lt_dd07v,
                   <dd01_text> LIKE LINE OF lt_dd01_texts,
                   <dd07_text> LIKE LINE OF lt_dd07_texts.

    lv_name = ms_item-obj_name.

    " Collect additional languages
    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd01v
      WHERE domname = lv_name
      AND   ddlanguage <> mv_language. " Skip master lang - it was serialized already

    LOOP AT lt_i18n_langs ASSIGNING <lang>.
      lv_index = sy-tabix.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_name
          langu         = <lang>
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

      APPEND INITIAL LINE TO lt_dd01_texts ASSIGNING <dd01_text>.
      MOVE-CORRESPONDING ls_dd01v TO <dd01_text>.

      LOOP AT lt_dd07v ASSIGNING <dd07v>.
        APPEND INITIAL LINE TO lt_dd07_texts ASSIGNING <dd07_text>.
        MOVE-CORRESPONDING <dd07v> TO <dd07_text>.
      ENDLOOP.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd01_texts BY ddlanguage ASCENDING.
    SORT lt_dd07_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 1.
      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      io_xml->add( iv_name = 'DD01_TEXTS'
                   ig_data = lt_dd01_texts ).

      io_xml->add( iv_name = 'DD07_TEXTS'
                   ig_data = lt_dd07_texts ).
    ENDIF.

  ENDMETHOD.  "serialize_texts

  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd01v_tmp  TYPE dd01v,
          lt_dd07v_tmp  TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE tt_dd01_texts,
          lt_dd07_texts TYPE tt_dd07_texts.

    FIELD-SYMBOLS: <lang>      LIKE LINE OF lt_i18n_langs,
                   <dd07v>     LIKE LINE OF it_dd07v,
                   <dd01_text> LIKE LINE OF lt_dd01_texts,
                   <dd07_text> LIKE LINE OF lt_dd07_texts.

    lv_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    io_xml->read( EXPORTING iv_name = 'DD01_TEXTS'
                  CHANGING  cg_data = lt_dd01_texts ).

    io_xml->read( EXPORTING iv_name = 'DD07_TEXTS'
                  CHANGING  cg_data = lt_dd07_texts ).

    SORT lt_i18n_langs.
    SORT lt_dd07_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lang>.

      " Domain description
      ls_dd01v_tmp = is_dd01v.
      READ TABLE lt_dd01_texts ASSIGNING <dd01_text> WITH KEY ddlanguage = <lang>.
      IF sy-subrc > 0.
        lcx_exception=>raise( |DD01_TEXTS cannot find lang { <lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <dd01_text> TO ls_dd01v_tmp.

      " Domain values
      lt_dd07v_tmp = it_dd07v.
      LOOP AT lt_dd07v_tmp ASSIGNING <dd07v>.
        READ TABLE lt_dd07_texts ASSIGNING <dd07_text>
          WITH KEY ddlanguage = <lang> valpos = <dd07v>-valpos.
        CHECK sy-subrc = 0. " ! no translation -> master translation remain (maybe not OK)
        MOVE-CORRESPONDING <dd07_text> TO <dd07v>.
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
        lcx_exception=>raise( 'error from DDIF_DOMA_PUT @TEXTS' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.  "deserialize_texts

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_doma IMPLEMENTATION