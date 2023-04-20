CLASS zcl_abapgit_object_view DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_dd26v TYPE STANDARD TABLE OF dd26v
                          WITH NON-UNIQUE DEFAULT KEY,
           ty_dd27p TYPE STANDARD TABLE OF dd27p
                          WITH NON-UNIQUE DEFAULT KEY,
           ty_dd28j TYPE STANDARD TABLE OF dd28j
                          WITH NON-UNIQUE DEFAULT KEY,
           ty_dd28v TYPE STANDARD TABLE OF dd28v
                          WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF ty_dd25_text,
             ddlanguage TYPE dd25t-ddlanguage,
             ddtext     TYPE dd25t-ddtext,
           END OF ty_dd25_text ,
           ty_dd25_texts TYPE STANDARD TABLE OF ty_dd25_text.
    CONSTANTS: BEGIN OF co_viewclass,
                 help         TYPE viewclass VALUE 'H',
                 database     TYPE viewclass VALUE 'D',
                 projection   TYPE viewclass VALUE 'P',
                 structure    TYPE viewclass VALUE 'S',
                 maintenance  TYPE viewclass VALUE 'C',
                 entity       TYPE viewclass VALUE 'E',
                 view_variant TYPE viewclass VALUE 'V',
                 append       TYPE viewclass VALUE 'A',
                 external     TYPE viewclass VALUE 'X',
                 replication  TYPE viewclass VALUE 'R',
               END OF co_viewclass.
    CONSTANTS c_longtext_id_view TYPE dokil-id VALUE 'VW'.

    METHODS:
      read_view
        IMPORTING
          iv_language TYPE sy-langu
        EXPORTING
          ev_state    TYPE ddgotstate
          es_dd25v    TYPE dd25v
          es_dd09l    TYPE dd09l
          et_dd26v    TYPE ty_dd26v
          et_dd27p    TYPE ty_dd27p
          et_dd28j    TYPE ty_dd28j
          et_dd28v    TYPE ty_dd28v
        RAISING
          zcx_abapgit_exception,

      serialize_texts
        IMPORTING
          ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING
          zcx_abapgit_exception,

      deserialize_texts
        IMPORTING
          ii_xml   TYPE REF TO zif_abapgit_xml_input
          is_dd25v TYPE dd25v
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_view IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA:
      lv_name       TYPE ddobjname,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_dd25_texts TYPE ty_dd25_texts,
      ls_dd25v_tmp  TYPE dd25v.

    FIELD-SYMBOLS:
      <lv_lang>      TYPE langu,
      <ls_dd25_text> LIKE LINE OF lt_dd25_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD25_TEXTS'
                  CHANGING  cg_data = lt_dd25_texts ).

    zcl_abapgit_lxe_texts=>trim_saplangu_by_iso(
      EXPORTING it_iso_filter = ii_xml->i18n_params( )-translation_languages
      CHANGING ct_sap_langs   = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd25_texts BY ddlanguage.

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " View description
      ls_dd25v_tmp = is_dd25v.
      READ TABLE lt_dd25_texts ASSIGNING <ls_dd25_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |DD25_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd25_text> TO ls_dd25v_tmp.
      CALL FUNCTION 'DDIF_VIEW_PUT'
        EXPORTING
          name              = lv_name
          dd25v_wa          = ls_dd25v_tmp
        EXCEPTIONS
          view_not_found    = 1
          name_inconsistent = 2
          view_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_view.

    DATA: lv_name TYPE ddobjname.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = iv_language
      IMPORTING
        gotstate      = ev_state
        dd25v_wa      = es_dd25v
        dd09l_wa      = es_dd09l
      TABLES
        dd26v_tab     = et_dd26v
        dd27p_tab     = et_dd27p
        dd28j_tab     = et_dd28j
        dd28v_tab     = et_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA:
      lv_index           TYPE i,
      ls_dd25v           TYPE dd25v,
      lt_dd25_texts      TYPE ty_dd25_texts,
      lt_i18n_langs      TYPE TABLE OF langu,
      lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_dd25_text> LIKE LINE OF lt_dd25_texts.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).

    zcl_abapgit_lxe_texts=>add_iso_langs_to_lang_filter(
      EXPORTING it_iso_filter      = ii_xml->i18n_params( )-translation_languages
      CHANGING  ct_language_filter = lt_language_filter ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd25v
      WHERE viewname = ms_item-obj_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CLEAR: ls_dd25v.

      TRY.
          read_view(
            EXPORTING
              iv_language = <lv_lang>
            IMPORTING
              es_dd25v    = ls_dd25v ).

        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      IF ls_dd25v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd25_texts ASSIGNING <ls_dd25_text>.
      MOVE-CORRESPONDING ls_dd25v TO <ls_dd25_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd25_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD25_TEXTS'
                   ig_data = lt_dd25_texts ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l INTO rv_user
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'V' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name = 'DD26V_TABLE'
                  CHANGING cg_data = lt_dd26v ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).
    io_xml->read( EXPORTING iv_name = 'DD28J_TABLE'
                  CHANGING cg_data = lt_dd28j ).
    io_xml->read( EXPORTING iv_name = 'DD28V_TABLE'
                  CHANGING cg_data = lt_dd28v ).

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      <ls_dd27p>-objpos = sy-tabix.
      <ls_dd27p>-viewname = lv_name.
      " rollname seems to be mandatory in the API, but is typically not defined in the VIEW
      SELECT SINGLE rollname FROM dd03l INTO <ls_dd27p>-rollname
        WHERE tabname = <ls_dd27p>-tabname
        AND fieldname = <ls_dd27p>-fieldname.
      IF <ls_dd27p>-rollnamevi IS INITIAL.
        <ls_dd27p>-rollnamevi = <ls_dd27p>-rollname.
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = lt_dd26v
        dd27p_tab         = lt_dd27p
        dd28j_tab         = lt_dd28j
        dd28v_tab         = lt_dd28v
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF io_xml->i18n_params( )-translation_languages IS INITIAL OR io_xml->i18n_params( )-use_lxe = abap_false.
      deserialize_texts(
        ii_xml   = io_xml
        is_dd25v = ls_dd25v ).
    ELSE.
      deserialize_lxe_texts( io_xml ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_view ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname,
          lv_ddl_view TYPE abap_bool.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

    IF rv_bool = abap_true.
      TRY.
          CALL METHOD ('CL_DD_DDL_UTILITIES')=>('CHECK_FOR_DDL_VIEW')
            EXPORTING
              objname     = lv_viewname
            RECEIVING
              is_ddl_view = lv_ddl_view.

          IF lv_ddl_view = abap_true.
            rv_bool = abap_false.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
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
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_dd25v TYPE dd25v,
          lv_state TYPE ddgotstate,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE ty_dd26v,
          lt_dd27p TYPE ty_dd27p,
          lt_dd28j TYPE ty_dd28j,
          lt_dd28v TYPE ty_dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    read_view(
      EXPORTING
        iv_language = mv_language
      IMPORTING
        ev_state    = lv_state
        es_dd25v    = ls_dd25v
        es_dd09l    = ls_dd09l
        et_dd26v    = lt_dd26v
        et_dd27p    = lt_dd27p
        et_dd28j    = lt_dd28j
        et_dd28v    = lt_dd28v ).

    IF ls_dd25v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      CLEAR: <ls_dd27p>-ddtext,
             <ls_dd27p>-reptext,
             <ls_dd27p>-scrtext_s,
             <ls_dd27p>-scrtext_m,
             <ls_dd27p>-scrtext_l,
             <ls_dd27p>-outputlen,
             <ls_dd27p>-decimals,
             <ls_dd27p>-lowercase,
             <ls_dd27p>-convexit,
             <ls_dd27p>-signflag,
             <ls_dd27p>-flength,
             <ls_dd27p>-domname,
             <ls_dd27p>-datatype,
             <ls_dd27p>-entitytab,
             <ls_dd27p>-inttype,
             <ls_dd27p>-intlen,
             <ls_dd27p>-headlen,
             <ls_dd27p>-scrlen1,
             <ls_dd27p>-scrlen2,
             <ls_dd27p>-scrlen3,
             <ls_dd27p>-memoryid.
      IF <ls_dd27p>-rollchange = abap_false.
        CLEAR <ls_dd27p>-rollnamevi.
      ENDIF.
      CLEAR <ls_dd27p>-ddlanguage.
      CLEAR <ls_dd27p>-rollname.
      CLEAR <ls_dd27p>-viewname.
      CLEAR <ls_dd27p>-objpos.
    ENDLOOP.

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( iv_name = 'DD09L'
                 ig_data = ls_dd09l ).
    io_xml->add( ig_data = lt_dd26v
                 iv_name = 'DD26V_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).
    io_xml->add( ig_data = lt_dd28j
                 iv_name = 'DD28J_TABLE' ).
    io_xml->add( ig_data = lt_dd28v
                 iv_name = 'DD28V_TABLE' ).

    IF io_xml->i18n_params( )-translation_languages IS INITIAL OR io_xml->i18n_params( )-use_lxe = abap_false.
      serialize_texts( io_xml ).
    ELSE.
      serialize_lxe_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_view ).

  ENDMETHOD.
ENDCLASS.
