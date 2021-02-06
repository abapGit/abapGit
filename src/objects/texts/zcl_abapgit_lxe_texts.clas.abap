CLASS zcl_abapgit_lxe_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_lxe_texts .

    CLASS-METHODS get_translation_languages
      IMPORTING
        !iv_main_language   TYPE spras
        !it_i18n_languages  TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS get_installed_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS convert_lang_string_to_table
      IMPORTING
        !iv_langs              TYPE string
        !iv_skip_main_language TYPE spras OPTIONAL
      RETURNING
        VALUE(rt_languages)    TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS convert_table_to_lang_string
      IMPORTING
        !it_languages   TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rv_langs) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS detect_unsupported_languages
      IMPORTING
        !it_languages TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_unsupported_languages) TYPE zif_abapgit_definitions=>ty_languages
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gt_installed_languages_cache TYPE zif_abapgit_definitions=>ty_languages.

    METHODS
      get_lang_iso4
        IMPORTING
          iv_src         TYPE laiso
        RETURNING
          VALUE(rv_iso4) TYPE lxeisolang
        RAISING
          zcx_abapgit_exception.
    METHODS
      get_lxe_object_list
        IMPORTING
          iv_object_type     TYPE trobjtype
          iv_object_name     TYPE sobj_name
        RETURNING
          VALUE(rt_obj_list) TYPE lxe_tt_colob .
    CLASS-METHODS
      langu_to_laiso_safe
        IMPORTING
          iv_langu TYPE sy-langu
        RETURNING
          VALUE(rv_laiso) TYPE laiso
        RAISING
          zcx_abapgit_exception.
    CLASS-METHODS
      check_langs_versus_installed
        IMPORTING
          it_languages TYPE zif_abapgit_definitions=>ty_languages
          it_installed TYPE zif_abapgit_definitions=>ty_languages
        EXPORTING
          et_intersection TYPE zif_abapgit_definitions=>ty_languages
          et_missfits TYPE zif_abapgit_definitions=>ty_languages.

ENDCLASS.



CLASS ZCL_ABAPGIT_LXE_TEXTS IMPLEMENTATION.


  METHOD check_langs_versus_installed.

    DATA lt_installed_hash TYPE HASHED TABLE OF laiso WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS <lv_lang> LIKE LINE OF it_languages.

    CLEAR: et_intersection, et_missfits.
    lt_installed_hash = it_installed.

    LOOP AT it_languages ASSIGNING <lv_lang>.
      READ TABLE lt_installed_hash WITH KEY table_line = <lv_lang> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lv_lang> TO et_intersection.
      ELSE.
        APPEND <lv_lang> TO et_missfits.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_lang_string_to_table.

    DATA:
      lt_langs_str TYPE string_table,
      lv_laiso     TYPE laiso,
      lv_langu     TYPE spras,
      lv_skip_main_lang_iso TYPE laiso.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs = '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      lv_laiso = condense( to_upper( <lv_str> ) ).
      APPEND lv_laiso TO rt_languages.
    ENDLOOP.

    IF iv_skip_main_language IS NOT INITIAL.
      lv_skip_main_lang_iso = langu_to_laiso_safe( iv_skip_main_language ).
      DELETE rt_languages WHERE table_line = lv_skip_main_lang_iso.
    ENDIF.

    SORT rt_languages.
    DELETE ADJACENT DUPLICATES FROM rt_languages.

  ENDMETHOD.


  METHOD convert_table_to_lang_string.

    DATA:
      lt_langs_str TYPE string_table.

    FIELD-SYMBOLS:
      <lv_lang> LIKE LINE OF it_languages,
      <lv_str>  TYPE string.

    " Convert table of sy-langu codes into string of 2-letter ISO languages
    LOOP AT it_languages ASSIGNING <lv_lang>.
      " Keep * as indicator for 'all installed languages'
      IF <lv_lang> = '*'.
        CLEAR lt_langs_str.
        APPEND '*' TO lt_langs_str.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = <lv_lang>.
    ENDLOOP.

    CONCATENATE LINES OF lt_langs_str INTO rv_langs SEPARATED BY ','.

  ENDMETHOD.


  METHOD detect_unsupported_languages.

    check_langs_versus_installed(
      EXPORTING
        it_languages = it_languages
        it_installed = get_installed_languages( )
      IMPORTING
        et_missfits = rt_unsupported_languages ).

  ENDMETHOD.


  METHOD get_installed_languages.

    DATA:
      lv_index               TYPE i,
      lv_langu               TYPE sy-langu,
      lv_laiso               TYPE laiso,
      lv_installed_languages TYPE string.

    IF gt_installed_languages_cache IS INITIAL.
      CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
        IMPORTING
          languages       = lv_installed_languages
        EXCEPTIONS
          sapgparam_error = 1                " Error requesting profile parameter
          OTHERS          = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Fail to get system SYSTEM_INSTALLED_LANGUAGES' ).
      ENDIF.

      DO strlen( lv_installed_languages ) TIMES.
        lv_index = sy-index - 1.
        lv_langu = lv_installed_languages+lv_index(1).
        lv_laiso = langu_to_laiso_safe( lv_langu ).
        APPEND lv_laiso TO gt_installed_languages_cache.
      ENDDO.
    ENDIF.

    rt_languages = gt_installed_languages_cache.

  ENDMETHOD.


  METHOD get_lang_iso4.

    DATA lv_lang_iso639	TYPE i18_a_langiso2.
    DATA lv_country	TYPE land1.

    cl_i18n_languages=>sap2_to_iso639_1(
      EXPORTING
        im_lang_sap2   = iv_src
      IMPORTING
        ex_lang_iso639 = lv_lang_iso639
        ex_country     = lv_country
      EXCEPTIONS
        no_assignment  = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to convert [{ iv_src }] lang to iso639| ).
    ENDIF.

    CONCATENATE lv_lang_iso639 lv_country INTO rv_iso4.

  ENDMETHOD.


  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_object_name.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'R3TR'
        object          = iv_object_type
        obj_name        = lv_object_name
      TABLES
        ex_colob        = rt_obj_list
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN. " Ignore error and return empty list
    ENDIF.

  ENDMETHOD.


  METHOD get_translation_languages.

    " Returns a list of translation languages for serialization
    " If the setting is initial, no translations shall be serialized
    " If the setting is `*`, all all installed system languages shall be serialized
    " Else, the setting shall contain all languages to be serialized

    DATA lv_main_lang_laiso TYPE laiso.

    IF it_i18n_languages IS NOT INITIAL.
      READ TABLE it_i18n_languages TRANSPORTING NO FIELDS WITH KEY table_line = '*'.
      IF sy-subrc = 0.
        rt_languages = get_installed_languages( ).
      ELSE.
        check_langs_versus_installed(
          EXPORTING
            it_languages = it_i18n_languages
            it_installed = get_installed_languages( )
          IMPORTING
            et_intersection = rt_languages ).
      ENDIF.
    ENDIF.

    " Remove main language from translation languages
    lv_main_lang_laiso = langu_to_laiso_safe( iv_main_language ).
    DELETE rt_languages WHERE table_line = lv_main_lang_laiso.

  ENDMETHOD.


  METHOD langu_to_laiso_safe.

    cl_i18n_languages=>sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_langu
      RECEIVING
        re_lang_sap2  = rv_laiso
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Could not convert lang [{ iv_langu }] to ISO| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~deserialize.

    DATA:
      lt_lxe_texts      TYPE zif_abapgit_lxe_texts=>ty_tlxe_i18n,
      ls_lxe_item       TYPE zif_abapgit_lxe_texts=>ty_lxe_i18n,
      lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    ii_xml->read( EXPORTING iv_name = iv_lxe_text_name
                  CHANGING  cg_data = lt_lxe_texts ).

    LOOP AT lt_lxe_texts INTO ls_lxe_item.
      " Call Read first for buffer prefill
      CLEAR: lt_text_pairs_tmp.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
        EXPORTING
          s_lang    = ls_lxe_item-source_lang
          t_lang    = ls_lxe_item-target_lang
          custmnr   = ls_lxe_item-custmnr
          objtype   = ls_lxe_item-objtype
          objname   = ls_lxe_item-objname
          read_only = abap_false
        TABLES
          lt_pcx_s1 = lt_text_pairs_tmp.

      "Call actual Write FM
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          s_lang    = ls_lxe_item-source_lang
          t_lang    = ls_lxe_item-target_lang
          custmnr   = ls_lxe_item-custmnr
          objtype   = ls_lxe_item-objtype
          objname   = ls_lxe_item-objname
        TABLES
          lt_pcx_s1 = ls_lxe_item-text_pairs.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~serialize.

    DATA:
      lt_obj_list      TYPE lxe_tt_colob,
      lv_main_lang     TYPE lxeisolang,
      lt_languages     TYPE zif_abapgit_definitions=>ty_languages,
      lt_lxe_texts     TYPE zif_abapgit_lxe_texts=>ty_tlxe_i18n,
      ls_lxe_text_item TYPE zif_abapgit_lxe_texts=>ty_lxe_i18n.

    FIELD-SYMBOLS:
      <lv_language>   LIKE LINE OF lt_languages,
      <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
                    iv_object_name = iv_object_name
                    iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    " Get list of languages that need to be serialized (already resolves * and installed languages)
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( ii_xml->i18n_params( )-main_language ) ).
    lt_languages = ii_xml->i18n_params( )-translation_languages.

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT lt_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = lv_main_lang.
        ls_lxe_text_item-target_lang = get_lang_iso4( <lv_language> ).
        IF ls_lxe_text_item-source_lang = ls_lxe_text_item-target_lang.
          CONTINUE. " if source = target -> skip
        ENDIF.

        CLEAR ls_lxe_text_item-text_pairs.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = ls_lxe_text_item-source_lang
            t_lang    = ls_lxe_text_item-target_lang
            custmnr   = ls_lxe_text_item-custmnr
            objtype   = ls_lxe_text_item-objtype
            objname   = ls_lxe_text_item-objname
          TABLES
            lt_pcx_s1 = ls_lxe_text_item-text_pairs.

        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO lt_lxe_texts.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ii_xml->add( iv_name = iv_lxe_text_name
                 ig_data = lt_lxe_texts ).

  ENDMETHOD.
ENDCLASS.
