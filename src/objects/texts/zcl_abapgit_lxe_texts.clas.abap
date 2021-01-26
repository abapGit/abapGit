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
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages .
    CLASS-METHODS get_installed_languages
      RETURNING
        VALUE(rt_languages) TYPE zif_abapgit_definitions=>ty_languages .
    CLASS-METHODS convert_lang_string_to_table
      IMPORTING
        !iv_langs              TYPE string
        !iv_skip_main_language TYPE spras
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
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS
      get_lang_iso4
        IMPORTING
          iv_src         TYPE spras
        RETURNING
          VALUE(rv_iso4) TYPE lxeisolang .
    METHODS
      get_lxe_object_list
        IMPORTING
          iv_object_type     TYPE trobjtype
          iv_object_name     TYPE sobj_name
        RETURNING
          VALUE(rt_obj_list) TYPE lxe_tt_colob .

ENDCLASS.



CLASS zcl_abapgit_lxe_texts IMPLEMENTATION.


  METHOD convert_lang_string_to_table.

    DATA:
      lt_langs_str TYPE string_table,
      lv_laiso     TYPE laiso,
      lv_langu     TYPE spras.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str,
      <lv_lang> LIKE LINE OF rt_languages.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs = '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      lv_laiso = condense( to_upper( <lv_str> ) ).

      cl_i18n_languages=>sap2_to_sap1(
        EXPORTING
          im_lang_sap2      = lv_laiso
        RECEIVING
          re_lang_sap1      = lv_langu
        EXCEPTIONS
          no_assignment     = 1
          no_representation = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Unknown language code { <lv_str> }| ).
      ENDIF.

      APPEND INITIAL LINE TO rt_languages ASSIGNING <lv_lang>.
      <lv_lang> = lv_langu.
    ENDLOOP.

    DELETE rt_languages WHERE table_line = iv_skip_main_language.

    SORT rt_languages.
    DELETE ADJACENT DUPLICATES FROM rt_languages.

  ENDMETHOD.


  METHOD convert_table_to_lang_string.

    DATA:
      lt_langs_str TYPE string_table,
      lv_laiso     TYPE laiso.

    FIELD-SYMBOLS:
      <lv_langu> LIKE LINE OF it_languages,
      <lv_str>   TYPE string.

    " Convert table of sy-langu codes into string of 2-letter ISO languages
    LOOP AT it_languages ASSIGNING <lv_langu>.
      " Keep * as indicator for 'all installed languages'
      IF <lv_langu> = '*'.
        APPEND '*' TO lt_langs_str.
        EXIT.
      ENDIF.

      cl_i18n_languages=>sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = <lv_langu>
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Unknown language code { <lv_langu> }| ).
      ENDIF.

      APPEND INITIAL LINE TO lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = lv_laiso.
    ENDLOOP.

    CONCATENATE LINES OF lt_langs_str INTO rv_langs SEPARATED BY ','.

  ENDMETHOD.


  METHOD get_installed_languages.

    DATA:
      lv_index               TYPE i,
      lv_installed_languages TYPE string.

    CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
      IMPORTING
        languages       = lv_installed_languages
      EXCEPTIONS
        sapgparam_error = 1                " Error requesting profile parameter
        OTHERS          = 2.
    IF sy-subrc <> 0.
    ENDIF.

    DO strlen( lv_installed_languages ) TIMES.
      lv_index = sy-index - 1.
      APPEND lv_installed_languages+lv_index(1) TO rt_languages.
    ENDDO.

  ENDMETHOD.


  METHOD get_lang_iso4.

    CALL FUNCTION 'LXE_T002_CONVERT_2_TO_4'
      EXPORTING
        old_r3_lang = iv_src
      IMPORTING
        new_lang    = rv_iso4.

  ENDMETHOD.


  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_object_name.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid    = 'R3TR'
        object   = iv_object_type
        obj_name = lv_object_name
      TABLES
        ex_colob = rt_obj_list.

  ENDMETHOD.


  METHOD get_translation_languages.

    " Returns a list of translation languages for serialization
    " If the setting is initial, no translations shall be serialized
    " If the setting is `*`, all all installed system languages shall be serialized
    " Else, the setting shall contain all languages to be serialized

    IF it_i18n_languages IS NOT INITIAL.
      READ TABLE it_i18n_languages TRANSPORTING NO FIELDS WITH KEY table_line = '*'.
      IF sy-subrc = 0.
        rt_languages = get_installed_languages( ).
      ELSE.
        rt_languages = it_i18n_languages.
      ENDIF.
    ENDIF.

    " Remove main language from translation languages
    DELETE rt_languages WHERE table_line = iv_main_language.

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
      <lv_language>   TYPE langu,
      <lv_lxe_object> TYPE lxe_colob.

    lt_obj_list = get_lxe_object_list(
                    iv_object_name = iv_object_name
                    iv_object_type = iv_object_type ).

    " Get list of languages that need to be serialized (already resolves * and installed languages)
    lv_main_lang = get_lang_iso4( ii_xml->i18n_params( )-main_language ).
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
