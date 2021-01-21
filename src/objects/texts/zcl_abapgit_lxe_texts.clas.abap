CLASS zcl_abapgit_lxe_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_lxe_texts.
    ALIASES:
        ty_languages FOR zif_abapgit_lxe_texts~ty_languages,
        ty_lxe_i18n  FOR zif_abapgit_lxe_texts~ty_lxe_i18n,
        ty_tlxe_i18n FOR zif_abapgit_lxe_texts~ty_tlxe_i18n.

    CLASS-METHODS:
      get_lang_iso4
        IMPORTING
          iv_src         TYPE spras
        RETURNING
          VALUE(rv_iso4) TYPE lxeisolang ,
      get_lxe_object_list
        IMPORTING
          iv_obj_name        TYPE sobj_name
          iv_object_type     TYPE trobjtype
        RETURNING
          VALUE(rt_obj_list) TYPE lxe_tt_colob,
      get_lxe_texts
        IMPORTING
                  iv_original_language TYPE spras
                  iv_obj_name          TYPE sobj_name
                  iv_object_type       TYPE trobjtype
        RETURNING VALUE(rt_lxe_texts)  TYPE ty_tlxe_i18n,
      deserialize_lxe_texts
        IMPORTING
          it_lxe_texts TYPE ty_tlxe_i18n,
      get_installed_languages
        RETURNING
          VALUE(rt_installed_languages) TYPE ty_languages.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_lxe_texts IMPLEMENTATION.
  METHOD get_lang_iso4.
    CALL FUNCTION 'LXE_T002_CONVERT_2_TO_4'
      EXPORTING
        old_r3_lang = iv_src
      IMPORTING
        new_lang    = rv_iso4.
  ENDMETHOD.


  METHOD get_lxe_object_list.


    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_obj_name.
    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid    = 'R3TR'
        object   = iv_object_type
        obj_name = lv_object_name
      TABLES
        ex_colob = rt_obj_list.

  ENDMETHOD.
  METHOD get_lxe_texts.
    DATA:
      lt_obj_list            TYPE lxe_tt_colob,
      lt_installed_languages TYPE TABLE OF langu,

      ls_lxe_text_item       TYPE ty_lxe_i18n.

    FIELD-SYMBOLS:
      <lv_language>   TYPE langu,
      <lv_lxe_object> TYPE lxe_colob.

    lt_obj_list = get_lxe_object_list(
                    iv_obj_name    = iv_obj_name
                    iv_object_type = iv_object_type ).

    lt_installed_languages = get_installed_languages( ).

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT lt_installed_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = get_lang_iso4( iv_original_language ).
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

        DELETE ls_lxe_text_item-text_pairs WHERE t_text IS INITIAL. " No Target Text, no translation to be transported
        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO rt_lxe_texts.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD deserialize_lxe_texts.
    DATA: ls_lxe_item       TYPE ty_lxe_i18n,
          lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    LOOP AT it_lxe_texts INTO ls_lxe_item.
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
  METHOD get_installed_languages.

    DATA lv_index TYPE i.
    DATA lv_length TYPE i.
    DATA lv_char TYPE c.
    DATA lv_installed_languages TYPE string.

    CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
      IMPORTING
        languages       = lv_installed_languages
      EXCEPTIONS
        sapgparam_error = 1                " Error requesting profile parameter
        OTHERS          = 2.
    IF sy-subrc <> 0.
    ENDIF.

    lv_length = strlen( lv_installed_languages ).
    lv_index = 0.
    WHILE lv_index < lv_length.
      lv_char = lv_installed_languages+lv_index(1).
      APPEND lv_char TO rt_installed_languages.
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
