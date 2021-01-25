CLASS zcl_abapgit_lxe_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_lxe_texts .

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
      lv_langu     TYPE spras.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str,
      <lv_lang> LIKE LINE OF rt_languages.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs CS '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = condense( to_upper( <lv_str> ) ).

      SELECT SINGLE spras FROM t002 INTO lv_langu WHERE laiso = <lv_str>.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO rt_languages ASSIGNING <lv_lang>.
        <lv_lang> = lv_langu.
      ELSE.
        zcx_abapgit_exception=>raise( |Unknown language code { <lv_str> }| ).
      ENDIF.

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
  ENDMETHOD.


  METHOD get_lang_iso4.



  ENDMETHOD.


  METHOD get_lxe_object_list.



  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~deserialize.



  ENDMETHOD.


  METHOD zif_abapgit_lxe_texts~serialize.



  ENDMETHOD.
ENDCLASS.
