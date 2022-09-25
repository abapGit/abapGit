CLASS zcl_abapgit_persist_tag_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_allowed_chars TYPE string VALUE `-_.a-zA-Z0-9` ##NO_TEXT.
    " it is easier to whilelist chars, though potentially other chars can be added later if needed
    CLASS-METHODS class_constructor.

    CLASS-METHODS validate
      IMPORTING
        !iv_tags TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS split
      IMPORTING
        !iv_tags       TYPE string
      RETURNING
        VALUE(rt_tags) TYPE string_table.
    CLASS-METHODS normalize
      IMPORTING
        !iv_tags       TYPE string
      RETURNING
        VALUE(rv_tags) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_regex TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_TAG_UTILS IMPLEMENTATION.


  METHOD class_constructor.
    gv_regex = |^[{ c_allowed_chars }]*$|. " Must start with -
  ENDMETHOD.


  METHOD normalize.

    DATA lt_tags TYPE string_table.
    DATA lt_normalized TYPE string_table.
    FIELD-SYMBOLS <lv_tag> LIKE LINE OF lt_tags.

    lt_tags = split( iv_tags ).

    LOOP AT lt_tags ASSIGNING <lv_tag>.
      FIND REGEX gv_regex IN <lv_tag>.
      IF sy-subrc = 0.
        APPEND <lv_tag> TO lt_normalized.
      ENDIF.
    ENDLOOP.

    SORT lt_normalized.
    DELETE ADJACENT DUPLICATES FROM lt_normalized.

    rv_tags = concat_lines_of( table = lt_normalized sep = `,` ).

  ENDMETHOD.


  METHOD split.

    FIELD-SYMBOLS <lv_tag> LIKE LINE OF rt_tags.

    SPLIT iv_tags AT ',' INTO TABLE rt_tags.
    LOOP AT rt_tags ASSIGNING <lv_tag>.
      CONDENSE <lv_tag>.
    ENDLOOP.
    DELETE rt_tags WHERE table_line IS INITIAL.

  ENDMETHOD.


  METHOD validate.

    DATA lt_tags TYPE string_table.
    FIELD-SYMBOLS <lv_tag> LIKE LINE OF lt_tags.

    lt_tags = split( iv_tags ).

    LOOP AT lt_tags ASSIGNING <lv_tag>.
      FIND REGEX gv_regex IN <lv_tag>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Disallowed chars in tag #{ sy-tabix }| ).
      ENDIF.
      " TODO: maybe also limit length ?
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
