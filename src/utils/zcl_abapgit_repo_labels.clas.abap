CLASS zcl_abapgit_repo_labels DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_allowed_chars TYPE string VALUE `-_.a-zA-Z0-9` ##NO_TEXT.

    " it is easier to allow chars, though potentially other chars can be added later if needed
    CLASS-METHODS class_constructor.
    CLASS-METHODS validate
      IMPORTING
        !iv_labels TYPE string
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS split
      IMPORTING
        !iv_labels TYPE string
      RETURNING
        VALUE(rt_labels) TYPE string_table.
    CLASS-METHODS normalize
      IMPORTING
        !iv_labels TYPE string
      RETURNING
        VALUE(rv_labels) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_regex TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_LABELS IMPLEMENTATION.


  METHOD class_constructor.
    gv_regex = |^[{ c_allowed_chars }]*$|. " Must start with -
  ENDMETHOD.


  METHOD normalize.

    DATA lt_labels TYPE string_table.
    DATA lt_normalized TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc = 0.
        APPEND <lv_lab> TO lt_normalized.
      ENDIF.
    ENDLOOP.

    SORT lt_normalized.
    DELETE ADJACENT DUPLICATES FROM lt_normalized.

    rv_labels = concat_lines_of( table = lt_normalized sep = `,` ).

  ENDMETHOD.


  METHOD split.

    FIELD-SYMBOLS <lv_lab> LIKE LINE OF rt_labels.

    SPLIT iv_labels AT ',' INTO TABLE rt_labels.
    LOOP AT rt_labels ASSIGNING <lv_lab>.
      CONDENSE <lv_lab>.
    ENDLOOP.
    DELETE rt_labels WHERE table_line IS INITIAL.

  ENDMETHOD.


  METHOD validate.

    DATA lt_labels TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Disallowed chars in tag #{ sy-tabix }| ).
      ENDIF.
      " TODO: maybe also limit length ?
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
