CLASS zcl_abapgit_repo_labels DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_label_color,
        label TYPE string,
        color TYPE string,
      END OF ty_label_color,
      ty_label_colors TYPE STANDARD TABLE OF ty_label_color WITH KEY label.

    TYPES:
      BEGIN OF ty_color,
        cls TYPE string,
        fg TYPE string,
        bg TYPE string,
      END OF ty_color.

    CONSTANTS c_allowed_chars TYPE string VALUE `-_. a-zA-Z0-9` ##NO_TEXT.

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

    CLASS-METHODS validate_colors
      IMPORTING
        !iv_config TYPE string
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS split_colors
      IMPORTING
        !iv_config TYPE string
      RETURNING
        VALUE(rt_label_colors) TYPE ty_label_colors.
    CLASS-METHODS split_colors_into_map
      IMPORTING
        !iv_config TYPE string
      RETURNING
        VALUE(ro_map) TYPE REF TO zcl_abapgit_string_map.
    CLASS-METHODS normalize_colors
      IMPORTING
        !iv_config TYPE string
      RETURNING
        VALUE(rv_config) TYPE string.

    CLASS-METHODS parse_color
      IMPORTING
        iv_color TYPE string
      RETURNING
        VALUE(rs_parsed) TYPE ty_color.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_regex TYPE string.

    CLASS-METHODS validate_one_label_color
      IMPORTING
        !is_lc TYPE ty_label_color
        !iv_index TYPE i DEFAULT 0
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS validate_rgb_color
      IMPORTING
        !iv_color TYPE string
        !iv_index TYPE i DEFAULT 0
      RAISING
        zcx_abapgit_exception.

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

    rv_labels = concat_lines_of(
      table = lt_normalized
      sep = `, ` ).

  ENDMETHOD.


  METHOD normalize_colors.

    DATA lt_colors TYPE ty_label_colors.
    DATA lt_normalized TYPE ty_label_colors.
    DATA lt_pairs TYPE string_table.
    DATA lv_pair TYPE string.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    LOOP AT lt_colors ASSIGNING <ls_c>.
      TRY.
          validate_one_label_color( <ls_c> ).
          APPEND <ls_c> TO lt_normalized.
        CATCH zcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    SORT lt_normalized BY label.
    DELETE ADJACENT DUPLICATES FROM lt_normalized COMPARING label.

    LOOP AT lt_normalized ASSIGNING <ls_c>.
      lv_pair = <ls_c>-label && `:` && <ls_c>-color.
      APPEND lv_pair TO lt_pairs.
    ENDLOOP.

    rv_config = concat_lines_of(
      table = lt_pairs
      sep = `, ` ).

  ENDMETHOD.


  METHOD parse_color.

    DATA lv_tmp TYPE string.

    IF iv_color IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_color+0(1) = '#'.
      lv_tmp  = iv_color+1.
      SPLIT lv_tmp AT '/' INTO rs_parsed-fg rs_parsed-bg.
    ELSE.
      rs_parsed-cls = iv_color.
    ENDIF.

  ENDMETHOD.


  METHOD split.

    FIELD-SYMBOLS <lv_lab> LIKE LINE OF rt_labels.

    SPLIT iv_labels AT ',' INTO TABLE rt_labels.
    LOOP AT rt_labels ASSIGNING <lv_lab>.
      CONDENSE <lv_lab>.
    ENDLOOP.
    DELETE rt_labels WHERE table_line IS INITIAL.

  ENDMETHOD.


  METHOD split_colors.

    DATA lt_pairs TYPE string_table.
    DATA lv_clean_config LIKE iv_config.
    DATA ls_c LIKE LINE OF rt_label_colors.
    FIELD-SYMBOLS <lv_pair> LIKE LINE OF lt_pairs.

    lv_clean_config = replace(
      val = iv_config
      sub = cl_abap_char_utilities=>newline
      with = ` ` ). " text area ends with LF

    SPLIT lv_clean_config AT ',' INTO TABLE lt_pairs.
    LOOP AT lt_pairs ASSIGNING <lv_pair>.
      CONDENSE <lv_pair>.
      IF <lv_pair> IS NOT INITIAL.
        SPLIT <lv_pair> AT ':' INTO ls_c-label ls_c-color.
        CONDENSE ls_c-label.
        CONDENSE ls_c-color.
        APPEND ls_c TO rt_label_colors.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD split_colors_into_map.

    DATA lt_colors TYPE ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    ro_map = zcl_abapgit_string_map=>create( ).
    LOOP AT lt_colors ASSIGNING <ls_c>.
      TRY.
          ro_map->set(
            iv_key = <ls_c>-label
            iv_val = <ls_c>-color ).
        CATCH zcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate.

    DATA lt_labels TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Disallowed chars in label #{ sy-tabix }| ).
      ENDIF.
      " TODO: maybe also limit length ?
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_colors.

    DATA lt_colors TYPE ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    LOOP AT lt_colors ASSIGNING <ls_c>.
      validate_one_label_color(
        is_lc    = <ls_c>
        iv_index = sy-tabix ).
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_one_label_color.

    DATA ls_parsed_color TYPE ty_color.

    IF is_lc-label IS INITIAL.
      zcx_abapgit_exception=>raise( |Label is empty in pair #{ iv_index }| ).
    ENDIF.

    IF is_lc-color IS INITIAL.
      zcx_abapgit_exception=>raise( |Color is empty in pair #{ iv_index }| ).
    ENDIF.

    FIND REGEX gv_regex IN is_lc-label.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Disallowed chars in label in pair #{ iv_index }| ).
    ENDIF.

    ls_parsed_color = parse_color( is_lc-color ).
    IF ls_parsed_color-cls IS NOT INITIAL.
      FIND REGEX '^[-_A-Za-z]+$' IN ls_parsed_color-cls.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Disallowed chars in color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.
    IF ls_parsed_color-fg IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-fg ).
    ENDIF.
    IF ls_parsed_color-bg IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-bg ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_rgb_color.

    DATA lv_len TYPE i.

    IF iv_color IS NOT INITIAL.
      FIND REGEX '^[0-9A-Fa-f]+$' IN iv_color.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Disallowed chars in color in pair #{ iv_index }| ).
      ENDIF.
      lv_len = strlen( iv_color ).
      IF NOT ( lv_len = 3 OR lv_len = 6 ).
        zcx_abapgit_exception=>raise( |Icorrect color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
