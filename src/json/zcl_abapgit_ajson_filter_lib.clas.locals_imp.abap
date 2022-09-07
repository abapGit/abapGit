**********************************************************************
*  FILTER EMPTY VALUES
**********************************************************************

CLASS lcl_empty_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_filter.
ENDCLASS.

CLASS lcl_empty_filter IMPLEMENTATION.
  METHOD zif_abapgit_ajson_filter~keep_node.

    rv_keep = boolc(
      ( iv_visit = zif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
      ( iv_visit <> zif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
    " children = 0 on open for initially empty nodes and on close for filtered ones

  ENDMETHOD.
ENDCLASS.

**********************************************************************
*  FILTER PREDEFINED PATHS
**********************************************************************

CLASS lcl_paths_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        it_skip_paths TYPE string_table OPTIONAL
        iv_skip_paths TYPE string OPTIONAL
        iv_pattern_search TYPE abap_bool
      RAISING
        zcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_skip_paths TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA mv_pattern_search TYPE abap_bool.
ENDCLASS.

CLASS lcl_paths_filter IMPLEMENTATION.

  METHOD zif_abapgit_ajson_filter~keep_node.

    DATA lv_full_path TYPE string.
    FIELD-SYMBOLS <p> LIKE LINE OF mt_skip_paths.

    lv_full_path = is_node-path && is_node-name.

    IF mv_pattern_search = abap_true.
      rv_keep = abap_true.
      LOOP AT mt_skip_paths ASSIGNING <p>.
        IF lv_full_path CP <p>.
          rv_keep = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY table_line = lv_full_path TRANSPORTING NO FIELDS.
      rv_keep = boolc( sy-subrc <> 0 ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    DATA lv_s TYPE string.
    DATA lt_tab TYPE string_table.
    FIELD-SYMBOLS <s> TYPE string.

    IF boolc( iv_skip_paths IS INITIAL ) = boolc( it_skip_paths IS INITIAL ). " XOR
      zcx_abapgit_ajson_error=>raise( 'no filter path specified' ).
    ENDIF.

    LOOP AT it_skip_paths INTO lv_s.
      lv_s = to_lower( lv_s ).
      APPEND lv_s TO lt_tab.
    ENDLOOP.

    IF iv_skip_paths IS NOT INITIAL.
      SPLIT iv_skip_paths AT ',' INTO TABLE lt_tab.
      LOOP AT lt_tab ASSIGNING <s>.
        IF <s> IS INITIAL.
          DELETE lt_tab INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        <s> = condense( to_lower( <s> ) ).
      ENDLOOP.
    ENDIF.

    SORT lt_tab BY table_line.
    DELETE ADJACENT DUPLICATES FROM lt_tab.

    mt_skip_paths = lt_tab.
    mv_pattern_search = iv_pattern_search.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* MULTI FILTER
**********************************************************************

CLASS lcl_and_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        it_filters TYPE zif_abapgit_ajson_filter=>ty_filter_tab
      RAISING
        zcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_filters TYPE zif_abapgit_ajson_filter=>ty_filter_tab.
ENDCLASS.

CLASS lcl_and_filter IMPLEMENTATION.

  METHOD zif_abapgit_ajson_filter~keep_node.

    DATA li_filter LIKE LINE OF mt_filters.

    rv_keep = abap_true.
    LOOP AT mt_filters INTO li_filter.
      rv_keep = li_filter->keep_node(
        is_node  = is_node
        iv_visit = iv_visit ).
      IF rv_keep = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    DATA li_filter LIKE LINE OF it_filters.

    LOOP AT it_filters INTO li_filter WHERE table_line IS BOUND.
      APPEND li_filter TO mt_filters.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
