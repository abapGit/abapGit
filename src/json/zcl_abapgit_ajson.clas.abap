CLASS zcl_abapgit_ajson DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS ported_from_url TYPE string VALUE 'https://github.com/sbcgua/ajson'.

    INTERFACES zif_abapgit_ajson_reader .

    TYPES:
      BEGIN OF ty_node,
        path     TYPE string,
        name     TYPE string,
        type     TYPE string,
        value    TYPE string,
        index    TYPE i,
        children TYPE i,
      END OF ty_node .
    TYPES:
      ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH KEY path name .
    TYPES:
      ty_nodes_ts TYPE SORTED TABLE OF ty_node
        WITH UNIQUE KEY path name
        WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index .
    TYPES:
      BEGIN OF ty_path_name,
        path TYPE string,
        name TYPE string,
      END OF ty_path_name.

    CLASS-METHODS parse
      IMPORTING
        !iv_json           TYPE string
        !iv_freeze         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .

    METHODS freeze.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_json_tree TYPE ty_nodes_ts.
    DATA mv_read_only TYPE abap_bool.

    METHODS get_item
      IMPORTING
        iv_path        TYPE string
      RETURNING
        VALUE(rv_item) TYPE REF TO ty_node.

ENDCLASS.



CLASS ZCL_ABAPGIT_AJSON IMPLEMENTATION.


  METHOD freeze.
    mv_read_only = abap_true.
  ENDMETHOD.


  METHOD get_item.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_json_tree.
    DATA ls_path_name TYPE ty_path_name.
    ls_path_name = lcl_utils=>split_path( iv_path ).

    READ TABLE mt_json_tree
      ASSIGNING <ls_item>
      WITH KEY
        path = ls_path_name-path
        name = ls_path_name-name.
    IF sy-subrc = 0.
      GET REFERENCE OF <ls_item> INTO rv_item.
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    DATA lo_parser TYPE REF TO lcl_json_parser.

    CREATE OBJECT ro_instance.
    CREATE OBJECT lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).

    IF iv_freeze = abap_true.
      ro_instance->freeze( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~array_to_string_table.

    DATA lv_normalized_path TYPE string.
    DATA lr_node TYPE REF TO ty_node.
    DATA lv_tmp TYPE string.
    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    IF lr_node IS INITIAL.
      zcx_abapgit_ajson_error=>raise_json( |Path not found: { iv_path }| ).
    ENDIF.
    IF lr_node->type <> 'array'.
      zcx_abapgit_ajson_error=>raise_json( |Array expected at: { iv_path }| ).
    ENDIF.

    LOOP AT mt_json_tree ASSIGNING <ls_item> WHERE path = lv_normalized_path.
      CASE <ls_item>-type.
        WHEN 'num' OR 'str'.
          APPEND <ls_item>-value TO rt_string_table.
        WHEN 'null'.
          APPEND '' TO rt_string_table.
        WHEN 'bool'.
          IF <ls_item>-value = 'true'.
            lv_tmp = abap_true.
          ELSE.
            CLEAR lv_tmp.
          ENDIF.
          APPEND lv_tmp TO rt_string_table.
        WHEN OTHERS.
          zcx_abapgit_ajson_error=>raise_json(
            |Cannot convert [{ <ls_item>-type }] to string at [{ <ls_item>-path }{ <ls_item>-name }]| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~exists.

    DATA lv_item TYPE REF TO ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get.

    DATA lv_item TYPE REF TO ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_boolean.

    DATA lv_item TYPE REF TO ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS INITIAL OR lv_item->type = 'null'.
      RETURN.
    ELSEIF lv_item->type = 'bool'.
      rv_value = boolc( lv_item->value = 'true' ).
    ELSEIF lv_item->value IS NOT INITIAL.
      rv_value = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_date.

    DATA lv_item TYPE REF TO ty_node.
    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    lv_item = get_item( iv_path ).

    IF lv_item IS NOT INITIAL AND lv_item->type = 'str'.
      FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        IN lv_item->value
        SUBMATCHES lv_y lv_m lv_d.
      CONCATENATE lv_y lv_m lv_d INTO rv_value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_integer.

    DATA lv_item TYPE REF TO ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type = 'num'.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_number.

    DATA lv_item TYPE REF TO ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type = 'num'.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_string.

    DATA lv_item TYPE REF TO ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type <> 'null'.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~members.

    DATA lv_normalized_path TYPE string.
    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    LOOP AT mt_json_tree ASSIGNING <ls_item> WHERE path = lv_normalized_path.
      APPEND <ls_item>-name TO rt_members.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~slice.

    DATA lo_section         TYPE REF TO zcl_abapgit_ajson.
    DATA ls_item            LIKE LINE OF mt_json_tree.
    DATA lv_normalized_path TYPE string.
    DATA ls_path_parts      TYPE ty_path_name.
    DATA lv_path_len        TYPE i.

    CREATE OBJECT lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    LOOP AT mt_json_tree INTO ls_item.
      " TODO potentially improve performance due to sorted tree (all path started from same prefix go in a row)
      IF strlen( ls_item-path ) >= lv_path_len
          AND substring(
            val = ls_item-path
            len = lv_path_len ) = lv_normalized_path.
        ls_item-path = substring(
          val = ls_item-path
          off = lv_path_len - 1 ). " less closing '/'
        INSERT ls_item INTO TABLE lo_section->mt_json_tree.
      ELSEIF ls_item-path = ls_path_parts-path AND ls_item-name = ls_path_parts-name.
        CLEAR: ls_item-path, ls_item-name. " this becomes a new root
        INSERT ls_item INTO TABLE lo_section->mt_json_tree.
      ENDIF.
    ENDLOOP.

    ri_json = lo_section.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~to_abap.

    DATA lo_to_abap TYPE REF TO lcl_json_to_abap.

    CLEAR ev_container.
    lcl_json_to_abap=>bind(
      CHANGING
        cv_obj = ev_container
        co_instance = lo_to_abap ).
    lo_to_abap->to_abap( mt_json_tree ).

  ENDMETHOD.
ENDCLASS.
