CLASS zcl_abapgit_ajson DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_ajson .

    ALIASES:
      exists FOR zif_abapgit_ajson_reader~exists,
      members FOR zif_abapgit_ajson_reader~members,
      get FOR zif_abapgit_ajson_reader~get,
      get_boolean FOR zif_abapgit_ajson_reader~get_boolean,
      get_integer FOR zif_abapgit_ajson_reader~get_integer,
      get_number FOR zif_abapgit_ajson_reader~get_number,
      get_date FOR zif_abapgit_ajson_reader~get_date,
      get_timestamp FOR zif_abapgit_ajson_reader~get_timestamp,
      get_string FOR zif_abapgit_ajson_reader~get_string,
      slice FOR zif_abapgit_ajson_reader~slice,
      to_abap FOR zif_abapgit_ajson_reader~to_abap,
      array_to_string_table FOR zif_abapgit_ajson_reader~array_to_string_table.

    ALIASES:
      clear FOR zif_abapgit_ajson_writer~clear,
      set FOR zif_abapgit_ajson_writer~set,
      set_boolean FOR zif_abapgit_ajson_writer~set_boolean,
      set_string FOR zif_abapgit_ajson_writer~set_string,
      set_integer FOR zif_abapgit_ajson_writer~set_integer,
      set_date FOR zif_abapgit_ajson_writer~set_date,
      set_timestamp FOR zif_abapgit_ajson_writer~set_timestamp,
      set_null FOR zif_abapgit_ajson_writer~set_null,
      delete FOR zif_abapgit_ajson_writer~delete,
      touch_array FOR zif_abapgit_ajson_writer~touch_array,
      push FOR zif_abapgit_ajson_writer~push,
      stringify FOR zif_abapgit_ajson_writer~stringify.

    ALIASES:
      mt_json_tree FOR zif_abapgit_ajson~mt_json_tree,
      keep_item_order FOR zif_abapgit_ajson~keep_item_order,
      freeze FOR zif_abapgit_ajson~freeze.

    CLASS-METHODS parse
      IMPORTING
        !iv_json           TYPE string
        !iv_freeze         TYPE abap_bool DEFAULT abap_false
        !ii_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .

    CLASS-METHODS create_empty
      IMPORTING
        !ii_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      tty_node_stack TYPE STANDARD TABLE OF REF TO zif_abapgit_ajson=>ty_node WITH DEFAULT KEY.

    DATA mv_read_only TYPE abap_bool.
    DATA mi_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA mv_keep_item_order TYPE abap_bool.

    METHODS get_item
      IMPORTING
        iv_path        TYPE string
      RETURNING
        VALUE(rv_item) TYPE REF TO zif_abapgit_ajson=>ty_node.
    METHODS prove_path_exists
      IMPORTING
        iv_path              TYPE string
      RETURNING
        VALUE(rt_node_stack) TYPE tty_node_stack
      RAISING
        zcx_abapgit_ajson_error.
    METHODS delete_subtree
      IMPORTING
        iv_path           TYPE string
        iv_name           TYPE string
      RETURNING
        VALUE(rv_deleted) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_ajson IMPLEMENTATION.


  METHOD create_empty.
    CREATE OBJECT ro_instance.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
  ENDMETHOD.


  METHOD delete_subtree.

    DATA lv_parent_path TYPE string.
    DATA lv_parent_path_len TYPE i.
    FIELD-SYMBOLS <node> LIKE LINE OF mt_json_tree.
    READ TABLE mt_json_tree ASSIGNING <node>
      WITH KEY
        path = iv_path
        name = iv_name.
    IF sy-subrc = 0. " Found ? delete !
      IF <node>-children > 0. " only for objects and arrays
        lv_parent_path = iv_path && iv_name && '/'.
        lv_parent_path_len = strlen( lv_parent_path ).
        LOOP AT mt_json_tree ASSIGNING <node>.
          IF strlen( <node>-path ) >= lv_parent_path_len
            AND substring( val = <node>-path
                           len = lv_parent_path_len ) = lv_parent_path.
            DELETE mt_json_tree INDEX sy-tabix.
          ENDIF.
        ENDLOOP.
      ENDIF.

      DELETE mt_json_tree WHERE path = iv_path AND name = iv_name.
      rv_deleted = abap_true.

      DATA ls_path TYPE zif_abapgit_ajson=>ty_path_name.
      ls_path = lcl_utils=>split_path( iv_path ).
      READ TABLE mt_json_tree ASSIGNING <node>
        WITH KEY
          path = ls_path-path
          name = ls_path-name.
      IF sy-subrc = 0.
        <node>-children = <node>-children - 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_item.

    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.
    DATA ls_path_name TYPE zif_abapgit_ajson=>ty_path_name.
    ls_path_name = lcl_utils=>split_path( iv_path ).

    READ TABLE mt_json_tree
      ASSIGNING <item>
      WITH KEY
        path = ls_path_name-path
        name = ls_path_name-name.
    IF sy-subrc = 0.
      GET REFERENCE OF <item> INTO rv_item.
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    DATA lo_parser TYPE REF TO lcl_json_parser.

    CREATE OBJECT ro_instance.
    CREATE OBJECT lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).
    ro_instance->mi_custom_mapping = ii_custom_mapping.

    IF iv_freeze = abap_true.
      ro_instance->freeze( ).
    ENDIF.

  ENDMETHOD.


  METHOD prove_path_exists.

    DATA lt_path TYPE string_table.
    DATA lr_node LIKE LINE OF rt_node_stack.
    DATA lr_node_parent LIKE LINE OF rt_node_stack.
    DATA lv_cur_path TYPE string.
    DATA lv_cur_name TYPE string.
    DATA ls_new_node LIKE LINE OF mt_json_tree.

    SPLIT iv_path AT '/' INTO TABLE lt_path.
    DELETE lt_path WHERE table_line IS INITIAL.

    DO.
      lr_node_parent = lr_node.
      READ TABLE mt_json_tree REFERENCE INTO lr_node
        WITH KEY
          path = lv_cur_path
          name = lv_cur_name.
      IF sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        CLEAR ls_new_node.
        IF lr_node_parent IS NOT INITIAL. " if has parent
          lr_node_parent->children = lr_node_parent->children + 1.
          IF lr_node_parent->type = zif_abapgit_ajson=>node_type-array.
            ls_new_node-index = lcl_utils=>validate_array_index(
              iv_path  = lv_cur_path
              iv_index = lv_cur_name ).
          ENDIF.
        ENDIF.
        ls_new_node-path = lv_cur_path.
        ls_new_node-name = lv_cur_name.
        ls_new_node-type = zif_abapgit_ajson=>node_type-object.
        INSERT ls_new_node INTO TABLE mt_json_tree REFERENCE INTO lr_node.
      ENDIF.
      INSERT lr_node INTO rt_node_stack INDEX 1.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      READ TABLE lt_path INDEX sy-index INTO lv_cur_name.
      IF sy-subrc <> 0.
        EXIT. " no more segments
      ENDIF.
    ENDDO.

    ASSERT lv_cur_path = iv_path. " Just in case

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~array_to_string_table.

    DATA lv_normalized_path TYPE string.
    DATA lr_node TYPE REF TO zif_abapgit_ajson=>ty_node.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    IF lr_node IS INITIAL.
      zcx_abapgit_ajson_error=>raise( |Path not found: { iv_path }| ).
    ENDIF.
    IF lr_node->type <> zif_abapgit_ajson=>node_type-array.
      zcx_abapgit_ajson_error=>raise( |Array expected at: { iv_path }| ).
    ENDIF.

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      CASE <item>-type.
        WHEN zif_abapgit_ajson=>node_type-number OR zif_abapgit_ajson=>node_type-string.
          APPEND <item>-value TO rt_string_table.
        WHEN zif_abapgit_ajson=>node_type-null.
          APPEND '' TO rt_string_table.
        WHEN zif_abapgit_ajson=>node_type-boolean.
          DATA lv_tmp TYPE string.
          IF <item>-value = 'true'.
            lv_tmp = abap_true.
          ELSE.
            CLEAR lv_tmp.
          ENDIF.
          APPEND lv_tmp TO rt_string_table.
        WHEN OTHERS.
          zcx_abapgit_ajson_error=>raise( |Cannot convert [{ <item>-type
            }] to string at [{ <item>-path }{ <item>-name }]| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~exists.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_boolean.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS INITIAL OR lv_item->type = zif_abapgit_ajson=>node_type-null.
      RETURN.
    ELSEIF lv_item->type = zif_abapgit_ajson=>node_type-boolean.
      rv_value = boolc( lv_item->value = 'true' ).
    ELSEIF lv_item->value IS NOT INITIAL.
      rv_value = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_date.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    lv_item = get_item( iv_path ).

    IF lv_item IS NOT INITIAL AND lv_item->type = zif_abapgit_ajson=>node_type-string.
      FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        IN lv_item->value
        SUBMATCHES lv_y lv_m lv_d.
      CONCATENATE lv_y lv_m lv_d INTO rv_value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_integer.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type = zif_abapgit_ajson=>node_type-number.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_node_type.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_node_type = lv_item->type.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_number.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type = zif_abapgit_ajson=>node_type-number.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_string.

    DATA lv_item TYPE REF TO zif_abapgit_ajson=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type <> zif_abapgit_ajson=>node_type-null.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~get_timestamp.

    DATA lo_to_abap TYPE REF TO lcl_json_to_abap.
    DATA lr_item TYPE REF TO zif_abapgit_ajson=>ty_node.

    lr_item = get_item( iv_path ).

    IF lr_item IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_to_abap.

    TRY.
        rv_value = lo_to_abap->to_timestamp( is_path = lr_item->* ).
      CATCH zcx_abapgit_ajson_error.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~members.

    DATA lv_normalized_path TYPE string.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      APPEND <item>-name TO rt_members.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_reader~slice.

    DATA lo_section         TYPE REF TO zcl_abapgit_ajson.
    DATA ls_item            LIKE LINE OF mt_json_tree.
    DATA lv_normalized_path TYPE string.
    DATA ls_path_parts      TYPE zif_abapgit_ajson=>ty_path_name.
    DATA lv_path_len        TYPE i.

    CREATE OBJECT lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    LOOP AT mt_json_tree INTO ls_item.
      " TODO potentially improve performance due to sorted tree (all path started from same prefix go in a row)
      IF strlen( ls_item-path ) >= lv_path_len
          AND substring( val = ls_item-path
                         len = lv_path_len ) = lv_normalized_path.
        ls_item-path = substring( val = ls_item-path
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
      EXPORTING
        ii_custom_mapping = mi_custom_mapping
      CHANGING
        c_obj             = ev_container
        co_instance       = lo_to_abap ).
    lo_to_abap->to_abap( mt_json_tree ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~clear.

    IF mv_read_only = abap_true.
      zcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.

    CLEAR mt_json_tree.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~delete.

    IF mv_read_only = abap_true.
      zcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.

    DATA ls_split_path TYPE zif_abapgit_ajson=>ty_path_name.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~push.

    DATA lr_parent TYPE REF TO zif_abapgit_ajson=>ty_node.
    DATA lr_new_node TYPE REF TO zif_abapgit_ajson=>ty_node.

    IF mv_read_only = abap_true.
      zcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.

    lr_parent = get_item( iv_path ).

    IF lr_parent IS INITIAL.
      zcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] does not exist| ).
    ENDIF.

    IF lr_parent->type <> zif_abapgit_ajson=>node_type-array.
      zcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] is not array| ).
    ENDIF.

    DATA lt_new_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    DATA ls_new_path TYPE zif_abapgit_ajson=>ty_path_name.

    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ lr_parent->children + 1 }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      iv_keep_item_order = mv_keep_item_order
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    READ TABLE lt_new_nodes INDEX 1 REFERENCE INTO lr_new_node. " assume first record is the array item - not ideal !
    ASSERT sy-subrc = 0.
    lr_new_node->index = lr_parent->children + 1.

    " update data
    lr_parent->children = lr_parent->children + 1.
    INSERT LINES OF lt_new_nodes INTO TABLE mt_json_tree.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set.

    DATA ls_split_path TYPE zif_abapgit_ajson=>ty_path_name.
    DATA lr_parent TYPE REF TO zif_abapgit_ajson=>ty_node.
    DATA lt_node_stack TYPE tty_node_stack.

    IF mv_read_only = abap_true.
      zcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.

    IF iv_val IS INITIAL AND iv_ignore_empty = abap_true AND iv_node_type IS INITIAL.
      RETURN. " nothing to assign
    ENDIF.

    IF iv_node_type IS NOT INITIAL
      AND iv_node_type <> zif_abapgit_ajson=>node_type-boolean AND iv_node_type <> zif_abapgit_ajson=>node_type-null
      AND iv_node_type <> zif_abapgit_ajson=>node_type-number AND iv_node_type <> zif_abapgit_ajson=>node_type-string.
      zcx_abapgit_ajson_error=>raise( |Unexpected type { iv_node_type }| ).
    ENDIF.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      IF iv_node_type IS NOT INITIAL.
        mt_json_tree = lcl_abap_to_json=>insert_with_type(
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ELSE.
        mt_json_tree = lcl_abap_to_json=>convert(
          iv_keep_item_order = mv_keep_item_order
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ENDIF.
      RETURN.
    ENDIF.

    " Ensure whole path exists
    lt_node_stack = prove_path_exists( ls_split_path-path ).
    READ TABLE lt_node_stack INDEX 1 INTO lr_parent.
    ASSERT sy-subrc = 0.

    " delete if exists with subtree
    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    " convert to json
    DATA lt_new_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    DATA lv_array_index TYPE i.

    IF lr_parent->type = zif_abapgit_ajson=>node_type-array.
      lv_array_index = lcl_utils=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    ENDIF.

    IF iv_node_type IS NOT INITIAL.
      lt_new_nodes = lcl_abap_to_json=>insert_with_type(
        iv_keep_item_order = mv_keep_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ELSE.
      lt_new_nodes = lcl_abap_to_json=>convert(
        iv_keep_item_order = mv_keep_item_order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ENDIF.

    " update data
    lr_parent->children = lr_parent->children + 1.
    INSERT LINES OF lt_new_nodes INTO TABLE mt_json_tree.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set_boolean.

    DATA lv_bool TYPE abap_bool.
    lv_bool = boolc( iv_val IS NOT INITIAL ).
    zif_abapgit_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set_date.

    DATA lv_val TYPE string.

    IF iv_val IS NOT INITIAL.
      lv_val = iv_val+0(4) && '-' && iv_val+4(2) && '-' && iv_val+6(2).
    ENDIF.

    zif_abapgit_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set_integer.

    zif_abapgit_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set_null.

    DATA lv_null_ref TYPE REF TO data.
    zif_abapgit_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set_string.

    DATA lv_val TYPE string.
    lv_val = iv_val.
    zif_abapgit_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~set_timestamp.

    DATA:
      lv_tz            TYPE tznzone,
      lv_date          TYPE d,
      lv_time          TYPE t,
      lv_timestamp_iso TYPE string.

    IF iv_val IS INITIAL.
      " The zero value is January 1, year 1, 00:00:00.000000000 UTC.
      lv_date = '00010101'.
    ELSE.

      lv_tz = 'UTC'.
      CONVERT TIME STAMP iv_val TIME ZONE lv_tz
        INTO DATE lv_date TIME lv_time.

    ENDIF.

    lv_timestamp_iso =
        lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) &&
        'T' &&
        lv_time+0(2) && '-' && lv_time+2(2) && '-' && lv_time+4(2) &&
        'Z'.

    zif_abapgit_ajson_writer~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_timestamp_iso ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~stringify.

    rv_json = lcl_json_serializer=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = mv_keep_item_order
      iv_indent          = iv_indent ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_writer~touch_array.

    DATA lr_node TYPE REF TO zif_abapgit_ajson=>ty_node.
    DATA ls_new_node LIKE LINE OF mt_json_tree.
    DATA ls_split_path TYPE zif_abapgit_ajson=>ty_path_name.

    IF mv_read_only = abap_true.
      zcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = 'array'.
      INSERT ls_new_node INTO TABLE mt_json_tree.
      RETURN.
    ENDIF.

    IF iv_clear = abap_true.
      delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    ELSE.
      lr_node = get_item( iv_path ).
    ENDIF.

    IF lr_node IS INITIAL. " Or node was cleared

      DATA lr_parent TYPE REF TO zif_abapgit_ajson=>ty_node.
      DATA lt_node_stack TYPE tty_node_stack.

      lt_node_stack = prove_path_exists( ls_split_path-path ).
      READ TABLE lt_node_stack INDEX 1 INTO lr_parent.
      ASSERT sy-subrc = 0.
      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = zif_abapgit_ajson=>node_type-array.
      INSERT ls_new_node INTO TABLE mt_json_tree.

    ELSEIF lr_node->type <> zif_abapgit_ajson=>node_type-array.
      zcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~freeze.
    mv_read_only = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_ajson~keep_item_order.
    mv_keep_item_order = abap_true.
  ENDMETHOD.
ENDCLASS.
