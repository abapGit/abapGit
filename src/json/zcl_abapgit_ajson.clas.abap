CLASS zcl_abapgit_ajson DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_ajson .

    ALIASES:
      is_empty FOR zif_abapgit_ajson~is_empty,
      exists FOR zif_abapgit_ajson~exists,
      members FOR zif_abapgit_ajson~members,
      get FOR zif_abapgit_ajson~get,
      get_boolean FOR zif_abapgit_ajson~get_boolean,
      get_integer FOR zif_abapgit_ajson~get_integer,
      get_number FOR zif_abapgit_ajson~get_number,
      get_date FOR zif_abapgit_ajson~get_date,
      get_timestamp FOR zif_abapgit_ajson~get_timestamp,
      get_string FOR zif_abapgit_ajson~get_string,
      slice FOR zif_abapgit_ajson~slice,
      to_abap FOR zif_abapgit_ajson~to_abap,
      array_to_string_table FOR zif_abapgit_ajson~array_to_string_table.

    ALIASES:
      clear FOR zif_abapgit_ajson~clear,
      set FOR zif_abapgit_ajson~set,
      setx FOR zif_abapgit_ajson~setx,
      set_boolean FOR zif_abapgit_ajson~set_boolean,
      set_string FOR zif_abapgit_ajson~set_string,
      set_integer FOR zif_abapgit_ajson~set_integer,
      set_date FOR zif_abapgit_ajson~set_date,
      set_timestamp FOR zif_abapgit_ajson~set_timestamp,
      set_null FOR zif_abapgit_ajson~set_null,
      delete FOR zif_abapgit_ajson~delete,
      touch_array FOR zif_abapgit_ajson~touch_array,
      push FOR zif_abapgit_ajson~push,
      stringify FOR zif_abapgit_ajson~stringify.

    ALIASES:
      clone FOR zif_abapgit_ajson~clone,
      filter FOR zif_abapgit_ajson~filter,
      map FOR zif_abapgit_ajson~map.

    ALIASES:
      mt_json_tree FOR zif_abapgit_ajson~mt_json_tree,
      keep_item_order FOR zif_abapgit_ajson~keep_item_order,
      format_datetime FOR zif_abapgit_ajson~format_datetime,
      to_abap_corresponding_only FOR zif_abapgit_ajson~to_abap_corresponding_only,
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

    CLASS-METHODS create_empty " Might be deprecated, prefer using new( ) or create object
      IMPORTING
        !ii_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
        iv_format_datetime TYPE abap_bool DEFAULT abap_true
        iv_to_abap_corresponding_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson.

    " Experimental ! May change
    CLASS-METHODS create_from " TODO, rename to 'from' ?
      IMPORTING
        !ii_source_json TYPE REF TO zif_abapgit_ajson
        !ii_filter TYPE REF TO zif_abapgit_ajson_filter OPTIONAL " Might be deprecated, use filter() instead
        !ii_mapper TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL " Might be deprecated, use map() instead
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson
      RAISING
        zcx_abapgit_ajson_error .

    METHODS constructor
      IMPORTING
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
        iv_format_datetime TYPE abap_bool DEFAULT abap_true
        iv_to_abap_corresponding_only TYPE abap_bool DEFAULT abap_false.
    CLASS-METHODS new
      IMPORTING
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
        iv_format_datetime TYPE abap_bool DEFAULT abap_true
        iv_to_abap_corresponding_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_ajson.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA go_float_regex TYPE REF TO cl_abap_regex.

    DATA ms_opts TYPE zif_abapgit_ajson=>ty_opts.
    DATA mi_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping. " DEPRECATED, will be removed

    METHODS get_item
      IMPORTING
        iv_path        TYPE string
      RETURNING
        VALUE(rv_item) TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    METHODS prove_path_exists
      IMPORTING
        iv_path              TYPE string
      RETURNING
        VALUE(rr_end_node) TYPE REF TO zif_abapgit_ajson_types=>ty_node
      RAISING
        zcx_abapgit_ajson_error.
    METHODS delete_subtree
      IMPORTING
        iv_path           TYPE string
        iv_name           TYPE string
        ir_parent         TYPE REF TO zif_abapgit_ajson_types=>ty_node OPTIONAL
      RETURNING
        VALUE(rs_top_node) TYPE zif_abapgit_ajson_types=>ty_node.
    METHODS read_only_watchdog
      RAISING
        zcx_abapgit_ajson_error.
ENDCLASS.



CLASS zcl_abapgit_ajson IMPLEMENTATION.


  METHOD constructor.
    ms_opts-keep_item_order = iv_keep_item_order.
    ms_opts-to_abap_corresponding_only = iv_to_abap_corresponding_only.
    format_datetime( iv_format_datetime ).
  ENDMETHOD.


  METHOD create_empty.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
  ENDMETHOD.


  METHOD create_from.

    DATA lo_mutator_queue TYPE REF TO lcl_mutator_queue.

    IF ii_source_json IS NOT BOUND.
      zcx_abapgit_ajson_error=>raise( 'Source not bound' ).
    ENDIF.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = ii_source_json->opts( )-to_abap_corresponding_only
        iv_format_datetime = ii_source_json->opts( )-format_datetime
        iv_keep_item_order = ii_source_json->opts( )-keep_item_order.

    IF ii_filter IS NOT BOUND AND ii_mapper IS NOT BOUND.
      ro_instance->mt_json_tree = ii_source_json->mt_json_tree.
    ELSE.
      CREATE OBJECT lo_mutator_queue.
      IF ii_mapper IS BOUND.
        " Mapping goes first. But maybe it should be a freely definable queue of processors ?
        lo_mutator_queue->add( lcl_mapper_runner=>new( ii_mapper ) ).
      ENDIF.
      IF ii_filter IS BOUND.
        lo_mutator_queue->add( lcl_filter_runner=>new( ii_filter ) ).
      ENDIF.
      lo_mutator_queue->lif_mutator_runner~run(
        EXPORTING
          it_source_tree = ii_source_json->mt_json_tree
        IMPORTING
          et_dest_tree = ro_instance->mt_json_tree ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_subtree.

    DATA lv_parent_path TYPE string.
    DATA lr_parent LIKE ir_parent.

    READ TABLE mt_json_tree INTO rs_top_node
      WITH KEY
        path = iv_path
        name = iv_name.
    IF sy-subrc <> 0.
      RETURN. " Not found ? nothing to delete !
    ENDIF.

    DELETE mt_json_tree INDEX sy-tabix. " where path = iv_path and name = iv_name.

    IF rs_top_node-children > 0. " only for objects and arrays
      lv_parent_path = iv_path && iv_name && '/*'.
      DELETE mt_json_tree WHERE path CP lv_parent_path.
    ENDIF.

    " decrement parent children
    IF ir_parent IS SUPPLIED.
      ir_parent->children = ir_parent->children - 1.
    ELSE.
      lr_parent = get_item( iv_path ).
      IF lr_parent IS NOT INITIAL.
        lr_parent->children = lr_parent->children - 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_item.

    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.
    DATA ls_path_name TYPE zif_abapgit_ajson_types=>ty_path_name.
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


  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
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
    DATA lr_node_parent LIKE rr_end_node.
    DATA lv_cur_path TYPE string.
    DATA lv_cur_name TYPE string.
    DATA ls_new_node LIKE LINE OF mt_json_tree.

    SPLIT iv_path AT '/' INTO TABLE lt_path.
    DELETE lt_path WHERE table_line IS INITIAL.

    DO.
      lr_node_parent = rr_end_node.
      READ TABLE mt_json_tree REFERENCE INTO rr_end_node
        WITH KEY
          path = lv_cur_path
          name = lv_cur_name.
      IF sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        CLEAR ls_new_node.
        IF lr_node_parent IS NOT INITIAL. " if has parent
          lr_node_parent->children = lr_node_parent->children + 1.
          IF lr_node_parent->type = zif_abapgit_ajson_types=>node_type-array.
            ls_new_node-index = lcl_utils=>validate_array_index(
              iv_path  = lv_cur_path
              iv_index = lv_cur_name ).
          ENDIF.
        ENDIF.
        ls_new_node-path = lv_cur_path.
        ls_new_node-name = lv_cur_name.
        ls_new_node-type = zif_abapgit_ajson_types=>node_type-object.
        INSERT ls_new_node INTO TABLE mt_json_tree REFERENCE INTO rr_end_node.
      ENDIF.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      READ TABLE lt_path INDEX sy-index INTO lv_cur_name.
      IF sy-subrc <> 0.
        EXIT. " no more segments
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD read_only_watchdog.
    IF ms_opts-read_only = abap_true.
      zcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_ajson~array_to_string_table.

    DATA lv_normalized_path TYPE string.
    DATA lr_node TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    IF lr_node IS INITIAL.
      zcx_abapgit_ajson_error=>raise( |Path not found: { iv_path }| ).
    ENDIF.
    IF lr_node->type <> zif_abapgit_ajson_types=>node_type-array.
      zcx_abapgit_ajson_error=>raise( |Array expected at: { iv_path }| ).
    ENDIF.

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      CASE <item>-type.
        WHEN zif_abapgit_ajson_types=>node_type-number OR zif_abapgit_ajson_types=>node_type-string.
          APPEND <item>-value TO rt_string_table.
        WHEN zif_abapgit_ajson_types=>node_type-null.
          APPEND '' TO rt_string_table.
        WHEN zif_abapgit_ajson_types=>node_type-boolean.
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


  METHOD zif_abapgit_ajson~clear.

    read_only_watchdog( ).
    CLEAR mt_json_tree.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~clone.
    ri_json = create_from( me ).
  ENDMETHOD.


  METHOD zif_abapgit_ajson~delete.

    read_only_watchdog( ).

    DATA ls_split_path TYPE zif_abapgit_ajson_types=>ty_path_name.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    ri_json = me.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~exists.
    rv_exists = boolc( get_item( iv_path ) IS NOT INITIAL ).
  ENDMETHOD.


  METHOD zif_abapgit_ajson~filter.
    ri_json = create_from(
      ii_source_json = me
      ii_filter      = ii_filter ).
  ENDMETHOD.


  METHOD zif_abapgit_ajson~format_datetime.
    ms_opts-format_datetime = iv_use_iso.
    ri_json = me.
  ENDMETHOD.


  METHOD zif_abapgit_ajson~freeze.
    ms_opts-read_only = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_ajson~get.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_boolean.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS INITIAL OR lr_item->type = zif_abapgit_ajson_types=>node_type-null.
      RETURN.
    ELSEIF lr_item->type = zif_abapgit_ajson_types=>node_type-boolean.
      rv_value = boolc( lr_item->value = 'true' ).
    ELSEIF lr_item->value IS NOT INITIAL.
      rv_value = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_date.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    lr_item = get_item( iv_path ).

    IF lr_item IS NOT INITIAL AND lr_item->type = zif_abapgit_ajson_types=>node_type-string.
      FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        IN lr_item->value
        SUBMATCHES lv_y lv_m lv_d.
      CONCATENATE lv_y lv_m lv_d INTO rv_value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_integer.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL AND lr_item->type = zif_abapgit_ajson_types=>node_type-number.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_node_type.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL.
      rv_node_type = lr_item->type.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_number.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL AND lr_item->type = zif_abapgit_ajson_types=>node_type-number.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_string.

    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL AND lr_item->type <> zif_abapgit_ajson_types=>node_type-null.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~get_timestamp.

    DATA lo_to_abap TYPE REF TO lcl_json_to_abap.
    DATA lr_item TYPE REF TO zif_abapgit_ajson_types=>ty_node.

    lr_item = get_item( iv_path ).

    IF lr_item IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_to_abap.

    TRY.
        rv_value = lo_to_abap->to_timestamp( lr_item->value ).
      CATCH zcx_abapgit_ajson_error.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~is_empty.
    rv_yes = boolc( lines( mt_json_tree ) = 0 ).
  ENDMETHOD.


  METHOD zif_abapgit_ajson~keep_item_order.
    ms_opts-keep_item_order = abap_true.
    ri_json = me.
  ENDMETHOD.


  METHOD zif_abapgit_ajson~map.
    ri_json = create_from(
      ii_source_json = me
      ii_mapper      = ii_mapper ).
  ENDMETHOD.


  METHOD zif_abapgit_ajson~members.

    DATA lv_normalized_path TYPE string.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      APPEND <item>-name TO rt_members.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~opts.
    rs_opts = ms_opts.
  ENDMETHOD.


  METHOD zif_abapgit_ajson~push.

    DATA lr_parent TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    DATA lr_new_node TYPE REF TO zif_abapgit_ajson_types=>ty_node.

    read_only_watchdog( ).

    lr_parent = get_item( iv_path ).

    IF lr_parent IS INITIAL.
      zcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] does not exist| ).
    ENDIF.

    IF lr_parent->type <> zif_abapgit_ajson_types=>node_type-array.
      zcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] is not array| ).
    ENDIF.

    DATA lt_new_nodes TYPE zif_abapgit_ajson_types=>ty_nodes_tt.
    DATA ls_new_path TYPE zif_abapgit_ajson_types=>ty_path_name.
    DATA lv_new_index TYPE i.

    lv_new_index     = lr_parent->children + 1.
    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      is_opts            = ms_opts
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    READ TABLE lt_new_nodes INDEX 1 REFERENCE INTO lr_new_node. " assume first record is the array item - not ideal !
    ASSERT sy-subrc = 0.
    lr_new_node->index = lv_new_index.

    " update data
    lr_parent->children = lv_new_index.
    INSERT LINES OF lt_new_nodes INTO TABLE mt_json_tree.

    ri_json = me.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set.

    DATA ls_split_path TYPE zif_abapgit_ajson_types=>ty_path_name.
    DATA lr_parent TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    DATA ls_deleted_node TYPE zif_abapgit_ajson_types=>ty_node.

    read_only_watchdog( ).

    ri_json = me.

    IF iv_val IS INITIAL AND iv_ignore_empty = abap_true AND iv_node_type IS INITIAL.
      RETURN. " nothing to assign
    ENDIF.

    IF iv_node_type IS NOT INITIAL
      AND iv_node_type <> zif_abapgit_ajson_types=>node_type-boolean AND iv_node_type <> zif_abapgit_ajson_types=>node_type-null
      AND iv_node_type <> zif_abapgit_ajson_types=>node_type-number AND iv_node_type <> zif_abapgit_ajson_types=>node_type-string.
      zcx_abapgit_ajson_error=>raise( |Unexpected type { iv_node_type }| ).
    ENDIF.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      IF iv_node_type IS NOT INITIAL.
        mt_json_tree = lcl_abap_to_json=>insert_with_type(
          is_opts            = ms_opts
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ELSE.
        mt_json_tree = lcl_abap_to_json=>convert(
          is_opts            = ms_opts
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ENDIF.
      RETURN.
    ENDIF.

    " Ensure whole path exists
    lr_parent = prove_path_exists( ls_split_path-path ).
    ASSERT lr_parent IS NOT INITIAL.

    " delete if exists with subtree
    ls_deleted_node = delete_subtree(
      ir_parent = lr_parent
      iv_path   = ls_split_path-path
      iv_name   = ls_split_path-name ).

    " convert to json
    DATA lt_new_nodes TYPE zif_abapgit_ajson_types=>ty_nodes_tt.
    DATA lv_array_index TYPE i.

    IF lr_parent->type = zif_abapgit_ajson_types=>node_type-array.
      lv_array_index = lcl_utils=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    ENDIF.

    IF iv_node_type IS NOT INITIAL.
      lt_new_nodes = lcl_abap_to_json=>insert_with_type(
        is_opts            = ms_opts
        iv_item_order      = ls_deleted_node-order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ELSE.
      lt_new_nodes = lcl_abap_to_json=>convert(
        is_opts            = ms_opts
        iv_item_order      = ls_deleted_node-order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ENDIF.

    " update nodes
    IF lines( lt_new_nodes ) > 0.
      lr_parent->children = lr_parent->children + 1.
      INSERT LINES OF lt_new_nodes INTO TABLE mt_json_tree.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~setx.

    DATA lv_path TYPE string.
    DATA lv_val TYPE string.
    DATA lv_int TYPE i.
    DATA lv_dec TYPE decfloat34.
    DATA lv_last TYPE i.

    IF iv_param IS INITIAL.
      ri_json = me.
      RETURN.
    ENDIF.

    SPLIT iv_param AT ':' INTO lv_path lv_val.
    CONDENSE lv_path.
    CONDENSE lv_val.

    IF lv_val IS INITIAL.
      ri_json = me.
      RETURN. " Hmm ? or empty string ? or null ?
    ENDIF.

    IF go_float_regex IS NOT BOUND.
      CREATE OBJECT go_float_regex EXPORTING pattern = '^([1-9][0-9]*|0)\.[0-9]+$'.
      " expects fractional, because ints are detected separately
    ENDIF.

    IF lv_val = 'null'.
      zif_abapgit_ajson~set_null( lv_path ).
    ELSEIF lv_val = 'true'.
      zif_abapgit_ajson~set_boolean(
        iv_path = lv_path
        iv_val  = abap_true ).
    ELSEIF lv_val = 'false'.
      zif_abapgit_ajson~set_boolean(
        iv_path = lv_path
        iv_val  = abap_false ).
    ELSEIF lv_val CO '0123456789'.
      lv_int = lv_val.
      zif_abapgit_ajson~set_integer(
        iv_path = lv_path
        iv_val  = lv_int ).
    ELSEIF lv_val CO '0123456789.' AND go_float_regex->create_matcher( text = lv_val )->match( ) = abap_true.
      lv_dec = lv_val.
      zif_abapgit_ajson~set(
        iv_path = lv_path
        iv_val  = lv_dec ).
    ELSEIF lv_val+0(1) = '{' OR lv_val+0(1) = '['.
      "Expect object/array, but no further checks, parser will catch errors
      zif_abapgit_ajson~set(
        iv_path = lv_path
        iv_val  = parse( lv_val ) ).
    ELSE. " string
      lv_last = strlen( lv_val ) - 1.
      IF lv_val+0(1) = '"' AND lv_val+lv_last(1) = '"'.
        lv_val = substring(
          val = lv_val
          off = 1
          len = lv_last - 1 ).
      ENDIF.
      zif_abapgit_ajson~set_string(
        iv_path = lv_path
        iv_val  = lv_val ).
    ENDIF.

    ri_json = me.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set_boolean.

    ri_json = me.

    DATA lv_bool TYPE abap_bool.
    lv_bool = boolc( iv_val IS NOT INITIAL ).
    zif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set_date.

    ri_json = me.

    DATA lv_val TYPE string.
    lv_val = lcl_abap_to_json=>format_date( iv_val ).

    zif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set_integer.

    ri_json = me.

    zif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set_null.

    ri_json = me.

    DATA lv_null_ref TYPE REF TO data.
    zif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set_string.

    ri_json = me.

    DATA lv_val TYPE string.
    lv_val = iv_val.
    zif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~set_timestamp.

    ri_json = me.

    DATA lv_timestamp_iso TYPE string.
    lv_timestamp_iso = lcl_abap_to_json=>format_timestamp( iv_val ).

    zif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_timestamp_iso ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~slice.

    DATA lo_section         TYPE REF TO zcl_abapgit_ajson.
    DATA ls_item            LIKE LINE OF mt_json_tree.
    DATA lv_normalized_path TYPE string.
    DATA ls_path_parts      TYPE zif_abapgit_ajson_types=>ty_path_name.
    DATA lv_path_len        TYPE i.
    DATA lv_path_pattern    TYPE string.

    CREATE OBJECT lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    READ TABLE mt_json_tree INTO ls_item
      WITH KEY path = ls_path_parts-path name = ls_path_parts-name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: ls_item-path, ls_item-name. " this becomes a new root
    INSERT ls_item INTO TABLE lo_section->mt_json_tree.

    lv_path_pattern = lv_normalized_path && `*`.

    LOOP AT mt_json_tree INTO ls_item WHERE path CP lv_path_pattern.

      ls_item-path = substring( val = ls_item-path
                                off = lv_path_len - 1 ). " less closing '/'
      INSERT ls_item INTO TABLE lo_section->mt_json_tree.

    ENDLOOP.

    ri_json = lo_section.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~stringify.

    rv_json = lcl_json_serializer=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = ms_opts-keep_item_order
      iv_indent          = iv_indent ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~touch_array.

    DATA lr_node TYPE REF TO zif_abapgit_ajson_types=>ty_node.
    DATA ls_deleted_node TYPE zif_abapgit_ajson_types=>ty_node.
    DATA ls_new_node LIKE LINE OF mt_json_tree.
    DATA ls_split_path TYPE zif_abapgit_ajson_types=>ty_path_name.

    read_only_watchdog( ).

    ls_split_path = lcl_utils=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = zif_abapgit_ajson_types=>node_type-array.
      INSERT ls_new_node INTO TABLE mt_json_tree.
      RETURN.
    ENDIF.

    IF iv_clear = abap_true.
      ls_deleted_node = delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    ELSE.
      lr_node = get_item( iv_path ).
    ENDIF.

    IF lr_node IS INITIAL. " Or node was cleared

      DATA lr_parent TYPE REF TO zif_abapgit_ajson_types=>ty_node.
      lr_parent = prove_path_exists( ls_split_path-path ).
      ASSERT lr_parent IS NOT INITIAL.

      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = zif_abapgit_ajson_types=>node_type-array.

      IF ms_opts-keep_item_order = abap_true AND ls_deleted_node IS NOT INITIAL.
        ls_new_node-order = ls_deleted_node-order.
      ENDIF.

      INSERT ls_new_node INTO TABLE mt_json_tree.

    ELSEIF lr_node->type <> zif_abapgit_ajson_types=>node_type-array.
      zcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    ENDIF.

    ri_json = me.

  ENDMETHOD.


  METHOD zif_abapgit_ajson~to_abap.

    DATA lo_to_abap TYPE REF TO lcl_json_to_abap.

    CLEAR ev_container.
    CREATE OBJECT lo_to_abap
      EXPORTING
        iv_corresponding  = boolc( iv_corresponding = abap_true OR ms_opts-to_abap_corresponding_only = abap_true )
        ii_custom_mapping = mi_custom_mapping.

    lo_to_abap->to_abap(
      EXPORTING
        it_nodes    = zif_abapgit_ajson~mt_json_tree
      CHANGING
        c_container = ev_container ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson~to_abap_corresponding_only.
    ms_opts-to_abap_corresponding_only = iv_enable.
    ri_json = me.
  ENDMETHOD.
ENDCLASS.
