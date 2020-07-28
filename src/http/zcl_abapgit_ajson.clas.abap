CLASS zcl_abapgit_ajson DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    constants ported_from_url type string value 'https://github.com/sbcgua/ajson'.

    interfaces zif_abapgit_ajson_reader .

    types:
      begin of ty_node,
        path type string,
        name type string,
        type type string,
        value type string,
        index type i,
        children type i,
      end of ty_node .
    types:
      ty_nodes_tt type standard table of ty_node with key path name .
    types:
      ty_nodes_ts type sorted table of ty_node
        with unique key path name
        with non-unique sorted key array_index components path index .
    types:
      begin of ty_path_name,
        path type string,
        name type string,
      end of ty_path_name.

    class-methods parse
      importing
        !iv_json type string
        !iv_freeze type abap_bool default abap_false
      returning
        value(ro_instance) type ref to zcl_abapgit_ajson
      raising
        zcx_abapgit_ajson_error .

    methods freeze.

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      tty_node_stack type standard table of ref to ty_node with default key.

    data mt_json_tree type ty_nodes_ts.
    data mv_read_only type abap_bool.

    methods get_item
      importing
        iv_path type string
      returning
        value(rv_item) type ref to ty_node.

ENDCLASS.



CLASS ZCL_ABAPGIT_AJSON IMPLEMENTATION.


  method freeze.
    mv_read_only = abap_true.
  endmethod.


  method get_item.

    field-symbols <item> like line of mt_json_tree.
    data ls_path_name type ty_path_name.
    ls_path_name = lcl_utils=>split_path( iv_path ).

    read table mt_json_tree
      assigning <item>
      with key
        path = ls_path_name-path
        name = ls_path_name-name.
    if sy-subrc = 0.
      get reference of <item> into rv_item.
    endif.

  endmethod.


  method parse.

    data lo_parser type ref to lcl_json_parser.

    create object ro_instance.
    create object lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).

    if iv_freeze = abap_true.
      ro_instance->freeze( ).
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~array_to_string_table.

    data lv_normalized_path type string.
    data lr_node type ref to ty_node.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    if lr_node is initial.
      zcx_abapgit_ajson_error=>raise_json( |Path not found: { iv_path }| ).
    endif.
    if lr_node->type <> 'array'.
      zcx_abapgit_ajson_error=>raise_json( |Array expected at: { iv_path }| ).
    endif.

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      case <item>-type.
        when 'num' or 'str'.
          append <item>-value to rt_string_table.
        when 'null'.
          append '' to rt_string_table.
        when 'bool'.
          data lv_tmp type string.
          if <item>-value = 'true'.
            lv_tmp = abap_true.
          else.
            clear lv_tmp.
          endif.
          append lv_tmp to rt_string_table.
        when others.
          zcx_abapgit_ajson_error=>raise_json( |Cannot convert [{ <item>-type }] to string at [{ <item>-path }{ <item>-name }]| ).
      endcase.
    endloop.

  endmethod.


  method zif_abapgit_ajson_reader~exists.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_exists = abap_true.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~get.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~get_boolean.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is initial or lv_item->type = 'null'.
      return.
    elseif lv_item->type = 'bool'.
      rv_value = boolc( lv_item->value = 'true' ).
    elseif lv_item->value is not initial.
      rv_value = abap_true.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~get_date.

    data lv_item type ref to ty_node.
    data lv_y type c length 4.
    data lv_m type c length 2.
    data lv_d type c length 2.

    lv_item = get_item( iv_path ).

    if lv_item is not initial and lv_item->type = 'str'.
      find first occurrence of regex '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        in lv_item->value
        submatches lv_y lv_m lv_d.
      concatenate lv_y lv_m lv_d into rv_value.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~get_integer.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = 'num'.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~get_number.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type = 'num'.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~get_string.

    data lv_item type ref to ty_node.
    lv_item = get_item( iv_path ).
    if lv_item is not initial and lv_item->type <> 'null'.
      rv_value = lv_item->value.
    endif.

  endmethod.


  method zif_abapgit_ajson_reader~members.

    data lv_normalized_path type string.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      append <item>-name to rt_members.
    endloop.

  endmethod.


  method zif_abapgit_ajson_reader~slice.

    data lo_section         type ref to zcl_abapgit_ajson.
    data ls_item            like line of mt_json_tree.
    data lv_normalized_path type string.
    data ls_path_parts      type ty_path_name.
    data lv_path_len        type i.

    create object lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    loop at mt_json_tree into ls_item.
      " TODO potentially improve performance due to sorted tree (all path started from same prefix go in a row)
      if strlen( ls_item-path ) >= lv_path_len
          and substring( val = ls_item-path len = lv_path_len ) = lv_normalized_path.
        ls_item-path = substring( val = ls_item-path off = lv_path_len - 1 ). " less closing '/'
        insert ls_item into table lo_section->mt_json_tree.
      elseif ls_item-path = ls_path_parts-path and ls_item-name = ls_path_parts-name.
        clear: ls_item-path, ls_item-name. " this becomes a new root
        insert ls_item into table lo_section->mt_json_tree.
      endif.
    endloop.

    ri_json = lo_section.

  endmethod.


  method zif_abapgit_ajson_reader~to_abap.

    data lo_to_abap type ref to lcl_json_to_abap.

    clear ev_container.
    lcl_json_to_abap=>bind(
      changing
        c_obj = ev_container
        co_instance = lo_to_abap ).
    lo_to_abap->to_abap( mt_json_tree ).

  endmethod.
ENDCLASS.
