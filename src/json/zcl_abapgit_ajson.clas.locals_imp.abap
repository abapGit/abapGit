**********************************************************************
* UTILS
**********************************************************************

CLASS lcl_utils DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS normalize_path
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_path) TYPE string.
    CLASS-METHODS split_path
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE zif_abapgit_ajson=>ty_path_name.
    CLASS-METHODS validate_array_index
      IMPORTING
        iv_path TYPE string
        iv_index TYPE string
      RETURNING
        VALUE(rv_index) TYPE i
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.

  METHOD validate_array_index.

    IF NOT iv_index CO '0123456789'.
      zcx_abapgit_ajson_error=>raise( |Cannot add non-numeric key [{ iv_index }] to array [{ iv_path }]| ).
    ENDIF.
    rv_index = iv_index.
    IF rv_index = 0.
      zcx_abapgit_ajson_error=>raise( |Cannot add zero key to array [{ iv_path }]| ).
    ENDIF.

  ENDMETHOD.

  METHOD normalize_path.

    rv_path = iv_path.
    IF strlen( rv_path ) = 0.
      rv_path = '/'.
    ENDIF.
    IF rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    ENDIF.
    IF substring( val = rv_path
                  off = strlen( rv_path ) - 1 ) <> '/'.
      rv_path = rv_path && '/'.
    ENDIF.

  ENDMETHOD.

  METHOD split_path.

    DATA lv_offs TYPE i.
    DATA lv_len TYPE i.
    DATA lv_trim_slash TYPE i.

    lv_len = strlen( iv_path ).
    IF lv_len = 0 OR iv_path = '/'.
      RETURN. " empty path is the alias for root item = '' + ''
    ENDIF.

    IF substring( val = iv_path
                  off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    ENDIF.

    lv_offs = find( val = reverse( iv_path )
                    sub = '/'
                    off = lv_trim_slash ).
    IF lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    ENDIF.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring( val = iv_path
                                                   len = lv_offs ) ).
    rv_path_name-name = substring( val = iv_path
                                   off = lv_offs
                                   len = lv_len - lv_offs - lv_trim_slash ).

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* PARSER
**********************************************************************

CLASS lcl_json_parser DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        iv_json TYPE string
      RETURNING
        VALUE(rt_json_tree) TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.

    TYPES:
      ty_stack_tt TYPE STANDARD TABLE OF REF TO zif_abapgit_ajson=>ty_node.

    DATA mt_stack TYPE ty_stack_tt.

    CLASS-METHODS join_path
      IMPORTING
        it_stack TYPE ty_stack_tt
      RETURNING
        VALUE(rv_path) TYPE string.

    METHODS raise
      IMPORTING
        iv_error TYPE string
      RAISING
        zcx_abapgit_ajson_error.

    METHODS _parse
      IMPORTING
        iv_json TYPE string
      RETURNING
        VALUE(rt_json_tree) TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error cx_sxml_error.

    METHODS _get_location
      IMPORTING
        iv_json            TYPE string
        iv_offset          TYPE i
      RETURNING
        VALUE(rv_location) TYPE string.

ENDCLASS.

CLASS lcl_json_parser IMPLEMENTATION.

  METHOD parse.
    DATA lx_sxml_parse TYPE REF TO cx_sxml_parse_error.
    DATA lx_sxml TYPE REF TO cx_sxml_error.
    DATA lv_location TYPE string.
    TRY.
        rt_json_tree = _parse( iv_json ).
      CATCH cx_sxml_parse_error INTO lx_sxml_parse.
        lv_location = _get_location(
        iv_json   = iv_json
        iv_offset = lx_sxml_parse->xml_offset ).
        zcx_abapgit_ajson_error=>raise(
        iv_msg      = |Json parsing error (SXML): { lx_sxml_parse->get_text( ) }|
        iv_location = lv_location ).
      CATCH cx_sxml_error INTO lx_sxml.
        zcx_abapgit_ajson_error=>raise(
        iv_msg      = |Json parsing error (SXML): { lx_sxml->get_text( ) }|
        iv_location = '@PARSER' ).
    ENDTRY.
  ENDMETHOD.

  METHOD _get_location.

    DATA lv_json TYPE string.
    DATA lv_offset TYPE i.
    DATA lt_text TYPE TABLE OF string.
    DATA lv_text TYPE string.
    DATA lv_line TYPE i.
    DATA lv_pos TYPE i.

    lv_offset = iv_offset.
    IF lv_offset < 0.
      lv_offset = 0.
    ENDIF.
    IF lv_offset > strlen( iv_json ).
      lv_offset = strlen( iv_json ).
    ENDIF.

    lv_json = iv_json(lv_offset).

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN lv_json WITH cl_abap_char_utilities=>newline.

    SPLIT lv_json AT cl_abap_char_utilities=>newline INTO TABLE lt_text.

    lv_line = lines( lt_text ).
    IF lv_line = 0.
      lv_line = 1.
      lv_pos = 1.
    ELSE.
      READ TABLE lt_text INDEX lv_line INTO lv_text.
      lv_pos = strlen( lv_text ) + 1.
    ENDIF.

    rv_location = |Line { lv_line }, Offset { lv_pos }|.

  ENDMETHOD.

  METHOD _parse.

    DATA lo_reader TYPE REF TO if_sxml_reader.
    DATA lr_stack_top LIKE LINE OF mt_stack.
    DATA lo_node TYPE REF TO if_sxml_node.
    FIELD-SYMBOLS <item> LIKE LINE OF rt_json_tree.

    CLEAR mt_stack.
    IF iv_json IS INITIAL.
      RETURN.
    ENDIF.
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    DO.
      lo_node = lo_reader->read_next_node( ).
      IF lo_node IS NOT BOUND.
        EXIT.
      ENDIF.


      CASE lo_node->type.
        WHEN if_sxml_node=>co_nt_element_open.
          DATA lt_attributes TYPE if_sxml_attribute=>attributes.
          DATA lo_attr LIKE LINE OF lt_attributes.
          DATA lo_open TYPE REF TO if_sxml_open_element.
          lo_open ?= lo_node.

          APPEND INITIAL LINE TO rt_json_tree ASSIGNING <item>.

          <item>-type = to_lower( lo_open->qname-name ).

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          IF sy-subrc = 0.
            <item>-path = join_path( mt_stack ).
            lr_stack_top->children = lr_stack_top->children + 1.

            IF lr_stack_top->type = 'array'.
              <item>-name = |{ lr_stack_top->children }|.
              <item>-index = lr_stack_top->children.
            ELSE.
              lt_attributes = lo_open->get_attributes( ).
              LOOP AT lt_attributes INTO lo_attr.
                IF lo_attr->qname-name = 'name' AND lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <item>-name = lo_attr->get_value( ).
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          GET REFERENCE OF <item> INTO lr_stack_top.
          INSERT lr_stack_top INTO mt_stack INDEX 1.

        WHEN if_sxml_node=>co_nt_element_close.
          DATA lo_close TYPE REF TO if_sxml_close_element.
          lo_close ?= lo_node.

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          DELETE mt_stack INDEX 1.
          IF lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          ENDIF.

        WHEN if_sxml_node=>co_nt_value.
          DATA lo_value TYPE REF TO if_sxml_value_node.
          lo_value ?= lo_node.

          <item>-value = lo_value->get_value( ).

        WHEN OTHERS.
          raise( 'Unexpected node type' ).
      ENDCASE.
    ENDDO.

    IF lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    ENDIF.

  ENDMETHOD.

  METHOD join_path.

    FIELD-SYMBOLS <ref> LIKE LINE OF it_stack.

    LOOP AT it_stack ASSIGNING <ref>.
      rv_path = <ref>->name && '/' && rv_path.
    ENDLOOP.

  ENDMETHOD.

  METHOD raise.

    zcx_abapgit_ajson_error=>raise(
      iv_location = join_path( mt_stack )
      iv_msg      = |JSON PARSER: { iv_error } @ { join_path( mt_stack ) }| ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* SERIALIZER
**********************************************************************

CLASS lcl_json_serializer DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.

    CLASS-METHODS stringify
      IMPORTING
        it_json_tree TYPE zif_abapgit_ajson=>ty_nodes_ts
        iv_indent TYPE i DEFAULT 0
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_json_string) TYPE string
      RAISING
        zcx_abapgit_ajson_error.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.

    CLASS-DATA gv_comma_with_lf TYPE string.

    DATA mt_json_tree TYPE zif_abapgit_ajson=>ty_nodes_ts.
    DATA mv_keep_item_order TYPE abap_bool.
    DATA mt_buffer TYPE string_table.
    DATA mv_indent_step TYPE i.
    DATA mv_level TYPE i.

    CLASS-METHODS escape
      IMPORTING
        iv_unescaped TYPE string
      RETURNING
        VALUE(rv_escaped) TYPE string.

    METHODS _stringify
      RETURNING
        VALUE(rv_json_string) TYPE string
      RAISING
        zcx_abapgit_ajson_error.

    METHODS stringify_node
      IMPORTING
        is_node TYPE zif_abapgit_ajson=>ty_node
      RAISING
        zcx_abapgit_ajson_error.

    METHODS stringify_set
      IMPORTING
        iv_parent_path TYPE string
        iv_array TYPE abap_bool
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_json_serializer IMPLEMENTATION.

  METHOD class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD stringify.

    DATA lo TYPE REF TO lcl_json_serializer.
    CREATE OBJECT lo.
    lo->mt_json_tree = it_json_tree.
    lo->mv_indent_step = iv_indent.
    lo->mv_keep_item_order = iv_keep_item_order.
    rv_json_string = lo->_stringify( ).

  ENDMETHOD.

  METHOD _stringify.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_json_tree.
    READ TABLE mt_json_tree ASSIGNING <n>
      WITH KEY
        path = ''
        name = ''. " Root
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    stringify_node( <n> ).

    rv_json_string = concat_lines_of( table = mt_buffer ).

  ENDMETHOD.

  METHOD stringify_node.

    DATA lv_item TYPE string.
    DATA lv_indent_prefix TYPE string.

    IF mv_indent_step > 0.
      lv_indent_prefix = repeat( val = ` `
                                 occ = mv_indent_step * mv_level ).
      lv_item = lv_indent_prefix.
    ENDIF.

    IF is_node-name IS NOT INITIAL AND is_node-index IS INITIAL. " Not root, not array item
      IF mv_indent_step > 0.
        lv_item = lv_item && |"{ is_node-name }": |.
      ELSE.
        lv_item = |"{ is_node-name }":|.
      ENDIF.
    ENDIF.

    CASE is_node-type.
      WHEN zif_abapgit_ajson=>node_type-array.
        lv_item = lv_item && '['.
      WHEN zif_abapgit_ajson=>node_type-object.
        lv_item = lv_item && '{'.
      WHEN zif_abapgit_ajson=>node_type-string.
        lv_item = lv_item && |"{ escape( is_node-value ) }"|.
      WHEN zif_abapgit_ajson=>node_type-boolean OR zif_abapgit_ajson=>node_type-number.
        lv_item = lv_item && is_node-value.
      WHEN zif_abapgit_ajson=>node_type-null.
        lv_item = lv_item && 'null'.
      WHEN OTHERS.
        zcx_abapgit_ajson_error=>raise(
          iv_msg = |Unexpected type [{ is_node-type }]|
          iv_location = is_node-path && is_node-name ).
    ENDCASE.

    IF mv_indent_step > 0
      AND ( is_node-type = zif_abapgit_ajson=>node_type-array OR is_node-type = zif_abapgit_ajson=>node_type-object )
      AND is_node-children > 0.
      mv_level = mv_level + 1.
      lv_item = lv_item && cl_abap_char_utilities=>newline.
    ENDIF.

    APPEND lv_item TO mt_buffer.

    " finish complex item

    IF is_node-type = zif_abapgit_ajson=>node_type-array OR is_node-type = zif_abapgit_ajson=>node_type-object.
      DATA lv_children_path TYPE string.
      DATA lv_tail TYPE string.

      lv_children_path = is_node-path && is_node-name && '/'. " for root: path = '' and name = '', so result is '/'

      CASE is_node-type.
        WHEN zif_abapgit_ajson=>node_type-array.
          IF is_node-children > 0.
            stringify_set(
              iv_parent_path = lv_children_path
              iv_array       = abap_true ).
          ENDIF.
          lv_tail = ']'.
        WHEN zif_abapgit_ajson=>node_type-object.
          IF is_node-children > 0.
            stringify_set(
              iv_parent_path = lv_children_path
              iv_array       = abap_false ).
          ENDIF.
          lv_tail = '}'.
      ENDCASE.

      IF mv_indent_step > 0 AND is_node-children > 0.
        lv_tail = lv_indent_prefix && lv_tail.
        mv_level = mv_level - 1.
      ENDIF.
      APPEND lv_tail TO mt_buffer.
    ENDIF.

  ENDMETHOD.

  METHOD stringify_set.

    DATA lv_tab_key TYPE string.
    DATA lv_first_done TYPE abap_bool.
    FIELD-SYMBOLS <n> LIKE LINE OF mt_json_tree.

    IF iv_array = abap_true.
      lv_tab_key = 'array_index'. " path + index
    ELSEIF mv_keep_item_order = abap_true.
      lv_tab_key = 'item_order'. " path + order
    ELSE.
      lv_tab_key = 'primary_key'. " path + name
    ENDIF.

    LOOP AT mt_json_tree ASSIGNING <n> USING KEY (lv_tab_key) WHERE path = iv_parent_path.
      IF lv_first_done = abap_false.
        lv_first_done = abap_true.
      ELSEIF mv_indent_step > 0.
        APPEND gv_comma_with_lf TO mt_buffer.
      ELSE.
        APPEND ',' TO mt_buffer.
      ENDIF.
      stringify_node( <n> ).
    ENDLOOP.

    IF mv_indent_step > 0 AND lv_first_done = abap_true. " only of items were in the list
      APPEND cl_abap_char_utilities=>newline TO mt_buffer.
    ENDIF.

  ENDMETHOD.

  METHOD escape.

    rv_escaped = iv_unescaped.
    IF rv_escaped CA |"\\\t\n\r|.
      " TODO consider performance ...
      " see also https://www.json.org/json-en.html
      rv_escaped = replace(
        val = rv_escaped
        sub = '\'
        with = '\\'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\n|
        with = '\n'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\r|
        with = '\r'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\t|
        with = '\t'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = '"'
        with = '\"'
        occ = 0 ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* JSON_TO_ABAP
**********************************************************************

CLASS lcl_json_to_abap DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ii_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL.

    METHODS to_abap
      IMPORTING
        it_nodes     TYPE zif_abapgit_ajson=>ty_nodes_ts
      CHANGING
        c_container TYPE any
      RAISING
        zcx_abapgit_ajson_error.

    METHODS to_timestamp
      IMPORTING
        iv_value         TYPE zif_abapgit_ajson=>ty_node-value
      RETURNING
        VALUE(rv_result) TYPE timestamp
      RAISING
        zcx_abapgit_ajson_error.

    METHODS to_date
      IMPORTING
        iv_value         TYPE zif_abapgit_ajson=>ty_node-value
      RETURNING
        VALUE(rv_result) TYPE d
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_type_cache,
        type_path         TYPE string,
        target_field_name TYPE string,
        dd                TYPE REF TO cl_abap_datadescr,
        type_kind         LIKE cl_abap_typedescr=>typekind_any,
        tab_item_buf      TYPE REF TO data,
      END OF ty_type_cache.
    DATA mt_node_type_cache TYPE HASHED TABLE OF ty_type_cache WITH UNIQUE KEY type_path.

    DATA mr_nodes TYPE REF TO zif_abapgit_ajson=>ty_nodes_ts.
    DATA mi_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping.

    METHODS any_to_abap
      IMPORTING
        iv_path        TYPE string
        is_parent_type TYPE ty_type_cache OPTIONAL
        i_container_ref TYPE REF TO data
      RAISING
        zcx_abapgit_ajson_error.

    METHODS value_to_abap
      IMPORTING
        is_node      TYPE zif_abapgit_ajson=>ty_node
        is_node_type TYPE ty_type_cache
        i_container_ref TYPE REF TO data
      RAISING
        zcx_abapgit_ajson_error
        cx_sy_conversion_no_number.

    METHODS get_node_type
      IMPORTING
        is_node            TYPE zif_abapgit_ajson=>ty_node OPTIONAL " Empty for root
        is_parent_type     TYPE ty_type_cache OPTIONAL
        i_container_ref    TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rs_node_type) TYPE ty_type_cache
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_json_to_abap IMPLEMENTATION.

  METHOD constructor.
    mi_custom_mapping = ii_custom_mapping.
  ENDMETHOD.

  METHOD to_abap.

    DATA lr_ref TYPE REF TO data.

    CLEAR c_container. " what about data/obj refs ?
    CLEAR mt_node_type_cache.

    GET REFERENCE OF c_container INTO lr_ref.
    GET REFERENCE OF it_nodes INTO mr_nodes.

    get_node_type( i_container_ref = lr_ref ). " Pre-cache root node type

    any_to_abap(
      iv_path         = ''
      i_container_ref = lr_ref ).

  ENDMETHOD.

  METHOD get_node_type.

    DATA lv_node_type_path TYPE string.
    DATA lo_sdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_tdescr TYPE REF TO cl_abap_tabledescr.
    DATA lo_ddescr TYPE REF TO cl_abap_datadescr.

    " Calculate type path
    IF is_parent_type-type_kind = cl_abap_typedescr=>typekind_table.
      lv_node_type_path = is_parent_type-type_path && '/-'. " table item type
    ELSEIF is_parent_type-type_kind IS NOT INITIAL.
      lv_node_type_path = is_parent_type-type_path && '/' && is_node-name.
    ENDIF. " For root node lv_node_type_path remains ''

    " Get or create cached
    READ TABLE mt_node_type_cache INTO rs_node_type WITH KEY type_path = lv_node_type_path.
    IF sy-subrc <> 0.

      rs_node_type-type_path         = lv_node_type_path.

      IF mi_custom_mapping IS BOUND.
        rs_node_type-target_field_name = to_upper( mi_custom_mapping->to_abap(
          iv_path = is_node-path
          iv_name = is_node-name ) ).
        IF rs_node_type-target_field_name IS INITIAL.
          rs_node_type-target_field_name = to_upper( is_node-name ).
        ENDIF.
      ELSE.
        rs_node_type-target_field_name = to_upper( is_node-name ).
      ENDIF.

      CASE is_parent_type-type_kind.
        WHEN 'h'. " Table
          lo_tdescr ?= is_parent_type-dd.
          rs_node_type-dd = lo_tdescr->get_table_line_type( ).

        WHEN 'u' OR 'v'. " Structure
          lo_sdescr ?= is_parent_type-dd.
          lo_sdescr->get_component_type(
            EXPORTING
              p_name      = rs_node_type-target_field_name
            RECEIVING
              p_descr_ref = rs_node_type-dd
            EXCEPTIONS
              component_not_found = 4 ).
          IF sy-subrc <> 0.
            zcx_abapgit_ajson_error=>raise( |Path not found| ).
          ENDIF.

        WHEN ''. " Root node
          rs_node_type-dd ?= cl_abap_typedescr=>describe_by_data_ref( i_container_ref ).

        WHEN OTHERS.
          zcx_abapgit_ajson_error=>raise( |Unexpected parent type| ).
      ENDCASE.

      rs_node_type-type_kind         = rs_node_type-dd->type_kind. " for caching and cleaner unintialized access
      IF rs_node_type-type_kind = 'h'. " Table
        lo_tdescr ?= rs_node_type-dd.
        IF lo_tdescr->table_kind <> 'S'. " standard
          lo_ddescr = lo_tdescr->get_table_line_type( ).
          CREATE DATA rs_node_type-tab_item_buf TYPE HANDLE lo_ddescr.
        ENDIF.
      ENDIF.

      INSERT rs_node_type INTO TABLE mt_node_type_cache.
    ENDIF.

  ENDMETHOD.

  METHOD any_to_abap.

    DATA ls_node_type LIKE LINE OF mt_node_type_cache.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.
    DATA lx_root TYPE REF TO cx_root.
    DATA lr_target_field TYPE REF TO data.

    FIELD-SYMBOLS <n> TYPE zif_abapgit_ajson=>ty_node.
    FIELD-SYMBOLS <parent_stdtab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <parent_anytab> TYPE ANY TABLE.
    FIELD-SYMBOLS <parent_struc> TYPE any.
    FIELD-SYMBOLS <tab_item> TYPE any.

    " Assign container
    CASE is_parent_type-type_kind.
      WHEN 'h'. " Table
        IF is_parent_type-tab_item_buf IS BOUND. " Indirect hint that table was sorted/hashed, see get_node_type.
          ASSIGN i_container_ref->* TO <parent_anytab>.
          ASSERT sy-subrc = 0.

          lr_target_field = is_parent_type-tab_item_buf. " For hashed/sorted table - same buffer for all children
          ASSIGN is_parent_type-tab_item_buf->* TO <tab_item>.
          ASSERT sy-subrc = 0.

        ELSE.
          ASSIGN i_container_ref->* TO <parent_stdtab>.
          ASSERT sy-subrc = 0.
        ENDIF.

      WHEN 'u' OR 'v'. " Structure
        ASSIGN i_container_ref->* TO <parent_struc>.
        ASSERT sy-subrc = 0.
    ENDCASE.

    TRY.

      " array_index because stringified index goes in wrong order [1, 10, 2 ...]
        LOOP AT mr_nodes->* ASSIGNING <n> USING KEY array_index WHERE path = iv_path.

        " Get or create type cache record
          IF is_parent_type-type_kind <> 'h' OR ls_node_type-type_kind IS INITIAL.
          " table records are the same, no need to refetch twice
            ls_node_type = get_node_type(
            is_node        = <n>
            is_parent_type = is_parent_type ).
          ENDIF.

        " Validate node type
          IF ls_node_type-type_kind CA 'lr'. " data/obj ref
          " TODO maybe in future
            zcx_abapgit_ajson_error=>raise( 'Cannot assign to ref' ).
          ENDIF.

        " Find target field reference
          CASE is_parent_type-type_kind.
            WHEN 'h'. " Table
              IF NOT ls_node_type-target_field_name CO '0123456789'.
              " Does not affect anything actually but for integrity
                zcx_abapgit_ajson_error=>raise( 'Need index to access tables' ).
              ENDIF.

              IF is_parent_type-tab_item_buf IS NOT BOUND. " Indirect hint that table was srt/hsh, see get_node_type
                APPEND INITIAL LINE TO <parent_stdtab> REFERENCE INTO lr_target_field.
                ASSERT sy-subrc = 0.
              ENDIF.

            WHEN 'u' OR 'v'.
              FIELD-SYMBOLS <field> TYPE any.
              ASSIGN COMPONENT ls_node_type-target_field_name OF STRUCTURE <parent_struc> TO <field>.
              ASSERT sy-subrc = 0.
              GET REFERENCE OF <field> INTO lr_target_field.

            WHEN ''. " Root node
              lr_target_field = i_container_ref.

            WHEN OTHERS.
              zcx_abapgit_ajson_error=>raise( 'Unexpected parent type' ).
          ENDCASE.

        " Process value assignment
          CASE <n>-type.
            WHEN zif_abapgit_ajson=>node_type-object.
              IF NOT ls_node_type-type_kind CO 'uv'.
                zcx_abapgit_ajson_error=>raise( 'Expected structure' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN zif_abapgit_ajson=>node_type-array.
              IF NOT ls_node_type-type_kind = 'h'.
                zcx_abapgit_ajson_error=>raise( 'Expected table' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN OTHERS.
              value_to_abap(
              is_node         = <n>
              is_node_type    = ls_node_type
              i_container_ref = lr_target_field ).
          ENDCASE.

          IF is_parent_type-tab_item_buf IS BOUND. " Indirect hint that table was sorted/hashed, see get_node_type.
            TRY.
                INSERT <tab_item> INTO TABLE <parent_anytab>.
              CATCH cx_sy_itab_duplicate_key.
                sy-subrc = 4.
            ENDTRY.
            IF sy-subrc <> 0.
              zcx_abapgit_ajson_error=>raise( 'Duplicate insertion' ).
            ENDIF.
          ENDIF.

        ENDLOOP.

      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        IF lx_ajson->location IS INITIAL.
          lx_ajson->set_location( <n>-path && <n>-name ).
        ENDIF.
        RAISE EXCEPTION lx_ajson.
      CATCH cx_sy_conversion_no_number.
        zcx_abapgit_ajson_error=>raise(
        iv_msg = 'Source is not a number'
        iv_location = <n>-path && <n>-name ).
      CATCH cx_root INTO lx_root.
        zcx_abapgit_ajson_error=>raise(
        iv_msg = lx_root->get_text( )
        iv_location = <n>-path && <n>-name ).
    ENDTRY.

  ENDMETHOD.

  METHOD value_to_abap.

    FIELD-SYMBOLS <container> TYPE any.

    IF is_node_type-type_kind CA 'lruvh'. " refs, table, strucs
      zcx_abapgit_ajson_error=>raise( |Unsupported target for value [{ is_node_type-type_kind }]| ).
    ENDIF.

    ASSIGN i_container_ref->* TO <container>.
    ASSERT sy-subrc = 0.

    CASE is_node-type.
      WHEN zif_abapgit_ajson=>node_type-null.
        " Do nothing
      WHEN zif_abapgit_ajson=>node_type-boolean.
        " TODO: check type ?
        <container> = boolc( is_node-value = 'true' ).
      WHEN zif_abapgit_ajson=>node_type-number.
        " TODO: check type ?
        <container> = is_node-value.

      WHEN zif_abapgit_ajson=>node_type-string.
        " TODO: check type ?
        IF is_node_type-type_kind = 'D' AND is_node-value IS NOT INITIAL.
          <container> = to_date( is_node-value ).
        ELSEIF is_node_type-type_kind = 'P' AND is_node-value IS NOT INITIAL.
          <container> = to_timestamp( is_node-value ).
        ELSE.
          <container> = is_node-value.
        ENDIF.
      WHEN OTHERS.
        zcx_abapgit_ajson_error=>raise( |Unexpected JSON type [{ is_node-type }]| ).
    ENDCASE.

  ENDMETHOD.

  METHOD to_date.

    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
      IN iv_value
      SUBMATCHES lv_y lv_m lv_d.
    IF sy-subrc <> 0.
      zcx_abapgit_ajson_error=>raise( 'Unexpected date format' ).
    ENDIF.
    CONCATENATE lv_y lv_m lv_d INTO rv_result.

  ENDMETHOD.

  METHOD to_timestamp.

    CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.
    CONSTANTS lc_regex_ts_with_hour TYPE string
      VALUE `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(\+)(\d{2}):(\d{2})`.
    CONSTANTS lc_regex_ts_utc TYPE string
      VALUE `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(Z|$)`.

    DATA:
      BEGIN OF ls_timestamp,
        year         TYPE c LENGTH 4,
        month        TYPE c LENGTH 2,
        day          TYPE c LENGTH 2,
        t            TYPE c LENGTH 1,
        hour         TYPE c LENGTH 2,
        minute       TYPE c LENGTH 2,
        second       TYPE c LENGTH 2,
        local_sign   TYPE c LENGTH 1,
        local_hour   TYPE c LENGTH 2,
        local_minute TYPE c LENGTH 2,
      END OF ls_timestamp.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_seconds_conv TYPE i.
    DATA lv_timestamp TYPE timestampl.

    FIND FIRST OCCURRENCE OF REGEX lc_regex_ts_with_hour
      IN iv_value SUBMATCHES
        ls_timestamp-year ls_timestamp-month ls_timestamp-day ls_timestamp-t
        ls_timestamp-hour ls_timestamp-minute ls_timestamp-second
        ls_timestamp-local_sign ls_timestamp-local_hour ls_timestamp-local_minute.

    IF sy-subrc = 0.

      lv_seconds_conv = ( ls_timestamp-local_hour * 3600 ) + ( ls_timestamp-local_minute * 60 ).

    ELSE.

      FIND FIRST OCCURRENCE OF REGEX lc_regex_ts_utc
        IN iv_value SUBMATCHES
          ls_timestamp-year ls_timestamp-month ls_timestamp-day ls_timestamp-t
          ls_timestamp-hour ls_timestamp-minute ls_timestamp-second.

      IF sy-subrc <> 0.
        zcx_abapgit_ajson_error=>raise( 'Unexpected timestamp format' ).
      ENDIF.

    ENDIF.

    CONCATENATE ls_timestamp-year ls_timestamp-month ls_timestamp-day INTO lv_date.
    CONCATENATE ls_timestamp-hour ls_timestamp-minute ls_timestamp-second INTO lv_time.

    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE lc_utc.

    TRY.

        CASE ls_timestamp-local_sign.
          WHEN '-'.
            lv_timestamp = cl_abap_tstmp=>add(
            tstmp = lv_timestamp
            secs  = lv_seconds_conv ).
          WHEN '+'.
            lv_timestamp = cl_abap_tstmp=>subtractsecs(
            tstmp = lv_timestamp
            secs  = lv_seconds_conv ).
        ENDCASE.

      CATCH cx_parameter_invalid_range cx_parameter_invalid_type.
        zcx_abapgit_ajson_error=>raise( 'Unexpected error calculating timestamp' ).
    ENDTRY.

    cl_abap_tstmp=>move(
      EXPORTING
        tstmp_src = lv_timestamp
      IMPORTING
        tstmp_tgt = rv_result ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* ABAP_TO_JSON
**********************************************************************

CLASS lcl_abap_to_json DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS convert
      IMPORTING
        iv_data            TYPE any
        is_prefix          TYPE zif_abapgit_ajson=>ty_path_name OPTIONAL
        iv_array_index     TYPE i DEFAULT 0
        ii_custom_mapping  TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_nodes)   TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    CLASS-METHODS insert_with_type
      IMPORTING
        iv_data            TYPE any
        iv_type            TYPE string
        is_prefix          TYPE zif_abapgit_ajson=>ty_path_name OPTIONAL
        iv_array_index     TYPE i DEFAULT 0
        ii_custom_mapping  TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_nodes)   TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.

    CLASS-DATA gv_ajson_absolute_type_name TYPE string.
    DATA mi_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA mv_keep_item_order TYPE abap_bool.

    METHODS convert_any
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    METHODS convert_ajson
      IMPORTING
        io_json TYPE REF TO zcl_abapgit_ajson
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    METHODS convert_value
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    METHODS convert_ref
      IMPORTING
        iv_data TYPE any
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    METHODS convert_struc
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
        cs_root  TYPE zif_abapgit_ajson=>ty_node OPTIONAL
      RAISING
        zcx_abapgit_ajson_error.

    METHODS convert_table
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

    METHODS insert_value_with_type
      IMPORTING
        iv_data TYPE any
        iv_type TYPE string
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE zif_abapgit_ajson=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_abap_to_json IMPLEMENTATION.

  METHOD class_constructor.

    DATA lo_dummy TYPE REF TO zcl_abapgit_ajson.
    DATA lo_type TYPE REF TO cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  ENDMETHOD.

  METHOD convert.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO lcl_abap_to_json.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    CREATE OBJECT lo_converter.
    lo_converter->mi_custom_mapping  = ii_custom_mapping.
    lo_converter->mv_keep_item_order = iv_keep_item_order.

    lo_converter->convert_any(
      EXPORTING
        iv_data   = iv_data
        io_type   = lo_type
        is_prefix = is_prefix
        iv_index  = iv_array_index
      CHANGING
        ct_nodes = rt_nodes ).

  ENDMETHOD.

  METHOD convert_any.

    CASE io_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        convert_value(
          EXPORTING
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      WHEN cl_abap_typedescr=>kind_struct.
        convert_struc(
          EXPORTING
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      WHEN cl_abap_typedescr=>kind_table.
        convert_table(
          EXPORTING
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      WHEN OTHERS.

        IF io_type->type_kind = cl_abap_typedescr=>typekind_dref.
          convert_ref(
            EXPORTING
              iv_data   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
              iv_item_order = iv_item_order
            CHANGING
              ct_nodes = ct_nodes ).

        ELSEIF io_type->type_kind = cl_abap_typedescr=>typekind_oref
          AND cl_abap_typedescr=>describe_by_object_ref( iv_data )->absolute_name = gv_ajson_absolute_type_name.
          convert_ajson(
            EXPORTING
              io_json   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
            CHANGING
              ct_nodes = ct_nodes ).
        ELSE.
          zcx_abapgit_ajson_error=>raise( |Unsupported type [{ io_type->type_kind
            }] @{ is_prefix-path && is_prefix-name }| ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD convert_ajson.

    FIELD-SYMBOLS <src> LIKE LINE OF ct_nodes.
    FIELD-SYMBOLS <dst> LIKE LINE OF ct_nodes.

    IF io_json IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT io_json->mt_json_tree ASSIGNING <src>.
      APPEND <src> TO ct_nodes ASSIGNING <dst>.

      IF <dst>-path IS INITIAL AND <dst>-name IS INITIAL. " root node
        <dst>-path  = is_prefix-path.
        <dst>-name  = is_prefix-name.
        <dst>-index = iv_index.
      ELSE.
        <dst>-path = is_prefix-path && is_prefix-name && <dst>-path.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD convert_value.

    DATA ls_node LIKE LINE OF ct_nodes.

    ls_node-path  = is_prefix-path.
    ls_node-name  = is_prefix-name.
    ls_node-index = iv_index.
    ls_node-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_node-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_node-name IS INITIAL.
      ls_node-name  = is_prefix-name.
    ENDIF.

    IF io_type->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL'
        OR io_type->absolute_name = '\TYPE=ABAP_BOOLEAN'
        OR io_type->absolute_name = '\TYPE=XSDBOOLEAN'
        OR io_type->absolute_name = '\TYPE=FLAG'
        OR io_type->absolute_name = '\TYPE=XFELD'.
      ls_node-type = zif_abapgit_ajson=>node_type-boolean.
      IF iv_data IS NOT INITIAL.
        ls_node-value = 'true'.
      ELSE.
        ls_node-value = 'false'.
      ENDIF.
    ELSEIF io_type->type_kind CO 'CNgXyDT'. " Char like, date/time, xstring
      ls_node-type = zif_abapgit_ajson=>node_type-string.
      ls_node-value = |{ iv_data }|.
    ELSEIF io_type->type_kind CO 'bsI8PaeF'. " Numeric
      ls_node-type = zif_abapgit_ajson=>node_type-number.
      ls_node-value = |{ iv_data }|.
    ELSE.
      zcx_abapgit_ajson_error=>raise( |Unexpected elementary type [{
        io_type->type_kind }] @{ is_prefix-path && is_prefix-name }| ).
    ENDIF.

    APPEND ls_node TO ct_nodes.

  ENDMETHOD.

  METHOD convert_ref.

    DATA ls_node LIKE LINE OF ct_nodes.

    ls_node-path  = is_prefix-path.
    ls_node-name  = is_prefix-name.
    ls_node-index = iv_index.
    ls_node-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_node-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_node-name IS INITIAL.
      ls_node-name  = is_prefix-name.
    ENDIF.

    IF iv_data IS INITIAL.
      ls_node-type  = zif_abapgit_ajson=>node_type-null.
      ls_node-value = 'null'.
    ELSE.
      " TODO support data references
      zcx_abapgit_ajson_error=>raise( |Unexpected reference @{ is_prefix-path && is_prefix-name }| ).
    ENDIF.

    APPEND ls_node TO ct_nodes.

  ENDMETHOD.

  METHOD convert_struc.

    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    DATA lt_comps TYPE cl_abap_structdescr=>component_table.
    DATA ls_next_prefix LIKE is_prefix.
    DATA lv_item_order TYPE i.
    DATA ls_root LIKE LINE OF ct_nodes.

    FIELD-SYMBOLS <root> LIKE ls_root.
    FIELD-SYMBOLS <c> LIKE LINE OF lt_comps.
    FIELD-SYMBOLS <val> TYPE any.

    " Object root

    IF cs_root IS SUPPLIED. " call for include structure
      ASSIGN cs_root TO <root>.
    ELSE. " First call
      ls_root-path  = is_prefix-path.
      ls_root-name  = is_prefix-name.
      ls_root-type  = zif_abapgit_ajson=>node_type-object.
      ls_root-index = iv_index.

      IF mi_custom_mapping IS BOUND.
        ls_root-name = mi_custom_mapping->to_json(
          iv_path = is_prefix-path
          iv_name = is_prefix-name ).
      ENDIF.

      IF ls_root-name IS INITIAL.
        ls_root-name  = is_prefix-name.
      ENDIF.

      ls_root-order = iv_item_order.

      APPEND ls_root TO ct_nodes ASSIGNING <root>.

    ENDIF.

    " Object attributes

    lo_struc ?= io_type.
    lt_comps = lo_struc->get_components( ).
    " get_components is potentially much slower than lo_struc->components
    " but ! we still need it to identify booleans
    " and rtti seems to cache type descriptions really well (https://github.com/sbcgua/benchmarks.git)
    " the structures will be repeated in real life

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.

    LOOP AT lt_comps ASSIGNING <c>.

      IF <c>-as_include = abap_true.

        convert_struc(
          EXPORTING
            iv_data   = iv_data
            io_type   = <c>-type
            is_prefix = is_prefix
          CHANGING
            cs_root  = <root>
            ct_nodes = ct_nodes ).

      ELSE.

        <root>-children = <root>-children + 1.
        ls_next_prefix-name = to_lower( <c>-name ).
        ASSIGN COMPONENT <c>-name OF STRUCTURE iv_data TO <val>.
        ASSERT sy-subrc = 0.

        IF mv_keep_item_order = abap_true.
          lv_item_order = <root>-children.
        ENDIF.

        convert_any(
          EXPORTING
            iv_data   = <val>
            io_type   = <c>-type
            is_prefix = ls_next_prefix
            iv_item_order = lv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD convert_table.

    DATA lo_table TYPE REF TO cl_abap_tabledescr.
    DATA lo_ltype TYPE REF TO cl_abap_typedescr.
    DATA ls_next_prefix LIKE is_prefix.
    DATA lv_tabix TYPE sy-tabix.
    DATA ls_root LIKE LINE OF ct_nodes.

    FIELD-SYMBOLS <root> LIKE ls_root.
    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <val> TYPE any.

    " Array root

    ls_root-path  = is_prefix-path.
    ls_root-name  = is_prefix-name.
    ls_root-type  = zif_abapgit_ajson=>node_type-array.
    ls_root-index = iv_index.
    ls_root-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_root-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_root-name IS INITIAL.
      ls_root-name  = is_prefix-name.
    ENDIF.

    APPEND ls_root TO ct_nodes ASSIGNING <root>.

    " Array items

    lo_table ?= io_type.
    lo_ltype  = lo_table->get_table_line_type( ).

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.
    ASSIGN iv_data TO <tab>.

    lv_tabix = 1.
    LOOP AT <tab> ASSIGNING <val>.
      ls_next_prefix-name = to_lower( |{ lv_tabix }| ).

      convert_any(
        EXPORTING
          iv_data   = <val>
          io_type   = lo_ltype
          is_prefix = ls_next_prefix
          iv_index  = <root>-children + 1
        CHANGING
          ct_nodes = ct_nodes ).

      <root>-children = <root>-children + 1.
      lv_tabix = lv_tabix + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD insert_with_type.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO lcl_abap_to_json.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    CREATE OBJECT lo_converter.
    lo_converter->mi_custom_mapping  = ii_custom_mapping.
    lo_converter->mv_keep_item_order = iv_keep_item_order.

    lo_converter->insert_value_with_type(
      EXPORTING
        iv_data   = iv_data
        iv_type   = iv_type
        io_type   = lo_type
        is_prefix = is_prefix
        iv_index  = iv_array_index
      CHANGING
        ct_nodes = rt_nodes ).

  ENDMETHOD.

  METHOD insert_value_with_type.

    DATA lv_prefix TYPE string.
    DATA ls_node LIKE LINE OF ct_nodes.

    lv_prefix = is_prefix-path && is_prefix-name.
    IF io_type->type_kind CO 'CNgXyDT'. " Char like, date/time, xstring
      IF iv_type = zif_abapgit_ajson=>node_type-boolean AND iv_data <> 'true' AND iv_data <> 'false'.
        zcx_abapgit_ajson_error=>raise( |Unexpected boolean value [{ iv_data }] @{ lv_prefix }| ).
      ELSEIF iv_type = zif_abapgit_ajson=>node_type-null AND iv_data IS NOT INITIAL.
        zcx_abapgit_ajson_error=>raise( |Unexpected null value [{ iv_data }] @{ lv_prefix }| ).
      ELSEIF iv_type = zif_abapgit_ajson=>node_type-number AND iv_data CN '0123456789. E+-'.
        zcx_abapgit_ajson_error=>raise( |Unexpected numeric value [{ iv_data }] @{ lv_prefix }| ).
      ELSEIF iv_type <> zif_abapgit_ajson=>node_type-string AND iv_type <> zif_abapgit_ajson=>node_type-boolean
        AND iv_type <> zif_abapgit_ajson=>node_type-null AND iv_type <> zif_abapgit_ajson=>node_type-number.
        zcx_abapgit_ajson_error=>raise( |Unexpected type for value [{ iv_type },{ iv_data }] @{ lv_prefix }| ).
      ENDIF.
    ELSEIF io_type->type_kind CO 'bsI8PaeF'. " Numeric
      IF iv_type <> zif_abapgit_ajson=>node_type-number.
        zcx_abapgit_ajson_error=>raise( |Unexpected value for numeric [{ iv_data }] @{ lv_prefix }| ).
      ENDIF.
    ELSE.
      zcx_abapgit_ajson_error=>raise( |Unexpected type [{ io_type->type_kind }] @{ lv_prefix }| ).
    ENDIF.

    ls_node-path  = is_prefix-path.
    ls_node-name  = is_prefix-name.
    ls_node-index = iv_index.
    ls_node-value = iv_data.
    ls_node-type  = iv_type.
    ls_node-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_node-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_node-name IS INITIAL.
      ls_node-name  = is_prefix-name.
    ENDIF.

    APPEND ls_node TO ct_nodes.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* FILTER RUNNER
**********************************************************************

CLASS lcl_filter_runner DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        ii_filter TYPE REF TO zif_abapgit_ajson_filter
        it_source_tree TYPE zif_abapgit_ajson=>ty_nodes_ts
      CHANGING
        ct_dest_tree TYPE zif_abapgit_ajson=>ty_nodes_ts
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.
    DATA mi_filter TYPE REF TO zif_abapgit_ajson_filter.
    DATA mr_source_tree TYPE REF TO zif_abapgit_ajson=>ty_nodes_ts.
    DATA mr_dest_tree TYPE REF TO zif_abapgit_ajson=>ty_nodes_ts.

    METHODS walk
      IMPORTING
        iv_path TYPE string
      CHANGING
        cs_parent TYPE zif_abapgit_ajson=>ty_node OPTIONAL
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_filter_runner IMPLEMENTATION.

  METHOD run.

    ASSERT ii_filter IS BOUND.
    mi_filter = ii_filter.
    CLEAR ct_dest_tree.

    GET REFERENCE OF it_source_tree INTO mr_source_tree.
    GET REFERENCE OF ct_dest_tree INTO mr_dest_tree.

    walk( iv_path = '' ).

  ENDMETHOD.

  METHOD walk.

    DATA ls_node TYPE zif_abapgit_ajson=>ty_node.

    LOOP AT mr_source_tree->* INTO ls_node WHERE path = iv_path.
      CASE ls_node-type.
        WHEN zif_abapgit_ajson=>node_type-boolean OR zif_abapgit_ajson=>node_type-null
          OR zif_abapgit_ajson=>node_type-number OR zif_abapgit_ajson=>node_type-string.

          IF mi_filter->keep_node( ls_node ) = abap_false.
            CONTINUE.
          ENDIF.

        WHEN zif_abapgit_ajson=>node_type-array OR zif_abapgit_ajson=>node_type-object.

          IF mi_filter->keep_node(
              is_node  = ls_node
              iv_visit = zif_abapgit_ajson_filter=>visit_type-open ) = abap_false.
            CONTINUE.
          ENDIF.

          " Intentionally clear AFTER "open"
          CLEAR ls_node-children.

          walk(
            EXPORTING
              iv_path = iv_path && ls_node-name && `/`
            CHANGING
              cs_parent    = ls_node ).

          IF mi_filter->keep_node(
              is_node  = ls_node
              iv_visit = zif_abapgit_ajson_filter=>visit_type-close ) = abap_false.
            CONTINUE.
          ENDIF.

        WHEN OTHERS.
          zcx_abapgit_ajson_error=>raise( |Unexpected node type { ls_node-type }| ).
      ENDCASE.

      IF cs_parent IS SUPPLIED.
        cs_parent-children = cs_parent-children + 1.
        IF cs_parent-type = zif_abapgit_ajson=>node_type-array.
          ls_node-name  = |{ cs_parent-children }|.
          ls_node-index = cs_parent-children.
        ENDIF.
      ENDIF.
      INSERT ls_node INTO TABLE mr_dest_tree->*.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
