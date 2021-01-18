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

ENDCLASS.

CLASS lcl_json_parser IMPLEMENTATION.

  METHOD parse.
    DATA lx_sxml TYPE REF TO cx_sxml_error.
    TRY.
        rt_json_tree = _parse( iv_json ).
      CATCH cx_sxml_error INTO lx_sxml.
        zcx_abapgit_ajson_error=>raise( `SXML: ` && lx_sxml->get_text( ) ).
    ENDTRY.
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

    METHODS find_loc
      IMPORTING
        iv_path TYPE string
        iv_name TYPE string OPTIONAL " not mandatory
        iv_append_tables TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_ref) TYPE REF TO data
      RAISING
        zcx_abapgit_ajson_error.

    CLASS-METHODS bind
      IMPORTING
        !ii_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
      CHANGING
        c_obj              TYPE any
        co_instance        TYPE REF TO lcl_json_to_abap.

    METHODS to_abap
      IMPORTING
        it_nodes TYPE zif_abapgit_ajson=>ty_nodes_ts
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.
    DATA mr_obj TYPE REF TO data.
    DATA mi_custom_mapping TYPE REF TO zif_abapgit_ajson_mapping.

ENDCLASS.

CLASS lcl_json_to_abap IMPLEMENTATION.

  METHOD bind.
    CREATE OBJECT co_instance.
    GET REFERENCE OF c_obj INTO co_instance->mr_obj.
    co_instance->mi_custom_mapping = ii_custom_mapping.
  ENDMETHOD.

  METHOD to_abap.

    DATA lr_ref TYPE REF TO data.
    DATA lv_type TYPE c.
    DATA lx TYPE REF TO cx_root.
    FIELD-SYMBOLS <n> LIKE LINE OF it_nodes.
    FIELD-SYMBOLS <value> TYPE any.

    TRY.
        LOOP AT it_nodes ASSIGNING <n> USING KEY array_index.
          lr_ref = find_loc(
          iv_append_tables = abap_true
          iv_path = <n>-path
          iv_name = <n>-name ).
          ASSIGN lr_ref->* TO <value>.
          ASSERT sy-subrc = 0.
          DESCRIBE FIELD <value> TYPE lv_type.

          CASE <n>-type.
            WHEN zif_abapgit_ajson=>node_type-null.
            " Do nothing
            WHEN zif_abapgit_ajson=>node_type-boolean.
              <value> = boolc( <n>-value = 'true' ).
            WHEN zif_abapgit_ajson=>node_type-number.
              <value> = <n>-value.
            WHEN zif_abapgit_ajson=>node_type-string.
              IF lv_type = 'D' AND <n>-value IS NOT INITIAL.
                DATA lv_y TYPE c LENGTH 4.
                DATA lv_m TYPE c LENGTH 2.
                DATA lv_d TYPE c LENGTH 2.

                FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
                IN <n>-value
                SUBMATCHES lv_y lv_m lv_d.
                IF sy-subrc <> 0.
                  zcx_abapgit_ajson_error=>raise(
                  iv_msg      = 'Unexpected date format'
                  iv_location = <n>-path && <n>-name ).
                ENDIF.
                CONCATENATE lv_y lv_m lv_d INTO <value>.
              ELSE.
                <value> = <n>-value.
              ENDIF.
            WHEN zif_abapgit_ajson=>node_type-object.
              IF NOT lv_type CO 'uv'.
                zcx_abapgit_ajson_error=>raise(
                iv_msg      = 'Expected structure'
                iv_location = <n>-path && <n>-name ).
              ENDIF.
            WHEN zif_abapgit_ajson=>node_type-array.
              IF NOT lv_type CO 'h'.
                zcx_abapgit_ajson_error=>raise(
                iv_msg      = 'Expected table'
                iv_location = <n>-path && <n>-name ).
              ENDIF.
            WHEN OTHERS.
              zcx_abapgit_ajson_error=>raise(
              iv_msg      = |Unexpected JSON type [{ <n>-type }]|
              iv_location = <n>-path && <n>-name ).
          ENDCASE.

        ENDLOOP.
      CATCH cx_sy_conversion_no_number INTO lx.
        zcx_abapgit_ajson_error=>raise(
        iv_msg      = |Source is not a number|
        iv_location = <n>-path && <n>-name ).
    ENDTRY.

  ENDMETHOD.

  METHOD find_loc.

    DATA lt_path TYPE string_table.
    DATA lv_trace TYPE string.
    DATA lv_seg LIKE LINE OF lt_path.
    DATA lv_type TYPE c.
    DATA lv_size TYPE i.
    DATA lv_index TYPE i.
    FIELD-SYMBOLS <struc> TYPE any.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <value> TYPE any.
    FIELD-SYMBOLS <seg> LIKE LINE OF lt_path.

    SPLIT iv_path AT '/' INTO TABLE lt_path.
    DELETE lt_path WHERE table_line IS INITIAL.
    IF iv_name IS NOT INITIAL.
      APPEND iv_name TO lt_path.
    ENDIF.

    r_ref = mr_obj.

    LOOP AT lt_path ASSIGNING <seg>.
      lv_trace = lv_trace && '/' && <seg>.

      IF mi_custom_mapping IS BOUND.
        lv_seg = mi_custom_mapping->to_abap( iv_path = iv_path
                                             iv_name = <seg> ).
      ELSE.
        CLEAR lv_seg.
      ENDIF.

      IF lv_seg IS INITIAL.
        lv_seg = to_upper( <seg> ).
      ELSE.
        lv_seg = to_upper( lv_seg ).
      ENDIF.

      ASSIGN r_ref->* TO <struc>.
      ASSERT sy-subrc = 0.
      DESCRIBE FIELD <struc> TYPE lv_type.

      IF lv_type CA 'lr'. " data/obj ref
        " TODO maybe in future
        zcx_abapgit_ajson_error=>raise(
          iv_msg      = 'Cannot assign to ref'
          iv_location = lv_trace ).

      ELSEIF lv_type = 'h'. " table
        IF NOT lv_seg CO '0123456789'.
          zcx_abapgit_ajson_error=>raise(
            iv_msg      = 'Need index to access tables'
            iv_location = lv_trace ).
        ENDIF.
        lv_index = lv_seg.
        ASSIGN r_ref->* TO <table>.
        ASSERT sy-subrc = 0.

        lv_size = lines( <table> ).
        IF iv_append_tables = abap_true AND lv_index = lv_size + 1.
          APPEND INITIAL LINE TO <table>.
        ENDIF.

        READ TABLE <table> INDEX lv_index ASSIGNING <value>.
        IF sy-subrc <> 0.
          zcx_abapgit_ajson_error=>raise(
            iv_msg      = 'Index not found in table'
            iv_location = lv_trace ).
        ENDIF.

      ELSEIF lv_type CA 'uv'. " structure
        ASSIGN COMPONENT lv_seg OF STRUCTURE <struc> TO <value>.
        IF sy-subrc <> 0.
          zcx_abapgit_ajson_error=>raise(
            iv_msg      = 'Path not found'
            iv_location = lv_trace ).
        ENDIF.
      ELSE.
        zcx_abapgit_ajson_error=>raise(
          iv_msg = 'Target is not deep'
          iv_location = lv_trace ).
      ENDIF.
      GET REFERENCE OF <value> INTO r_ref.
    ENDLOOP.

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
        ct_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.

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
    lo_converter->mi_custom_mapping = ii_custom_mapping.
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

    FIELD-SYMBOLS <n> LIKE LINE OF ct_nodes.

    ct_nodes = io_json->mt_json_tree.

    LOOP AT ct_nodes ASSIGNING <n>.
      IF <n>-path IS INITIAL AND <n>-name IS INITIAL. " root node
        <n>-path  = is_prefix-path.
        <n>-name  = is_prefix-name.
        <n>-index = iv_index.
      ELSE.
        <n>-path = is_prefix-path && is_prefix-name && <n>-path.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD convert_value.

    FIELD-SYMBOLS <n> LIKE LINE OF ct_nodes.

    APPEND INITIAL LINE TO ct_nodes ASSIGNING <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.
    <n>-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      <n>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path
                                             iv_name = is_prefix-name ).
    ENDIF.

    IF <n>-name IS INITIAL.
      <n>-name  = is_prefix-name.
    ENDIF.

    IF io_type->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL' OR io_type->absolute_name = '\TYPE=XFELD'.
      <n>-type = zif_abapgit_ajson=>node_type-boolean.
      IF iv_data IS NOT INITIAL.
        <n>-value = 'true'.
      ELSE.
        <n>-value = 'false'.
      ENDIF.
    ELSEIF io_type->type_kind CO 'CNgXyDT'. " Char like, date/time, xstring
      <n>-type = zif_abapgit_ajson=>node_type-string.
      <n>-value = |{ iv_data }|.
    ELSEIF io_type->type_kind CO 'bsI8PaeF'. " Numeric
      <n>-type = zif_abapgit_ajson=>node_type-number.
      <n>-value = |{ iv_data }|.
    ELSE.
      zcx_abapgit_ajson_error=>raise( |Unexpected elemetary type [{
        io_type->type_kind }] @{ is_prefix-path && is_prefix-name }| ).
    ENDIF.

  ENDMETHOD.

  METHOD convert_ref.

    FIELD-SYMBOLS <n> LIKE LINE OF ct_nodes.

    APPEND INITIAL LINE TO ct_nodes ASSIGNING <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.
    <n>-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      <n>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path
                                             iv_name = is_prefix-name ).
    ENDIF.

    IF <n>-name IS INITIAL.
      <n>-name  = is_prefix-name.
    ENDIF.

    IF iv_data IS INITIAL.
      <n>-type  = zif_abapgit_ajson=>node_type-null.
      <n>-value = 'null'.
    ELSE.
      " TODO support data references
      zcx_abapgit_ajson_error=>raise( |Unexpected reference @{ is_prefix-path && is_prefix-name }| ).
    ENDIF.

  ENDMETHOD.

  METHOD convert_struc.

    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    DATA lt_comps TYPE cl_abap_structdescr=>component_table.
    DATA ls_next_prefix LIKE is_prefix.
    DATA lv_item_order TYPE i.

    FIELD-SYMBOLS <root> LIKE LINE OF ct_nodes.
    FIELD-SYMBOLS <c> LIKE LINE OF lt_comps.
    FIELD-SYMBOLS <val> TYPE any.

    lo_struc ?= io_type.
    lt_comps = lo_struc->get_components( ).
    " get_components is potentially much slower than lo_struc->components
    " but ! we still need it to identify booleans
    " and rtti seems to cache type descriptions really well (https://github.com/sbcgua/benchmarks.git)
    " the structures will be repeated in real life

    IF cs_root IS SUPPLIED. " call for include structure
      ASSIGN cs_root TO <root>.
    ELSE. " First call
      APPEND INITIAL LINE TO ct_nodes ASSIGNING <root>.
      <root>-path  = is_prefix-path.
      <root>-name  = is_prefix-name.
      <root>-type  = zif_abapgit_ajson=>node_type-object.
      <root>-index = iv_index.

      IF mi_custom_mapping IS BOUND.
        <root>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path
                                                  iv_name = is_prefix-name ).
      ENDIF.

      IF <root>-name IS INITIAL.
        <root>-name  = is_prefix-name.
      ENDIF.

      <root>-order = iv_item_order.
    ENDIF.

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

    FIELD-SYMBOLS <root> LIKE LINE OF ct_nodes.
    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <val> TYPE any.

    lo_table ?= io_type.
    lo_ltype = lo_table->get_table_line_type( ).

    APPEND INITIAL LINE TO ct_nodes ASSIGNING <root>.
    <root>-path  = is_prefix-path.
    <root>-name  = is_prefix-name.
    <root>-type  = zif_abapgit_ajson=>node_type-array.
    <root>-index = iv_index.
    <root>-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      <root>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path
                                                iv_name = is_prefix-name ).
    ENDIF.

    IF <root>-name IS INITIAL.
      <root>-name  = is_prefix-name.
    ENDIF.

    ls_next_prefix-path = is_prefix-path && is_prefix-name && '/'.
    ASSIGN iv_data TO <tab>.

    LOOP AT <tab> ASSIGNING <val>.
      ls_next_prefix-name = to_lower( |{ sy-tabix }| ).

      convert_any(
        EXPORTING
          iv_data   = <val>
          io_type   = lo_ltype
          is_prefix = ls_next_prefix
          iv_index  = <root>-children + 1
        CHANGING
          ct_nodes = ct_nodes ).

      <root>-children = <root>-children + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD insert_with_type.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO lcl_abap_to_json.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    CREATE OBJECT lo_converter.
    lo_converter->mi_custom_mapping = ii_custom_mapping.
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

    FIELD-SYMBOLS <n> LIKE LINE OF ct_nodes.

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

    APPEND INITIAL LINE TO ct_nodes ASSIGNING <n>.

    <n>-path  = is_prefix-path.
    <n>-name  = is_prefix-name.
    <n>-index = iv_index.
    <n>-value = iv_data.
    <n>-type  = iv_type.
    <n>-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      <n>-name = mi_custom_mapping->to_json( iv_path = is_prefix-path
                                             iv_name = is_prefix-name ).
    ENDIF.

    IF <n>-name IS INITIAL.
      <n>-name  = is_prefix-name.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
