**********************************************************************
* UTILS
**********************************************************************

class lcl_utils definition final.
  public section.

    class-methods normalize_path
      importing
        iv_path type string
      returning
        value(rv_path) type string.
    class-methods split_path
      importing
        iv_path type string
      returning
        value(rv_path_name) type zcl_abapgit_ajson=>ty_path_name.

endclass.

class lcl_utils implementation.

  method normalize_path.

    rv_path = iv_path.
    if strlen( rv_path ) = 0.
      rv_path = '/'.
    endif.
    if rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    endif.
    if substring( val = rv_path off = strlen( rv_path ) - 1 ) <> '/'.
      rv_path = rv_path && '/'.
    endif.

  endmethod.

  method split_path.

    data lv_offs type i.
    data lv_len type i.
    data lv_trim_slash type i.

    lv_len = strlen( iv_path ).
    if lv_len = 0 or iv_path = '/'.
      return. " empty path is the alias for root item = '' + ''
    endif.

    if substring( val = iv_path off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    endif.

    lv_offs = find( val = reverse( iv_path ) sub = '/' off = lv_trim_slash ).
    if lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    endif.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring( val = iv_path len = lv_offs ) ).
    rv_path_name-name = substring( val = iv_path off = lv_offs len = lv_len - lv_offs - lv_trim_slash ).

  endmethod.

endclass.


**********************************************************************
* PARSER
**********************************************************************

class lcl_json_parser definition final.
  public section.

    methods parse
      importing
        iv_json type string
      returning
        value(rt_json_tree) type zcl_abapgit_ajson=>ty_nodes_tt
      raising
        zcx_abapgit_ajson_error.

  private section.

    types:
      ty_stack_tt type standard table of ref to zcl_abapgit_ajson=>ty_node.

    data mt_stack type ty_stack_tt.

    class-methods join_path
      importing
        it_stack type ty_stack_tt
      returning
        value(rv_path) type string.

    methods raise
      importing
        iv_error type string
      raising
        zcx_abapgit_ajson_error.

endclass.

class lcl_json_parser implementation.

  method parse.

    data lo_reader type ref to if_sxml_reader.
    data lr_stack_top like line of mt_stack.
    data lo_node type ref to if_sxml_node.
    field-symbols <item> like line of rt_json_tree.

    clear mt_stack.
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    do.
      lo_node = lo_reader->read_next_node( ).
      if lo_node is not bound.
        exit.
      endif.


      case lo_node->type.
        when if_sxml_node=>co_nt_element_open.
          data lt_attributes type if_sxml_attribute=>attributes.
          data lo_attr like line of lt_attributes.
          data lo_open type ref to if_sxml_open_element.
          lo_open ?= lo_node.

          append initial line to rt_json_tree assigning <item>.

          <item>-type = to_lower( lo_open->qname-name ).

          read table mt_stack index 1 into lr_stack_top.
          if sy-subrc = 0.
            <item>-path = join_path( mt_stack ).
            lr_stack_top->children = lr_stack_top->children + 1.

            if lr_stack_top->type = 'array'.
              <item>-name = |{ lr_stack_top->children }|.
              <item>-index = lr_stack_top->children.
            else.
              lt_attributes = lo_open->get_attributes( ).
              loop at lt_attributes into lo_attr.
                if lo_attr->qname-name = 'name' and lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <item>-name = lo_attr->get_value( ).
                endif.
              endloop.
            endif.
          endif.

          get reference of <item> into lr_stack_top.
          insert lr_stack_top into mt_stack index 1.

        when if_sxml_node=>co_nt_element_close.
          data lo_close type ref to if_sxml_close_element.
          lo_close ?= lo_node.

          read table mt_stack index 1 into lr_stack_top.
          delete mt_stack index 1.
          if lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          endif.

        when if_sxml_node=>co_nt_value.
          data lo_value type ref to if_sxml_value_node.
          lo_value ?= lo_node.

          <item>-value = lo_value->get_value( ).

        when others.
          raise( 'Unexpected node type' ).
      endcase.
    enddo.

    if lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    endif.

  endmethod.

  method join_path.

    field-symbols <ref> like line of it_stack.

    loop at it_stack assigning <ref>.
      rv_path = <ref>->name && '/' && rv_path.
    endloop.

  endmethod.

  method raise.

    zcx_abapgit_ajson_error=>raise_json(
      iv_location = join_path( mt_stack )
      iv_msg      = |JSON PARSER: { iv_error } @ { join_path( mt_stack ) }| ).

  endmethod.

endclass.


**********************************************************************
* JSON_TO_ABAP
**********************************************************************

class lcl_json_to_abap definition final.
  public section.

    methods find_loc
      importing
        iv_path type string
        iv_name type string optional " not mandatory
        iv_append_tables type abap_bool default abap_false
      returning
        value(r_ref) type ref to data
      raising
        zcx_abapgit_ajson_error.

    class-methods bind
      changing
        c_obj type any
        co_instance type ref to lcl_json_to_abap.

    methods to_abap
      importing
        it_nodes type zcl_abapgit_ajson=>ty_nodes_ts
      raising
        zcx_abapgit_ajson_error.

  private section.
    data mr_obj type ref to data.
endclass.

class lcl_json_to_abap implementation.

  method bind.
    create object co_instance.
    get reference of c_obj into co_instance->mr_obj.
  endmethod.

  method to_abap.

    data ref type ref to data.
    data lv_type type c.
    data lx type ref to cx_root.
    field-symbols <n> like line of it_nodes.
    field-symbols <value> type any.

    try.
      loop at it_nodes assigning <n> using key array_index.
        ref = find_loc(
          iv_append_tables = abap_true
          iv_path = <n>-path
          iv_name = <n>-name ).
        assign ref->* to <value>.
        assert sy-subrc = 0.
        describe field <value> type lv_type.

        case <n>-type.
          when 'null'.
            " Do nothing
          when 'bool'.
            <value> = boolc( <n>-value = 'true' ).
          when 'num'.
            <value> = <n>-value.
          when 'str'.
            <value> = <n>-value.
          when 'object'.
            if not lv_type co 'uv'.
             zcx_abapgit_ajson_error=>raise_json(
               iv_msg      = 'Expected structure'
               iv_location = <n>-path && <n>-name ).
            endif.
          when 'array'.
            if not lv_type co 'h'.
              zcx_abapgit_ajson_error=>raise_json(
                iv_msg      = 'Expected table'
                iv_location = <n>-path && <n>-name ).
            endif.
          when others.
           zcx_abapgit_ajson_error=>raise_json(
             iv_msg      = |Unexpected JSON type [{ <n>-type }]|
             iv_location = <n>-path && <n>-name ).
        endcase.

      endloop.
    catch cx_sy_conversion_no_number into lx.
      zcx_abapgit_ajson_error=>raise_json(
        iv_msg      = |Source is not a number|
        iv_location = <n>-path && <n>-name ).
    endtry.

  endmethod.

  method find_loc.

    data lt_path type string_table.
    data lv_trace type string.
    data lv_type type c.
    data lv_size type i.
    data lv_index type i.
    field-symbols <struc> type any.
    field-symbols <table> type standard table.
    field-symbols <value> type any.
    field-symbols <seg> like line of lt_path.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.
    if iv_name is not initial.
      append iv_name to lt_path.
    endif.

    r_ref = mr_obj.

    loop at lt_path assigning <seg>.
      lv_trace = lv_trace && '/' && <seg>.
      <seg> = to_upper( <seg> ).

      assign r_ref->* to <struc>.
      assert sy-subrc = 0.
      describe field <struc> type lv_type.

      if lv_type ca 'lr'. " data/obj ref
        " TODO maybe in future
        zcx_abapgit_ajson_error=>raise_json(
          iv_msg      = 'Cannot assign to ref'
          iv_location = lv_trace ).

      elseif lv_type = 'h'. " table
        if not <seg> co '0123456789'.
          zcx_abapgit_ajson_error=>raise_json(
            iv_msg      = 'Need index to access tables'
            iv_location = lv_trace ).
        endif.
        lv_index = <seg>.
        assign r_ref->* to <table>.
        assert sy-subrc = 0.

        lv_size = lines( <table> ).
        if iv_append_tables = abap_true and lv_index = lv_size + 1.
          append initial line to <table>.
        endif.

        read table <table> index lv_index assigning <value>.
        if sy-subrc <> 0.
          zcx_abapgit_ajson_error=>raise_json(
            iv_msg      = 'Index not found in table'
            iv_location = lv_trace ).
        endif.

      elseif lv_type ca 'uv'. " structure
        assign component <seg> of structure <struc> to <value>.
        if sy-subrc <> 0.
          zcx_abapgit_ajson_error=>raise_json(
            iv_msg      =  'Path not found'
            iv_location = lv_trace ).
        endif.
      else.
        zcx_abapgit_ajson_error=>raise_json(
          iv_msg = 'Target is not deep'
          iv_location = lv_trace ).
      endif.
      get reference of <value> into r_ref.
    endloop.

  endmethod.

endclass.
