**********************************************************************
* UTILS
**********************************************************************

CLASS lcl_utils DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS normalize_path
      IMPORTING
        iv_path        TYPE string
      RETURNING
        VALUE(rv_path) TYPE string.
    CLASS-METHODS split_path
      IMPORTING
        iv_path             TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE zcl_abapgit_ajson=>ty_path_name.

ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.

  METHOD normalize_path.

    rv_path = iv_path.
    IF strlen( rv_path ) = 0.
      rv_path = '/'.
    ENDIF.
    IF rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    ENDIF.
    IF substring(
      val = rv_path
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

    IF substring(
      val = iv_path
      off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    ENDIF.

    lv_offs = find(
      val = reverse( iv_path )
      sub = '/'
      off = lv_trim_slash ).
    IF lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    ENDIF.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring(
      val = iv_path
      len = lv_offs ) ).
    rv_path_name-name = substring(
      val = iv_path
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
        iv_json             TYPE string
      RETURNING
        VALUE(rt_json_tree) TYPE zcl_abapgit_ajson=>ty_nodes_tt
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.

    TYPES:
      ty_stack_tt TYPE STANDARD TABLE OF REF TO zcl_abapgit_ajson=>ty_node.

    DATA mt_stack TYPE ty_stack_tt.

    CLASS-METHODS join_path
      IMPORTING
        it_stack       TYPE ty_stack_tt
      RETURNING
        VALUE(rv_path) TYPE string.

    METHODS raise
      IMPORTING
        iv_error TYPE string
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_json_parser IMPLEMENTATION.

  METHOD parse.

    DATA lo_reader TYPE REF TO if_sxml_reader.
    DATA lr_stack_top LIKE LINE OF mt_stack.
    DATA lo_node TYPE REF TO if_sxml_node.
    FIELD-SYMBOLS <ls_item> LIKE LINE OF rt_json_tree.
    DATA lt_attributes TYPE if_sxml_attribute=>attributes.
    DATA lo_attr LIKE LINE OF lt_attributes.
    DATA lo_open TYPE REF TO if_sxml_open_element.
    DATA lo_close TYPE REF TO if_sxml_close_element.
    DATA lo_value TYPE REF TO if_sxml_value_node.

    CLEAR mt_stack.
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    DO.
      lo_node = lo_reader->read_next_node( ).
      IF lo_node IS NOT BOUND.
        EXIT.
      ENDIF.


      CASE lo_node->type.
        WHEN if_sxml_node=>co_nt_element_open.
          lo_open ?= lo_node.

          APPEND INITIAL LINE TO rt_json_tree ASSIGNING <ls_item>.

          <ls_item>-type = to_lower( lo_open->qname-name ).

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          IF sy-subrc = 0.
            <ls_item>-path = join_path( mt_stack ).
            lr_stack_top->children = lr_stack_top->children + 1.

            IF lr_stack_top->type = 'array'.
              <ls_item>-name = |{ lr_stack_top->children }|.
              <ls_item>-index = lr_stack_top->children.
            ELSE.
              lt_attributes = lo_open->get_attributes( ).
              LOOP AT lt_attributes INTO lo_attr.
                IF lo_attr->qname-name = 'name' AND lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <ls_item>-name = lo_attr->get_value( ).
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          GET REFERENCE OF <ls_item> INTO lr_stack_top.
          INSERT lr_stack_top INTO mt_stack INDEX 1.

        WHEN if_sxml_node=>co_nt_element_close.
          lo_close ?= lo_node.

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          DELETE mt_stack INDEX 1.
          IF lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          ENDIF.

        WHEN if_sxml_node=>co_nt_value.
          lo_value ?= lo_node.

          <ls_item>-value = lo_value->get_value( ).

        WHEN OTHERS.
          raise( 'Unexpected node type' ).
      ENDCASE.
    ENDDO.

    IF lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    ENDIF.

  ENDMETHOD.

  METHOD join_path.

    FIELD-SYMBOLS <ls_ref> LIKE LINE OF it_stack.

    LOOP AT it_stack ASSIGNING <ls_ref>.
      rv_path = <ls_ref>->name && '/' && rv_path.
    ENDLOOP.

  ENDMETHOD.

  METHOD raise.

    zcx_abapgit_ajson_error=>raise_json(
      iv_location = join_path( mt_stack )
      iv_msg      = |JSON PARSER: { iv_error } @ { join_path( mt_stack ) }| ).

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* JSON_TO_ABAP
**********************************************************************

CLASS lcl_json_to_abap DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS find_loc
      IMPORTING
        iv_path          TYPE string
        iv_name          TYPE string OPTIONAL " not mandatory
        iv_append_tables TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_ref)    TYPE REF TO data
      RAISING
        zcx_abapgit_ajson_error.

    CLASS-METHODS bind
      CHANGING
        cv_obj       TYPE any
        co_instance TYPE REF TO lcl_json_to_abap.

    METHODS to_abap
      IMPORTING
        it_nodes TYPE zcl_abapgit_ajson=>ty_nodes_ts
      RAISING
        zcx_abapgit_ajson_error.

  PRIVATE SECTION.
    DATA mr_obj TYPE REF TO data.
ENDCLASS.

CLASS lcl_json_to_abap IMPLEMENTATION.

  METHOD bind.
    CREATE OBJECT co_instance.
    GET REFERENCE OF cv_obj INTO co_instance->mr_obj.
  ENDMETHOD.

  METHOD to_abap.

    DATA lv_ref TYPE REF TO data.
    DATA lv_type TYPE c.
    DATA lo_x TYPE REF TO cx_root.
    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    FIELD-SYMBOLS <ls_n> LIKE LINE OF it_nodes.
    FIELD-SYMBOLS <lv_value> TYPE any.

    TRY.
        LOOP AT it_nodes ASSIGNING <ls_n> USING KEY array_index.
          lv_ref = find_loc(
            iv_append_tables = abap_true
            iv_path = <ls_n>-path
            iv_name = <ls_n>-name ).
          ASSIGN lv_ref->* TO <lv_value>.
          ASSERT sy-subrc = 0.
          DESCRIBE FIELD <lv_value> TYPE lv_type.

          CASE <ls_n>-type.
            WHEN 'null'.
              " Do nothing
            WHEN 'bool'.
              <lv_value> = boolc( <ls_n>-value = 'true' ).
            WHEN 'num'.
              <lv_value> = <ls_n>-value.
            WHEN 'str'.
              IF lv_type = 'D' AND <ls_n>-value IS NOT INITIAL.
                FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
                  IN <ls_n>-value
                  SUBMATCHES lv_y lv_m lv_d.
                IF sy-subrc <> 0.
                  zcx_abapgit_ajson_error=>raise_json(
                    iv_msg      = 'Unexpected date format'
                    iv_location = <ls_n>-path && <ls_n>-name ).
                ENDIF.
                CONCATENATE lv_y lv_m lv_d INTO <lv_value>.
              ELSE.
                <lv_value> = <ls_n>-value.
              ENDIF.
            WHEN 'object'.
              IF NOT lv_type CO 'uv'.
                zcx_abapgit_ajson_error=>raise_json(
                  iv_msg      = 'Expected structure'
                  iv_location = <ls_n>-path && <ls_n>-name ).
              ENDIF.
            WHEN 'array'.
              IF NOT lv_type CO 'h'.
                zcx_abapgit_ajson_error=>raise_json(
                  iv_msg      = 'Expected table'
                  iv_location = <ls_n>-path && <ls_n>-name ).
              ENDIF.
            WHEN OTHERS.
              zcx_abapgit_ajson_error=>raise_json(
                iv_msg      = |Unexpected JSON type [{ <ls_n>-type }]|
                iv_location = <ls_n>-path && <ls_n>-name ).
          ENDCASE.

        ENDLOOP.
      CATCH cx_sy_conversion_no_number INTO lo_x.
        zcx_abapgit_ajson_error=>raise_json(
          iv_msg      = |Source is not a number|
          iv_location = <ls_n>-path && <ls_n>-name ).
    ENDTRY.

  ENDMETHOD.

  METHOD find_loc.

    DATA lt_path TYPE string_table.
    DATA lv_trace TYPE string.
    DATA lv_type TYPE c.
    DATA lv_size TYPE i.
    DATA lv_index TYPE i.
    FIELD-SYMBOLS <ls_struc> TYPE any.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lv_value> TYPE any.
    FIELD-SYMBOLS <lv_seg> LIKE LINE OF lt_path.

    SPLIT iv_path AT '/' INTO TABLE lt_path.
    DELETE lt_path WHERE table_line IS INITIAL.
    IF iv_name IS NOT INITIAL.
      APPEND iv_name TO lt_path.
    ENDIF.

    rv_ref = mr_obj.

    LOOP AT lt_path ASSIGNING <lv_seg>.
      lv_trace = lv_trace && '/' && <lv_seg>.
      <lv_seg> = to_upper( <lv_seg> ).

      ASSIGN rv_ref->* TO <ls_struc>.
      ASSERT sy-subrc = 0.
      DESCRIBE FIELD <ls_struc> TYPE lv_type.

      IF lv_type CA 'lr'. " data/obj ref
        " TODO maybe in future
        zcx_abapgit_ajson_error=>raise_json(
          iv_msg      = 'Cannot assign to ref'
          iv_location = lv_trace ).

      ELSEIF lv_type = 'h'. " table
        IF NOT <lv_seg> CO '0123456789'.
          zcx_abapgit_ajson_error=>raise_json(
            iv_msg      = 'Need index to access tables'
            iv_location = lv_trace ).
        ENDIF.
        lv_index = <lv_seg>.
        ASSIGN rv_ref->* TO <lt_table>.
        ASSERT sy-subrc = 0.

        lv_size = lines( <lt_table> ).
        IF iv_append_tables = abap_true AND lv_index = lv_size + 1.
          APPEND INITIAL LINE TO <lt_table>.
        ENDIF.

        READ TABLE <lt_table> INDEX lv_index ASSIGNING <lv_value>.
        IF sy-subrc <> 0.
          zcx_abapgit_ajson_error=>raise_json(
            iv_msg      = 'Index not found in table'
            iv_location = lv_trace ).
        ENDIF.

      ELSEIF lv_type CA 'uv'. " structure
        ASSIGN COMPONENT <lv_seg> OF STRUCTURE <ls_struc> TO <lv_value>.
        IF sy-subrc <> 0.
          zcx_abapgit_ajson_error=>raise_json(
            iv_msg      =  'Path not found'
            iv_location = lv_trace ).
        ENDIF.
      ELSE.
        zcx_abapgit_ajson_error=>raise_json(
          iv_msg = 'Target is not deep'
          iv_location = lv_trace ).
      ENDIF.
      GET REFERENCE OF <lv_value> INTO rv_ref.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
