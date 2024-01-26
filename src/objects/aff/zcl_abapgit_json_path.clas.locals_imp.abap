*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_json_path DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      serialize_rec
        IMPORTING io_reader     TYPE REF TO if_sxml_reader
                  it_path       TYPE string_table
        CHANGING  ct_json_paths TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      is_array
        IMPORTING io_reader        TYPE REF TO if_sxml_reader
        RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS:
      is_string
        IMPORTING io_reader        TYPE REF TO if_sxml_reader
        RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS:
      is_object
        IMPORTING io_reader        TYPE REF TO if_sxml_reader
        RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS:
      serialize_rec_array
        IMPORTING io_reader     TYPE REF TO if_sxml_reader
                  it_path       TYPE string_table
        CHANGING  ct_json_paths TYPE string_table.

ENDCLASS.

CLASS lcl_json_path IMPLEMENTATION.

  METHOD is_array.
    rv_result = xsdbool( io_reader->name = 'array' ).
  ENDMETHOD.

  METHOD is_string.
    rv_result = xsdbool( io_reader->name = 'str' ).
  ENDMETHOD.

  METHOD is_object.
    rv_result = xsdbool( io_reader->name = 'object' ).
  ENDMETHOD.

  METHOD serialize_rec.
    DATA: lt_new_path TYPE string_table,
          lv_key      TYPE string.

    lt_new_path = it_path.

    IF io_reader->read_next_node( ) IS INITIAL.
      RETURN.
    ENDIF.

    IF is_string( io_reader ) AND io_reader->node_type = if_sxml_node=>co_nt_element_open.
      " (1) 4 2
      APPEND io_reader->value TO lt_new_path.
      lv_key = concat_lines_of( table = lt_new_path
                                sep   = `.` ).

      io_reader->read_next_node( ).
      APPEND |{ lv_key }={ io_reader->value }| TO ct_json_paths.

      io_reader->read_next_node( ).
      DELETE lt_new_path INDEX lines( lt_new_path ).

      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_object( io_reader ) AND io_reader->node_type = if_sxml_node=>co_nt_element_open.
      " normal object, where name is object-name

      APPEND io_reader->value TO lt_new_path.
      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_array( io_reader ) AND io_reader->node_type = if_sxml_node=>co_nt_element_open.

      APPEND io_reader->value TO lt_new_path.
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF ( is_object( io_reader ) OR is_array( io_reader ) ) AND io_reader->node_type = if_sxml_node=>co_nt_element_close.

      DELETE lt_new_path INDEX lines( lt_new_path ).
      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ENDIF.

  ENDMETHOD.


  METHOD serialize_rec_array.
    DATA: lt_new_path         TYPE string_table,
          lv_key              TYPE string,
          lv_array_key        TYPE string,
          lv_new_path_element TYPE string,
          lv_gen_path         TYPE string.

    lt_new_path = it_path.

    io_reader->read_next_node( ). " object

    IF is_string( io_reader ) AND io_reader->node_type = if_sxml_node=>co_nt_element_open.
      " (1) 4 2
      APPEND io_reader->value TO lt_new_path.
      lv_key = concat_lines_of( table = lt_new_path
                                sep   = `.` ).

      io_reader->read_next_node( ).
      APPEND |{ lv_key }={ io_reader->value }| TO ct_json_paths.

      io_reader->read_next_node( ).
      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_object( io_reader ) AND io_reader->node_type = if_sxml_node=>co_nt_element_open.
      " object in array, where name is trash

      " array index - @.key==value
      " 1 4 2
      io_reader->read_next_node( ). " key
      lv_array_key = |[?(@.{ io_reader->value }|.
      io_reader->read_next_node( ). " value
      lv_new_path_element = |{ lv_array_key }=='{ io_reader->value }')]|.
      APPEND lv_new_path_element TO lt_new_path.
      io_reader->read_next_node( ). " close

      " 1 4 2
      io_reader->read_next_node( ). " key
      APPEND io_reader->value TO lt_new_path.
      io_reader->read_next_node( ). " value
      lv_gen_path = concat_lines_of( table = lt_new_path
                                     sep   = `.` ).
      APPEND |{ lv_gen_path }={ io_reader->value }| TO ct_json_paths.
      io_reader->read_next_node( ).

      DELETE lt_new_path INDEX lines( lt_new_path ).
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_array( io_reader ) AND io_reader->node_type = if_sxml_node=>co_nt_element_open.

      APPEND io_reader->value TO lt_new_path.
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF ( is_object( io_reader ) OR is_array( io_reader ) ) AND io_reader->node_type = if_sxml_node=>co_nt_element_close.

      DELETE lt_new_path INDEX lines( lt_new_path ).
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
