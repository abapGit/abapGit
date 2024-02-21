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
      is_string_open
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
    METHODS:
      get_json_path
        IMPORTING it_path          TYPE string_table
        RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_json_path IMPLEMENTATION.

  METHOD is_array.
    rv_result = boolc( io_reader->name = 'array' ).
  ENDMETHOD.

  METHOD is_string_open.
    rv_result = boolc( io_reader->name = 'str' AND io_reader->node_type = if_sxml_node=>co_nt_element_open ).
  ENDMETHOD.

  METHOD is_object.
    rv_result = boolc( io_reader->name = 'object' ).
  ENDMETHOD.

  METHOD serialize_rec.
    DATA: lt_new_path TYPE string_table,
          lv_key      TYPE string.

    lt_new_path = it_path.

    IF io_reader->read_next_node( ) IS INITIAL.
      RETURN.
    ENDIF.

    IF is_string_open( io_reader ) = abap_true.

      APPEND io_reader->value TO lt_new_path.
      lv_key = get_json_path( lt_new_path ).

      io_reader->read_next_node( ).
      lv_key = |{ lv_key }={ io_reader->value }|.
      APPEND lv_key TO ct_json_paths.

      io_reader->read_next_node( ).
      DELETE lt_new_path INDEX lines( lt_new_path ).

      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_object( io_reader ) = abap_true AND io_reader->node_type = if_sxml_node=>co_nt_element_open.

      APPEND io_reader->value TO lt_new_path.
      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_array( io_reader ) = abap_true AND io_reader->node_type = if_sxml_node=>co_nt_element_open.

      APPEND io_reader->value TO lt_new_path.
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF ( is_object( io_reader ) = abap_true OR is_array( io_reader ) = abap_true )
             AND io_reader->node_type = if_sxml_node=>co_nt_element_close.

      DELETE lt_new_path INDEX lines( lt_new_path ).
      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ENDIF.

  ENDMETHOD.


  METHOD serialize_rec_array.
    DATA: lt_new_path  TYPE string_table,
          lv_json_path TYPE string,
          lv_array_key TYPE string.

    lt_new_path = it_path.

    IF io_reader->read_next_node( ) IS INITIAL.
      RETURN.
    ENDIF.

    IF is_string_open( io_reader ) = abap_true.

      APPEND io_reader->value TO lt_new_path.
      lv_json_path = get_json_path( lt_new_path ).

      io_reader->read_next_node( ).
      lv_json_path = |{ lv_json_path }={ io_reader->value }|.
      APPEND lv_json_path TO ct_json_paths.
      io_reader->read_next_node( ).

      serialize_rec( EXPORTING io_reader     = io_reader
                               it_path       = lt_new_path
                     CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_object( io_reader ) = abap_true AND io_reader->node_type = if_sxml_node=>co_nt_element_open.

      io_reader->read_next_node( ).
      lv_array_key = io_reader->value.
      io_reader->read_next_node( ).
      lv_array_key = |[?(@.{ lv_array_key }=='{ io_reader->value }')]|.
      APPEND lv_array_key TO lt_new_path.
      io_reader->read_next_node( ).

      io_reader->read_next_node( ).
      APPEND io_reader->value TO lt_new_path.
      lv_json_path = get_json_path( lt_new_path ).

      io_reader->read_next_node( ).
      lv_json_path = |{ lv_json_path }={ io_reader->value }|.
      APPEND lv_json_path TO ct_json_paths.
      io_reader->read_next_node( ).

      DELETE lt_new_path INDEX lines( lt_new_path ).
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF is_array( io_reader ) = abap_true AND io_reader->node_type = if_sxml_node=>co_nt_element_open.

      APPEND io_reader->value TO lt_new_path.
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ELSEIF ( is_object( io_reader ) = abap_true OR is_array( io_reader ) = abap_true )
             AND io_reader->node_type = if_sxml_node=>co_nt_element_close.

      DELETE lt_new_path INDEX lines( lt_new_path ).
      serialize_rec_array( EXPORTING io_reader     = io_reader
                                     it_path       = lt_new_path
                           CHANGING  ct_json_paths = ct_json_paths ).

    ENDIF.

  ENDMETHOD.

  METHOD get_json_path.
    rv_result = concat_lines_of( table = it_path
                                 sep   = `.` ).
    REPLACE ALL OCCURRENCES OF `.[` IN rv_result WITH `[`.

  ENDMETHOD.

ENDCLASS.
