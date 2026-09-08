CLASS zcl_abapgit_json_path DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: serialize
      IMPORTING iv_json          TYPE string
      RETURNING VALUE(rt_result) TYPE string_table
      RAISING   zcx_abapgit_exception.
    METHODS: deserialize
      IMPORTING it_json_path     TYPE string_table
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_json_path IMPLEMENTATION.


  METHOD deserialize.

    rv_result = lcl_json_path=>deserialize( it_json_path ).

  ENDMETHOD.


  METHOD serialize.
    DATA: lo_json_path    TYPE REF TO lcl_json_path,
          lv_json_xstring TYPE xstring,
          lt_root_path    TYPE string_table,
          lo_reader       TYPE REF TO if_sxml_reader,
          lx_parse_error  TYPE REF TO cx_sxml_parse_error.

    lv_json_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( iv_json ).
    lo_reader = cl_sxml_string_reader=>create( input = lv_json_xstring ).

    TRY.
        IF lo_reader->read_next_node( ) IS INITIAL.
          RETURN.
        ENDIF.
      CATCH cx_sxml_parse_error INTO lx_parse_error.
        zcx_abapgit_exception=>raise_with_text( lx_parse_error ).
    ENDTRY.

    APPEND `$` TO lt_root_path.

    CREATE OBJECT lo_json_path.
    lo_json_path->serialize_rec( EXPORTING io_reader     = lo_reader
                                           it_path       = lt_root_path
                                 CHANGING  ct_json_paths = rt_result ).
  ENDMETHOD.
ENDCLASS.
