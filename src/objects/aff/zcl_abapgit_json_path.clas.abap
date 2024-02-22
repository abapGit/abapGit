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

  METHOD deserialize.
    DATA: lo_merged            TYPE REF TO zif_abapgit_ajson,
          lv_json_path         TYPE string,
          lo_deserialization_result TYPE ref to zif_abapgit_ajson,
          lx_ajson             TYPE REF TO zcx_abapgit_ajson_error.

    TRY.
        lo_merged = zcl_abapgit_ajson=>parse( `` ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.

    LOOP AT it_json_path INTO lv_json_path.

      TRY.
          lo_deserialization_result = lcl_json_path=>to_json( lv_json_path ).
        CATCH zcx_abapgit_ajson_error cx_sy_regex cx_sy_matcher.
          zcx_abapgit_exception=>raise( iv_text = `Failed to deserialize translation.` ).
      ENDTRY.

      TRY.
          lo_merged = zcl_abapgit_ajson_utilities=>new( )->merge( io_json_a = lo_merged
                                                                  io_json_b = lo_deserialization_result ).
        CATCH zcx_abapgit_ajson_error INTO lx_ajson.
          zcx_abapgit_exception=>raise_with_text( lx_ajson ).
      ENDTRY.

    ENDLOOP.

    TRY.
        rv_result = lo_merged->stringify( iv_indent = 2 ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
