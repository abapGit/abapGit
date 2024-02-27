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

    CLASS-METHODS: deserialize
      IMPORTING it_json_path     TYPE string_table
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_abapgit_exception.

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
    CLASS-METHODS get_path_elements
      IMPORTING iv_path          TYPE string
      RETURNING VALUE(rt_result) TYPE string_table
      RAISING   cx_sy_matcher
                cx_sy_regex.
    CLASS-METHODS build_json
      IMPORTING it_path_elements TYPE string_table
                iv_value         TYPE string
      CHANGING  cv_json_string   TYPE string.
    CLASS-METHODS path_contains_array
      IMPORTING iv_path          TYPE string
      RETURNING VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS: to_json
      IMPORTING iv_json_path     TYPE string
      RETURNING VALUE(ro_result) TYPE REF TO zcl_abapgit_ajson
      RAISING   zcx_abapgit_ajson_error
                cx_sy_regex
                cx_sy_matcher.

ENDCLASS.

CLASS lcl_json_path IMPLEMENTATION.

  METHOD to_json.
    DATA: lo_matcher       TYPE REF TO cl_abap_matcher,
          lv_path          TYPE string,
          lv_value         TYPE string,
          lt_path_elements TYPE string_table,
          lv_json          TYPE string.


    lo_matcher = cl_abap_regex=>create_pcre( `(.*)=(.*$)` )->create_matcher( text = iv_json_path ).

    IF lo_matcher->match( ) = abap_true.
      lv_path  = lo_matcher->get_submatch( 1 ).
      lv_value = lo_matcher->get_submatch( 2 ).
    ENDIF.

    IF path_contains_array( lv_path ) = abap_true.

      lt_path_elements = get_path_elements( lv_path ).
      build_json( EXPORTING it_path_elements = lt_path_elements
                            iv_value         = lv_value
                  CHANGING  cv_json_string   = lv_json ).

      ro_result = zcl_abapgit_ajson=>parse( lv_json ).
    ELSE.

      REPLACE FIRST OCCURRENCE OF '$.' IN lv_path WITH ''.
      REPLACE '.' IN lv_path WITH '/'.
      ro_result = zcl_abapgit_ajson=>create_empty( iv_keep_item_order = abap_true ).
      ro_result->set( iv_path = lv_path
                      iv_val  = lv_value ).
    ENDIF.


  ENDMETHOD.

  METHOD path_contains_array.
    DATA lv_array_pattern TYPE string VALUE `.*(\[.*?\]).*`.
    rv_result = xsdbool( matches( val = iv_path
                                  pcre = lv_array_pattern ) ).
  ENDMETHOD.

  METHOD build_json.
    DATA: lt_new_path_element TYPE string_table,
          lv_sub_match        TYPE string,
          lv_key_name         TYPE string,
          lv_key_value        TYPE string,
          lv_name             TYPE string,
          lv_first_elem       TYPE string.

    lt_new_path_element = it_path_elements.

    IF lines( lt_new_path_element ) = 0.
      RETURN.
    ENDIF.

    READ TABLE lt_new_path_element INTO lv_first_elem INDEX 1.

    IF lv_first_elem = `$`. " is root level

      DELETE lt_new_path_element INDEX 1.
      build_json( EXPORTING it_path_elements = lt_new_path_element
                            iv_value         = iv_value
                  CHANGING  cv_json_string   = cv_json_string ).

    ELSE.
      IF matches( val = lv_first_elem
                    pcre = `^.\w+` ). " string start with .

        lv_name = lv_first_elem.
        cv_json_string = cv_json_string && | \{"{ lv_name+1 }": |.

        DELETE lt_new_path_element INDEX 1.

        build_json( EXPORTING it_path_elements = lt_new_path_element
                              iv_value         = iv_value
                    CHANGING  cv_json_string   = cv_json_string ).

        cv_json_string = cv_json_string && ` }`.

      ELSE. " is array

        FIND PCRE `(\[.*?\])` IN lv_first_elem SUBMATCHES lv_sub_match.
        FIND PCRE `(\w+)(?==='([^']*)')` IN lv_sub_match SUBMATCHES lv_key_name lv_key_value.
        READ TABLE lt_new_path_element INTO lv_name INDEX 2.


        DELETE lt_new_path_element INDEX 1.
        DELETE lt_new_path_element INDEX 1.

        IF lines( lt_new_path_element ) = 0.

          cv_json_string = cv_json_string &&
            |[ \{ "{ lv_key_name }": "{ lv_key_value }", "{ lv_name+1 }": "{ iv_value }"\} ]|.

        ELSE.

          cv_json_string = cv_json_string && |[ \{ "{ lv_key_name }": "{ lv_key_value }", "{ lv_name+1 }":|.

          build_json( EXPORTING it_path_elements = lt_new_path_element
                                iv_value         = iv_value
                      CHANGING  cv_json_string   = cv_json_string ).

          cv_json_string = cv_json_string && `} ] `.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_path_elements.
    DATA: lv_pcre_pattern TYPE string,
          lo_matcher TYPE REF TO cl_abap_matcher,
          lt_match_result TYPE match_result_tab,
          lv_match TYPE match_result,
          lv_hit TYPE string.

    lv_pcre_pattern = `(^\$)|(\.\w+)|(\[.*?\])|\.(\w+)`.

    lo_matcher = cl_abap_regex=>create_pcre( lv_pcre_pattern )->create_matcher( text = iv_path ).
    lt_match_result = lo_matcher->find_all( ).

    LOOP AT lt_match_result INTO lv_match.
      lv_hit = substring( val = iv_path
                          off = lv_match-offset
                          len = lv_match-length ).
      APPEND lv_hit TO rt_result.
    ENDLOOP.

  ENDMETHOD.

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

  METHOD deserialize.

    DATA: lo_merged                 TYPE REF TO zif_abapgit_ajson,
          lv_json_path              TYPE string,
          lo_deserialization_result TYPE REF TO zif_abapgit_ajson,
          lx_ajson                  TYPE REF TO zcx_abapgit_ajson_error.

    TRY.
        lo_merged = zcl_abapgit_ajson=>parse( `` ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.

    LOOP AT it_json_path INTO lv_json_path.

      TRY.
          lo_deserialization_result = to_json( lv_json_path ).
        CATCH zcx_abapgit_ajson_error cx_sy_regex cx_sy_matcher.
          zcx_abapgit_exception=>raise( `Failed to deserialize translation.` ).
      ENDTRY.

      TRY.
          lo_merged = zcl_abapgit_ajson_utilities=>new( )->merge( io_json_a = lo_merged
                                                                  io_json_b = lo_deserialization_result ).
        CATCH zcx_abapgit_ajson_error INTO lx_ajson.
          zcx_abapgit_exception=>raise_with_text( lx_ajson ).
      ENDTRY.

    ENDLOOP.

    TRY.
        rv_result = lo_merged->stringify( 2 ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.