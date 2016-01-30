REPORT zabapgit.

* See https://github.com/larshp/abapGit/

CONSTANTS: gc_xml_version  TYPE string VALUE 'v0.2-alpha',  "#EC NOTEXT
           gc_abap_version TYPE string VALUE 'v0.99'.       "#EC NOTEXT

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2014 Lars Hvam Petersen
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

TYPE-POOLS seop.

TYPES: ty_type    TYPE c LENGTH 6,
       ty_bitbyte TYPE c LENGTH 8,
       ty_sha1    TYPE c LENGTH 40.

TYPES: BEGIN OF ty_file,
         path     TYPE string,
         filename TYPE string,
         data     TYPE xstring,
       END OF ty_file.
TYPES: ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

TYPES: ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

TYPES: BEGIN OF ty_comment,
         username TYPE string,
         email    TYPE string,
         comment  TYPE string,
       END OF ty_comment.

TYPES: BEGIN OF ty_item,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
       END OF ty_item.

CONSTANTS: BEGIN OF gc_type,
             commit TYPE ty_type VALUE 'commit',            "#EC NOTEXT
             tree   TYPE ty_type VALUE 'tree',              "#EC NOTEXT
             ref_d  TYPE ty_type VALUE 'ref_d',             "#EC NOTEXT
             blob   TYPE ty_type VALUE 'blob',              "#EC NOTEXT
           END OF gc_type.

CONSTANTS: BEGIN OF gc_chmod,
             file TYPE c LENGTH 6 VALUE '100644',
             dir  TYPE c LENGTH 5 VALUE '40000',
           END OF gc_chmod.

CONSTANTS: gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

CONSTANTS: gc_english TYPE spras VALUE 'E'.

DEFINE _raise.
  raise exception type lcx_exception
    exporting
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

******************

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

******************

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text TYPE string.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCX_NOT_FOUND DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_not_found DEFINITION INHERITING FROM cx_static_check FINAL.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_NOT_FOUND IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_not_found IMPLEMENTATION.

ENDCLASS.                    "lcx_not_found IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_huffman DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_huffman DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_lengths TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    CONSTANTS: c_maxbits TYPE i VALUE 15.

    METHODS:
      constructor
        IMPORTING it_lengths TYPE ty_lengths,
      get_count
        IMPORTING iv_index        TYPE i
        RETURNING VALUE(rv_value) TYPE i,
      get_symbol
        IMPORTING iv_index        TYPE i
        RETURNING VALUE(rv_value) TYPE i.

  PRIVATE SECTION.

    DATA: mt_count  TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          mt_symbol TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

ENDCLASS.                    "lcl_zlib_huffman DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_huffman DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_huffman IMPLEMENTATION.

  METHOD get_count.
    READ TABLE mt_count INDEX iv_index INTO rv_value.     "#EC CI_SUBRC
  ENDMETHOD.                    "count

  METHOD get_symbol.
    READ TABLE mt_symbol INDEX iv_index INTO rv_value.    "#EC CI_SUBRC
  ENDMETHOD.                    "symbol

  METHOD constructor.

    DATA: lv_index  TYPE i,
          lt_offset TYPE TABLE OF i,
          lv_length LIKE LINE OF it_lengths,
          lv_prev   TYPE i,
          lv_count  LIKE LINE OF mt_count.

    FIELD-SYMBOLS: <lv_offset> LIKE LINE OF lt_offset,
                   <lv_symbol> LIKE LINE OF mt_symbol,
                   <lv_i>      LIKE LINE OF it_lengths.


    DO c_maxbits TIMES.
      APPEND 0 TO mt_count.
    ENDDO.
    LOOP AT it_lengths INTO lv_index.
      IF lv_index = 0.
        CONTINUE.
      ENDIF.
      READ TABLE mt_count INDEX lv_index ASSIGNING <lv_i>.
      ASSERT sy-subrc = 0.
      <lv_i> = <lv_i> + 1.
    ENDLOOP.

************

    APPEND 0 TO lt_offset.
    DO c_maxbits - 1 TIMES.
      READ TABLE mt_count INDEX sy-index INTO lv_count.
      ASSERT sy-subrc = 0.
      lv_prev = lv_prev + lv_count.
      APPEND lv_prev TO lt_offset.
    ENDDO.

    DO lines( it_lengths ) TIMES.
      APPEND 0 TO mt_symbol.
    ENDDO.
    DO lines( it_lengths ) TIMES.
      lv_index = sy-index.
      READ TABLE it_lengths INDEX lv_index INTO lv_length.
      ASSERT sy-subrc = 0.
      IF lv_length = 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_offset INDEX lv_length ASSIGNING <lv_offset>.
      ASSERT sy-subrc = 0.
      READ TABLE mt_symbol INDEX <lv_offset> + 1 ASSIGNING <lv_symbol>.
      ASSERT sy-subrc = 0.
      <lv_symbol> = lv_index - 1.
      <lv_offset> = <lv_offset> + 1.
    ENDDO.

  ENDMETHOD.                    "constructor

ENDCLASS.                    "lcl_zlib_huffman DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_convert DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      hex_to_bits
        IMPORTING iv_hex         TYPE xsequence
        RETURNING VALUE(rv_bits) TYPE string,
      bits_to_int
        IMPORTING iv_bits       TYPE clike
        RETURNING VALUE(rv_int) TYPE i,
      int_to_hex
        IMPORTING iv_int        TYPE i
        RETURNING VALUE(rv_hex) TYPE xstring.

ENDCLASS.                    "lcl_zlib_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_convert IMPLEMENTATION.

  METHOD hex_to_bits.

    DATA: lv_x   TYPE x LENGTH 1,
          lv_c   TYPE c LENGTH 1,
          lv_bit TYPE i,
          lv_hex TYPE xstring.


    lv_hex = iv_hex.
    WHILE NOT lv_hex IS INITIAL.
      lv_x = lv_hex.
      DO 8 TIMES.
        lv_bit = sy-index.
        GET BIT lv_bit OF lv_x INTO lv_c.
        CONCATENATE rv_bits lv_c INTO rv_bits.
      ENDDO.
      lv_hex = lv_hex+1.
    ENDWHILE.

  ENDMETHOD.                    "hex_to_bits

  METHOD bits_to_int.

    DATA: lv_c    TYPE c LENGTH 1,
          lv_bits TYPE string.

    lv_bits = iv_bits.

    WHILE NOT lv_bits IS INITIAL.
      lv_c = lv_bits.
      rv_int = rv_int * 2.
      rv_int = rv_int + lv_c.
      lv_bits = lv_bits+1.
    ENDWHILE.

  ENDMETHOD.                    "bits_to_int

  METHOD int_to_hex.

    DATA: lv_x TYPE x.


    lv_x = iv_int.
    rv_hex = lv_x.

  ENDMETHOD.                    "int_to_hex

ENDCLASS.                    "lcl_zlib_convert IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_stream DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_stream DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_data TYPE xstring,
      take_bits
        IMPORTING iv_length      TYPE i
        RETURNING VALUE(rv_bits) TYPE string,
      take_int
        IMPORTING iv_length     TYPE i
        RETURNING VALUE(rv_int) TYPE i,
      remaining
        RETURNING VALUE(rv_length) TYPE i.

  PRIVATE SECTION.
    DATA: mv_compressed TYPE xstring,
          mv_bits       TYPE string.

ENDCLASS.                    "lcl_zlib_stream DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib_stream IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib_stream IMPLEMENTATION.

  METHOD constructor.

    mv_compressed = iv_data.

  ENDMETHOD.                    "constructor

  METHOD remaining.

    rv_length = xstrlen( mv_compressed ) + 1.

  ENDMETHOD.                    "remaining

  METHOD take_int.

    rv_int = lcl_zlib_convert=>bits_to_int( take_bits( iv_length ) ).

  ENDMETHOD.                    "take_int

  METHOD take_bits.

    DATA: lv_left  TYPE i,
          lv_index TYPE i,
          lv_x     TYPE x LENGTH 1.


    WHILE strlen( rv_bits ) < iv_length.
      IF mv_bits IS INITIAL.
        lv_x = mv_compressed(1).
        mv_bits = lcl_zlib_convert=>hex_to_bits( lv_x ).
        mv_compressed = mv_compressed+1.
      ENDIF.
      lv_left = iv_length - strlen( rv_bits ).
      IF lv_left >= strlen( mv_bits ).
        CONCATENATE mv_bits rv_bits INTO rv_bits.
        CLEAR mv_bits.
      ELSE.
        lv_index = strlen( mv_bits ) - lv_left.
        CONCATENATE mv_bits+lv_index(lv_left) rv_bits INTO rv_bits.
        mv_bits = mv_bits(lv_index).
      ENDIF.

    ENDWHILE.

  ENDMETHOD.                    "take_bits

ENDCLASS.                    "lcl_zlib_stream IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_decompress,
             raw            TYPE xstring,
             compressed_len TYPE i,
           END OF ty_decompress.

    CLASS-METHODS:
      decompress
        IMPORTING iv_compressed  TYPE xsequence
        RETURNING VALUE(rs_data) TYPE ty_decompress.

  PRIVATE SECTION.
    CONSTANTS: c_maxdcodes TYPE i VALUE 30.

    CLASS-DATA: gv_out      TYPE xstring,
                go_lencode  TYPE REF TO lcl_zlib_huffman,
                go_distcode TYPE REF TO lcl_zlib_huffman,
                go_stream   TYPE REF TO lcl_zlib_stream.

    TYPES: BEGIN OF ty_pair,
             length   TYPE i,
             distance TYPE i,
           END OF ty_pair.

    CLASS-METHODS:
      decode
        IMPORTING io_huffman       TYPE REF TO lcl_zlib_huffman
        RETURNING VALUE(rv_symbol) TYPE i,
      map_length
        IMPORTING iv_code          TYPE i
        RETURNING VALUE(rv_length) TYPE i,
      map_distance
        IMPORTING iv_code            TYPE i
        RETURNING VALUE(rv_distance) TYPE i,
      dynamic,
      fixed,
      read_pair
        IMPORTING iv_length      TYPE i
        RETURNING VALUE(rs_pair) TYPE ty_pair,
      copy_out
        IMPORTING is_pair TYPE ty_pair.

ENDCLASS.                    "lcl_zlib DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zlib IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zlib IMPLEMENTATION.

  METHOD decode.

    DATA: lv_bit   TYPE c LENGTH 1,
          lv_len   TYPE i,
          lv_count TYPE i,
          lv_code  TYPE i,
          lv_index TYPE i,
          lv_first TYPE i,
          lv_bits  TYPE string.


    DO lcl_zlib_huffman=>c_maxbits TIMES.
      lv_len = sy-index.

      lv_bit = go_stream->take_bits( 1 ).
      CONCATENATE lv_bits lv_bit INTO lv_bits.
      lv_code = lcl_zlib_convert=>bits_to_int( lv_bits ).
      lv_count = io_huffman->get_count( lv_len ).

      IF lv_code - lv_count < lv_first.
        rv_symbol = io_huffman->get_symbol( lv_index + lv_code - lv_first + 1 ).
        RETURN.
      ENDIF.
      lv_index = lv_index + lv_count.
      lv_first = lv_first + lv_count.
      lv_first = lv_first * 2.
    ENDDO.

  ENDMETHOD.                    "decode

  METHOD fixed.

    DATA: lt_lengths TYPE lcl_zlib_huffman=>ty_lengths.


    DO 144 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.
    DO 112 TIMES.
      APPEND 9 TO lt_lengths.
    ENDDO.
    DO 24 TIMES.
      APPEND 7 TO lt_lengths.
    ENDDO.
    DO 8 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CLEAR lt_lengths.
    DO c_maxdcodes TIMES.
      APPEND 5 TO lt_lengths.
    ENDDO.

    CREATE OBJECT go_distcode
      EXPORTING
        it_lengths = lt_lengths.

  ENDMETHOD.                    "fixed

  METHOD copy_out.

* copy one byte at a time, it is not possible to copy using
* string offsets, as it might copy data that does not exist
* in mv_out yet

    DATA: lv_distance TYPE i,
          lv_index    TYPE i,
          lv_x        TYPE x LENGTH 1.


    lv_distance = xstrlen( gv_out ) - is_pair-distance.
    DO is_pair-length TIMES.
      lv_index = sy-index - 1 + lv_distance.
      lv_x = gv_out+lv_index(1).
      CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
    ENDDO.

  ENDMETHOD.                    "copy_out

  METHOD dynamic.

    DATA: lv_nlen    TYPE i,
          lv_ndist   TYPE i,
          lv_ncode   TYPE i,
          lv_index   TYPE i,
          lv_length  TYPE i,
          lv_symbol  TYPE i,
          lt_order   TYPE TABLE OF i,
          lt_lengths TYPE lcl_zlib_huffman=>ty_lengths,
          lt_dists   TYPE lcl_zlib_huffman=>ty_lengths.

    FIELD-SYMBOLS: <lv_length> LIKE LINE OF lt_lengths.


    APPEND 16 TO lt_order.
    APPEND 17 TO lt_order.
    APPEND 18 TO lt_order.
    APPEND  0 TO lt_order.
    APPEND  8 TO lt_order.
    APPEND  7 TO lt_order.
    APPEND  9 TO lt_order.
    APPEND  6 TO lt_order.
    APPEND 10 TO lt_order.
    APPEND  5 TO lt_order.
    APPEND 11 TO lt_order.
    APPEND  4 TO lt_order.
    APPEND 12 TO lt_order.
    APPEND  3 TO lt_order.
    APPEND 13 TO lt_order.
    APPEND  2 TO lt_order.
    APPEND 14 TO lt_order.
    APPEND  1 TO lt_order.
    APPEND 15 TO lt_order.

    lv_nlen = go_stream->take_int( 5 ) + 257.
    lv_ndist = go_stream->take_int( 5 ) + 1.
    lv_ncode = go_stream->take_int( 4 ) + 4.

    DO 19 TIMES.
      APPEND 0 TO lt_lengths.
    ENDDO.

    DO lv_ncode TIMES.
      READ TABLE lt_order INDEX sy-index INTO lv_index.
      ASSERT sy-subrc = 0.
      lv_index = lv_index + 1.
      READ TABLE lt_lengths INDEX lv_index ASSIGNING <lv_length>.
      ASSERT sy-subrc = 0.
      <lv_length> = go_stream->take_int( 3 ).
    ENDDO.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CLEAR lt_lengths.
    WHILE lines( lt_lengths ) < lv_nlen + lv_ndist.
      lv_symbol = decode( go_lencode ).

      IF lv_symbol < 16.
        APPEND lv_symbol TO lt_lengths.
      ELSE.
        lv_length = 0.
        IF lv_symbol = 16.
          READ TABLE lt_lengths INDEX lines( lt_lengths ) INTO lv_length.
          ASSERT sy-subrc = 0.
          lv_symbol = go_stream->take_int( 2 ) + 3.
        ELSEIF lv_symbol = 17.
          lv_symbol = go_stream->take_int( 3 ) + 3.
        ELSE.
          lv_symbol = go_stream->take_int( 7 ) + 11.
        ENDIF.
        DO lv_symbol TIMES.
          APPEND lv_length TO lt_lengths.
        ENDDO.
      ENDIF.
    ENDWHILE.

    lt_dists = lt_lengths.
    DELETE lt_lengths FROM lv_nlen + 1.
    DELETE lt_dists TO lv_nlen.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CREATE OBJECT go_distcode
      EXPORTING
        it_lengths = lt_dists.

  ENDMETHOD.                    "dynamic

  METHOD read_pair.

    DATA: lv_symbol TYPE i.


    rs_pair-length = map_length( iv_length ).

    lv_symbol = decode( go_distcode ).
    rs_pair-distance = map_distance( lv_symbol ).

  ENDMETHOD.                    "read_pair

  METHOD map_distance.

    DEFINE _distance.
      rv_distance = go_stream->take_int( &1 ).
      rv_distance = rv_distance + &2.
    END-OF-DEFINITION.

    CASE iv_code.
      WHEN 0.
        _distance 0 1.
      WHEN 1.
        _distance 0 2.
      WHEN 2.
        _distance 0 3.
      WHEN 3.
        _distance 0 4.
      WHEN 4.
        _distance 1 5.
      WHEN 5.
        _distance 1 7.
      WHEN 6.
        _distance 2 9.
      WHEN 7.
        _distance 2 13.
      WHEN 8.
        _distance 3 17.
      WHEN 9.
        _distance 3 25.
      WHEN 10.
        _distance 4 33.
      WHEN 11.
        _distance 4 49.
      WHEN 12.
        _distance 5 65.
      WHEN 13.
        _distance 5 97.
      WHEN 14.
        _distance 6 129.
      WHEN 15.
        _distance 6 193.
      WHEN 16.
        _distance 7 257.
      WHEN 17.
        _distance 7 385.
      WHEN 18.
        _distance 8 513.
      WHEN 19.
        _distance 8 769.
      WHEN 20.
        _distance 9 1025.
      WHEN 21.
        _distance 9 1537.
      WHEN 22.
        _distance 10 2049.
      WHEN 23.
        _distance 10 3073.
      WHEN 24.
        _distance 11 4097.
      WHEN 25.
        _distance 11 6145.
      WHEN 26.
        _distance 12 8193.
      WHEN 27.
        _distance 12 12289.
      WHEN 28.
        _distance 13 16385.
      WHEN 29.
        _distance 13 24577.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.                    "map_distance

  METHOD map_length.

    DEFINE _length.
      rv_length = go_stream->take_int( &1 ).
      rv_length = rv_length + &2.
    END-OF-DEFINITION.

    CASE iv_code.
      WHEN 257.
        _length 0 3.
      WHEN 258.
        _length 0 4.
      WHEN 259.
        _length 0 5.
      WHEN 260.
        _length 0 6.
      WHEN 261.
        _length 0 7.
      WHEN 262.
        _length 0 8.
      WHEN 263.
        _length 0 9.
      WHEN 264.
        _length 0 10.
      WHEN 265.
        _length 1 11.
      WHEN 266.
        _length 1 13.
      WHEN 267.
        _length 1 15.
      WHEN 268.
        _length 1 17.
      WHEN 269.
        _length 2 19.
      WHEN 270.
        _length 2 23.
      WHEN 271.
        _length 2 27.
      WHEN 272.
        _length 2 31.
      WHEN 273.
        _length 3 35.
      WHEN 274.
        _length 3 43.
      WHEN 275.
        _length 3 51.
      WHEN 276.
        _length 3 59.
      WHEN 277.
        _length 4 67.
      WHEN 278.
        _length 4 83.
      WHEN 279.
        _length 4 99.
      WHEN 280.
        _length 4 115.
      WHEN 281.
        _length 5 131.
      WHEN 282.
        _length 5 163.
      WHEN 283.
        _length 5 195.
      WHEN 284.
        _length 5 227.
      WHEN 285.
        _length 0 258.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.                    "map_length

  METHOD decompress.

    DATA: lv_x      TYPE x LENGTH 1,
          lv_symbol TYPE i,
          lv_bfinal TYPE c LENGTH 1,
          lv_btype  TYPE c LENGTH 2.


    IF iv_compressed IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR gv_out.
    CREATE OBJECT go_stream
      EXPORTING
        iv_data = iv_compressed.

    DO.
      lv_bfinal = go_stream->take_bits( 1 ).

      lv_btype = go_stream->take_bits( 2 ).
      CASE lv_btype.
        WHEN '01'.
          fixed( ).
        WHEN '10'.
          dynamic( ).
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

      DO.
        lv_symbol = decode( go_lencode ).

        IF lv_symbol < 256.
          lv_x = lcl_zlib_convert=>int_to_hex( lv_symbol ).
          CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
        ELSEIF lv_symbol = 256.
          EXIT.
        ELSE.
          copy_out( read_pair( lv_symbol ) ).
        ENDIF.

      ENDDO.

      IF lv_bfinal = '1'.
        EXIT.
      ENDIF.

    ENDDO.

    rs_data-raw = gv_out.
    rs_data-compressed_len = xstrlen( iv_compressed ) - go_stream->remaining( ).

  ENDMETHOD.                    "decompress

ENDCLASS.                    "lcl_zlib IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_tadir DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tadir DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_tadir,
             pgmid    TYPE tadir-pgmid,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
             korrnum  TYPE tadir-korrnum,
             path     TYPE string,
           END OF ty_tadir.
    TYPES: ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

    CLASS-METHODS:
      read
        IMPORTING iv_package      TYPE tadir-devclass
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception,
      read_single
        IMPORTING iv_pgmid        TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object       TYPE tadir-object
                  iv_obj_name     TYPE tadir-obj_name
        RETURNING VALUE(rs_tadir) TYPE tadir.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_exists
        IMPORTING it_tadir        TYPE ty_tadir_tt
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception,
      build
        IMPORTING iv_package      TYPE tadir-devclass
                  iv_parent       TYPE tadir-devclass
                  iv_path         TYPE string
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_tadir DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_user DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS set_username
      IMPORTING iv_username TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_username
      RETURNING VALUE(rv_username) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS set_email
      IMPORTING iv_email TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_email
      RETURNING VALUE(rv_email) TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS read
      IMPORTING iv_name         TYPE tdobname
      RETURNING VALUE(rv_value) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS save
      IMPORTING iv_name  TYPE tdobname
                iv_value TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_user DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_user IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user IMPLEMENTATION.

  METHOD read.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_line  LIKE LINE OF lt_lines.


    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = gc_english
        name                    = iv_name
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 4 AND sy-subrc <> 0.
      _raise 'error from READ_TEXT'.
    ENDIF.

    READ TABLE lt_lines INTO ls_line INDEX 1.
    IF sy-subrc = 0.
      rv_value = ls_line-tdline.
    ENDIF.

  ENDMETHOD.                    "get_details

  METHOD save.

    DATA: ls_header TYPE thead,
          lt_lines  TYPE TABLE OF tline,
          ls_line   LIKE LINE OF lt_lines.


    ls_line-tdformat = '*'.
    ls_line-tdline = iv_value.
    APPEND ls_line TO lt_lines.

    ls_header-tdid       = 'ST'.
    ls_header-tdspras    = gc_english.
    ls_header-tdname     = iv_name.
    ls_header-tdobject   = 'TEXT'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header   = ls_header
      TABLES
        lines    = lt_lines
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      _raise 'error from SAVE_TEXT'.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.                    "change

  METHOD set_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' sy-uname INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_username ).

  ENDMETHOD.                    "set_username

  METHOD get_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' sy-uname INTO lv_name.

    rv_username = read( lv_name ).

  ENDMETHOD.                    "get_username

  METHOD set_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' sy-uname INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_email ).

  ENDMETHOD.                    "set_email

  METHOD get_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' sy-uname INTO lv_name.

    rv_email = read( lv_name ).

  ENDMETHOD.                    "get_email

ENDCLASS.                    "lcl_user IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION FINAL.

  PUBLIC SECTION.
    DATA: mi_xml_doc TYPE REF TO if_ixml_document.

    METHODS constructor
      IMPORTING iv_xml   TYPE string OPTIONAL
                iv_empty TYPE sap_bool DEFAULT abap_false
      RAISING   lcx_exception.

    METHODS element_add
      IMPORTING ig_element TYPE data
                iv_name    TYPE string OPTIONAL
                ii_root    TYPE REF TO if_ixml_element OPTIONAL
      RAISING   lcx_exception.

    METHODS element_read
      IMPORTING ii_root    TYPE REF TO if_ixml_element OPTIONAL
                iv_name    TYPE string OPTIONAL
      EXPORTING ev_success TYPE abap_bool
      CHANGING  cg_element TYPE data
      RAISING   lcx_exception.

    METHODS structure_add
      IMPORTING ig_structure TYPE data
                iv_name      TYPE string OPTIONAL
                ii_root      TYPE REF TO if_ixml_element OPTIONAL
      RAISING   lcx_exception.

    METHODS structure_read
      IMPORTING ii_root      TYPE REF TO if_ixml_element OPTIONAL
                iv_name      TYPE string OPTIONAL
      EXPORTING ev_success   TYPE abap_bool
      CHANGING  cg_structure TYPE data
      RAISING   lcx_exception.

    METHODS table_add
      IMPORTING it_table TYPE STANDARD TABLE
                iv_name  TYPE string OPTIONAL
                ii_root  TYPE REF TO if_ixml_element OPTIONAL
      RAISING   lcx_exception.

    METHODS table_read
      IMPORTING ii_root  TYPE REF TO if_ixml_element OPTIONAL
                iv_name  TYPE string OPTIONAL
      CHANGING  ct_table TYPE STANDARD TABLE
      RAISING   lcx_exception.

    METHODS xml_render
      IMPORTING iv_normalize     TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rv_string) TYPE string.

    METHODS xml_element
      IMPORTING iv_name           TYPE string
      RETURNING VALUE(ri_element) TYPE REF TO if_ixml_element.

    METHODS xml_add
      IMPORTING ii_root    TYPE REF TO if_ixml_element OPTIONAL
                ii_element TYPE REF TO if_ixml_element.

    METHODS xml_find
      IMPORTING ii_root           TYPE REF TO if_ixml_element OPTIONAL
                iv_name           TYPE string
      RETURNING VALUE(ri_element) TYPE REF TO if_ixml_element.

  PRIVATE SECTION.

    DATA: mi_ixml TYPE REF TO if_ixml,
          mi_root TYPE REF TO if_ixml_element.

    METHODS special_names
      CHANGING cv_name TYPE string.

    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.

  METHOD xml_find.

    DATA: li_root LIKE ii_root.


    IF ii_root IS BOUND.
      li_root = ii_root.
    ELSE.
      li_root = mi_root.
    ENDIF.

    ri_element = li_root->find_from_name( depth = 0 name = iv_name ).
    IF NOT ri_element IS BOUND.
      RETURN.
    ENDIF.
    li_root->remove_child( ri_element ).

  ENDMETHOD.                    "xml_find

  METHOD xml_element.

    ri_element = mi_xml_doc->create_element( iv_name ).

  ENDMETHOD.                    "xml_element

  METHOD special_names.

    IF cv_name(1) = '*'.
      CONCATENATE 'STAR' cv_name+1 INTO cv_name.
    ELSEIF cv_name(1) = '2'.
      CONCATENATE 'TWO' cv_name+1 INTO cv_name.
    ENDIF.

  ENDMETHOD.                    "special_names

  METHOD structure_read.

    DATA: lv_name      TYPE string,
          li_struct    TYPE REF TO if_ixml_element,
          lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_descr_ref TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <lg_any>  TYPE any,
                   <ls_comp> LIKE LINE OF lo_descr_ref->components.


    CLEAR cg_structure.
    ev_success = abap_true.

    lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( cg_structure ).
    IF iv_name IS INITIAL.
      lv_name = lo_descr_ref->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, structure read'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.

    li_struct = xml_find( ii_root = ii_root
                          iv_name = lv_name ).
    IF NOT li_struct IS BOUND.
      ev_success = abap_false.
      RETURN.
    ENDIF.

    LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE cg_structure TO <lg_any>.
      ASSERT sy-subrc = 0.

      lv_name = <ls_comp>-name.
      special_names( CHANGING cv_name = lv_name ).

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <lg_any> ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_read( EXPORTING ii_root  = li_struct
                                iv_name  = lv_name
                      CHANGING  ct_table = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_read( EXPORTING ii_root      = li_struct
                                    iv_name      = lv_name
                          CHANGING  cg_structure = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_read( EXPORTING ii_root    = li_struct
                                  iv_name    = lv_name
                        CHANGING  cg_element = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_ref.
          CONTINUE.
        WHEN OTHERS.
          _raise 'unknown kind, structure read'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "structure_read

  METHOD table_read.

    DATA: lv_name        TYPE string,
          li_root        TYPE REF TO if_ixml_element,
          lv_kind        TYPE abap_typecategory,
          lv_index       TYPE i,
          lv_success     TYPE abap_bool,
          lo_data_descr  TYPE REF TO cl_abap_datadescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    CLEAR ct_table[].

    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( ct_table ).
    lv_name = lo_table_descr->get_relative_name( ).

    IF lv_name IS INITIAL.
      lv_name = iv_name.
    ENDIF.

    IF lv_name IS INITIAL.
      _raise 'no name, table read'.
    ENDIF.

    li_root = xml_find( ii_root   = ii_root
                        iv_name   = lv_name ).
    IF NOT li_root IS BOUND.
      RETURN.
    ENDIF.

    lo_data_descr = lo_table_descr->get_table_line_type( ).
    lv_kind = lo_data_descr->kind.

    DO.
      APPEND INITIAL LINE TO ct_table ASSIGNING <lg_line>.
      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_struct.
          structure_read( EXPORTING ii_root    = li_root
                          IMPORTING ev_success = lv_success
                          CHANGING cg_structure = <lg_line> ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_read( EXPORTING ii_root    = li_root
                        IMPORTING ev_success = lv_success
                        CHANGING  cg_element = <lg_line> ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.

      IF lv_success = abap_false.
        lv_index = lines( ct_table ).
        DELETE ct_table INDEX lv_index.
        ASSERT sy-subrc = 0.
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "table_read

  METHOD error.

    DATA: lv_error TYPE i,
          lv_txt1  TYPE string,
          lv_txt2  TYPE string,
          lv_txt3  TYPE string,
          lv_times TYPE i,
          li_error TYPE REF TO if_ixml_parse_error.


    IF ii_parser->num_errors( ) <> 0.
      lv_times = ii_parser->num_errors( ).
      DO lv_times TIMES.
        lv_error = sy-index - 1.
        li_error = ii_parser->get_error( lv_error ).

        lv_txt1 = li_error->get_column( ).
        CONCATENATE 'Column:' lv_txt1 INTO lv_txt1.         "#EC NOTEXT
        lv_txt2 = li_error->get_line( ).
        CONCATENATE 'Line:' lv_txt2 INTO lv_txt2.           "#EC NOTEXT
        lv_txt3 = li_error->get_reason( ).

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Error from XML parser'                 "#EC NOTEXT
            txt1  = lv_txt1
            txt2  = lv_txt2
            txt3  = lv_txt3.
      ENDDO.
    ENDIF.

    _raise 'Error while parsing XML'.
  ENDMETHOD.                    "error

  METHOD constructor.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser.


    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).

    IF iv_xml IS SUPPLIED.
      li_stream_factory = mi_ixml->create_stream_factory( ).
      li_istream = li_stream_factory->create_istream_string( iv_xml ).
      li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                          istream        = li_istream
                                          document       = mi_xml_doc ).
      li_parser->set_normalizing( abap_false ).
      IF li_parser->parse( ) <> 0.
        error( li_parser ).
      ENDIF.

      li_istream->close( ).

      mi_root = mi_xml_doc->find_from_name( depth = 0 name = 'abapGit' ).
    ELSEIF iv_empty = abap_false.
      mi_root = mi_xml_doc->create_element( 'abapGit' ).
      mi_root->set_attribute( name = 'version' value = gc_xml_version ). "#EC NOTEXT
      mi_xml_doc->append_child( mi_root ).
    ENDIF.

  ENDMETHOD.                    "xml_root

  METHOD table_add.

    DATA: lv_name        TYPE string,
          li_table       TYPE REF TO if_ixml_element,
          lv_kind        TYPE abap_typecategory,
          lo_data_descr  TYPE REF TO cl_abap_datadescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( it_table ).
    lv_name = lo_table_descr->get_relative_name( ).

    IF lv_name IS INITIAL.
      lv_name = iv_name.
    ENDIF.

    IF lv_name IS INITIAL.
      _raise 'no name, table add'.
    ENDIF.

    li_table = mi_xml_doc->create_element( lv_name ).
    lo_data_descr = lo_table_descr->get_table_line_type( ).
    lv_kind = lo_data_descr->kind.

    LOOP AT it_table ASSIGNING <lg_line>.
      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_struct.
          structure_add( ig_structure = <lg_line>
                         ii_root      = li_table ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_add( ig_element = <lg_line>
                       ii_root    = li_table ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.
    ENDLOOP.

    xml_add( ii_root    = ii_root
             ii_element = li_table ).

  ENDMETHOD.                    "table_add

  METHOD xml_add.

    IF ii_root IS BOUND.
      ii_root->append_child( ii_element ).
    ELSE.
      mi_root->append_child( ii_element ).
    ENDIF.

  ENDMETHOD.                    "xml_add

  METHOD element_add.

    DATA: lo_descr   TYPE REF TO cl_abap_elemdescr,
          lv_string  TYPE string,
          li_element TYPE REF TO if_ixml_element,
          li_text    TYPE REF TO if_ixml_text,
          lv_name    TYPE string.


    lo_descr ?= cl_abap_typedescr=>describe_by_data( ig_element ).

    IF iv_name IS INITIAL.
      lv_name = lo_descr->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, element add'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.

    li_element = mi_xml_doc->create_element( lv_name ).

    lv_string  = ig_element.
    li_text    = mi_xml_doc->create_text( lv_string ).

    li_element->append_child( li_text ).

    xml_add( ii_root    = ii_root
             ii_element = li_element ).

  ENDMETHOD.                    "element_add

  METHOD element_read.

    DATA: lo_descr   TYPE REF TO cl_abap_elemdescr,
          li_element TYPE REF TO if_ixml_element,
          lv_name    TYPE string.


    ev_success = abap_true.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( cg_element ).

    IF iv_name IS INITIAL.
      lv_name = lo_descr->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, element read'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.

    li_element = xml_find( ii_root = ii_root
                           iv_name = lv_name ).
    IF NOT li_element IS BOUND.
      ev_success = abap_false.
      RETURN.
    ENDIF.

    cg_element = li_element->get_value( ).

  ENDMETHOD.                    "element_read

  METHOD structure_add.

    DATA: li_structure TYPE REF TO if_ixml_element,
          lv_name      TYPE string,
          lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_descr     TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lo_descr->components,
                   <lg_any>  TYPE any.


    lo_descr ?= cl_abap_typedescr=>describe_by_data( ig_structure ).

    IF iv_name IS INITIAL.
      lv_name = lo_descr->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, structure add'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.
    li_structure = mi_xml_doc->create_element( lv_name ).

    LOOP AT lo_descr->components ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ig_structure TO <lg_any>.
      ASSERT sy-subrc = 0.

      lv_name  = <ls_comp>-name.
      special_names( CHANGING cv_name = lv_name ).

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <lg_any> ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_add( it_table = <lg_any>
                     iv_name  = lv_name
                     ii_root  = li_structure ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_add( ig_structure = <lg_any>
                         iv_name      = lv_name
                         ii_root      = li_structure ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_add( ig_element = <lg_any>
                       iv_name    = lv_name
                       ii_root    = li_structure ).
        WHEN cl_abap_typedescr=>kind_ref.
          CONTINUE.
        WHEN OTHERS.
          _raise 'unknown kind, structure add'.
      ENDCASE.

    ENDLOOP.

    xml_add( ii_root    = ii_root
             ii_element = li_structure ).

  ENDMETHOD.                    "structure_to_xml

  METHOD xml_render.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_string ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    IF iv_normalize = abap_true.
      li_renderer->set_normalizing( ).
    ENDIF.
    li_renderer->render( ).

  ENDMETHOD.                    "xml_render

ENDCLASS.                    "lcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_unixtime TYPE c LENGTH 16.

    CLASS-METHODS get
      RETURNING VALUE(rv_time) TYPE ty_unixtime
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CONSTANTS: c_epoch TYPE datum VALUE '19700101'.

ENDCLASS.                    "lcl_time DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_time IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time IMPLEMENTATION.

  METHOD get.

    DATA: lv_i       TYPE i,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.


    lv_i = sy-datum - c_epoch.
    lv_i = lv_i * 86400.
    lv_i = lv_i + sy-uzeit.

    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = lv_tz.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = lv_tz
        if_local_date    = sy-datum
        if_local_time    = sy-uzeit
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      _raise 'Timezone error'.
    ENDIF.

    CASE lv_utcsign.
      WHEN '+'.
        lv_i = lv_i - lv_utcdiff.
      WHEN '-'.
        lv_i = lv_i + lv_utcdiff.
    ENDCASE.

    rv_time = lv_i.
    CONDENSE rv_time.
    rv_time+11 = lv_utcsign.
    rv_time+12 = lv_utcdiff.

  ENDMETHOD.                    "get

ENDCLASS.                    "lcl_time IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_url DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS host
      IMPORTING iv_repo        TYPE string
      RETURNING VALUE(rv_host) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS name
      IMPORTING iv_repo        TYPE string
      RETURNING VALUE(rv_name) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS path_name
      IMPORTING iv_repo             TYPE string
      RETURNING VALUE(rv_path_name) TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS regex
      IMPORTING iv_repo TYPE string
      EXPORTING ev_host TYPE string
                ev_path TYPE string
                ev_name TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_repo DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_url IMPLEMENTATION.

  METHOD host.
    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_host = rv_host ).
  ENDMETHOD.                    "host

  METHOD name.
    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_name = rv_name ).
  ENDMETHOD.                    "short_name

  METHOD path_name.

    DATA: lv_path TYPE string,
          lv_name TYPE string.

    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_path = lv_path
                     ev_name = lv_name ).

    CONCATENATE lv_path lv_name INTO rv_path_name.

  ENDMETHOD.                    "path_name

  METHOD regex.

    FIND REGEX '(.*://[^/]*)(.*/)(.*).git' IN iv_repo
                     SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      _raise 'Malformed URL'.
    ENDIF.

  ENDMETHOD.                    "url

ENDCLASS.                    "lcl_repo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS bitbyte_to_int
      IMPORTING iv_bits       TYPE clike
      RETURNING VALUE(rv_int) TYPE i.

    CLASS-METHODS x_to_bitbyte
      IMPORTING iv_x              TYPE x
      RETURNING VALUE(rv_bitbyte) TYPE ty_bitbyte.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING iv_string         TYPE string
      RETURNING VALUE(rv_xstring) TYPE xstring.

    CLASS-METHODS xstring_to_string_utf8
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS xstring_to_int
      IMPORTING iv_xstring  TYPE xstring
      RETURNING VALUE(rv_i) TYPE i
      RAISING   lcx_exception.

    CLASS-METHODS int_to_xstring
      IMPORTING iv_i              TYPE i
                iv_length         TYPE i
      RETURNING VALUE(rv_xstring) TYPE xstring.

ENDCLASS.                    "lcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert IMPLEMENTATION.

  METHOD int_to_xstring.

    DATA: lv_x TYPE x LENGTH 4.


    ASSERT iv_length = 4. " other cases not implemented

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.                    "int_to_xstring

  METHOD xstring_to_int.

    DATA: lv_xstring TYPE xstring,
          lv_x       TYPE x.


    lv_xstring = iv_xstring.
    WHILE xstrlen( lv_xstring ) > 0.
      lv_x = lv_xstring(1).
      rv_i = rv_i * 256 + lv_x.
      lv_xstring = lv_xstring+1.
    ENDWHILE.

  ENDMETHOD.                    "xstring_to_int

  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "xstring_to_string_utf8

  METHOD string_to_xstring_utf8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "string_to_xstring_utf8

  METHOD bitbyte_to_int.

    DATA: lv_bits TYPE string.


    lv_bits = iv_bits.

    rv_int = 0.
    WHILE strlen( lv_bits ) > 0.
      rv_int = rv_int * 2.
      IF lv_bits(1) = '1'.
        rv_int = rv_int + 1.
      ENDIF.
      lv_bits = lv_bits+1.
    ENDWHILE.

  ENDMETHOD.                    "bitbyte_to_int

  METHOD x_to_bitbyte.

    DATA: lv_b TYPE n.

    CLEAR rv_bitbyte.

    DO 8 TIMES.
      GET BIT sy-index OF iv_x INTO lv_b.
      CONCATENATE rv_bitbyte lv_b INTO rv_bitbyte.
    ENDDO.

  ENDMETHOD.                    "x_to_bitbyte

ENDCLASS.                    "lcl_convert IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_diff DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_diff,
                 insert TYPE c LENGTH 1 VALUE 'I',
                 delete TYPE c LENGTH 1 VALUE 'D',
                 update TYPE c LENGTH 1 VALUE 'U',
               END OF c_diff.

    TYPES: BEGIN OF ty_diff,
             local  TYPE string,
             result TYPE c LENGTH 1,
             remote TYPE string,
           END OF ty_diff.
    TYPES: ty_diffs_tt TYPE STANDARD TABLE OF ty_diff WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_count,
             insert TYPE i,
             delete TYPE i,
             update TYPE i,
           END OF ty_count.

* assumes data is UTF8 based with newlines
* only works with lines up to 255 characters
    METHODS constructor
      IMPORTING iv_local  TYPE xstring
                iv_remote TYPE xstring.

    METHODS get
      RETURNING VALUE(rt_diff) TYPE ty_diffs_tt.

    METHODS stats
      RETURNING VALUE(rs_count) TYPE ty_count.

  PRIVATE SECTION.
    DATA mt_diff TYPE ty_diffs_tt.

    CLASS-METHODS: unpack
      IMPORTING iv_local  TYPE xstring
                iv_remote TYPE xstring
      EXPORTING et_local  TYPE abaptxt255_tab
                et_remote TYPE abaptxt255_tab.

    CLASS-METHODS: render
      IMPORTING it_local       TYPE abaptxt255_tab
                it_remote      TYPE abaptxt255_tab
                it_delta       TYPE vxabapt255_tab
      RETURNING VALUE(rt_diff) TYPE ty_diffs_tt.

    CLASS-METHODS: compute
      IMPORTING it_local        TYPE abaptxt255_tab
                it_remote       TYPE abaptxt255_tab
      RETURNING VALUE(rt_delta) TYPE vxabapt255_tab.

ENDCLASS.                    "lcl_diff DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_diff IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff IMPLEMENTATION.

  METHOD get.
    rt_diff = mt_diff.
  ENDMETHOD.                    "get

  METHOD stats.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    LOOP AT mt_diff ASSIGNING <ls_diff>.
      CASE <ls_diff>-result.
        WHEN lcl_diff=>c_diff-insert.
          rs_count-insert = rs_count-insert + 1.
        WHEN lcl_diff=>c_diff-delete.
          rs_count-delete = rs_count-delete + 1.
        WHEN lcl_diff=>c_diff-update.
          rs_count-update = rs_count-update + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "count

  METHOD unpack.

    DATA: lv_local  TYPE string,
          lv_remote TYPE string.


    lv_local  = lcl_convert=>xstring_to_string_utf8( iv_local ).
    lv_remote = lcl_convert=>xstring_to_string_utf8( iv_remote ).

    SPLIT lv_local  AT gc_newline INTO TABLE et_local.
    SPLIT lv_remote AT gc_newline INTO TABLE et_remote.

  ENDMETHOD.                    "unpack

  METHOD compute.

    DATA: lt_trdirtab_old TYPE TABLE OF trdir,
          lt_trdirtab_new TYPE TABLE OF trdir,
          lt_trdir_delta  TYPE TABLE OF xtrdir.


    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = it_remote
        texttab_new  = it_local
        trdirtab_old = lt_trdirtab_old
        trdirtab_new = lt_trdirtab_new
        trdir_delta  = lt_trdir_delta
        text_delta   = rt_delta.

  ENDMETHOD.                    "compute

  METHOD constructor.

    DATA: lt_delta  TYPE vxabapt255_tab,
          lt_local  TYPE abaptxt255_tab,
          lt_remote TYPE abaptxt255_tab.


    unpack( EXPORTING iv_local  = iv_local
                      iv_remote = iv_remote
            IMPORTING et_local  = lt_local
                      et_remote = lt_remote ).

    lt_delta = compute( it_local  = lt_local
                        it_remote = lt_remote ).

    mt_diff = render( it_local  = lt_local
                      it_remote = lt_remote
                      it_delta  = lt_delta ).

  ENDMETHOD.                    "diff

  METHOD render.

    DEFINE _append.
      clear ls_diff.
      ls_diff-local = &1.
      ls_diff-result = &2.
      ls_diff-remote = &3.
      append ls_diff to rt_diff.
    END-OF-DEFINITION.

    DATA: lv_rindex TYPE i VALUE 1,
          lv_lindex TYPE i VALUE 1,
          ls_local  LIKE LINE OF it_local,
          ls_remote LIKE LINE OF it_remote,
          ls_diff   LIKE LINE OF rt_diff,
          lt_delta  LIKE it_delta,
          ls_delta  LIKE LINE OF it_delta.


    lt_delta = it_delta.

    DO.
      READ TABLE lt_delta INTO ls_delta WITH KEY number = lv_rindex.
      IF sy-subrc = 0.
        DELETE lt_delta INDEX sy-tabix.

        CASE ls_delta-vrsflag.
          WHEN c_diff-delete.
            _append '' c_diff-delete ls_delta-line.
            lv_rindex = lv_rindex + 1.
          WHEN c_diff-insert.
            _append ls_delta-line c_diff-insert ''.
            lv_lindex = lv_lindex + 1.
          WHEN c_diff-update.
            CLEAR ls_local.
            READ TABLE it_local INTO ls_local INDEX lv_lindex.
            ASSERT sy-subrc = 0.
            _append ls_local c_diff-update ls_delta-line.
            lv_lindex = lv_lindex + 1.
            lv_rindex = lv_rindex + 1.
          WHEN OTHERS.
            ASSERT 1 = 1 + 1.
        ENDCASE.
      ELSE.
        CLEAR ls_local.
        READ TABLE it_local INTO ls_local INDEX lv_lindex. "#EC CI_SUBRC
        lv_lindex = lv_lindex + 1.
        CLEAR ls_remote.
        READ TABLE it_remote INTO ls_remote INDEX lv_rindex. "#EC CI_SUBRC
        lv_rindex = lv_rindex + 1.
        _append ls_local '' ls_remote.
      ENDIF.

      IF lv_lindex > lines( it_local ) AND lv_rindex > lines( it_remote ).
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "render

ENDCLASS.                    "lcl_diff IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_pack DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_node,
             chmod TYPE string,
             name  TYPE string,
             sha1  TYPE ty_sha1,
           END OF ty_node.
    TYPES: ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_object,
             sha1 TYPE ty_sha1,
             type TYPE ty_type,
             data TYPE xstring,
           END OF ty_object.
    TYPES: ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_commit,
             tree      TYPE ty_sha1,
             parent    TYPE ty_sha1,
             author    TYPE string,
             committer TYPE string,
             body      TYPE string,
           END OF ty_commit.

    CLASS-METHODS decode
      IMPORTING iv_data           TYPE xstring
      RETURNING VALUE(rt_objects) TYPE ty_objects_tt
      RAISING   lcx_exception.

    CLASS-METHODS decode_tree
      IMPORTING iv_data         TYPE xstring
      RETURNING VALUE(rt_nodes) TYPE ty_nodes_tt
      RAISING   lcx_exception.

    CLASS-METHODS decode_deltas
      CHANGING ct_objects TYPE ty_objects_tt
      RAISING  lcx_exception.

    CLASS-METHODS decode_commit
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rs_commit) TYPE ty_commit
      RAISING   lcx_exception.

    CLASS-METHODS encode
      IMPORTING it_objects     TYPE ty_objects_tt
      RETURNING VALUE(rv_data) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS encode_tree
      IMPORTING it_nodes       TYPE ty_nodes_tt
      RETURNING VALUE(rv_data) TYPE xstring.

    CLASS-METHODS encode_commit
      IMPORTING is_commit      TYPE ty_commit
      RETURNING VALUE(rv_data) TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: c_pack_start TYPE x LENGTH 4 VALUE '5041434B', " PACK
               c_debug_pack TYPE sap_bool VALUE abap_false,
               c_zlib       TYPE x LENGTH 2 VALUE '789C',
               c_zlib_hmm   TYPE x LENGTH 2 VALUE '7801',
               c_version    TYPE x LENGTH 4 VALUE '00000002'.

    CLASS-METHODS type_and_length
      IMPORTING is_object         TYPE ty_object
      RETURNING VALUE(rv_xstring) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS delta
      IMPORTING is_object  TYPE ty_object
      CHANGING  ct_objects TYPE ty_objects_tt
      RAISING   lcx_exception.

    CLASS-METHODS delta_header
      EXPORTING ev_header TYPE i
      CHANGING  cv_delta  TYPE xstring.

    CLASS-METHODS get_type
      IMPORTING iv_x           TYPE x
      RETURNING VALUE(rv_type) TYPE ty_type
      RAISING   lcx_exception.

    CLASS-METHODS get_length
      EXPORTING ev_length TYPE i
      CHANGING  cv_data   TYPE xstring.

ENDCLASS.                    "lcl_pack DEFINITION

INTERFACE lif_object.

  METHODS:
    serialize
      RAISING lcx_exception,
    deserialize
      IMPORTING iv_package TYPE devclass
      RAISING   lcx_exception,
    delete
      RAISING lcx_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool
      RAISING   lcx_exception,
    jump
      RAISING lcx_exception.

ENDINTERFACE.

CLASS lcl_objects_activation DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING iv_type TYPE trobjtype
                iv_name TYPE clike
      RAISING   lcx_exception.

    CLASS-METHODS add_item
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS activate
      RAISING lcx_exception.

    CLASS-METHODS clear.

  PRIVATE SECTION.
    CLASS-DATA: gt_ddic     TYPE TABLE OF dwinactiv,
                gt_programs TYPE TABLE OF dwinactiv.

ENDCLASS.

CLASS lcl_objects_activation IMPLEMENTATION.

  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.

  METHOD clear.
    CLEAR: gt_ddic,
           gt_programs.
  ENDMETHOD.

  METHOD activate.

* ddic
    IF NOT gt_ddic IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_true
          with_popup             = abap_true
        TABLES
          objects                = gt_ddic
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        _raise 'error from RS_WORKING_OBJECTS_ACTIVATE'.
      ENDIF.
    ENDIF.

* programs
    IF NOT gt_programs IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_false
          with_popup             = abap_true
        TABLES
          objects                = gt_programs
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        _raise 'error from RS_WORKING_OBJECTS_ACTIVATE'.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "activate

  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    DATA: lt_objects  TYPE dwinactiv_tab,
          lv_obj_name TYPE dwinactiv-obj_name.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    lv_obj_name = iv_name.

* todo, refactoring
    CASE iv_type.
      WHEN 'CLAS' OR 'WDYN'.
        CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
          EXPORTING
            obj_name         = lv_obj_name
            object           = iv_type
          TABLES
            inactive_objects = lt_objects
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          _raise 'Error from RS_INACTIVE_OBJECTS_IN_OBJECT'.
        ENDIF.

        APPEND LINES OF lt_objects TO gt_programs.
      WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'INDX' OR 'TTYP' OR 'VIEW' OR 'SHLP' OR 'ENQU'.
* todo also insert_into_working_area?
        APPEND INITIAL LINE TO gt_ddic ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN 'REPS' OR 'DYNP' OR 'CUAD' OR 'REPT' OR 'INTF' OR 'FUNC' OR 'ENHO' OR 'TYPE'.
* these seem to go into the workarea automatically
        APPEND INITIAL LINE TO gt_programs ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN OTHERS.
        _raise 'activate, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "activate

ENDCLASS.

CLASS lcl_objects_files DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_item TYPE ty_item,
      add_xml
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO lcl_xml
                  iv_normalize TYPE sap_bool DEFAULT abap_true
        RAISING   lcx_exception,
      read_xml
        IMPORTING iv_extra      TYPE clike OPTIONAL
        RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml
        RAISING   lcx_exception,
      read_abap
        IMPORTING iv_extra       TYPE clike OPTIONAL
                  iv_error       TYPE sap_bool DEFAULT abap_true
        RETURNING VALUE(rt_abap) TYPE abaptxt255_tab
        RAISING   lcx_exception,
      add_abap
        IMPORTING iv_extra TYPE clike OPTIONAL
                  it_abap  TYPE STANDARD TABLE
        RAISING   lcx_exception,
      add
        IMPORTING is_file TYPE ty_file,
      get_files
        RETURNING VALUE(rt_files) TYPE ty_files_tt,
      set_files
        IMPORTING it_files TYPE ty_files_tt.

  PRIVATE SECTION.
    DATA: ms_item  TYPE ty_item,
          mt_files TYPE ty_files_tt.

    METHODS:
      filename
        IMPORTING iv_extra           TYPE clike OPTIONAL
                  iv_ext             TYPE string
        RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS.

CLASS lcl_objects_files IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.

  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.

  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.

  METHOD set_files.
    mt_files = it_files.
  ENDMETHOD.

  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_abap     TYPE string.

    FIELD-SYMBOLS: <ls_abap> LIKE LINE OF mt_files.


    CLEAR rt_abap.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'abap' ).            "#EC NOTEXT

    READ TABLE mt_files ASSIGNING <ls_abap> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        _raise 'abap not found'.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.
    lv_abap = lcl_convert=>xstring_to_string_utf8( <ls_abap>-data ).

    SPLIT lv_abap AT gc_newline INTO TABLE rt_abap.

  ENDMETHOD.                    "read_abap

  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY gc_newline.
    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'abap' ).       "#EC NOTEXT
    ls_file-data = lcl_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "abap_to_file

  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE ty_file.


    lv_xml = io_xml->xml_render( iv_normalize = iv_normalize ).
    ls_file-path = '/'.

    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'xml' ).        "#EC NOTEXT

    REPLACE FIRST OCCURRENCE
      OF '<?xml version="1.0" encoding="utf-16"?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ls_file-data = lcl_convert=>string_to_xstring_utf8( lv_xml ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "do

  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_xml      TYPE string.

    FIELD-SYMBOLS: <ls_xml> LIKE LINE OF mt_files.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'xml' ).             "#EC NOTEXT

    READ TABLE mt_files ASSIGNING <ls_xml> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'xml not found'.
    ENDIF.

    lv_xml = lcl_convert=>xstring_to_string_utf8( <ls_xml>-data ).

    CREATE OBJECT ro_xml
      EXPORTING
        iv_xml = lv_xml.

  ENDMETHOD.

  METHOD filename.

    DATA: lv_obj_name TYPE string.


    IF ms_item-obj_type = 'SICF'.
* multiple SICF nodes with same name cannot be added to repository
      lv_obj_name = ms_item-obj_name(15).
    ELSE.
      lv_obj_name = ms_item-obj_name.
    ENDIF.
* handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN lv_obj_name WITH '#'.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ELSE.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_extra '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ENDIF.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item TYPE ty_item,
      set_files
        IMPORTING
          io_files TYPE REF TO lcl_objects_files.

  PROTECTED SECTION.

    DATA: ms_item  TYPE ty_item,
          mo_files TYPE REF TO lcl_objects_files.

    METHODS:
      corr_insert
        IMPORTING iv_package TYPE devclass
        RAISING   lcx_exception,
      jump_se11
        IMPORTING iv_radio TYPE string
                  iv_field TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_objects_super DEFINITION

CLASS lcl_objects_program DEFINITION INHERITING FROM lcl_objects_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_progdir,
             name    TYPE progdir-name,
             state   TYPE progdir-state,
             sqlx    TYPE progdir-sqlx,
             edtx    TYPE progdir-edtx,
             varcl   TYPE progdir-varcl,
             dbapl   TYPE progdir-dbapl,
             dbna    TYPE progdir-dbna,
             clas    TYPE progdir-clas,
             type    TYPE progdir-type,
             occurs  TYPE progdir-occurs,
             subc    TYPE progdir-subc,
             appl    TYPE progdir-appl,
             secu    TYPE progdir-secu,
             cnam    TYPE progdir-cnam,
             cdat    TYPE progdir-cdat,
             unam    TYPE progdir-unam,
             udat    TYPE progdir-udat,
             vern    TYPE progdir-vern,
             levl    TYPE progdir-levl,
             rstat   TYPE progdir-rstat,
             rmand   TYPE progdir-rmand,
             rload   TYPE progdir-rload,
             fixpt   TYPE progdir-fixpt,
             sset    TYPE progdir-sset,
             sdate   TYPE progdir-sdate,
             stime   TYPE progdir-stime,
             idate   TYPE progdir-idate,
             itime   TYPE progdir-itime,
             ldbname TYPE progdir-ldbname,
             uccheck TYPE progdir-uccheck,
           END OF ty_progdir.

    CLASS-METHODS serialize_program
      IMPORTING is_item    TYPE ty_item
                io_files   TYPE REF TO lcl_objects_files
                iv_program TYPE programm OPTIONAL
                iv_extra   TYPE clike OPTIONAL
      RAISING   lcx_exception.

    CLASS-METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    CLASS-METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS serialize_dynpros
      IMPORTING iv_program_name TYPE programm
                io_xml          TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS serialize_cua
      IMPORTING iv_program_name TYPE programm
                io_xml          TYPE REF TO lcl_xml
      RAISING   lcx_exception.
ENDCLASS.

CLASS lcl_objects_program IMPLEMENTATION.

  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          lo_xml          TYPE REF TO lcl_xml.

    IF iv_program IS INITIAL.
      lv_program_name = is_item-obj_name.
    ELSE.
      lv_program_name = iv_program.
    ENDIF.

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'Error reading program'.
    ENDIF.

    ls_progdir = read_progdir( lv_program_name ).

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ig_structure = ls_progdir
                           iv_name      = 'PROGDIR' ).
    IF ls_progdir-subc = '1'.
      serialize_dynpros( iv_program_name = lv_program_name
                         io_xml          = lo_xml ).
      serialize_cua( iv_program_name = lv_program_name
                     io_xml          = lo_xml ).
    ENDIF.

    IF lines( lt_tpool ) = 1.
      READ TABLE lt_tpool INDEX 1 INTO ls_tpool.
      ASSERT sy-subrc = 0.
      IF ls_tpool-id = 'R' AND ls_tpool-key = '' AND ls_tpool-length = 0.
        DELETE lt_tpool INDEX 1.
      ENDIF.
    ENDIF.

    lo_xml->table_add( lt_tpool ).

    io_files->add_xml( iv_extra = iv_extra
                       io_xml   = lo_xml ).

    io_files->add_abap( iv_extra = iv_extra
                        it_abap  = lt_source ).

  ENDMETHOD.                    "serialize_program

  METHOD deserialize_program.

    DATA: lv_exists      TYPE sap_bool,
          lv_progname    TYPE reposrc-progname,
          ls_tpool       LIKE LINE OF it_tpool,
          lv_title       TYPE rglif-title,
          ls_progdir_new TYPE progdir.


    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = is_progdir-name
        object_class        = 'ABAP'
        devclass            = iv_package
        master_language     = gc_english
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      _raise 'Cancelled'.
    ELSEIF sy-subrc <> 0.
      _raise 'error from RS_CORR_INSERT'.
    ENDIF.

    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.  "#EC CI_SUBRC
    lv_title = ls_tpool-entry.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = is_progdir-name
      AND r3state = 'A'.
    IF sy-subrc = 0.
      lv_exists = abap_true.
    ELSE.
      lv_exists = abap_false.
    ENDIF.

    IF lv_exists = abap_true.
      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
        EXPORTING
          program_name     = is_progdir-name
          title_string     = lv_title
          save_inactive    = 'I'
        TABLES
          source_extended  = it_source
        EXCEPTIONS
          cancelled        = 1
          permission_error = 2
          not_found        = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        IF sy-msgid = 'EU' AND sy-msgno = '510'.
          _raise 'User is currently editing program'.
        ELSE.
          _raise 'PROG, error updating'.
        ENDIF.
      ENDIF.
    ELSE.
* function module RPY_PROGRAM_INSERT cannot handle function group includes

      INSERT REPORT is_progdir-name
        FROM it_source
        STATE 'I'
        PROGRAM TYPE is_progdir-subc.
      IF sy-subrc <> 0.
        _raise 'error from INSERT REPORT'.
      ENDIF.

      IF NOT it_tpool[] IS INITIAL.
        INSERT TEXTPOOL is_progdir-name
          FROM it_tpool
          LANGUAGE gc_english
          STATE 'I'.
        IF sy-subrc <> 0.
          _raise 'error from INSERT TEXTPOOL'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'READ_PROGDIR'
        EXPORTING
          i_progname = is_progdir-name
          i_state    = 'I'
        IMPORTING
          e_progdir  = ls_progdir_new
        EXCEPTIONS
          not_exists = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
        _raise 'not found in PROGDIR'.
      ENDIF.

* todo, package?

      ls_progdir_new-ldbname = is_progdir-ldbname.
      ls_progdir_new-dbna    = is_progdir-dbna.
      ls_progdir_new-dbapl   = is_progdir-dbapl.
      ls_progdir_new-rload   = is_progdir-rload.
      ls_progdir_new-fixpt   = is_progdir-fixpt.
      ls_progdir_new-varcl   = is_progdir-varcl.
      ls_progdir_new-appl    = is_progdir-appl.

      CALL FUNCTION 'UPDATE_PROGDIR'
        EXPORTING
          i_progdir    = ls_progdir_new
          i_progname   = ls_progdir_new-name
          i_state      = ls_progdir_new-state
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        _raise 'PROG, error inserting'.
      ENDIF.

    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPS'
                                 iv_name = is_progdir-name ).

  ENDMETHOD.                    "deserialize_program

  METHOD read_progdir.

    DATA: ls_sapdir TYPE progdir.


    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_program
        i_state    = 'A'
      IMPORTING
        e_progdir  = ls_sapdir.
    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime.

  ENDMETHOD.                    "read_progdir

  METHOD serialize_cua.

    DATA: ls_adm TYPE rsmpe_adm,
          lt_sta TYPE TABLE OF rsmpe_stat,
          lt_fun TYPE TABLE OF rsmpe_funt,
          lt_men TYPE TABLE OF rsmpe_men,
          lt_mtx TYPE TABLE OF rsmpe_mnlt,
          lt_act TYPE TABLE OF rsmpe_act,
          lt_but TYPE TABLE OF rsmpe_but,
          lt_pfk TYPE TABLE OF rsmpe_pfk,
          lt_set TYPE TABLE OF rsmpe_staf,
          lt_doc TYPE TABLE OF rsmpe_atrt,
          lt_tit TYPE TABLE OF rsmpe_titt,
          lt_biv TYPE TABLE OF rsmpe_buts.


    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = gc_english
        state           = 'A'
      IMPORTING
        adm             = ls_adm
      TABLES
        sta             = lt_sta
        fun             = lt_fun
        men             = lt_men
        mtx             = lt_mtx
        act             = lt_act
        but             = lt_but
        pfk             = lt_pfk
        set             = lt_set
        doc             = lt_doc
        tit             = lt_tit
        biv             = lt_biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_FETCH'.
    ENDIF.

    io_xml->structure_add( ls_adm ).
    io_xml->table_add( it_table = lt_sta
                       iv_name = 'RSMPE_STAT_TABLE' ).
    io_xml->table_add( it_table = lt_fun
                       iv_name = 'RSMPE_FUNT_TABLE' ).
    io_xml->table_add( it_table = lt_men
                       iv_name = 'RSMPE_MEN_TABLE' ).
    io_xml->table_add( it_table = lt_mtx
                       iv_name = 'RSMPE_MNLT_TABLE' ).
    io_xml->table_add( it_table = lt_act
                       iv_name = 'RSMPE_ACT_TABLE' ).
    io_xml->table_add( it_table = lt_but
                       iv_name = 'RSMPE_BUT_TABLE' ).
    io_xml->table_add( it_table = lt_pfk
                       iv_name = 'RSMPE_PFK_TABLE' ).
    io_xml->table_add( it_table = lt_set
                       iv_name = 'RSMPE_STAF_TABLE' ).
    io_xml->table_add( it_table = lt_doc
                       iv_name = 'RSMPE_ATRT_TABLE' ).
    io_xml->table_add( it_table = lt_tit
                       iv_name = 'RSMPE_TITT_TABLE' ).
    io_xml->table_add( it_table = lt_biv
                       iv_name = 'RSMPE_BUTS_TABLE' ).

  ENDMETHOD.                    "serialize_cua

  METHOD serialize_dynpros.

    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          li_element              TYPE REF TO if_ixml_element,
          lt_dynpros              TYPE TABLE OF d020s.

    FIELD-SYMBOLS: <ls_dynpro> LIKE LINE OF lt_dynpros.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_dynpros
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      _raise 'error from screen_list'.
    ENDIF.

* loop dynpros and skip generated selection screens
    LOOP AT lt_dynpros ASSIGNING <ls_dynpro> WHERE type <> 'S'.

      li_element = io_xml->xml_element( 'SCREEN' ).

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_dynpro>-dnum
        IMPORTING
          header               = ls_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields_to_containers
          flow_logic           = lt_flow_logic
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        _raise 'Error while reading dynpro'.
      ENDIF.

      io_xml->structure_add( ig_structure = ls_header
                             ii_root      = li_element ).

      io_xml->table_add( it_table = lt_containers
                         ii_root  = li_element ).
      io_xml->table_add( it_table = lt_fields_to_containers
                         ii_root  = li_element ).
      io_xml->table_add( it_table = lt_flow_logic
                         ii_root  = li_element ).

      io_xml->xml_add( li_element ).

    ENDLOOP.

  ENDMETHOD.                    "serialize_dynpros

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super IMPLEMENTATION.

  METHOD constructor.
    ms_item  = is_item.
  ENDMETHOD.

  METHOD jump_se11.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSD_ENTRY'.
    <ls_bdcdata>-dynpro   = '1000'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=WB_DISPLAY'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_radio.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_field.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE11'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4 ##fm_subrc_ok. "#EC CI_SUBRC

  ENDMETHOD.                                                "jump_se11

  METHOD set_files.
    mo_files = io_files.
  ENDMETHOD.

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = ms_item-obj_type.
    ls_object-objname = ms_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        devclass            = iv_package
        master_language     = gc_english
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      _raise 'Cancelled'.
    ELSEIF sy-subrc <> 0.
      _raise 'error from RS_CORR_INSERT'.
    ENDIF.

  ENDMETHOD.                    "corr_insert

ENDCLASS.                    "lcl_objects_super IMPLEMENTATION

CLASS lcl_object_acid DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS: create_object
      RETURNING VALUE(ro_aab) TYPE REF TO cl_aab_id
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_acid IMPLEMENTATION.

  METHOD create_object.

    DATA: lv_name TYPE aab_id_name.


    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab
      EXPORTING
        im_name          = lv_name
      EXCEPTIONS
        name_not_allowed = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      _raise 'error creating CL_AAB_ID object'.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_xml         TYPE REF TO lcl_xml,
          lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    create_object( )->get_descript( IMPORTING ex_descript = lv_description ).

    CREATE OBJECT lo_xml.
    lo_xml->element_add( lv_description ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lo_xml         TYPE REF TO lcl_xml,
          lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    lo_xml = mo_files->read_xml( ).
    lo_xml->element_read( CHANGING cg_element = lv_description ).

    lo_aab = create_object( ).
    lo_aab->set_descript( lv_description ).
    lo_aab->save( ).

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->delete(
      EXCEPTIONS
        prop_error       = 1
        propt_error      = 2
        act_error        = 3
        cts_error        = 4
        cts_devclass     = 5
        id_not_found     = 6
        no_authorization = 7
        id_still_used    = 8
        where_used_error = 9
        OTHERS           = 10 ).
    IF sy-subrc <> 0.
      _raise 'error deleting ACID object'.
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_state TYPE flag,
          lo_aab   TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).

    lo_aab->get_state(
      IMPORTING
        ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.

  METHOD lif_object~jump.
    _raise 'todo, jump, ACID'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_auth DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.

CLASS lcl_object_auth IMPLEMENTATION.

  METHOD lif_object~serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_authx TYPE authx.


    SELECT SINGLE * FROM authx INTO ls_authx
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_authx ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.

  METHOD lif_object~deserialize.
* see include LSAUT_FIELDF02

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_authx TYPE authx,
          lo_auth  TYPE REF TO cl_auth_tools.



    lo_xml = mo_files->read_xml( ).
    lo_xml->structure_read( CHANGING cg_structure = ls_authx ).

    CREATE OBJECT lo_auth.

    IF lo_auth->add_afield_to_trkorr( ls_authx-fieldname ) <> 0.
      _raise 'Error deserializing AUTH'.
    ENDIF.

    MODIFY authx FROM ls_authx.
    IF sy-subrc <> 0.
      _raise 'Error deserializing AUTH'.
    ENDIF.

    CALL FUNCTION 'DB_COMMIT'.
    lo_auth->set_authfld_info_from_db( ls_authx-fieldname ).

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: lv_fieldname TYPE authx-fieldname.


    lv_fieldname = ms_item-obj_name.

    CALL FUNCTION 'SUSR_AUTF_DELETE_FIELD'
      EXPORTING
        fieldname           = lv_fieldname
      EXCEPTIONS
        delete_not_possible = 1
        field_in_use        = 2
        not_existing        = 3
        no_authority        = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      _raise 'error from SUSR_AUTF_DELETE_FIELD'.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_fieldname TYPE authx-fieldname.


    SELECT SINGLE fieldname FROM authx
      INTO lv_fieldname
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    _raise 'todo, AUTH jump'.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_doma DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_domname TYPE dd01l-domname.


    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DOMA'
               iv_field = 'RSRD1-DOMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.
* see class CL_WB_DDIC

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'D'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, DOMA'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v,
          lo_xml   TYPE REF TO lcl_xml.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        langu         = gc_english
      IMPORTING
        dd01v_wa      = ls_dd01v
      TABLES
        dd07v_tab     = lt_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_DOMA_GET'.
    ENDIF.
    IF ls_dd01v IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd01v ).
    lo_xml->table_add( iv_name = 'DD07V_TAB'
                       it_table = lt_dd07v ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_dd01v TYPE dd01v,
          lv_name  TYPE ddobjname,
          lt_dd07v TYPE TABLE OF dd07v.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd01v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD07V_TAB'
                        CHANGING ct_table = lt_dd07v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = lv_name
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_DOMA_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_doma IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.


    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'E'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, DTEL'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd04v TYPE dd04v,
          ls_tpara TYPE tpara,
          lo_xml   TYPE REF TO lcl_xml.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = lv_name
        langu         = gc_english
      IMPORTING
        dd04v_wa      = ls_dd04v
        tpara_wa      = ls_tpara
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'Error from DDIF_DTEL_GET'.
    ENDIF.
    IF ls_dd04v IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    CLEAR: ls_dd04v-as4user,
           ls_dd04v-as4date,
           ls_dd04v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd04v ).
    lo_xml->structure_add( ls_tpara ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname,
          ls_tpara TYPE tpara.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd04v ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tpara ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = lv_name
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_DTEL_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_dtel IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    DATA mv_skip_testclass TYPE abap_bool.

    METHODS deserialize_abap
      IMPORTING io_xml     TYPE REF TO lcl_xml
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS deserialize_textpool
      IMPORTING io_xml TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    METHODS deserialize_docu
      IMPORTING io_xml TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    METHODS serialize_abap
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_locals_imp
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_locals_def
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_testclasses
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_macros
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_xml
      RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    METHODS remove_signatures
      CHANGING ct_source TYPE ty_string_tt.

    METHODS reduce
      CHANGING ct_source TYPE ty_string_tt.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = ls_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_bool = boolc( sy-subrc <> 2 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_interface = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_CLASS_DELETE_COMPLETE'.
        ENDIF.
      WHEN 'INTF'.
        CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
          EXPORTING
            intkey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_class     = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_INTERFACE_DELETE_COMPLETE'.
        ENDIF.
      WHEN OTHERS.
        _raise 'class delete, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "delete

  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE sap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source[].
    ENDIF.

  ENDMETHOD.                    "reduce

  METHOD serialize_locals_imp.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_locals_imp
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, imp'.
    ENDIF.

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_local

  METHOD serialize_locals_def.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_locals_def
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, def'.
    ENDIF.

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_locals_def

  METHOD serialize_testclasses.

    DATA: lv_line1 LIKE LINE OF rt_source,
          lv_line2 LIKE LINE OF rt_source.


    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_testclasses
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, test'.
    ENDIF.

* when creating classes in Eclipse it automatically generates the
* testclass include, but it is not needed, so skip to avoid
* creating an extra file in the repository.
* Also remove it if the content is manually removed, but
* the class still thinks it contains tests
    mv_skip_testclass = abap_false.
    IF lines( rt_source ) = 2.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      READ TABLE rt_source INDEX 2 INTO lv_line2.
      IF lv_line1(3) = '*"*' AND lv_line2 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 1.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      IF lv_line1(3) = '*"*' OR lv_line1 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 0.
      mv_skip_testclass = abap_true.
    ENDIF.

  ENDMETHOD.                    "serialize_test

  METHOD serialize_macros.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_macros
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, macros'.
    ENDIF.

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_macro

  METHOD serialize_abap.

    DATA: lo_source TYPE REF TO cl_oo_source.


    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      _raise 'error from CL_OO_SOURCE'.
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_abap

  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE sap_bool,
          lv_source LIKE LINE OF ct_source.


    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "remove_signatures

  METHOD lif_object~serialize.

    DATA: lt_source TYPE seop_source_string,
          lo_xml    TYPE REF TO lcl_xml,
          ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = seox_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = seox_true.

    lt_source = serialize_abap( ls_clskey ).
    mo_files->add_abap( lt_source ).

    IF ms_item-obj_type = 'CLAS'.
      lt_source = serialize_locals_def( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'locals_def'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_locals_imp( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'locals_imp'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_testclasses( ls_clskey ).
      IF NOT lt_source[] IS INITIAL AND mv_skip_testclass = abap_false.
        mo_files->add_abap( iv_extra = 'testclasses'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_macros( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'macros'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.
    ENDIF.

    lo_xml = serialize_xml( ).
    IF lo_xml IS BOUND.
      mo_files->add_xml( lo_xml ).
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD serialize_xml.

    DATA: ls_vseoclass  TYPE vseoclass,
          lv_cp         TYPE program,
          lt_tpool      TYPE textpool_table,
          lv_object     TYPE dokhl-object,
          lv_state      TYPE dokhl-dokstate,
          ls_vseointerf TYPE vseointerf,
          ls_clskey     TYPE seoclskey,
          lt_lines      TYPE tlinetab.


    ls_clskey-clsname = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = ls_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
        interface    = ls_vseointerf
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      _raise 'error from seo_clif_get'.
    ENDIF.

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release,
           ls_vseoclass-chgdanyby,
           ls_vseoclass-chgdanyon.

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-r3release.

    CREATE OBJECT ro_xml.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        ro_xml->structure_add( ls_vseoclass ).

        lv_cp = cl_oo_classname_service=>get_classpool_name( ls_clskey-clsname ).
        READ TEXTPOOL lv_cp INTO lt_tpool LANGUAGE gc_english. "#EC CI_READ_REP
        ro_xml->table_add( lt_tpool ).
      WHEN 'INTF'.
        ro_xml->structure_add( ls_vseointerf ).
      WHEN OTHERS.
        ASSERT 1 = 1 + 1.
    ENDCASE.

    lv_object = ls_clskey-clsname.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = gc_english
        object            = lv_object
      IMPORTING
        dokstate          = lv_state
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lv_state = 'R'.
      ro_xml->table_add( lt_lines ).
    ENDIF.

  ENDMETHOD.                    "serialize_xml

  METHOD lif_object~deserialize.

* function group SEOK
* function group SEOQ
* function group SEOP
* class CL_OO_CLASSNAME_SERVICE
* class CL_OO_SOURCE

    DATA: lo_xml TYPE REF TO lcl_xml.


    lo_xml = mo_files->read_xml( ).

    deserialize_abap( io_xml     = lo_xml
                      iv_package = iv_package ).

    IF ms_item-obj_type = 'CLAS'.
      deserialize_textpool( lo_xml ).
    ENDIF.

    deserialize_docu( lo_xml ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.


    io_xml->table_read( CHANGING ct_table = lt_lines ).

    IF lt_lines[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'CL'
        langu    = gc_english
        object   = lv_object
      TABLES
        line     = lt_lines
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      _raise 'error from DOCU_UPD'.
    ENDIF.

  ENDMETHOD.                    "deserialize_doku

  METHOD deserialize_textpool.

    DATA: lv_cp      TYPE program,
          lv_clsname TYPE seoclsname,
          lt_tpool   TYPE textpool_table.


    io_xml->table_read( CHANGING ct_table = lt_tpool ).

    IF lt_tpool[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.
    lv_cp = cl_oo_classname_service=>get_classpool_name( lv_clsname ).

    INSERT TEXTPOOL lv_cp
      FROM lt_tpool
      LANGUAGE gc_english
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = lv_cp ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_abap.

    DATA: ls_vseoclass   TYPE vseoclass,
          ls_vseointerf  TYPE vseointerf,
          lt_source      TYPE seop_source_string,
          lo_source      TYPE REF TO cl_oo_source,
          lt_locals_def  TYPE seop_source_string,
          lt_locals_imp  TYPE seop_source_string,
          lt_locals_mac  TYPE seop_source_string,
          lt_testclasses TYPE seop_source_string,
          ls_clskey      TYPE seoclskey.


    lt_source = mo_files->read_abap( ).

    lt_locals_def = mo_files->read_abap( iv_extra = 'locals_def'
                                         iv_error = abap_false ). "#EC NOTEXT

    lt_locals_imp = mo_files->read_abap( iv_extra = 'locals_imp'
                                         iv_error = abap_false ). "#EC NOTEXT

    lt_locals_mac = mo_files->read_abap( iv_extra = 'macros'
                                         iv_error = abap_false ). "#EC NOTEXT

    lt_testclasses = mo_files->read_abap( iv_extra = 'testclasses'
                                          iv_error = abap_false ). "#EC NOTEXT

    ls_clskey-clsname = ms_item-obj_name.


    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        io_xml->structure_read( CHANGING cg_structure = ls_vseoclass ).

        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = seox_true
          CHANGING
            class           = ls_vseoclass
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
        IF sy-subrc <> 0.
          _raise 'error from SEO_CLASS_CREATE_COMPLETE'.
        ENDIF.

      WHEN 'INTF'.
        io_xml->structure_read( CHANGING cg_structure = ls_vseointerf ).

        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = seox_true
          CHANGING
            interface       = ls_vseointerf
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_INTERFACE_CREATE_COMPLETE'.
        ENDIF.

      WHEN OTHERS.
        ASSERT 1 = 1 + 1.
    ENDCASE.

    IF ms_item-obj_type = 'CLAS'.
      CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
        EXPORTING
          clskey                 = ls_clskey
          force                  = seox_true
          locals_def             = lt_locals_def
          locals_imp             = lt_locals_imp
          locals_mac             = lt_locals_mac
          locals_testclasses     = lt_testclasses
        EXCEPTIONS
          not_existing           = 1
          model_only             = 2
          locals_not_generated   = 3
          locals_not_initialised = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
        _raise 'error from generate_locals'.
      ENDIF.
    ENDIF.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = ls_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      _raise 'error from CL_OO_SOURCE'.
    ENDIF.

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( lt_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        _raise 'permission error'.
      CATCH cx_oo_source_save_failure.
        _raise 'save failure'.
    ENDTRY.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_CLAS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_smim DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_smim DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS get_filename
      IMPORTING iv_url             TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

    METHODS find_content
      IMPORTING iv_url            TYPE string
      RETURNING VALUE(rv_content) TYPE xstring
      RAISING   lcx_exception.

    METHODS build_filename
      IMPORTING iv_filename        TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

    METHODS get_url_for_io
      EXPORTING ev_url       TYPE string
                ev_is_folder TYPE boole_d
      RAISING   lcx_not_found
                lcx_exception.

ENDCLASS.                    "lcl_object_smim DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_smim IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_smim IMPLEMENTATION.

  METHOD lif_object~exists.

    TRY.
        get_url_for_io( ).
        rv_bool = abap_true.
      CATCH lcx_not_found.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD get_url_for_io.

    DATA: ls_io       TYPE skwf_io,
          lv_url      TYPE skwf_url,
          ls_smimloio TYPE smimloio,
          lv_loio     TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    CLEAR ev_url.
    CLEAR ev_is_folder.

    SELECT SINGLE * FROM smimloio INTO ls_smimloio
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.

    IF ls_smimloio-lo_class = wbmr_c_skwf_folder_class.
      ev_is_folder = abap_true.
      ls_io-objtype = skwfc_obtype_folder.
    ELSE.
      ls_io-objtype = skwfc_obtype_loio.
    ENDIF.
    ls_io-class = ls_smimloio-lo_class.
    ls_io-objid = ls_smimloio-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = ls_io
      IMPORTING
        url = lv_url.

    ev_url = lv_url.

  ENDMETHOD.                    "get_url_for_io

  METHOD build_filename.

    CONCATENATE ms_item-obj_name ms_item-obj_type iv_filename
      INTO rv_filename SEPARATED BY '.'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "build_filename

  METHOD find_content.

    DATA: lv_filename TYPE string,
          lt_files    TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    lv_filename = get_filename( iv_url ).

    lv_filename = build_filename( lv_filename ).

    lt_files = mo_files->get_files( ).

    READ TABLE lt_files ASSIGNING <ls_file> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'SMIM, file not found'.
    ENDIF.

    rv_content = <ls_file>-data.

  ENDMETHOD.                    "find_content

  METHOD get_filename.

    DATA: lv_lines   TYPE i,
          lt_strings TYPE TABLE OF string.


    SPLIT iv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO rv_filename.
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "get_filename

  METHOD lif_object~serialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lv_filename TYPE string,
          ls_file     TYPE ty_file,
          lv_content  TYPE xstring,
          lo_xml      TYPE REF TO lcl_xml,
          li_api      TYPE REF TO if_mr_api.


    TRY.
        get_url_for_io(
          IMPORTING
            ev_url       = lv_url
            ev_is_folder = lv_folder ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    IF lv_folder = abap_false.
      li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
      li_api->get(
        EXPORTING
          i_url              = lv_url
        IMPORTING
          e_content          = lv_content
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4
          OTHERS             = 5 ).
      IF sy-subrc <> 0.
        _raise 'error from mime api->get'.
      ENDIF.

      lv_filename = get_filename( lv_url ).
      CLEAR ls_file.
      ls_file-filename = build_filename( lv_filename ).
      ls_file-path     = '/'.
      ls_file-data     = lv_content.
      mo_files->add( ls_file ).
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->element_add( iv_name    = 'URL'
                         ig_element = lv_url ).
    lo_xml->element_add( iv_name    = 'FOLDER'
                         ig_element = lv_folder ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lo_xml      TYPE REF TO lcl_xml,
          lv_content  TYPE xstring,
          lv_filename TYPE skwf_filnm,
          lv_io       TYPE sdok_docid,
          ls_skwf_io  TYPE skwf_io,
          li_api      TYPE REF TO if_mr_api.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = ms_item-obj_name.

    lo_xml = mo_files->read_xml( ).

    lo_xml->element_read( EXPORTING iv_name = 'URL'
                          CHANGING cg_element = lv_url ).
    lo_xml->element_read( EXPORTING iv_name = 'FOLDER'
                          CHANGING cg_element = lv_folder ).

    ls_skwf_io-objid = lv_io.

    IF lv_folder = abap_true.
      li_api->create_folder(
        EXPORTING
          i_url              = lv_url
          i_language         = sy-langu
          i_dev_package      = iv_package
          i_folder_loio      = ls_skwf_io
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          cancelled          = 3
          permission_failure = 4
          folder_exists      = 5
          OTHERS             = 6 ).
      IF sy-subrc <> 5 AND sy-subrc <> 0.
        _raise 'error frrom SMIM create_folder'.
      ENDIF.
    ELSE.
      lv_filename = get_filename( lv_url ).
      cl_wb_mime_repository=>determine_io_class(
        EXPORTING
          filename = lv_filename
        IMPORTING
          io_class = ls_skwf_io-class ).
      CONCATENATE ls_skwf_io-class '_L' INTO ls_skwf_io-class.

      lv_content = find_content( lv_url ).

      li_api->put(
        EXPORTING
          i_url                   = lv_url
          i_content               = lv_content
          i_dev_package           = iv_package
          i_new_loio              = ls_skwf_io
        EXCEPTIONS
          parameter_missing       = 1
          error_occured           = 2
          cancelled               = 3
          permission_failure      = 4
          data_inconsistency      = 5
          new_loio_already_exists = 6
          is_folder               = 7
          OTHERS                  = 8 ).
      IF sy-subrc <> 0.
        _raise 'error from SMIM put'.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: li_api TYPE REF TO if_mr_api,
          lv_url TYPE string.


    TRY.
        get_url_for_io(
          IMPORTING
            ev_url  = lv_url ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    li_api->delete(
      EXPORTING
        i_url              = lv_url
        i_delete_children  = abap_true
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        cancelled          = 3
        permission_failure = 4
        not_found          = 5
        OTHERS             = 6 ).
    IF sy-subrc <> 0.
      _raise 'error from delete'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.
    _raise 'todo, SMIM'.
  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_smim IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_sicf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sicf DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    TYPES: ty_icfhandler_tt TYPE STANDARD TABLE OF icfhandler WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_sicf_key,
             icf_name   TYPE icfservice-icf_name,
             icfparguid TYPE icfservice-icfparguid,
           END OF ty_sicf_key.

    METHODS read
      EXPORTING es_icfservice TYPE icfservice
                es_icfdocu    TYPE icfdocu
                et_icfhandler TYPE ty_icfhandler_tt
                ev_url        TYPE string
      RAISING   lcx_exception.

    METHODS insert_sicf
      IMPORTING is_icfservice TYPE icfservice
                is_icfdocu    TYPE icfdocu
                it_icfhandler TYPE ty_icfhandler_tt
                iv_package    TYPE devclass
                iv_url        TYPE string
      RAISING   lcx_exception.

    METHODS change_sicf
      IMPORTING is_icfservice TYPE icfservice
                is_icfdocu    TYPE icfdocu
                it_icfhandler TYPE ty_icfhandler_tt
                iv_package    TYPE devclass
                iv_parent     TYPE icfparguid
      RAISING   lcx_exception.

    METHODS to_icfhndlist
      IMPORTING it_list        TYPE ty_icfhandler_tt
      RETURNING VALUE(rt_list) TYPE icfhndlist.

    METHODS find_parent
      IMPORTING iv_url           TYPE string
      RETURNING VALUE(rv_parent) TYPE icfparguid
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_sicf DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_sicf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sicf IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: ls_icfservice TYPE icfservice.


    read( IMPORTING es_icfservice = ls_icfservice ).
    rv_bool = boolc( NOT ls_icfservice IS INITIAL ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lo_xml        TYPE REF TO lcl_xml,
          lt_icfhandler TYPE TABLE OF icfhandler.


    read( IMPORTING es_icfservice = ls_icfservice
                    es_icfdocu    = ls_icfdocu
                    et_icfhandler = lt_icfhandler
                    ev_url        = lv_url ).
    IF ls_icfservice IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_icfservice-icfnodguid.
    CLEAR ls_icfservice-icfparguid.

    CREATE OBJECT lo_xml.
    lo_xml->element_add( lv_url ).
    lo_xml->structure_add( ls_icfservice ).
    lo_xml->structure_add( ls_icfdocu ).
    lo_xml->table_add( iv_name  = 'ICFHANDLER_TABLE'
                       it_table = lt_icfhandler ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD read.

    DATA: lt_serv_info TYPE icfservtbl,
          ls_serv_info LIKE LINE OF lt_serv_info,
          ls_key       TYPE ty_sicf_key.

    FIELD-SYMBOLS: <ls_icfhandler> LIKE LINE OF et_icfhandler.


    CLEAR es_icfservice.
    CLEAR es_icfdocu.
    CLEAR et_icfhandler.
    CLEAR ev_url.

    ls_key = ms_item-obj_name.
    IF ls_key-icfparguid IS INITIAL.
* limitation: name must be unique
      SELECT SINGLE icfparguid FROM icfservice
        INTO ls_key-icfparguid
        WHERE icf_name = ls_key-icf_name
        AND icf_cuser <> 'SAP' ##warn_ok.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    cl_icf_tree=>if_icf_tree~get_info_from_serv(
      EXPORTING
        icf_name          = ls_key-icf_name
        icfparguid        = ls_key-icfparguid
        icf_langu         = gc_english
      IMPORTING
        serv_info         = lt_serv_info
        icfdocu           = es_icfdocu
        url               = ev_url
      EXCEPTIONS
        wrong_name        = 1
        wrong_parguid     = 2
        incorrect_service = 3
        no_authority      = 4
        OTHERS            = 5 ).
    IF sy-subrc <> 0.
      _raise 'error from get_info_from_serv'.
    ENDIF.

    ASSERT lines( lt_serv_info ) = 1.
    READ TABLE lt_serv_info INDEX 1 INTO ls_serv_info.
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING ls_serv_info-service TO es_icfservice.
    CLEAR es_icfservice-icf_cuser.
    CLEAR es_icfservice-icf_cdate.
    CLEAR es_icfservice-icf_muser.
    CLEAR es_icfservice-icf_mdate.

    CLEAR es_icfdocu-icfparguid.

    APPEND LINES OF ls_serv_info-handlertbl TO et_icfhandler.
    LOOP AT et_icfhandler ASSIGNING <ls_icfhandler>.
      CLEAR <ls_icfhandler>-icfparguid.
    ENDLOOP.

  ENDMETHOD.                    "read

  METHOD lif_object~deserialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_icfservice TYPE icfservice,
          ls_read       TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lt_icfhandler TYPE TABLE OF icfhandler.


    lo_xml = mo_files->read_xml( ).

    lo_xml->element_read( CHANGING cg_element = lv_url ).
    lo_xml->structure_read( CHANGING cg_structure = ls_icfservice ).
    lo_xml->structure_read( CHANGING cg_structure = ls_icfdocu ).
    lo_xml->table_read( EXPORTING iv_name = 'ICFHANDLER_TABLE'
                        CHANGING ct_table = lt_icfhandler ).

    read( IMPORTING es_icfservice = ls_read ).
    IF ls_read IS INITIAL.
      insert_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_url        = lv_url ).
    ELSE.
      change_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_parent     = ls_read-icfparguid ).
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD to_icfhndlist.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF it_list.


* convert to sorted table
    LOOP AT it_list ASSIGNING <ls_list>.
      INSERT <ls_list>-icfhandler INTO TABLE rt_list.
    ENDLOOP.

  ENDMETHOD.                    "to_icfhndlist

  METHOD find_parent.

    cl_icf_tree=>if_icf_tree~service_from_url(
      EXPORTING
        url                   = iv_url
        hostnumber            = 0
      IMPORTING
        icfnodguid            = rv_parent
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      _raise 'error from service_from_url'.
    ENDIF.

  ENDMETHOD.                    "find_parent

  METHOD insert_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc,
          ls_icfdocu    TYPE icfdocu,
          lv_parent     TYPE icfparguid.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).
    lv_parent = find_parent( iv_url ).

* nice, it seems that the structure should be mistreated
    ls_icfdocu = is_icfdocu-icf_docu.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~insert_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = lv_parent
        icfdocu                   = ls_icfdocu
        doculang                  = gc_english
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      _raise 'error from insert_node'.
    ENDIF.

  ENDMETHOD.                    "insert_sicf

  METHOD change_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~change_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = iv_parent
        icfdocu                   = is_icfdocu
        doculang                  = gc_english
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      _raise 'error from change_node'.
    ENDIF.

  ENDMETHOD.                    "change_sicf

  METHOD lif_object~delete.

    DATA: ls_icfservice TYPE icfservice.


    read( IMPORTING es_icfservice = ls_icfservice ).

    cl_icf_tree=>if_icf_tree~delete_node(
      EXPORTING
        icfparguid                  = ls_icfservice-icfparguid
      CHANGING
        icf_name                    = ls_icfservice-icf_name
      EXCEPTIONS
        no_virtual_host_delete      = 1
        special_service_error       = 2
        enqueue_error               = 3
        node_not_existing           = 4
        node_has_childs             = 5
        node_is_aliased             = 6
        node_not_in_original_system = 7
        transport_error             = 8
        tadir_error                 = 9
        db_error                    = 10
        no_authority                = 11
        OTHERS                      = 12 ).
    IF sy-subrc <> 0.
      _raise 'error from delete_node'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.
    _raise 'todo, SICF'.
  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_sicf IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ssst DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssst DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS validate_font
      IMPORTING iv_tdfamily TYPE tdfamily
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_ssst DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ssst IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssst IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_stylename TYPE stxsadm-stylename.


    SELECT SINGLE stylename FROM stxsadm INTO lv_stylename
      WHERE stylename = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD validate_font.

    DATA: lv_tdfamily TYPE tfo01-tdfamily.


    SELECT SINGLE tdfamily FROM tfo01 INTO lv_tdfamily
      WHERE tdfamily = iv_tdfamily.
    IF sy-subrc <> 0.
      _raise 'Font family not found'.
    ENDIF.

  ENDMETHOD.                    "validate_font

  METHOD lif_object~serialize.
* see fm SSF_DOWNLOAD_STYLE

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lv_style_name TYPE tdssname,
          ls_header     TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    lv_style_name = ms_item-obj_name.

    CALL FUNCTION 'SSF_READ_STYLE'
      EXPORTING
        i_style_name             = lv_style_name
        i_style_active_flag      = 'A'
        i_style_variant          = '%MAIN'
        i_style_language         = gc_english
      IMPORTING
        e_header                 = ls_header
      TABLES
        e_paragraphs             = lt_paragraphs
        e_strings                = lt_strings
        e_tabstops               = lt_tabstops
      EXCEPTIONS
        no_name                  = 1
        no_style                 = 2
        active_style_not_found   = 3
        inactive_style_not_found = 4
        no_variant               = 5
        no_main_variant          = 6
        cancelled                = 7
        no_access_permission     = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from SSF_READ_STYLE'.
    ENDIF.

    CLEAR ls_header-version.
    CLEAR ls_header-firstuser.
    CLEAR ls_header-firstdate.
    CLEAR ls_header-firsttime.
    CLEAR ls_header-lastuser.
    CLEAR ls_header-lastdate.
    CLEAR ls_header-lasttime.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_header ).
    lo_xml->table_add( it_table = lt_paragraphs
                       iv_name  = 'SSFPARAS' ).
    lo_xml->table_add( it_table = lt_strings
                       iv_name  = 'SSFSTRINGS' ).
    lo_xml->table_add( it_table = lt_tabstops
                       iv_name  = 'STXSTAB' ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see fm SSF_UPLOAD_STYLE

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_header     TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_header ).
    lo_xml->table_read( EXPORTING iv_name = 'SSFPARAS'
                        CHANGING ct_table = lt_paragraphs ).
    lo_xml->table_read( EXPORTING iv_name = 'SSFSTRINGS'
                        CHANGING ct_table = lt_strings ).
    lo_xml->table_read( EXPORTING iv_name = 'STXSTAB'
                        CHANGING ct_table = lt_tabstops ).

    validate_font( ls_header-tdfamily ).

    CALL FUNCTION 'SSF_SAVE_STYLE'
      EXPORTING
        i_header     = ls_header
      TABLES
        i_paragraphs = lt_paragraphs
        i_strings    = lt_strings
        i_tabstops   = lt_tabstops.

    CALL FUNCTION 'SSF_ACTIVATE_STYLE'
      EXPORTING
        i_stylename          = ls_header-stylename
      EXCEPTIONS
        no_name              = 1
        no_style             = 2
        cancelled            = 3
        no_access_permission = 4
        illegal_language     = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      _raise 'error from SSF_ACTIVATE_STYLE'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_stylename TYPE tdssname.


    lv_stylename = ms_item-obj_name.

    CALL FUNCTION 'SSF_DELETE_STYLE'
      EXPORTING
        i_stylename           = lv_stylename
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_style              = 2
        style_locked          = 3
        cancelled             = 4
        no_access_permission  = 5
        illegal_language      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'error from SSF_DELETE_STYLE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.
    _raise 'todo'.
  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_ssst IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_suso DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_suso DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_suso DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdyn DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdyn DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.

    DATA:
      mt_components TYPE TABLE OF wdy_ctlr_compo_vrs,
      mt_sources    TYPE TABLE OF wdy_ctlr_compo_source_vrs.

    METHODS:
      get_limu_objects
        RETURNING VALUE(rt_objects) TYPE wdy_md_transport_keys,
      read
        RETURNING VALUE(rs_component) TYPE wdy_component_metadata
        RAISING   lcx_exception,
      read_controller
        IMPORTING is_key               TYPE wdy_md_controller_key
        RETURNING VALUE(rs_controller) TYPE wdy_md_controller_meta_data
        RAISING   lcx_exception,
      read_definition
        IMPORTING is_key               TYPE wdy_md_component_key
        RETURNING VALUE(rs_definition) TYPE wdy_md_component_meta_data
        RAISING   lcx_exception,
      read_view
        IMPORTING is_key         TYPE wdy_md_view_key
        RETURNING VALUE(rs_view) TYPE wdy_md_view_meta_data
        RAISING   lcx_exception,
      recover_controller
        IMPORTING is_controller TYPE wdy_md_controller_meta_data
        RAISING   lcx_exception,
      recover_definition
        IMPORTING is_definition TYPE wdy_md_component_meta_data
        RAISING   lcx_exception,
      recover_view
        IMPORTING is_view TYPE wdy_md_view_meta_data
        RAISING   lcx_exception,
      delta_controller
        IMPORTING is_controller   TYPE wdy_md_controller_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   lcx_exception,
      delta_definition
        IMPORTING is_definition   TYPE wdy_md_component_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   lcx_exception,
      delta_view
        IMPORTING is_view         TYPE wdy_md_view_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_wdyn DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdyn IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdyn IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_component_name TYPE wdy_component-component_name.


    SELECT SINGLE component_name FROM wdy_component
      INTO lv_component_name
      WHERE component_name = ms_item-obj_name
      AND version = 'A'.                                "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD delta_definition.

    DATA: ls_key       TYPE wdy_md_component_key,
          lv_found     TYPE abap_bool,
          ls_obj_new   TYPE svrs2_versionable_object,
          li_component TYPE REF TO if_wdy_md_component,
          ls_obj_old   TYPE svrs2_versionable_object.


    ls_key-component_name = is_definition-definition-component_name.

    lv_found = cl_wdy_md_component=>check_existency( ls_key-component_name ).
    IF lv_found = abap_false.
      TRY.
          cl_wdy_md_component=>create_complete(
            EXPORTING
              name      = ls_key-component_name
            IMPORTING
              component = li_component ).
          li_component->save_to_database( ).
          li_component->unlock( ).
        CATCH cx_wdy_md_exception.
          _raise 'error creating dummy component'.
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_definition.
    ls_obj_new-objname = ls_key-component_name.

    ls_obj_old-objtype = wdyn_limu_component_definition.
    ls_obj_old-objname = ls_key-component_name.

    APPEND is_definition-definition TO ls_obj_old-wdyd-defin.
    ls_obj_old-wdyd-descr = is_definition-descriptions.
    ls_obj_old-wdyd-cusag = is_definition-component_usages.
    ls_obj_old-wdyd-intrf = is_definition-interface_implementings.
    ls_obj_old-wdyd-libra = is_definition-library_usages.
    ls_obj_old-wdyd-ctuse = is_definition-ext_ctlr_usages.
    ls_obj_old-wdyd-ctmap = is_definition-ext_ctx_mappings.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      _raise 'error from SVRS_MAKE_OBJECT_DELTA'.
    ENDIF.

  ENDMETHOD.                    "delta_definition

  METHOD delta_controller.

    DATA: li_controller TYPE REF TO if_wdy_md_controller,
          lv_found      TYPE abap_bool,
          ls_key        TYPE wdy_md_controller_key,
          ls_obj_new    TYPE svrs2_versionable_object,
          ls_obj_old    TYPE svrs2_versionable_object.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF mt_components,
                   <ls_source>    LIKE LINE OF mt_sources.


    ls_key-component_name = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    lv_found = cl_wdy_md_controller=>check_existency(
          component_name  = ls_key-component_name
          controller_name = ls_key-controller_name ).
    IF lv_found = abap_false.
      TRY.
          li_controller ?= cl_wdy_md_controller=>create_complete(
                component_name  = ls_key-component_name
                controller_name = ls_key-controller_name
                controller_type = is_controller-definition-controller_type ).
          li_controller->save_to_database( ).
          li_controller->unlock( ).
        CATCH cx_wdy_md_exception.
          _raise 'error creating dummy controller'.
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_controller.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_controller.
    ls_obj_old-objname = ls_key.

    APPEND is_controller-definition TO ls_obj_old-wdyc-defin.

    LOOP AT mt_components ASSIGNING <ls_component>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_component> TO ls_obj_old-wdyc-ccomp.
    ENDLOOP.
    LOOP AT mt_sources ASSIGNING <ls_source>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_source> TO ls_obj_old-wdyc-ccoms.
    ENDLOOP.

    ls_obj_old-wdyc-descr = is_controller-descriptions.
    ls_obj_old-wdyc-cusag = is_controller-controller_usages.
    ls_obj_old-wdyc-ccomt = is_controller-controller_component_texts.
    ls_obj_old-wdyc-cpara = is_controller-controller_parameters.
    ls_obj_old-wdyc-cpart = is_controller-controller_parameter_texts.
    ls_obj_old-wdyc-cnode = is_controller-context_nodes.
    ls_obj_old-wdyc-cattr = is_controller-context_attributes.
    ls_obj_old-wdyc-cmapp = is_controller-context_mappings.
    ls_obj_old-wdyc-excp  = is_controller-controller_exceptions.
    ls_obj_old-wdyc-excpt = is_controller-controller_exception_texts.
    ls_obj_old-wdyc-fgrps = is_controller-fieldgroups.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      _raise 'error from SVRS_MAKE_OBJECT_DELTA'.
    ENDIF.

  ENDMETHOD.                    "delta_controller

  METHOD delta_view.

    DATA: ls_key     TYPE wdy_md_view_key,
          ls_obj_new TYPE svrs2_versionable_object,
          ls_obj_old TYPE svrs2_versionable_object,
          lv_found   TYPE abap_bool,
          li_view    TYPE REF TO if_wdy_md_abstract_view.

    FIELD-SYMBOLS: <ls_def> LIKE LINE OF ls_obj_old-wdyv-defin.


    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    lv_found = cl_wdy_md_abstract_view=>check_existency(
                 component_name = ls_key-component_name
                 name           = ls_key-view_name ).
    IF lv_found = abap_false.
      TRY.
          li_view = cl_wdy_md_abstract_view=>create(
                      component_name = is_view-definition-component_name
                      view_name      = is_view-definition-view_name
                      type           = is_view-definition-type ).
          li_view->save_to_database( ).
          li_view->unlock( ).
        CATCH cx_wdy_md_exception.
          _raise 'error creating dummy view'.
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_view.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_view.
    ls_obj_old-objname = ls_key.

    APPEND INITIAL LINE TO ls_obj_old-wdyv-defin ASSIGNING <ls_def>.
    MOVE-CORRESPONDING is_view-definition TO <ls_def>.

    ls_obj_old-wdyv-descr = is_view-descriptions.
    ls_obj_old-wdyv-vcont = is_view-view_containers.
    ls_obj_old-wdyv-vcntt = is_view-view_container_texts.
    ls_obj_old-wdyv-ibplg = is_view-iobound_plugs.
    ls_obj_old-wdyv-ibplt = is_view-iobound_plug_texts.
    ls_obj_old-wdyv-plpar = is_view-plug_parameters.
    ls_obj_old-wdyv-plprt = is_view-plug_parameter_texts.
    ls_obj_old-wdyv-uiele = is_view-ui_elements.
    ls_obj_old-wdyv-uicon = is_view-ui_context_bindings.
    ls_obj_old-wdyv-uievt = is_view-ui_event_bindings.
    ls_obj_old-wdyv-uiddc = is_view-ui_ddic_bindings.
    ls_obj_old-wdyv-uiprp = is_view-ui_properties.
    ls_obj_old-wdyv-navil = is_view-navigation_links.
    ls_obj_old-wdyv-navit = is_view-navigation_target_refs.
    ls_obj_old-wdyv-vshno = is_view-vsh_nodes.
    ls_obj_old-wdyv-vshpl = is_view-vsh_placeholders.
    ls_obj_old-wdyv-views = is_view-viewset_properties.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      _raise 'error from SVRS_MAKE_OBJECT_DELTA'.
    ENDIF.

  ENDMETHOD.                    "delta_view

  METHOD recover_definition.

    DATA: ls_key    TYPE wdy_md_component_key,
          lv_corrnr TYPE trkorr,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_definition( is_definition ).
    ls_key-component_name = is_definition-definition-component_name.

    cl_wdy_md_component=>recover_version(
      EXPORTING
        component_key = ls_key
        delta         = ls_delta-wdyd
      CHANGING
        corrnr        = lv_corrnr ).

  ENDMETHOD.                    "recover_definition

  METHOD recover_controller.

    DATA: ls_key    TYPE wdy_controller_key,
          lv_corrnr TYPE trkorr,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_controller( is_controller ).
    ls_key-component_name  = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    cl_wdy_md_controller=>recover_version(
      EXPORTING
        controller_key = ls_key
        delta          = ls_delta-wdyc
      CHANGING
        corrnr         = lv_corrnr ).

  ENDMETHOD.                    "recover_controller

  METHOD recover_view.

    DATA: ls_key    TYPE wdy_md_view_key,
          lv_corrnr TYPE trkorr,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_view( is_view ).
    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    cl_wdy_md_abstract_view=>recover_version(
      EXPORTING
        view_key = ls_key
        delta    = ls_delta-wdyv
      CHANGING
        corrnr   = lv_corrnr ).

  ENDMETHOD.                    "recover_view

  METHOD read_controller.

    DATA: lt_components TYPE TABLE OF wdy_ctlr_compo_vrs,
          lt_sources    TYPE TABLE OF wdy_ctlr_compo_source_vrs,
          lt_definition TYPE TABLE OF wdy_controller,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.


    CALL FUNCTION 'WDYC_GET_OBJECT'
      EXPORTING
        controller_key               = is_key
        get_all_translations         = abap_false
      TABLES
        definition                   = lt_definition
        descriptions                 = rs_controller-descriptions
        controller_usages            = rs_controller-controller_usages
        controller_components        = lt_components
        controller_component_sources = lt_sources
        controller_component_texts   = rs_controller-controller_component_texts
        controller_parameters        = rs_controller-controller_parameters
        controller_parameter_texts   = rs_controller-controller_parameter_texts
        context_nodes                = rs_controller-context_nodes
        context_attributes           = rs_controller-context_attributes
        context_mappings             = rs_controller-context_mappings
        fieldgroups                  = rs_controller-fieldgroups
        controller_exceptions        = rs_controller-controller_exceptions
        controller_exception_texts   = rs_controller-controller_exception_texts
        psmodilog                    = lt_psmodilog " not optional in all versions
        psmodisrc                    = lt_psmodisrc " not optional in all versions
      EXCEPTIONS
        not_existing                 = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      _raise 'error from WDYC_GET_OBJECT'.
    ENDIF.

    APPEND LINES OF lt_components TO mt_components.
    APPEND LINES OF lt_sources TO mt_sources.

    READ TABLE lt_definition INDEX 1 INTO rs_controller-definition.
    IF sy-subrc <> 0.
      _raise 'WDYC, definition not found'.
    ENDIF.

    CLEAR: rs_controller-definition-author,
           rs_controller-definition-createdon,
           rs_controller-definition-changedby,
           rs_controller-definition-changedon.

  ENDMETHOD.                    "read_controller

  METHOD read_definition.

    DATA: lt_definition TYPE TABLE OF wdy_component,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.


    CALL FUNCTION 'WDYD_GET_OBJECT'
      EXPORTING
        component_key           = is_key
        get_all_translations    = abap_false
      TABLES
        definition              = lt_definition
        descriptions            = rs_definition-descriptions
        component_usages        = rs_definition-component_usages
        interface_implementings = rs_definition-interface_implementings
        library_usages          = rs_definition-library_usages
        ext_ctlr_usages         = rs_definition-ext_ctlr_usages
        ext_ctx_mappings        = rs_definition-ext_ctx_mappings
        psmodilog               = lt_psmodilog " not optional in all versions
        psmodisrc               = lt_psmodisrc " not optional in all versions
      EXCEPTIONS
        not_existing            = 1
        OTHERS                  = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from WDYD_GET_OBJECT'.
    ENDIF.

    READ TABLE lt_definition INDEX 1 INTO rs_definition-definition.
    IF sy-subrc <> 0.
      _raise 'WDYD, definition not found'.
    ENDIF.

    CLEAR: rs_definition-definition-author,
           rs_definition-definition-createdon,
           rs_definition-definition-changedby,
           rs_definition-definition-changedon,
           rs_definition-definition-gendate,
           rs_definition-definition-gentime.

  ENDMETHOD.                    "read_definition

  METHOD read_view.

    DATA: lt_definition TYPE TABLE OF wdy_view_vrs,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.

    FIELD-SYMBOLS: <ls_definition> LIKE LINE OF lt_definition.


    CALL FUNCTION 'WDYV_GET_OBJECT'
      EXPORTING
        view_key               = is_key
        get_all_translations   = abap_false
      TABLES
        definition             = lt_definition
        descriptions           = rs_view-descriptions
        view_containers        = rs_view-view_containers
        view_container_texts   = rs_view-view_container_texts
        iobound_plugs          = rs_view-iobound_plugs
        iobound_plug_texts     = rs_view-iobound_plug_texts
        plug_parameters        = rs_view-plug_parameters
        plug_parameter_texts   = rs_view-plug_parameter_texts
        ui_elements            = rs_view-ui_elements
        ui_context_bindings    = rs_view-ui_context_bindings
        ui_event_bindings      = rs_view-ui_event_bindings
        ui_ddic_bindings       = rs_view-ui_ddic_bindings
        ui_properties          = rs_view-ui_properties
        navigation_links       = rs_view-navigation_links
        navigation_target_refs = rs_view-navigation_target_refs
        vsh_nodes              = rs_view-vsh_nodes
        vsh_placeholders       = rs_view-vsh_placeholders
        viewset_properties     = rs_view-viewset_properties
        psmodilog              = lt_psmodilog
        psmodisrc              = lt_psmodisrc
      EXCEPTIONS
        not_existing           = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      _raise 'error from WDYV_GET_OBJECT'.
    ENDIF.

    READ TABLE lt_definition INDEX 1 ASSIGNING <ls_definition>.
    ASSERT sy-subrc = 0.
    MOVE-CORRESPONDING <ls_definition> TO rs_view-definition.

    CLEAR: rs_view-definition-author,
           rs_view-definition-createdon,
           rs_view-definition-changedby,
           rs_view-definition-changedon.

  ENDMETHOD.                    "read_view

  METHOD get_limu_objects.

    DATA: lv_name TYPE wdy_component_name.


    lv_name = ms_item-obj_name.
    CALL FUNCTION 'WDYN_GET_LIMU_OBJECTS'
      EXPORTING
        component_name = lv_name
      IMPORTING
        limu_objects   = rt_objects.

  ENDMETHOD.                    "get_limu_objects

  METHOD read.

    DATA: lt_objects        TYPE wdy_md_transport_keys,
          ls_controller_key TYPE wdy_md_controller_key,
          ls_component_key  TYPE wdy_md_component_key,
          ls_view_key       TYPE wdy_md_view_key.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    CLEAR mt_components.
    CLEAR mt_sources.

    lt_objects = get_limu_objects( ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      CASE <ls_object>-sub_type.
        WHEN wdyn_limu_component_controller.
          ls_controller_key = <ls_object>-sub_name.
          APPEND read_controller( ls_controller_key ) TO rs_component-ctlr_metadata.
        WHEN wdyn_limu_component_definition.
          ls_component_key = <ls_object>-sub_name.
          rs_component-comp_metadata = read_definition( ls_component_key ).
        WHEN wdyn_limu_component_view.
          ls_view_key = <ls_object>-sub_name.
          APPEND read_view( ls_view_key ) TO rs_component-view_metadata.
        WHEN OTHERS.
          ASSERT 1 = 1 + 1.
      ENDCASE.
    ENDLOOP.

    SORT rs_component-ctlr_metadata BY
      definition-component_name ASCENDING
      definition-controller_name ASCENDING.

    SORT mt_components BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING.

    SORT mt_sources BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING
      line_number ASCENDING.

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: lo_xml       TYPE REF TO lcl_xml,
          ls_component TYPE wdy_component_metadata.


    ls_component = read( ).

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_component ).
    lo_xml->table_add( it_table = mt_components
                       iv_name  = 'COMPONENTS' ).
    lo_xml->table_add( it_table = mt_sources
                       iv_name  = 'SOURCES' ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml       TYPE REF TO lcl_xml,
          ls_component TYPE wdy_component_metadata.

    FIELD-SYMBOLS: <ls_view>       LIKE LINE OF ls_component-view_metadata,
                   <ls_controller> LIKE LINE OF ls_component-ctlr_metadata.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_component ).
    lo_xml->table_read(
      EXPORTING
        iv_name  = 'COMPONENTS'
      CHANGING
        ct_table = mt_components ).
    lo_xml->table_read(
      EXPORTING
        iv_name  = 'SOURCES'
      CHANGING
        ct_table = mt_sources ).

    ls_component-comp_metadata-definition-author = sy-uname.
    ls_component-comp_metadata-definition-createdon = sy-datum.
    recover_definition( ls_component-comp_metadata ).

    LOOP AT ls_component-ctlr_metadata ASSIGNING <ls_controller>.
      <ls_controller>-definition-author = sy-uname.
      <ls_controller>-definition-createdon = sy-datum.
      recover_controller( <ls_controller> ).
    ENDLOOP.
    LOOP AT ls_component-view_metadata ASSIGNING <ls_view>.
      <ls_view>-definition-author = sy-uname.
      <ls_view>-definition-createdon = sy-datum.
      recover_view( <ls_view> ).
    ENDLOOP.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lo_component   TYPE REF TO cl_wdy_wb_component,
          lo_request     TYPE REF TO cl_wb_request,
          li_state       TYPE REF TO if_wb_program_state,
          lv_object_name TYPE seu_objkey.


    CREATE OBJECT lo_component.

    lv_object_name = ms_item-obj_name.
    CREATE OBJECT lo_request
      EXPORTING
        p_object_type = 'YC'
        p_object_name = lv_object_name
        p_operation   = swbm_c_op_delete_no_dialog.

    lo_component->if_wb_program~process_wb_request(
      p_wb_request       = lo_request
      p_wb_program_state = li_state ).

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_wdyn IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdca DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdca DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS read
      EXPORTING es_outline TYPE wdy_cfg_outline_data
                et_data    TYPE wdy_cfg_persist_data_appl_tab
      RAISING   lcx_exception.

    METHODS save
      IMPORTING is_outline TYPE wdy_cfg_outline_data
                it_data    TYPE wdy_cfg_persist_data_appl_tab
                iv_package TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_wdca DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdca IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdca IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: ls_outline TYPE wdy_cfg_outline_data.


    read( IMPORTING es_outline = ls_outline ).
    rv_bool = boolc( NOT ls_outline IS INITIAL ).

  ENDMETHOD.

  METHOD save.

    _raise 'WDCA, save, todo'.

*    DATA: lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
*          ls_key       TYPE wdy_config_key,
*          ls_data      LIKE LINE OF it_data,
*          lv_operation TYPE i,
*          lv_name      TYPE wdy_md_object_name.
*
*
*    MOVE-CORRESPONDING is_outline TO ls_key.
*
*    TRY.
*        CREATE OBJECT lo_cfg
*          EXPORTING
*            config_key  = ls_key
*            object_name = lv_name.
*
*        READ TABLE it_data INDEX 1 INTO ls_data.
*        ASSERT sy-subrc = 0.
*
*        lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_save.
*        lo_cfg->do_next_step( CHANGING c_operation = lv_operation ).
*        lo_cfg->do_next_step( CHANGING c_operation = lv_operation ).
*
*        lo_cfg->set_save_data( ls_data ).
*
*      CATCH cx_wd_configuration.
*        _raise 'WDCA, save error'.
*    ENDTRY.

  ENDMETHOD.                    "save

  METHOD read.

    DATA: lo_cfg    TYPE REF TO cl_wdr_cfg_persistence_appl,
          ls_key    TYPE wdy_config_key,
          lv_exists TYPE abap_bool,
          lx_err    TYPE REF TO cx_wd_configuration,
          lv_name   TYPE wdy_md_object_name.


    CLEAR et_data.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        MOVE-CORRESPONDING ls_key TO es_outline.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = es_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).
        IF lv_exists = abap_false.
          RETURN.
        ENDIF.

        es_outline = lo_cfg->read_outline_data( ).
      CATCH cx_wd_configuration INTO lx_err.
        IF lx_err->textid = cx_wd_configuration=>conf_config_not_exist.
          RETURN.
        ELSE.
          _raise 'WDCA, read error'.
        ENDIF.
    ENDTRY.

    CLEAR: es_outline-devclass,
           es_outline-author,
           es_outline-createdon,
           es_outline-changedby,
           es_outline-changedon.
    et_data = lo_cfg->read_data( ).

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: lo_xml     TYPE REF TO lcl_xml,
          ls_outline TYPE wdy_cfg_outline_data,
          lt_data    TYPE wdy_cfg_persist_data_appl_tab.


    read( IMPORTING es_outline = ls_outline
                    et_data    = lt_data ).
    IF ls_outline IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_outline ).
    lo_xml->table_add( lt_data ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml     TYPE REF TO lcl_xml,
          ls_outline TYPE wdy_cfg_outline_data,
          lt_data    TYPE wdy_cfg_persist_data_appl_tab.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_outline ).
    lo_xml->table_read( CHANGING ct_table = lt_data ).

    save( is_outline = ls_outline
          it_data    = lt_data
          iv_package = iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: ls_key TYPE wdy_config_key.


    ls_key = ms_item-obj_name.

    cl_wdr_configuration_utils=>delete_config_4_appl( ls_key ).

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_wdca IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdya DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdya DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS read
      EXPORTING es_app        TYPE wdy_application
                et_properties TYPE wdy_app_property_table
      RAISING   lcx_exception.

    METHODS save
      IMPORTING is_app        TYPE wdy_application
                it_properties TYPE wdy_app_property_table
                iv_package    TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_wdya DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdya IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdya IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_name TYPE wdy_application_name.


    lv_name = ms_item-obj_name.

    TRY.
        cl_wdy_md_application=>get_object_by_key(
          name    = lv_name
          version = 'A' ).
        rv_bool = abap_true.
      CATCH cx_wdy_md_not_existing.
        rv_bool = abap_false.
      CATCH cx_wdy_md_permission_failure.
        _raise 'WDYA, permission failure'.
    ENDTRY.

  ENDMETHOD.

  METHOD read.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          li_map  TYPE REF TO if_object_map,
          lo_prop TYPE REF TO cl_wdy_md_application_property,
          ls_prop LIKE LINE OF et_properties,
          lv_name TYPE wdy_application_name.


    CLEAR es_app.
    CLEAR et_properties.

    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_permission_failure.
        _raise 'WDYA, permission failure'.
    ENDTRY.

    li_app->if_wdy_md_object~get_definition( IMPORTING definition = es_app ).
    CLEAR: es_app-author,
           es_app-createdon,
           es_app-changedby,
           es_app-changedon.

    li_map = li_app->get_properties( ).
    DO li_map->size( ) TIMES.
      lo_prop ?= li_map->get_by_position( sy-index ).
      lo_prop->get_definition( IMPORTING definition = ls_prop ).
      APPEND ls_prop TO et_properties.
    ENDDO.

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.


    read( IMPORTING es_app        = ls_app
                    et_properties = lt_properties ).

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_app ).
    lo_xml->table_add( lt_properties ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD save.

    DATA: li_prop TYPE REF TO if_wdy_md_application_property,
          lo_app  TYPE REF TO cl_wdy_md_application.

    FIELD-SYMBOLS: <ls_property> LIKE LINE OF it_properties.


    TRY.
        CREATE OBJECT lo_app
          EXPORTING
            name       = is_app-application_name
            definition = is_app
            devclass   = iv_package.

        LOOP AT it_properties ASSIGNING <ls_property>.
          li_prop = lo_app->if_wdy_md_application~create_property( <ls_property>-name ).
          li_prop->set_value( <ls_property>-value ).
        ENDLOOP.

        lo_app->if_wdy_md_lockable_object~save_to_database( ).
      CATCH cx_wdy_md_exception.
        _raise 'error saving WDYA'.
    ENDTRY.

  ENDMETHOD.                    "save

  METHOD lif_object~deserialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_app ).
    lo_xml->table_read( CHANGING ct_table = lt_properties ).

    save( is_app        = ls_app
          it_properties = lt_properties
          iv_package    = iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: li_app    TYPE REF TO if_wdy_md_application,
          lv_objkey TYPE wdy_wb_appl_name,
          lv_type   TYPE seu_type,
          lv_name   TYPE wdy_application_name.


    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
        li_app->if_wdy_md_object~delete( ).
        li_app->if_wdy_md_lockable_object~save_to_database( ).

* method save_to_database calls function module TR_TADIR_INTERFACE
* with test mode = X, so it does not delete the TADIR entry.
* Instead the standard code uses RS_TREE_OBJECT_PLACEMENT to delete
* the TADIR entry
        lv_objkey = ms_item-obj_name.
        CONCATENATE 'O' swbm_c_type_wdy_application INTO lv_type.
        CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
          EXPORTING
            object    = lv_objkey
            type      = lv_type
            operation = 'DELETE'.

      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_exception.
        _raise 'WDYA, error deleting'.
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_wdya IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_susc DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_susc DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_susc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_suso IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_suso IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_objct TYPE tobj-objct.


    SELECT SINGLE objct FROM tobj INTO lv_objct
      WHERE objct = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    SELECT SINGLE * FROM tobj INTO ls_tobj
      WHERE objct = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_tobj-bname.

    SELECT SINGLE * FROM tobjt INTO ls_tobjt
      WHERE object = ms_item-obj_name
      AND langu = gc_english.                           "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'TOBJT no english description'.
    ENDIF.

    SELECT SINGLE * FROM tobjvorflg INTO ls_tobjvorflg
      WHERE objct = ms_item-obj_name.                     "#EC CI_SUBRC

    SELECT * FROM tactz INTO TABLE lt_tactz
      WHERE brobj = ms_item-obj_name
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvordat INTO TABLE lt_tobjvordat
      WHERE objct = ms_item-obj_name
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvor INTO TABLE lt_tobjvor
      WHERE objct = ms_item-obj_name
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tobj ).
    lo_xml->structure_add( ls_tobjt ).
    lo_xml->structure_add( ls_tobjvorflg ).
    lo_xml->table_add( it_table = lt_tactz
                       iv_name = 'TACTZ' ).
    lo_xml->table_add( it_table = lt_tobjvordat
                       iv_name = 'TOBJVORDAT' ).
    lo_xml->table_add( it_table = lt_tobjvor
                       iv_name = 'TOBJVOR' ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see function group SUSA

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lv_objectname TYPE e071-obj_name,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    ASSERT NOT ms_item-obj_name IS INITIAL.

    lo_xml = mo_files->read_xml( ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobj ).
    ls_tobj-bname = sy-uname.
    lo_xml->structure_read( CHANGING cg_structure = ls_tobjt ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobjvorflg ).
    lo_xml->table_read( EXPORTING iv_name  = 'TACTZ'
                        CHANGING  ct_table = lt_tactz ).
    lo_xml->table_read( EXPORTING iv_name  = 'TOBJVORDAT'
                        CHANGING  ct_table = lt_tobjvordat ).
    lo_xml->table_read( EXPORTING iv_name  = 'TOBJVOR'
                        CHANGING  ct_table = lt_tobjvor ).

    lv_objectname = ms_item-obj_name.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = 'O'.

    MODIFY tobj FROM ls_tobj.                             "#EC CI_SUBRC
    MODIFY tobjt FROM ls_tobjt.                           "#EC CI_SUBRC
    MODIFY tobjvorflg FROM ls_tobjvorflg.                 "#EC CI_SUBRC
    DELETE FROM tactz WHERE brobj = ms_item-obj_name.     "#EC CI_SUBRC
    INSERT tactz FROM TABLE lt_tactz.                     "#EC CI_SUBRC
    DELETE FROM tobjvordat WHERE objct = ms_item-obj_name. "#EC CI_SUBRC
    INSERT tobjvordat FROM TABLE lt_tobjvordat.           "#EC CI_SUBRC
    DELETE FROM tobjvor WHERE objct = ms_item-obj_name.   "#EC CI_SUBRC
    INSERT tobjvor FROM TABLE lt_tobjvor.                 "#EC CI_SUBRC

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_object TYPE tobj-objct.


    lv_object = ms_item-obj_name.
    CALL FUNCTION 'SUSR_DELETE_OBJECT'
      EXPORTING
        object = lv_object.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    DATA: lv_object TYPE tobj-objct.


    lv_object = ms_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT'
      EXPORTING
        object = lv_object.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_suso IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_object_susc IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_susc IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_oclss TYPE tobc-oclss.


    SELECT SINGLE oclss FROM tobc INTO lv_oclss
      WHERE oclss = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_tobc  TYPE tobc,
          ls_tobct TYPE tobct.


    SELECT SINGLE * FROM tobc INTO ls_tobc
      WHERE oclss = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tobct INTO ls_tobct
      WHERE oclss = ms_item-obj_name
      AND langu = gc_english.
    IF sy-subrc <> 0.
      _raise 'TOBCT no english description'.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tobc ).
    lo_xml->structure_add( ls_tobct ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see function group SUSA

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_tobc       TYPE tobc,
          lv_objectname TYPE e071-obj_name,
          ls_tobct      TYPE tobct.


    lo_xml = mo_files->read_xml( ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobc ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobct ).

    lv_objectname = ms_item-obj_name.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = 'C'.

    INSERT tobc FROM ls_tobc.                             "#EC CI_SUBRC
* ignore sy-subrc as all fields are key fields

    MODIFY tobct FROM ls_tobct.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_objclass TYPE tobc-oclss.


    lv_objclass = ms_item-obj_name.
    CALL FUNCTION 'SUSR_DELETE_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    DATA: lv_objclass TYPE tobc-oclss.


    lv_objclass = ms_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_susc IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_type DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_type DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS read
      EXPORTING ev_ddtext TYPE ddtypet-ddtext
                et_source TYPE abaptxt255_tab
      RAISING   lcx_exception
                lcx_not_found.

    METHODS create
      IMPORTING iv_ddtext   TYPE ddtypet-ddtext
                it_source   TYPE abaptxt255_tab
                iv_devclass TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_type DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_type IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_type IMPLEMENTATION.

  METHOD lif_object~exists.

    TRY.
        read( ).
        rv_bool = abap_true.
      CATCH lcx_not_found.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD read.

    DATA: lv_typdname  TYPE rsedd0-typegroup,
          lt_psmodisrc TYPE TABLE OF smodisrc,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_ptrdir    TYPE TABLE OF trdir.


    SELECT SINGLE ddtext FROM ddtypet
      INTO ev_ddtext
      WHERE typegroup = ms_item-obj_name
      AND ddlanguage = gc_english.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.

    lv_typdname = ms_item-obj_name.
    CALL FUNCTION 'TYPD_GET_OBJECT'
      EXPORTING
        typdname          = lv_typdname
      TABLES
        psmodisrc         = lt_psmodisrc
        psmodilog         = lt_psmodilog
        psource           = et_source
        ptrdir            = lt_ptrdir
      EXCEPTIONS
        version_not_found = 1
        reps_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      _raise 'error from TYPD_GET_OBJECT'.
    ENDIF.

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: lo_xml    TYPE REF TO lcl_xml,
          lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    TRY.
        read( IMPORTING
                ev_ddtext = lv_ddtext
                et_source = lt_source ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    CREATE OBJECT lo_xml.
    lo_xml->element_add( lv_ddtext ).
    mo_files->add_xml( lo_xml ).

    mo_files->add_abap( lt_source ).

  ENDMETHOD.                    "serialize

  METHOD create.

    DATA: lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_TYGR_INSERT_SOURCES'
      EXPORTING
        typegroupname        = lv_typegroup
        ddtext               = iv_ddtext
        corrnum              = ''
        devclass             = iv_devclass
      TABLES
        source               = it_source
      EXCEPTIONS
        already_exists       = 1
        not_executed         = 2
        permission_failure   = 3
        object_not_specified = 4
        illegal_name         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_TYGR_INSERT_SOURCES'.
    ENDIF.

    CONCATENATE '%_C' lv_typegroup INTO lv_progname.
    UPDATE progdir SET uccheck = abap_true
      WHERE name = lv_progname.
    IF sy-subrc <> 0.
      _raise 'error setting uccheck'.
    ENDIF.

  ENDMETHOD.                    "create

  METHOD lif_object~deserialize.

    DATA: lo_xml    TYPE REF TO lcl_xml,
          lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    lo_xml = mo_files->read_xml( ).
    lo_xml->element_read( CHANGING cg_element = lv_ddtext ).

    lt_source = mo_files->read_abap( ).

    create( iv_ddtext   = lv_ddtext
            it_source   = lt_source
            iv_devclass = iv_package ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_typename TYPE typegroup.


    lv_typename = ms_item-obj_name.

    CALL FUNCTION 'TYPD_INTERNAL_SERVICE'
      EXPORTING
        i_typename        = lv_typename
        i_operation       = swbm_c_op_delete
      EXCEPTIONS
        illegal_operation = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      _raise 'error from TYPD_INTERNAL_SERVICE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-TYMA'
               iv_field = 'RSRD1-TYMA_VAL' ).

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_type IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_para DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_para DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_para DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_para IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_para IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_paramid TYPE tpara-paramid.


    SELECT SINGLE paramid FROM tpara INTO lv_paramid
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_xml    TYPE REF TO lcl_xml,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tparat INTO ls_tparat
      WHERE paramid = ms_item-obj_name
      AND sprache = gc_english.                         "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'PARA no english description'.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tpara ).
    lo_xml->structure_add( ls_tparat ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see fm RS_PARAMETER_ADD and RS_PARAMETER_EDIT

    DATA: lo_xml    TYPE REF TO lcl_xml,
          lv_mode   TYPE c LENGTH 1,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc = 0.
      lv_mode = 'M'.
    ELSE.
      lv_mode = 'I'.
    ENDIF.

    lo_xml = mo_files->read_xml( ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tpara ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tparat ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ms_item-obj_name
        object_class        = 'PARA'
        mode                = lv_mode
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = gc_english
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_CORR_INSERT, PARA'.
    ENDIF.

    MODIFY tpara FROM ls_tpara.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    MODIFY tparat FROM ls_tparat.                         "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_paramid TYPE tpara-paramid.


    lv_paramid = ms_item-obj_name.
    CALL FUNCTION 'RS_PARAMETER_DELETE'
      EXPORTING
        objectname = lv_paramid
      EXCEPTIONS
        cancelled  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      _raise 'error from RS_PRAMETER_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PARA'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_para IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ssfo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssfo DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssfo IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_formname TYPE stxfadm-formname.


    SELECT SINGLE formname FROM stxfadm INTO lv_formname
      WHERE formname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSFO'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISPLAY'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RB_SF'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SSFSCREEN-FNAME'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SMARTFORMS'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4 ##fm_subrc_ok. "#EC CI_SUBRC

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_formname TYPE tdsfname.


    lv_formname = ms_item-obj_name.

    CALL FUNCTION 'FB_DELETE_FORM'
      EXPORTING
        i_formname            = lv_formname
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_form               = 2
        form_locked           = 3
        no_access_permission  = 4
        illegal_language      = 5
        illegal_formtype      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from FB_DELETE_FORM'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.
* see function module FB_DOWNLOAD_FORM

    DATA: lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lo_xml      TYPE REF TO lcl_xml,
          lv_name     TYPE string,
          li_node     TYPE REF TO if_ixml_node,
          li_element  TYPE REF TO if_ixml_element,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          li_attr     TYPE REF TO if_ixml_named_node_map,
          lv_formname TYPE tdsfname.


    CREATE OBJECT lo_xml
      EXPORTING
        iv_empty = abap_true.

    CREATE OBJECT lo_sf.
    lv_formname = ms_item-obj_name. " convert type
    TRY.
        lo_sf->load( im_formname = lv_formname
                     im_language = '' ).
      CATCH cx_ssf_fb.
* the smartform is not present in system, or other error occured
        RETURN.
    ENDTRY.

    lo_sf->xml_download( EXPORTING parent   = lo_xml->mi_xml_doc
                         CHANGING  document = lo_xml->mi_xml_doc ).

    li_iterator = lo_xml->mi_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.

      lv_name = li_node->get_name( ).
      IF lv_name = 'DEVCLASS'
          OR lv_name = 'LASTDATE'
          OR lv_name = 'LASTTIME'.
        li_node->set_value( '' ).
      ENDIF.
      IF lv_name = 'FIRSTUSER'
          OR lv_name = 'LASTUSER'.
        li_node->set_value( 'DUMMY' ).
      ENDIF.

* remove IDs it seems that they are not used for anything
* the IDs are "random" so it caused diff files
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_attr = li_node->get_attributes( ).
        li_attr->remove_named_item( 'ID' ).
      ENDIF.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_element = lo_xml->mi_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ). "#EC NOTEXT
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).              "#EC NOTEXT

* the upload fails when the smartform is normalized
    mo_files->add_xml( io_xml       = lo_xml
                       iv_normalize = abap_false ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see function module FB_UPLOAD_FORM

    DATA: lo_xml      TYPE REF TO lcl_xml,
          li_node     TYPE REF TO if_ixml_node,
          lv_formname TYPE tdsfname,
          lv_name     TYPE string,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lo_res      TYPE REF TO cl_ssf_fb_smart_form.


    CREATE OBJECT lo_sf.

    lo_xml = mo_files->read_xml( ).

* set "created by" and "changed by" to current user
    li_iterator = lo_xml->mi_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      CASE lv_name.
        WHEN 'LASTDATE'.
          li_node->set_value(
            sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
        WHEN 'LASTTIME'.
          li_node->set_value(
            sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
        WHEN 'FIRSTUSER' OR 'LASTUSER'.
          li_node->set_value( sy-uname && '' ).
      ENDCASE.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_node = lo_xml->mi_xml_doc->get_root_element( ).
    lv_formname = ms_item-obj_name.

* todo, iv_package?
    lo_sf->enqueue( suppress_corr_check = space
                    master_language     = gc_english
                    mode                = 'INSERT'
                    formname            = lv_formname ).

    lo_sf->xml_upload( EXPORTING dom      = li_node
                                 formname = lv_formname
                                 language = gc_english
                       CHANGING  sform    = lo_res ).

    lo_res->store( im_formname = lo_res->header-formname
                   im_language = gc_english
                   im_active   = abap_true ).

    lo_sf->dequeue( lv_formname ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_ssfo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tabl DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tabl IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.


    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_false
        objname              = lv_objname
        objtype              = 'T'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, TABL'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lo_xml   TYPE REF TO lcl_xml,
          ls_dd02v TYPE dd02v,
          ls_dd09l TYPE dd09l,
          lt_dd03p TYPE TABLE OF dd03p,
          lt_dd05m TYPE TABLE OF dd05m,
          lt_dd08v TYPE TABLE OF dd08v,
          lt_dd12v TYPE dd12vtab,
          lt_dd17v TYPE dd17vtab,
          lt_dd35v TYPE TABLE OF dd35v,
          lt_dd36m TYPE dd36mttyp.

    FIELD-SYMBOLS: <ls_dd12v> LIKE LINE OF lt_dd12v,
                   <ls_dd03p> LIKE LINE OF lt_dd03p.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = gc_english
      IMPORTING
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TABL_GET'.
    ENDIF.
    IF ls_dd02v IS INITIAL.
      RETURN. " object does not exits
    ENDIF.

    CLEAR: ls_dd02v-as4user,
           ls_dd02v-as4date,
           ls_dd02v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time.
    ENDLOOP.

    LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE NOT rollname IS INITIAL.
      CLEAR: <ls_dd03p>-ddlanguage,
        <ls_dd03p>-dtelmaster,
        <ls_dd03p>-ddtext,
        <ls_dd03p>-reptext,
        <ls_dd03p>-scrtext_s,
        <ls_dd03p>-scrtext_m,
        <ls_dd03p>-scrtext_l.
    ENDLOOP.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd02v ).
    lo_xml->structure_add( ls_dd09l ).
    lo_xml->table_add( it_table = lt_dd03p
                       iv_name  = 'DD03P_TABLE' ).
    lo_xml->table_add( it_table = lt_dd05m
                       iv_name  = 'DD05M_TABLE' ).
    lo_xml->table_add( it_table = lt_dd08v
                       iv_name  = 'DD08V_TABLE' ).
    lo_xml->table_add( lt_dd12v ).
    lo_xml->table_add( lt_dd17v ).
    lo_xml->table_add( it_table = lt_dd35v
                       iv_name  = 'DD35V_TALE' ).
    lo_xml->table_add( lt_dd36m ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name      TYPE ddobjname,
          lv_tname     TYPE trobj_name,
          lo_xml       TYPE REF TO lcl_xml,
          ls_dd02v     TYPE dd02v,
          ls_dd09l     TYPE dd09l,
          lt_dd03p     TYPE TABLE OF dd03p,
          lt_dd05m     TYPE TABLE OF dd05m,
          lt_dd08v     TYPE TABLE OF dd08v,
          lt_dd12v     TYPE dd12vtab,
          lt_dd17v     TYPE dd17vtab,
          ls_dd17v     LIKE LINE OF lt_dd17v,
          lt_secondary LIKE lt_dd17v,
          lt_dd35v     TYPE TABLE OF dd35v,
          lt_dd36m     TYPE dd36mttyp,
          ls_dd12v     LIKE LINE OF lt_dd12v.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd02v ).
    lo_xml->structure_read( CHANGING cg_structure = ls_dd09l ).

    lo_xml->table_read( EXPORTING iv_name  = 'DD03P_TABLE'
                        CHANGING ct_table = lt_dd03p ).
    lo_xml->table_read( EXPORTING iv_name = 'DD05M_TABLE'
                        CHANGING ct_table = lt_dd05m ).
    lo_xml->table_read( EXPORTING iv_name = 'DD08V_TABLE'
                        CHANGING ct_table = lt_dd08v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd12v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd17v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD35V_TALE'
                        CHANGING ct_table = lt_dd35v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd36m ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = lv_name
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
        dd05m_tab         = lt_dd05m
        dd08v_tab         = lt_dd08v
        dd35v_tab         = lt_dd35v
        dd36m_tab         = lt_dd36m
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TABL_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

* handle indexes
    LOOP AT lt_dd12v INTO ls_dd12v.

* todo, call corr_insert?

      CLEAR lt_secondary.
      LOOP AT lt_dd17v INTO ls_dd17v
          WHERE sqltab = ls_dd12v-sqltab AND indexname = ls_dd12v-indexname.
        APPEND ls_dd17v TO lt_secondary.
      ENDLOOP.

      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = ls_dd12v-sqltab
          id                = ls_dd12v-indexname
          dd12v_wa          = ls_dd12v
        TABLES
          dd17v_tab         = lt_secondary
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        _raise 'error from DDIF_INDX_PUT'.
      ENDIF.

      CALL FUNCTION 'DD_DD_TO_E071'
        EXPORTING
          type     = 'INDX'
          name     = ls_dd12v-sqltab
          id       = ls_dd12v-indexname
        IMPORTING
          obj_name = lv_tname.

      lcl_objects_activation=>add( iv_type = 'INDX'
                                   iv_name = lv_tname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_TABL IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho DEFINITION INHERITING FROM lcl_objects_super FINAL.
* For complete list of tool_type - see ENHTOOLS table
  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS deserialize_badi
      IMPORTING io_xml     TYPE REF TO lcl_xml
                iv_package TYPE devclass
      RAISING   lcx_exception.
    METHODS deserialize_hook
      IMPORTING io_xml     TYPE REF TO lcl_xml
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_badi
      IMPORTING iv_tool     TYPE enhtooltype
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING lcx_exception.
    METHODS serialize_hook
      IMPORTING iv_tool     TYPE enhtooltype
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING lcx_exception.

ENDCLASS.                    "lcl_object_enho DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir.

* todo, it should look up in the ENHO database tables or call some methods
* to see if the object exists, looking in TADIR will not work
    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS NOT INITIAL.
      rv_bool = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lv_enh_id    TYPE enhname,
          lv_tool      TYPE enhtooltype,
          lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lo_xml       TYPE REF TO lcl_xml,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it,
          li_enh_tool  TYPE REF TO if_enh_tool.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement( lv_enh_id ).
      CATCH cx_enh_root.
        _raise 'Error from CL_ENH_FACTORY'.
    ENDTRY.
    lv_tool = li_enh_tool->get_tool( ).

    CASE lv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        serialize_badi( iv_tool = lv_tool
                        ii_enh_tool = li_enh_tool ).
      WHEN cl_enh_tool_hook_impl=>tooltype.
        serialize_hook( iv_tool = lv_tool
                        ii_enh_tool = li_enh_tool ).
* ToDo:
*      WHEN cl_enh_tool_class=>tooltype.
*      WHEN 'ENHFUGRDATA'. "cl_enh_tool_fugr
*      WHEN cl_enh_tool_intf=>tooltype.
*      WHEN cl_wdr_cfg_enhancement=>tooltype.
*      WHEN 'ENHWDYN'. "cl_enh_tool_wdy
      WHEN OTHERS.
        _raise 'Unsupported ENHO type'.
    ENDCASE.

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml  TYPE REF TO lcl_xml,
          lv_tool TYPE enhtooltype.


    lo_xml = mo_files->read_xml( ).

    lo_xml->element_read( CHANGING cg_element = lv_tool ).

    CASE lv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        deserialize_badi( io_xml     = lo_xml
                          iv_package = iv_package ).
      WHEN cl_enh_tool_hook_impl=>tooltype.
        deserialize_hook( io_xml     = lo_xml
                          iv_package = iv_package ).
* ToDo:
*      WHEN cl_enh_tool_class=>tooltype.
*      WHEN 'ENHFUGRDATA'. "cl_enh_tool_fugr
*      WHEN cl_enh_tool_intf=>tooltype.
*      WHEN cl_wdr_cfg_enhancement=>tooltype.
*      WHEN 'ENHWDYN'. "cl_enh_tool_wdy
      WHEN OTHERS.
        _raise 'Unsupported ENHO type'.
    ENDCASE.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_badi.

    DATA: lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lv_enhname   TYPE enhname,
          lo_badi      TYPE REF TO cl_enh_tool_badi_impl,
          li_tool      TYPE REF TO if_enh_tool,
          lv_package   TYPE devclass,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl> LIKE LINE OF lt_impl.


    io_xml->element_read( EXPORTING iv_name   = 'SHORTTEXT'
                          CHANGING cg_element = lv_shorttext ).
    io_xml->element_read( CHANGING cg_element = lv_spot_name ).
    io_xml->table_read( CHANGING ct_table = lt_impl ).


    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname       = lv_enhname
            enhtype       = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype   = cl_enh_tool_badi_impl=>tooltype
          IMPORTING
            enhancement   = li_tool
          CHANGING
            devclass      = lv_package ).
        lo_badi ?= li_tool.

        lo_badi->set_spot_name( lv_spot_name ).
        lo_badi->if_enh_object_docu~set_shorttext( lv_shorttext ).
        LOOP AT lt_impl ASSIGNING <ls_impl>.
          lo_badi->add_implementation( <ls_impl> ).
        ENDLOOP.
        lo_badi->if_enh_object~save( ).
        lo_badi->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        _raise 'error deserializing ENHO badi'.
    ENDTRY.

  ENDMETHOD.                    "deserialize_badi

  METHOD deserialize_hook.


    DATA: lv_tool            TYPE enhtooltype,
          lv_shorttext       TYPE string,
          lo_xml             TYPE REF TO lcl_xml,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.

    io_xml->element_read( EXPORTING iv_name   = 'SHORTTEXT'
                          CHANGING cg_element = lv_shorttext ).
    io_xml->structure_read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                            CHANGING cg_structure = ls_original_object ).
    io_xml->table_read( CHANGING ct_table = lt_enhancements ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname       = lv_enhname
            enhtype       = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype   = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement   = li_tool
          CHANGING
            devclass      = lv_package ).
        lo_hook_impl ?= li_tool.

        lo_hook_impl->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_hook_impl->set_original_object(
            pgmid       = ls_original_object-pgmid
            obj_name    = ls_original_object-org_obj_name
            obj_type    = ls_original_object-org_obj_type
            program     = ls_original_object-programname
            main_type   = ls_original_object-org_main_type
            main_name   = ls_original_object-org_main_name ).
        lo_hook_impl->set_include_bound( ls_original_object-include_bound ).

        LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
          lo_hook_impl->add_hook_impl(
              overwrite                 = <ls_enhancement>-overwrite
              method                    = <ls_enhancement>-method
              enhmode                   = <ls_enhancement>-enhmode
              full_name                 = <ls_enhancement>-full_name
              source                    = <ls_enhancement>-source
              spot                      = <ls_enhancement>-spotname
              parent_full_name          = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        _raise 'error deserializing ENHO hook'.
    ENDTRY.

  ENDMETHOD.                    "deserialize_hook

  METHOD serialize_badi.

    DATA: lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lo_xml       TYPE REF TO lcl_xml,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    CREATE OBJECT lo_xml.
    lo_xml->element_add( iv_tool ).
    lo_xml->element_add( ig_element = lv_shorttext
                         iv_name    = 'SHORTTEXT' ).
    lo_xml->element_add( lv_spot_name ).
    lo_xml->table_add( lt_impl ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize_badi

  METHOD serialize_hook.

    DATA: lv_tool            TYPE enhtooltype,
          lv_shorttext       TYPE string,
          lo_xml             TYPE REF TO lcl_xml,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          ls_original_object TYPE enh_hook_admin,
          lt_enhancements    TYPE enh_hook_impl_it.

    lo_hook_impl ?= ii_enh_tool.

    lv_shorttext = lo_hook_impl->if_enh_object_docu~get_shorttext( ).
    lo_hook_impl->get_original_object(
      IMPORTING
        pgmid     = ls_original_object-pgmid
        obj_name  = ls_original_object-org_obj_name
        obj_type  = ls_original_object-org_obj_type
        main_type = ls_original_object-org_main_type
        main_name = ls_original_object-org_main_name
        program   = ls_original_object-programname ).
    ls_original_object-include_bound = lo_hook_impl->get_include_bound( ).
    lt_enhancements = lo_hook_impl->get_hook_impls( ).

    CREATE OBJECT lo_xml.
    lo_xml->element_add( iv_tool ).
    lo_xml->element_add( ig_element = lv_shorttext
                         iv_name    = 'SHORTTEXT' ).
    lo_xml->structure_add( ig_structure = ls_original_object
                           iv_name = 'ORIGINAL_OBJECT' ).
    lo_xml->table_add( lt_enhancements ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize_hook

  METHOD lif_object~delete.

    DATA: lv_enh_id     TYPE enhname,
          li_enh_object TYPE REF TO if_enh_object.


    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_object = cl_enh_factory=>get_enhancement(
          enhancement_id = lv_enh_id
          lock           = abap_true ).
        li_enh_object->delete( ).
        li_enh_object->save( ).
        li_enh_object->unlock( ).
      CATCH cx_enh_root.
        _raise 'Error deleting ENHO'.
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ENHO'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_enho IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enqu DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enqu DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enqu IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-ENQU'
               iv_field = 'RSRD1-ENQU_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'L'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, ENQU'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lo_xml   TYPE REF TO lcl_xml,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE TABLE OF dd27p.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd25v_wa      = ls_dd25v
      TABLES
        dd26e_tab     = lt_dd26e
        dd27p_tab     = lt_dd27p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_ENQU_GET'.
    ENDIF.
    IF ls_dd25v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd25v ).
    lo_xml->table_add( it_table = lt_dd26e
                       iv_name = 'DD26E_TABLE' ).
    lo_xml->table_add( it_table = lt_dd27p
                       iv_name = 'DD27P_TABLE' ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE TABLE OF dd27p.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd25v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD26E_TABLE'
                        CHANGING ct_table = lt_dd26e ).
    lo_xml->table_read( EXPORTING iv_name = 'DD27P_TABLE'
                        CHANGING ct_table = lt_dd27p ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_ENQU_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_enqu IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_shlp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_shlp DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_shlp IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_shlpname TYPE dd30l-shlpname.


    SELECT SINGLE shlpname FROM dd30l INTO lv_shlpname
      WHERE shlpname = ms_item-obj_name
      AND as4local = 'A'.                               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-SHMA'
               iv_field = 'RSRD1-SHMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'H'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, SHLP'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lo_xml   TYPE REF TO lcl_xml,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd30v_wa      = ls_dd30v
      TABLES
        dd31v_tab     = lt_dd31v
        dd32p_tab     = lt_dd32p
        dd33v_tab     = lt_dd33v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_SHLP_GET'.
    ENDIF.
    IF ls_dd30v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd30v-as4user,
           ls_dd30v-as4date,
           ls_dd30v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd30v ).
    lo_xml->table_add( it_table = lt_dd31v
                       iv_name  = 'DD31V_TABLE' ).
    lo_xml->table_add( it_table = lt_dd32p
                       iv_name  = 'DD32P_TABLE' ).
    lo_xml->table_add( it_table = lt_dd33v
                       iv_name  = 'DD33V_TABLE' ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd30v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD31V_TABLE'
                        CHANGING ct_table = lt_dd31v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD32P_TABLE'
                        CHANGING ct_table = lt_dd32p ).
    lo_xml->table_read( EXPORTING iv_name = 'DD33V_TABLE'
                        CHANGING ct_table = lt_dd33v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_PUT'
      EXPORTING
        name              = lv_name
        dd30v_wa          = ls_dd30v
      TABLES
        dd31v_tab         = lt_dd31v
        dd32p_tab         = lt_dd32p
        dd33v_tab         = lt_dd33v
      EXCEPTIONS
        shlp_not_found    = 1
        name_inconsistent = 2
        shlp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_SHLP_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_shlp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_TRAN DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tran DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_TRAN DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_msag IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tran IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_tcode TYPE tstc-tcode.


    SELECT SINGLE tcode FROM tstc INTO lv_tcode
      WHERE tcode = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSEUK'.
    <ls_bdcdata>-dynpro   = '0390'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TSTC-TCODE'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE93'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4 ##fm_subrc_ok. "#EC CI_SUBRC

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_transaction TYPE tstc-tcode.


    lv_transaction = ms_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~deserialize.

    CONSTANTS: lc_hex_tra TYPE x VALUE '00',
*               c_hex_men TYPE x VALUE '01',
               lc_hex_par TYPE x VALUE '02',
               lc_hex_rep TYPE x VALUE '80'.
*               c_hex_rpv TYPE x VALUE '10',
*               c_hex_obj TYPE x VALUE '08',
*               c_hex_chk TYPE x VALUE '04',
*               c_hex_enq TYPE x VALUE '20'.

    DATA: lv_dynpro TYPE d020s-dnum,
          lo_xml    TYPE REF TO lcl_xml,
          ls_tstc   TYPE tstc,
          lv_type   TYPE rglif-docutype,
          ls_tstct  TYPE tstct,
          ls_tstcc  TYPE tstcc.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_tstc ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tstcc ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tstct ).

    lv_dynpro = ls_tstc-dypno.

    CASE ls_tstc-cinfo.
      WHEN lc_hex_tra.
        lv_type = ststc_c_type_dialog.
      WHEN lc_hex_rep.
        lv_type = ststc_c_type_report.
      WHEN lc_hex_par.
        lv_type = ststc_c_type_parameters.
* todo, or ststc_c_type_variant?
      WHEN OTHERS.
        _raise 'Transaction, unknown CINFO'.
    ENDCASE.

    CALL FUNCTION 'RPY_TRANSACTION_INSERT'
      EXPORTING
        transaction         = ls_tstc-tcode
        program             = ls_tstc-pgmna
        dynpro              = lv_dynpro
        language            = gc_english
        development_class   = iv_package
        transaction_type    = lv_type
        shorttext           = ls_tstct-ttext
        html_enabled        = ls_tstcc-s_webgui
        java_enabled        = ls_tstcc-s_platin
        wingui_enabled      = ls_tstcc-s_win32
      EXCEPTIONS
        cancelled           = 1
        already_exist       = 2
        permission_error    = 3
        name_not_allowed    = 4
        name_conflict       = 5
        illegal_type        = 6
        object_inconsistent = 7
        db_access_error     = 8
        OTHERS              = 9.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_INSERT'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          lt_tcodes      TYPE TABLE OF tstc,
          ls_tcode       LIKE LINE OF lt_tcodes,
          ls_tstct       TYPE tstct,
          lt_gui_attr    TYPE TABLE OF tstcc,
          lo_xml         TYPE REF TO lcl_xml,
          ls_gui_attr    LIKE LINE OF lt_gui_attr.


    lv_transaction = ms_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = lv_transaction
      TABLES
        tcodes           = lt_tcodes
        gui_attributes   = lt_gui_attr
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc = 4 OR sy-subrc = 3.
      RETURN.
    ENDIF.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_READ'.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = gc_english
      AND tcode = lv_transaction.                       "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'Transaction description not found'.
    ENDIF.

    READ TABLE lt_tcodes INDEX 1 INTO ls_tcode.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO ls_gui_attr.
    ASSERT sy-subrc = 0.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tcode ).
    lo_xml->structure_add( ls_gui_attr ).
    lo_xml->structure_add( ls_tstct ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

ENDCLASS.                    "lcl_object_msag IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_tobj DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tobj DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_tobj DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_tobj IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tobj IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_objectname TYPE objh-objectname.


    SELECT SINGLE objectname FROM objh INTO lv_objectname
      WHERE objectname = ms_item-obj_name(10)
      AND objecttype = ms_item-obj_name+10.             "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm,
          lo_xml   TYPE REF TO lcl_xml.


    ls_objh-objectname = ms_item-obj_name(10).
    ls_objh-objecttype = ms_item-obj_name+10.

    CALL FUNCTION 'CTO_OBJECT_GET'
      EXPORTING
        iv_objectname      = ls_objh-objectname
        iv_objecttype      = ls_objh-objecttype
        iv_language        = gc_english
        iv_sel_objt        = abap_true
        iv_sel_objs        = abap_true
        iv_sel_objsl       = abap_true
        iv_sel_objm        = abap_true
      IMPORTING
        es_objh            = ls_objh
        es_objt            = ls_objt
      TABLES
        tt_objs            = lt_objs
        tt_objsl           = lt_objsl
        tt_objm            = lt_objm
      EXCEPTIONS
        object_not_defined = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from CTO_OBJECT_GET'.
    ENDIF.

    CLEAR: ls_objh-luser,
           ls_objh-ldate.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_objh ).
    lo_xml->structure_add( ls_objt ).
    lo_xml->table_add( lt_objs ).
    lo_xml->table_add( lt_objsl ).
    lo_xml->table_add( lt_objm ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm,
          lo_xml   TYPE REF TO lcl_xml.


    lo_xml = mo_files->read_xml( ).
    lo_xml->structure_read( CHANGING cg_structure = ls_objh ).
    lo_xml->structure_read( CHANGING cg_structure = ls_objt ).
    lo_xml->table_read( CHANGING ct_table = lt_objs ).
    lo_xml->table_read( CHANGING ct_table = lt_objsl ).
    lo_xml->table_read( CHANGING ct_table = lt_objm ).

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'I'
        iv_objecttext         = ls_objt-ddtext
        iv_objcateg           = ls_objh-objcateg
        iv_objtransp          = ls_objh-objtransp
        iv_devclass           = iv_package
      TABLES
        tt_v_obj_s            = lt_objs
        tt_objm               = lt_objm
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
* todo, TOBJ has to be saved/generated after the DDIC tables have been activated
      _raise 'error from OBJ_GENERATE'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: ls_objh TYPE objh.


    ls_objh-objectname = ms_item-obj_name(10).
    ls_objh-objecttype = ms_item-obj_name+10.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'D'
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _raise 'error from OBJ_GENERATE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.
    _raise 'todo, TOBJ jump'.
  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_tobj IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_msag DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_msag DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_msag DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_msag IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_arbgb TYPE t100a-arbgb.


    SELECT SINGLE arbgb FROM t100a INTO lv_arbgb
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'MSAG'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

* parameter SUPPRESS_DIALOG doesnt exist in all versions
    CALL FUNCTION 'RS_DELETE_MESSAGE_ID'
      EXPORTING
        nachrichtenklasse = ms_item-obj_name
      EXCEPTIONS
        not_executed      = 1
        not_found         = 2
        no_permission     = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      _raise 'Error from RS_DELETE_MESSAGE_ID'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_t100a TYPE t100a,
          ls_t100t TYPE t100t,
          ls_t100u TYPE t100u,
          lt_t100  TYPE TABLE OF t100.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.

    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_t100a ).
    lo_xml->table_read( EXPORTING iv_name = 'T100'
                        CHANGING ct_table = lt_t100 ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        global_lock         = abap_true
        devclass            = iv_package
        object              = ls_t100a-arbgb
        object_class        = 'T100'
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 01
        permission_failure  = 02
        unknown_objectclass = 03.
    IF sy-subrc <> 0.
      _raise 'Error from RS_CORR_INSERT'.
    ENDIF.

    LOOP AT lt_t100 ASSIGNING <ls_t100>.
      MODIFY t100 FROM <ls_t100>.                         "#EC CI_SUBRC
      ASSERT sy-subrc = 0.

      CLEAR ls_t100u.
      MOVE-CORRESPONDING <ls_t100> TO ls_t100u ##enh_ok.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      MODIFY t100u FROM ls_t100u.                         "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDLOOP.

    ls_t100a-masterlang = gc_english.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    ls_t100t-sprsl = gc_english.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_inf    TYPE t100a,
          lt_source TYPE TABLE OF t100,
          lo_xml    TYPE REF TO lcl_xml.


    lv_msg_id = ms_item-obj_name.

    SELECT SINGLE * FROM t100a INTO ls_inf
      WHERE arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_inf-respuser.

    SELECT * FROM t100 INTO TABLE lt_source
      WHERE sprsl = gc_english
      AND arbgb = lv_msg_id
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_inf ).
    lo_xml->table_add( it_table = lt_source
                       iv_name  = 'T100' ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

ENDCLASS.                    "lcl_object_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_fugr DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_fugr DEFINITION INHERITING FROM lcl_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    TYPES: ty_rs38l_incl_tt TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.

    METHODS main_name
      RETURNING VALUE(rv_program) TYPE program
      RAISING   lcx_exception.

    METHODS functions
      RETURNING VALUE(rt_functab) TYPE ty_rs38l_incl_tt
      RAISING   lcx_exception.

    METHODS includes
      RETURNING VALUE(rt_includes) TYPE rso_t_objnm
      RAISING   lcx_exception.

    METHODS serialize_functions
      RAISING lcx_exception.

    METHODS deserialize_functions
      RAISING lcx_exception.

    METHODS serialize_xml
      RAISING lcx_exception.

    METHODS deserialize_xml
      IMPORTING iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_includes
      RAISING lcx_exception.

    METHODS deserialize_includes
      IMPORTING iv_package TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_fugr DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_fugr IMPLEMENTATION.

* function group SEUF
* function group SIFP
* function group SUNI

  METHOD lif_object~exists.

    DATA: lv_pool  TYPE tlibg-area.


    lv_pool = ms_item-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.

  METHOD deserialize_functions.

    DATA: lv_include       TYPE rs38l-include,
          lv_area          TYPE rs38l-area,
          lo_xml           TYPE REF TO lcl_xml,
          lt_functab       TYPE ty_rs38l_incl_tt,
          lt_import        TYPE TABLE OF rsimp,
          lt_changing      TYPE TABLE OF rscha,
          lt_export        TYPE TABLE OF rsexp,
          lt_tables        TYPE TABLE OF rstbl,
          lt_exception     TYPE TABLE OF rsexc,
          lt_documentation TYPE TABLE OF rsfdo,
          lt_source        TYPE TABLE OF rssource,
          lv_global_flag   TYPE rs38l-global,
          lv_remote_call   TYPE rs38l-remote,
          lv_update_task   TYPE rs38l-utask,
          lv_short_text    TYPE tftit-stext,
          lv_remote_basxml TYPE rs38l-basxml_enabled.

    FIELD-SYMBOLS: <ls_functab> LIKE LINE OF lt_functab.


    lo_xml = mo_files->read_xml( ).

    lo_xml->table_read( CHANGING  ct_table = lt_functab ).

    LOOP AT lt_functab ASSIGNING <ls_functab>.

      lt_source = mo_files->read_abap( iv_extra = <ls_functab>-funcname ).

      lo_xml = mo_files->read_xml( iv_extra = <ls_functab>-funcname ).

      lo_xml->element_read( CHANGING cg_element = lv_global_flag ).
      lo_xml->element_read( CHANGING cg_element = lv_remote_call ).
      lo_xml->element_read( CHANGING cg_element = lv_update_task ).
      lo_xml->element_read( CHANGING cg_element = lv_short_text ).
      lo_xml->element_read( CHANGING cg_element = lv_remote_basxml ).

      lo_xml->table_read( EXPORTING iv_name = 'IMPORT'
                          CHANGING ct_table = lt_import ).
      lo_xml->table_read( EXPORTING iv_name = 'CHANGING'
                          CHANGING ct_table = lt_changing ).
      lo_xml->table_read( EXPORTING iv_name = 'EXPORT'
                          CHANGING ct_table = lt_export ).
      lo_xml->table_read( EXPORTING iv_name = 'TABLES'
                          CHANGING ct_table = lt_tables ).
      lo_xml->table_read( EXPORTING iv_name = 'EXCEPTION'
                          CHANGING ct_table = lt_exception ).
      lo_xml->table_read( EXPORTING iv_name = 'DOCUMENTATION'
                          CHANGING ct_table = lt_documentation ).

      lv_area = ms_item-obj_name.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = <ls_functab>-funcname
        IMPORTING
          include            = lv_include
        EXCEPTIONS
          function_not_exist = 1.
      IF sy-subrc = 0.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_functab>-funcname
            suppress_success_message = abap_true
          EXCEPTIONS
            error_message            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          _raise 'error from FUNCTION_DELETE'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = <ls_functab>-funcname
          function_pool           = lv_area
          interface_global        = lv_global_flag
          remote_call             = lv_remote_call
          short_text              = lv_short_text
*         NAMESPACE               = ' ' todo
          remote_basxml_supported = lv_remote_basxml
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = lt_import
          export_parameter        = lt_export
          tables_parameter        = lt_tables
          changing_parameter      = lt_changing
          exception_list          = lt_exception
          parameter_docu          = lt_documentation
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.
      IF sy-subrc <> 0.
        _raise 'error from RS_FUNCTIONMODULE_INSERT'.
      ENDIF.

      INSERT REPORT lv_include FROM lt_source.

      lcl_objects_activation=>add( iv_type = 'FUNC'
                                   iv_name = <ls_functab>-funcname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_functions

  METHOD deserialize_includes.

    DATA: lo_xml      TYPE REF TO lcl_xml,
          ls_progdir  TYPE ty_progdir,
          lt_includes TYPE rso_t_objnm,
          lt_tpool    TYPE textpool_table,
          lt_source   TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lo_xml = mo_files->read_xml( ).
    lo_xml->table_read( CHANGING ct_table = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      lt_source = mo_files->read_abap( iv_extra = <lv_include> ).

      lo_xml = mo_files->read_xml( iv_extra = <lv_include> ).

      lo_xml->structure_read( EXPORTING iv_name     = 'PROGDIR'
                              CHANGING cg_structure = ls_progdir ).

      lo_xml->table_read( CHANGING ct_table = lt_tpool ).

      deserialize_program( is_progdir = ls_progdir
                           it_source  = lt_source
                           it_tpool   = lt_tpool
                           iv_package = iv_package ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_includes

  METHOD deserialize_xml.

    DATA: lv_complete  TYPE rs38l-area,
          lo_xml       TYPE REF TO lcl_xml,
          lv_namespace TYPE rs38l-namespace,
          lv_areat     TYPE tlibt-areat,
          lv_stext     TYPE tftit-stext,
          lv_group     TYPE rs38l-area.


    lv_complete = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_complete
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      _raise 'error from FUNCTION_INCLUDE_SPLIT'.
    ENDIF.

    lo_xml = mo_files->read_xml( ).
    lo_xml->element_read( CHANGING cg_element = lv_areat ).
    lv_stext = lv_areat.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_group
        short_text              = lv_stext
        namespace               = lv_namespace
        devclass                = iv_package
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.
    IF sy-subrc <> 0 AND sy-subrc <> 1 AND sy-subrc <> 3.
* todo, change description
      _raise 'error from RS_FUNCTION_POOL_INSERT'.
    ENDIF.

  ENDMETHOD.                    "deserialize_xml

  METHOD serialize_xml.

    DATA: lo_xml      TYPE REF TO lcl_xml,
          lt_functab  TYPE ty_rs38l_incl_tt,
          lt_includes TYPE rso_t_objnm,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = gc_english
      AND area = ms_item-obj_name.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in TLIBT'.
    ENDIF.

    lt_functab = functions( ).
    lt_includes = includes( ).

* todo, dynpros

    CREATE OBJECT lo_xml.
    lo_xml->element_add( lv_areat ).
    lo_xml->table_add( it_table = lt_functab ).
    lo_xml->table_add( it_table = lt_includes ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize_xml

  METHOD includes.

    DATA: lv_program TYPE program,
          lv_cnam    TYPE reposrc-cnam,
          lt_functab TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF rt_includes,
                   <ls_func>    LIKE LINE OF lt_functab.


    lv_program = main_name( ).
    lt_functab = functions( ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
*       WITH_RESERVED_INCLUDES =
*       WITH_CLASS_INCLUDES    = ' ' hmm, todo
      TABLES
        includetab   = rt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      _raise 'Error from RS_GET_ALL_INCLUDES'.
    ENDIF.

    LOOP AT lt_functab ASSIGNING <ls_func>.
      DELETE TABLE rt_includes FROM <ls_func>-include.
    ENDLOOP.

* skip SAP standard includes
    LOOP AT rt_includes ASSIGNING <lv_include>.
      SELECT SINGLE cnam FROM reposrc INTO lv_cnam
        WHERE progname = <lv_include>
        AND r3state = 'A'
        AND cnam = 'SAP'.
      IF sy-subrc = 0.
        DELETE rt_includes INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    APPEND lv_program TO rt_includes.

  ENDMETHOD.                    "includes

  METHOD functions.

    DATA: lv_area TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      _raise 'Error from RS_FUNCTION_POOL_CONTENTS'.
    ENDIF.

  ENDMETHOD.                    "functions

  METHOD main_name.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      _raise 'Error from FUNCTION_INCLUDE_SPLIT'.
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.                    "main_name

  METHOD serialize_functions.

    DATA: lt_import        TYPE TABLE OF rsimp,
          lo_xml           TYPE REF TO lcl_xml,
          lt_changing      TYPE TABLE OF rscha,
          lt_export        TYPE TABLE OF rsexp,
          lt_tables        TYPE TABLE OF rstbl,
          lt_exception     TYPE TABLE OF rsexc,
          lt_documentation TYPE TABLE OF rsfdo,
          lt_source        TYPE TABLE OF rssource,
          lv_global_flag   TYPE rs38l-global,
          lv_remote_call   TYPE rs38l-remote,
          lv_update_task   TYPE rs38l-utask,
          lv_short_text    TYPE tftit-stext,
          lt_functab       TYPE ty_rs38l_incl_tt,
          lt_new_source    TYPE rsfb_source,
          lv_remote_basxml TYPE rs38l-basxml_enabled.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF lt_functab.


    lt_functab = functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = lv_global_flag
          remote_call             = lv_remote_call
          update_task             = lv_update_task
          short_text              = lv_short_text
          remote_basxml_supported = lv_remote_basxml
        TABLES
          import_parameter        = lt_import
          changing_parameter      = lt_changing
          export_parameter        = lt_export
          tables_parameter        = lt_tables
          exception_list          = lt_exception
          documentation           = lt_documentation
          source                  = lt_source
        CHANGING
          new_source              = lt_new_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        _raise 'Error from RPY_FUNCTIONMODULE_READ_NEW'.
      ENDIF.

      CREATE OBJECT lo_xml.
      lo_xml->element_add( lv_global_flag ).
      lo_xml->element_add( lv_remote_call ).
      lo_xml->element_add( lv_update_task ).
      lo_xml->element_add( lv_short_text ).
      lo_xml->element_add( lv_remote_basxml ).

      lo_xml->table_add( it_table = lt_import
                         iv_name  = 'IMPORT' ).
      lo_xml->table_add( it_table = lt_changing
                         iv_name  = 'CHANGING' ).
      lo_xml->table_add( it_table = lt_export
                         iv_name  = 'EXPORT' ).
      lo_xml->table_add( it_table = lt_tables
                         iv_name  = 'TABLES' ).
      lo_xml->table_add( it_table = lt_exception
                         iv_name  = 'EXCEPTION' ).
      lo_xml->table_add( it_table = lt_documentation
                         iv_name  = 'DOCUMENTATION' ).

      mo_files->add_xml( iv_extra = <ls_func>-funcname
                         io_xml   = lo_xml ).

      IF NOT lt_new_source IS INITIAL.
        mo_files->add_abap( iv_extra = <ls_func>-funcname
                            it_abap  = lt_new_source ).
      ELSE.
        mo_files->add_abap( iv_extra = <ls_func>-funcname
                            it_abap  = lt_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "serialize_functions

  METHOD serialize_includes.

    DATA: lt_includes TYPE rso_t_objnm.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lt_includes = includes( ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

* todo, filename is not correct, a include can be used in several programs
      serialize_program( is_item    = ms_item
                         io_files   = mo_files
                         iv_program = <lv_include>
                         iv_extra   = <lv_include> ).

    ENDLOOP.

  ENDMETHOD.                    "serialize_includes

  METHOD lif_object~serialize.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_xml( ).

    serialize_functions( ).

    serialize_includes( ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    deserialize_xml( iv_package ).

    deserialize_functions( ).

    deserialize_includes( iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_area TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = lv_area
        suppress_popups        = abap_true
        skip_progress_ind      = abap_true
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
      _raise 'error from RS_FUNCTION_POOL_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'FUGR'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_fugr IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_view DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_view IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-VIMA'
               iv_field = 'RSRD1-VIMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'V'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, VIEW'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd25v_wa      = ls_dd25v
        dd09l_wa      = ls_dd09l
      TABLES
        dd26v_tab     = lt_dd26v
        dd27p_tab     = lt_dd27p
        dd28j_tab     = lt_dd28j
        dd28v_tab     = lt_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_VIEW_GET'.
    ENDIF.
    IF ls_dd25v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd25v ).
    lo_xml->structure_add( ls_dd09l ).

    lo_xml->table_add( it_table = lt_dd26v
                       iv_name = 'DD26V_TABLE' ).
    lo_xml->table_add( it_table = lt_dd27p
                       iv_name = 'DD27P_TABLE' ).
    lo_xml->table_add( it_table = lt_dd28j
                       iv_name = 'DD28J_TABLE' ).
    lo_xml->table_add( it_table = lt_dd28v
                       iv_name = 'DD28V_TABLE' ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd25v ).
    lo_xml->structure_read( CHANGING cg_structure = ls_dd09l ).

    lo_xml->table_read( EXPORTING iv_name = 'DD26V_TABLE'
                        CHANGING ct_table = lt_dd26v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD27P_TABLE'
                        CHANGING ct_table = lt_dd27p ).
    lo_xml->table_read( EXPORTING iv_name = 'DD28J_TABLE'
                        CHANGING ct_table = lt_dd28j ).
    lo_xml->table_read( EXPORTING iv_name = 'DD28V_TABLE'
                        CHANGING ct_table = lt_dd28v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = lt_dd26v
        dd27p_tab         = lt_dd27p
        dd28j_tab         = lt_dd28j
        dd28v_tab         = lt_dd28v
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_VIEW_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_nrob DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_object TYPE tnro-object.


    SELECT SINGLE object FROM tnro INTO lv_object
      WHERE object = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lv_object     TYPE tnro-object,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    lv_object = ms_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
      EXPORTING
        language          = gc_english
        object            = lv_object
      IMPORTING
        object_attributes = ls_attributes
        object_text       = ls_text
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_READ'.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_attributes ).
    lo_xml->structure_add( ls_text ).
    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    lo_xml = mo_files->read_xml( ).
    lo_xml->structure_read( CHANGING cg_structure = ls_attributes ).
    lo_xml->structure_read( CHANGING cg_structure = ls_text ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_UPDATE'
      EXPORTING
        indicator                 = 'I'
        object_attributes         = ls_attributes
        object_text               = ls_text
      TABLES
        errors                    = lt_errors
      EXCEPTIONS
        object_already_exists     = 1
        object_attributes_missing = 2
        object_not_found          = 3
        object_text_missing       = 4
        wrong_indicator           = 5
        OTHERS                    = 6.
    IF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_UPDATE'.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
      EXPORTING
        object                 = ls_attributes-object
      EXCEPTIONS
        object_not_initialized = 1.
    IF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_CLOSE'.
    ENDIF.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = 'NROB'
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = gc_english
        wi_set_genflag      = abap_true
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      _raise 'error from TR_TADIR_INTERFACE'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_object TYPE tnro-object.


    lv_object = ms_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_DELETE'
      EXPORTING
        language           = gc_english
        object             = lv_object
      EXCEPTIONS
        delete_not_allowed = 1
        object_not_found   = 2
        wrong_indicator    = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    _raise 'todo'.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_nrob IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ttyp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ttyp DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ttyp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ttyp IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_typename TYPE dd40l-typename.


    SELECT SINGLE typename FROM dd40l INTO lv_typename
      WHERE typename = ms_item-obj_name
      AND as4local = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'A'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, TTYP'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd40v_wa      = ls_dd40v
      TABLES
        dd42v_tab     = lt_dd42v
        dd43v_tab     = lt_dd43v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TTYP_GET'.
    ENDIF.
    IF ls_dd40v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd40v-as4user,
           ls_dd40v-as4date,
           ls_dd40v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd40v ).
    lo_xml->table_add( lt_dd42v ).
    lo_xml->table_add( lt_dd43v ).

    mo_files->add_xml( lo_xml ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lo_xml = mo_files->read_xml( ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd40v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd42v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd43v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = lv_name
        dd40v_wa          = ls_dd40v
      TABLES
        dd42v_tab         = lt_dd42v
        dd43v_tab         = lt_dd43v
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TTYP_PUT'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_ttyp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog DEFINITION INHERITING FROM lcl_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS deserialize_dynpros
      IMPORTING io_xml TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    METHODS deserialize_cua
      IMPORTING io_xml TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    METHODS deserialize_textpool
      IMPORTING it_tpool TYPE textpool_table
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_prog DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: lv_progname TYPE reposrc-progname.


    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_program LIKE sy-repid.


    lv_program = ms_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        program            = lv_program
        suppress_popup     = abap_true
      EXCEPTIONS
        enqueue_lock       = 1
        object_not_found   = 2
        permission_failure = 3
        reject_deletion    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      _raise 'error from RS_DELETE_PROGRAM'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD deserialize_textpool.

    READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
    IF ( sy-subrc = 0 AND lines( it_tpool ) = 1 ) OR lines( it_tpool ) = 0.
      RETURN. " no action for includes
    ENDIF.

    INSERT TEXTPOOL ms_item-obj_name
      FROM it_tpool
      LANGUAGE gc_english
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = ms_item-obj_name ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey,
          ls_adm    TYPE rsmpe_adm,
          lt_sta    TYPE TABLE OF rsmpe_stat,
          lt_fun    TYPE TABLE OF rsmpe_funt,
          lt_men    TYPE TABLE OF rsmpe_men,
          lt_mtx    TYPE TABLE OF rsmpe_mnlt,
          lt_act    TYPE TABLE OF rsmpe_act,
          lt_but    TYPE TABLE OF rsmpe_but,
          lt_pfk    TYPE TABLE OF rsmpe_pfk,
          lt_set    TYPE TABLE OF rsmpe_staf,
          lt_doc    TYPE TABLE OF rsmpe_atrt,
          lt_tit    TYPE TABLE OF rsmpe_titt,
          lt_biv    TYPE TABLE OF rsmpe_buts.


    io_xml->structure_read( CHANGING cg_structure = ls_adm ).
    IF ls_adm IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->table_read( EXPORTING iv_name = 'RSMPE_STAT_TABLE'
                        CHANGING ct_table = lt_sta ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_FUNT_TABLE'
                        CHANGING ct_table = lt_fun ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_MEN_TABLE'
                        CHANGING ct_table = lt_men ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_MNLT_TABLE'
                        CHANGING ct_table = lt_mtx ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_ACT_TABLE'
                        CHANGING ct_table = lt_act ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_BUT_TABLE'
                        CHANGING ct_table = lt_but ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_PFK_TABLE'
                        CHANGING ct_table = lt_pfk ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_STAF_TABLE'
                        CHANGING ct_table = lt_set ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_ATRT_TABLE'
                        CHANGING ct_table = lt_doc ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_TITT_TABLE'
                        CHANGING ct_table = lt_tit ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_BUTS_TABLE'
                        CHANGING ct_table = lt_biv ).

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in tadir'.
    ENDIF.

    ls_tr_key-obj_type = ms_item-obj_type.
    ls_tr_key-obj_name = ms_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = ms_item-obj_name.

    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = ms_item-obj_name
        language  = gc_english
        tr_key    = ls_tr_key
        adm       = ls_adm
        state     = 'I'
      TABLES
        sta       = lt_sta
        fun       = lt_fun
        men       = lt_men
        mtx       = lt_mtx
        act       = lt_act
        but       = lt_but
        pfk       = lt_pfk
        set       = lt_set
        doc       = lt_doc
        tit       = lt_tit
        biv       = lt_biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_WRITE'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'CUAD'
                                 iv_name = ms_item-obj_name ).

  ENDMETHOD.                    "deserialize_cua

  METHOD lif_object~serialize.

    serialize_program( is_item = ms_item
                       io_files = mo_files ).

  ENDMETHOD.                    "lif_serialize~serialize

  METHOD lif_object~deserialize.

    DATA: lo_xml     TYPE REF TO lcl_xml,
          ls_progdir TYPE ty_progdir,
          lt_tpool   TYPE textpool_table,
          lt_source  TYPE abaptxt255_tab.


    lo_xml = mo_files->read_xml( ).

    lt_source = mo_files->read_abap( ).

    lo_xml->table_read( CHANGING ct_table = lt_tpool ).

    lo_xml->structure_read( EXPORTING iv_name = 'PROGDIR'
                            CHANGING cg_structure = ls_progdir ).
    deserialize_program( is_progdir = ls_progdir
                         it_source  = lt_source
                         it_tpool   = lt_tpool
                         iv_package = iv_package ).

    deserialize_dynpros( lo_xml ).

    deserialize_cua( lo_xml ).

    deserialize_textpool( lt_tpool ).

  ENDMETHOD.                    "lif_serialize~deserialize

  METHOD deserialize_dynpros.

    DATA: li_element              TYPE REF TO if_ixml_element,
          ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lv_name                 TYPE dwinactiv-obj_name,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow.


    DO.
      li_element = io_xml->xml_find( 'SCREEN' ).
      IF NOT li_element IS BOUND.
        EXIT. " current loop
      ENDIF.

      io_xml->structure_read( EXPORTING ii_root     = li_element
                              CHANGING cg_structure = ls_header ).

      io_xml->table_read( EXPORTING ii_root  = li_element
                          CHANGING  ct_table = lt_containers ).
      io_xml->table_read( EXPORTING ii_root  = li_element
                          CHANGING  ct_table = lt_fields_to_containers ).
      io_xml->table_read( EXPORTING ii_root  = li_element
                          CHANGING  ct_table = lt_flow_logic ).

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = lt_containers
          fields_to_containers   = lt_fields_to_containers
          flow_logic             = lt_flow_logic
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          OTHERS                 = 10.
      IF sy-subrc <> 2 AND sy-subrc <> 0.
        _raise 'error from RPY_DYNPRO_INSERT'.
      ENDIF.
* todo, RPY_DYNPRO_UPDATE?

      CONCATENATE ls_header-program ls_header-screen INTO lv_name RESPECTING BLANKS.

      lcl_objects_activation=>add( iv_type = 'DYNP'
                                   iv_name = lv_name ).

    ENDDO.

  ENDMETHOD.                    "deserialize_dynpros

ENDCLASS.                    "lcl_object_prog IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_item         TYPE ty_item
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING it_files   TYPE ty_files_tt
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING it_tadir TYPE lcl_tadir=>ty_tadir_tt
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS is_supported
      IMPORTING is_item        TYPE ty_item
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS exists
      IMPORTING is_item        TYPE ty_item
      RETURNING VALUE(rv_bool) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-METHODS create_object
      IMPORTING is_item       TYPE ty_item
      RETURNING VALUE(ri_obj) TYPE REF TO lif_object
      RAISING   lcx_exception.

    CLASS-METHODS class_name
      IMPORTING is_item              TYPE ty_item
      RETURNING VALUE(rv_class_name) TYPE string.

    CLASS-METHODS resolve_ddic
      CHANGING ct_tadir TYPE lcl_tadir=>ty_tadir_tt
      RAISING  lcx_exception.

    CLASS-METHODS check_warning
      IMPORTING is_item          TYPE ty_item
                iv_package       TYPE devclass
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING   lcx_exception.

    CLASS-METHODS update_package_tree
      IMPORTING iv_package TYPE devclass.

    CLASS-METHODS delete_obj
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS show_progress
      IMPORTING iv_current  TYPE i
                iv_total    TYPE i
                iv_obj_name TYPE tadir-obj_name.

ENDCLASS.                    "lcl_object DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_tadir IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tadir IMPLEMENTATION.

  METHOD read_single.

    DATA: lv_obj_name TYPE tadir-obj_name.


    IF iv_object = 'SICF'.
      CONCATENATE iv_obj_name '%' INTO lv_obj_name.
    ELSE.
      lv_obj_name = iv_obj_name.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name LIKE lv_obj_name.      "#EC CI_SUBRC "#EC CI_GENBUFF

  ENDMETHOD.                    "read_single

  METHOD check_exists.

    DATA: lv_exists TYPE abap_bool,
          ls_item   TYPE ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      lv_exists = lcl_objects=>exists( ls_item ).
      IF lv_exists = abap_true.
        APPEND <ls_tadir> TO rt_tadir.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD read.

* start recursion
    rt_tadir = build( iv_package = iv_package
                      iv_parent  = ''
                      iv_path    = '' ).

    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.                    "read

  METHOD build.

    DATA: lv_index    TYPE i,
          lt_tadir    TYPE ty_tadir_tt,
          lt_tdevc    TYPE STANDARD TABLE OF tdevc,
          lv_len      TYPE i,
          lv_path     TYPE string,
          lv_category TYPE seoclassdf-category.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc,
                   <ls_tadir> LIKE LINE OF rt_tadir.


    SELECT * FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_tadir
      WHERE devclass = iv_package
      AND object <> 'DEVC'
      AND object <> 'SOTR'
      ORDER BY PRIMARY KEY.               "#EC CI_GENBUFF "#EC CI_SUBRC

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.

      <ls_tadir>-path = iv_path.

      CASE <ls_tadir>-object.
        WHEN 'SICF'.
          <ls_tadir>-obj_name = <ls_tadir>-obj_name(15).
        WHEN 'INTF'.
          SELECT SINGLE category FROM seoclassdf INTO lv_category
            WHERE clsname = <ls_tadir>-obj_name
            AND ( version = '1'
            OR version = '0' ) ##warn_ok.               "#EC CI_GENBUFF
          IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
            DELETE rt_tadir INDEX lv_index.
          ENDIF.
      ENDCASE.
    ENDLOOP.

* look for subpackages
    SELECT * FROM tdevc INTO TABLE lt_tdevc
      WHERE parentcl = iv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF
    LOOP AT lt_tdevc ASSIGNING <ls_tdevc>.
      lv_len = strlen( iv_package ).
      IF <ls_tdevc>-devclass(lv_len) <> iv_package.
        _raise 'Unexpected package naming'.
      ENDIF.

      lv_path = <ls_tdevc>-devclass+lv_len.
      IF lv_path(1) = '_'.
        lv_path = lv_path+1.
      ENDIF.
      TRANSLATE lv_path TO LOWER CASE.
      CONCATENATE iv_path lv_path '/' INTO lv_path.

      lt_tadir = build( iv_package = <ls_tdevc>-devclass
                        iv_parent  = iv_package
                        iv_path    = lv_path ).
      APPEND LINES OF lt_tadir TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.                    "build

ENDCLASS.                    "lcl_tadir IMPLEMENTATION

CLASS lcl_file_status DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_result,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             match    TYPE sap_bool,
             filename TYPE string,
             package  TYPE devclass,
             path     TYPE string,
           END OF ty_result.
    TYPES: ty_results_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    CLASS-METHODS status
      IMPORTING it_files          TYPE ty_files_tt
                iv_package        TYPE devclass
      RETURNING VALUE(rt_results) TYPE ty_results_tt
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS compare_files
      IMPORTING it_repo         TYPE ty_files_tt
                is_gen          TYPE ty_file
      RETURNING VALUE(rv_match) TYPE sap_bool
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_file_status IMPLEMENTATION.

  METHOD compare_files.

    READ TABLE it_repo WITH KEY path = is_gen-path
                                filename = is_gen-filename
                                data = is_gen-data
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      rv_match = abap_false.
    ELSE.
      rv_match = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD status.

    DATA: lv_pre    TYPE tadir-obj_name,
          lt_files  TYPE ty_files_tt,
          ls_result LIKE LINE OF rt_results,
          lv_type   TYPE string,
          ls_item   TYPE ty_item,
          lt_tadir  TYPE lcl_tadir=>ty_tadir_tt,
          ls_tadir  TYPE tadir,
          lv_ext    TYPE string.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF it_files,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <ls_result> LIKE LINE OF rt_results,
                   <ls_gen>    LIKE LINE OF lt_files.


    LOOP AT it_files ASSIGNING <ls_file>.
      SPLIT <ls_file>-filename AT '.' INTO lv_pre lv_type lv_ext.
      TRANSLATE lv_pre TO UPPER CASE.
      TRANSLATE lv_type TO UPPER CASE.

      IF lv_ext <> 'xml' OR strlen( lv_type ) <> 4.
        CONTINUE. " current loop
      ENDIF.

* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN lv_pre WITH '/'.

      CLEAR ls_result.
      ls_result-obj_type = lv_type.
      ls_result-obj_name = lv_pre.

      CLEAR ls_item.
      ls_item-obj_type = lv_type.
      ls_item-obj_name = lv_pre.

      lt_files = lcl_objects=>serialize( ls_item ).

      IF lt_files[] IS INITIAL.
* item does not exist locally
        ls_result-filename = <ls_file>-filename.
        APPEND ls_result TO rt_results.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT lt_files ASSIGNING <ls_gen>.
        ls_result-filename = <ls_gen>-filename.
        ls_result-match = compare_files( it_repo = it_files
                                         is_gen  = <ls_gen> ).
        APPEND ls_result TO rt_results.
      ENDLOOP.
    ENDLOOP.

* find files only existing remotely, including non abapGit related
    LOOP AT it_files ASSIGNING <ls_file>.
      READ TABLE rt_results WITH KEY filename = <ls_file>-filename
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-filename = <ls_file>-filename.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* find objects only existing locally
    lt_tadir = lcl_tadir=>read( iv_package ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      READ TABLE rt_results
        WITH KEY obj_type = <ls_tadir>-object obj_name = <ls_tadir>-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-obj_type = <ls_tadir>-object.
        ls_result-obj_name = <ls_tadir>-obj_name.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* add path information for files
    LOOP AT it_files ASSIGNING <ls_file>.
      READ TABLE rt_results ASSIGNING <ls_result> WITH KEY filename = <ls_file>-filename.
      IF sy-subrc = 0.
        <ls_result>-path = <ls_file>-path.
      ENDIF.
    ENDLOOP.

* add package information
    LOOP AT rt_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL.
      ls_tadir = lcl_tadir=>read_single( iv_object   = <ls_result>-obj_type
                                         iv_obj_name = <ls_result>-obj_name ).
      <ls_result>-package = ls_tadir-devclass.
    ENDLOOP.

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_results
      COMPARING obj_type obj_name filename.

  ENDMETHOD.                    "status

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_package DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sap_package DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      check IMPORTING it_results       TYPE lcl_file_status=>ty_results_tt
                      iv_top           TYPE devclass
            RETURNING VALUE(rv_errors) TYPE string,
      create.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_to_path
        IMPORTING
          iv_top         TYPE devclass
          iv_package     TYPE devclass
        RETURNING
          VALUE(rv_path) TYPE string.

ENDCLASS.                    "lcl_package DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_package IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sap_package IMPLEMENTATION.

  METHOD class_to_path.

    DATA: lv_len      TYPE i,
          lv_path     TYPE string,
          lv_parentcl TYPE tdevc-parentcl.


    IF iv_top = iv_package.
      rv_path = '/'.
    ELSE.
      SELECT SINGLE parentcl FROM tdevc INTO lv_parentcl
        WHERE devclass = iv_package.      "#EC CI_SUBRC "#EC CI_GENBUFF
      ASSERT sy-subrc = 0.

      IF lv_parentcl IS INITIAL.
        rv_path = 'error' ##no_text.
      ELSE.
        lv_len = strlen( lv_parentcl ).
        lv_path = iv_package+lv_len.
        IF lv_path(1) = '_'.
          lv_path = lv_path+1.
        ENDIF.
        TRANSLATE lv_path TO LOWER CASE.
        CONCATENATE lv_path '/' INTO lv_path.

        rv_path = class_to_path( iv_top     = iv_top
                                 iv_package = lv_parentcl ).

        CONCATENATE rv_path lv_path INTO rv_path.

      ENDIF.

    ENDIF.

  ENDMETHOD.                    "class_to_path

  METHOD check.

    DATA: lv_path TYPE string.

    FIELD-SYMBOLS: <ls_res1> LIKE LINE OF it_results,
                   <ls_res2> LIKE LINE OF it_results.


* check files for one object is in the same folder
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT obj_type IS INITIAL.
      LOOP AT it_results ASSIGNING <ls_res2>
          WHERE obj_type = <ls_res1>-obj_type
          AND obj_name = <ls_res1>-obj_name
          AND path <> <ls_res1>-path.
        CONCATENATE rv_errors 'Files for object'
          <ls_res1>-obj_type <ls_res1>-obj_name
          'are not placed in the same folder<br>'
          INTO rv_errors SEPARATED BY space.                "#EC NOTEXT
        EXIT.
      ENDLOOP.
    ENDLOOP.

* check that objects are created in package corresponding to folder
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT package IS INITIAL AND NOT path IS INITIAL.
      lv_path = class_to_path( iv_top     = iv_top
                               iv_package = <ls_res1>-package ).
      IF lv_path <> <ls_res1>-path.
        CONCATENATE rv_errors 'Package and path does not match for object,'
          <ls_res1>-obj_type <ls_res1>-obj_name '<br>'
          INTO rv_errors SEPARATED BY space.                "#EC NOTEXT
      ENDIF.
    ENDLOOP.

* check for multiple files with same filename
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT filename IS INITIAL.
      LOOP AT it_results ASSIGNING <ls_res2>
          WHERE filename = <ls_res1>-filename
          AND path <> <ls_res1>-path.
        CONCATENATE rv_errors 'Multiple files with same filename,'
          <ls_res1>-filename '<br>'
          INTO rv_errors SEPARATED BY space.                "#EC NOTEXT
        EXIT.
      ENDLOOP.
    ENDLOOP.

    IF NOT rv_errors IS INITIAL.
      CONCATENATE '<br>' rv_errors INTO rv_errors.
    ENDIF.

  ENDMETHOD.                    "check

  METHOD create.
* todo, see https://github.com/larshp/abapGit/issues/5
  ENDMETHOD.                    "create

ENDCLASS.                    "lcl_package IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects IMPLEMENTATION.

  METHOD show_progress.

    DATA: lv_pct TYPE i,
          lv_f   TYPE f.


    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_obj_name.

  ENDMETHOD.                    "show_progress

  METHOD check_warning.

    DATA: lv_question TYPE c LENGTH 200,
          lv_answer   TYPE c,
          ls_tadir    TYPE tadir.


    ls_tadir = lcl_tadir=>read_single( iv_object   = is_item-obj_type
                                       iv_obj_name = is_item-obj_name ).
    IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> iv_package.
      CONCATENATE 'Overwrite object' is_item-obj_type is_item-obj_name
        'from package' ls_tadir-devclass
        INTO lv_question SEPARATED BY space.                "#EC NOTEXT

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning'
          text_question         = lv_question
          text_button_1         = 'Ok'
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.                        "#EC NOTEXT
      IF sy-subrc <> 0.
        _raise 'error from POPUP_TO_CONFIRM'.
      ENDIF.

      IF lv_answer = '2'.
        rv_cancel = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "check_warning

  METHOD update_package_tree.

    DATA: lv_tree TYPE dirtree-tname.


* update package tree for SE80
    lv_tree = 'EU_' && iv_package.
    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name              = lv_tree
        without_crossreference = abap_true
        with_tcode_index       = abap_true.

  ENDMETHOD.                    "update_package_tree

  METHOD create_object.

    DATA: lv_message    TYPE string,
          lv_class_name TYPE string.


    lv_class_name = class_name( is_item ).

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item = is_item.
      CATCH cx_sy_create_object_error.
        CONCATENATE 'Object type' is_item-obj_type 'not supported, serialize'
          INTO lv_message
          SEPARATED BY space.                               "#EC NOTEXT
        _raise lv_message.
    ENDTRY.

  ENDMETHOD.

  METHOD is_supported.

    TRY.
        create_object( is_item ).
        rv_bool = abap_true.
      CATCH lcx_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD exists.

    DATA: li_obj TYPE REF TO lif_object.


    TRY.
        li_obj = create_object( is_item ).
        rv_bool = li_obj->exists( ).
      CATCH lcx_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.

  METHOD class_name.

    CONCATENATE 'LCL_OBJECT_' is_item-obj_type INTO rv_class_name. "#EC NOTEXT
    IF rv_class_name = 'LCL_OBJECT_INTF'.
      rv_class_name = 'LCL_OBJECT_CLAS'.
    ENDIF.

  ENDMETHOD.                    "class_name

  METHOD jump.

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item ).
    li_obj->jump( ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: ls_item  TYPE ty_item,
          lt_tadir LIKE it_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* misuse field KORRNUM to fix deletion sequence

    lt_tadir[] = it_tadir[].

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'SUSC'.
          <ls_tadir>-korrnum = '5000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          <ls_tadir>-korrnum = '7000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '8000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '9000'.
        WHEN 'PROG'.
* delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '2000'.
          ELSE.
            <ls_tadir>-korrnum = '1000'.
          ENDIF.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '1000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = lt_tadir ).

    SORT lt_tadir BY korrnum ASCENDING.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      delete_obj( ls_item ).
    ENDLOOP.

  ENDMETHOD.                    "delete

  METHOD resolve_ddic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge.

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir,
                   <ls_edge>  LIKE LINE OF lt_edges,
                   <ls_found> LIKE LINE OF lt_founds,
                   <ls_node>  LIKE LINE OF lt_nodes.


* build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

* build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_founds ASSIGNING <ls_found>.
        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.

        <ls_edge>-to-obj_name   = <ls_found>-object.
        CASE <ls_found>-object_cls.
          WHEN 'DS'
              OR 'DT'.
            <ls_edge>-to-obj_type = 'TABL'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            _raise 'resolve_ddic, unknown object_cls'.
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <ls_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY
          from-obj_name = <ls_node>-obj_name
          from-obj_type = <ls_node>-obj_type
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <ls_node>-obj_name
              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges
            WHERE to-obj_name = <ls_node>-obj_name
            AND to-obj_type = <ls_node>-obj_type.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.                    "resolve_ddic

  METHOD delete_obj.

    DATA: li_obj     TYPE REF TO lif_object.


    li_obj = create_object( is_item ).
    li_obj->delete( ).

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lt_files TYPE ty_files_tt,
          li_obj   TYPE REF TO lif_object,
          lo_files TYPE REF TO lcl_objects_files,
          lo_obj   TYPE REF TO lcl_objects_super.


    CREATE OBJECT lo_files
      EXPORTING
        is_item = is_item.

    li_obj = create_object( is_item ).
    lo_obj ?= li_obj.
    lo_obj->set_files( lo_files ).
    li_obj->serialize( ).

    rt_files = lo_files->get_files( ).

* check for duplicates
    lt_files[] = rt_files[].
    SORT lt_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path filename.
    IF lines( lt_files ) <> lines( rt_files ).
      _raise 'Duplicates'.
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: ls_item    TYPE ty_item,
          lv_cancel  TYPE abap_bool,
          li_obj     TYPE REF TO lif_object,
          lo_files   TYPE REF TO lcl_objects_files,
          lo_obj     TYPE REF TO lcl_objects_super,
          lt_results TYPE lcl_file_status=>ty_results_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    lcl_objects_activation=>clear( ).

    lt_results = lcl_file_status=>status( it_files   = it_files
                                          iv_package = iv_package ).
    DELETE lt_results WHERE match = abap_true.
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    LOOP AT lt_results ASSIGNING <ls_result>.
      show_progress( iv_current  = sy-tabix
                     iv_total    = lines( lt_results )
                     iv_obj_name = <ls_result>-obj_name ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

      lv_cancel = check_warning( is_item    = ls_item
                                 iv_package = iv_package ).
      IF lv_cancel = abap_true.
        RETURN.
      ENDIF.

      CREATE OBJECT lo_files
        EXPORTING
          is_item = ls_item.
      lo_files->set_files( it_files ).
      li_obj = create_object( ls_item ).
      lo_obj ?= li_obj.
      lo_obj->set_files( lo_files ).
      li_obj->deserialize( iv_package ).

    ENDLOOP.

    lcl_objects_activation=>activate( ).

    update_package_tree( iv_package ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_hash DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_adler32 TYPE x LENGTH 4.

    CLASS-METHODS adler32
      IMPORTING iv_xstring         TYPE xstring
      RETURNING VALUE(rv_checksum) TYPE ty_adler32.

    CLASS-METHODS sha1
      IMPORTING iv_type        TYPE ty_type
                iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS sha1_raw
      IMPORTING iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE ty_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_hash DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_hash IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash IMPLEMENTATION.

  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = ( lv_a + iv_xstring+lv_index(1) ) MOD lc_adler.
      lv_b = ( lv_b + lv_a ) MOD lc_adler.
    ENDDO.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.                                                "adler32

  METHOD sha1_raw.

    DATA: lv_hash TYPE hash160.


    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        data           = iv_data
      IMPORTING
        hash           = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      _raise 'Error while calculating SHA1'.
    ENDIF.

    rv_sha1 = lv_hash.

    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.                                                "sha1_raw

  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.                                                "sha1

ENDCLASS.                    "lcl_hash IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_pack IMPLEMENTATION.

  METHOD type_and_length.

    DATA: lv_bits   TYPE string,
          lv_type   TYPE string,
          lv_result TYPE string,
          lv_c      TYPE c,
          lv_offset TYPE i,
          lv_x4     TYPE x LENGTH 4,
          lv_x      TYPE x LENGTH 1.


    CASE is_object-type.
      WHEN gc_type-commit.
        lv_type = '001'.
      WHEN gc_type-tree.
        lv_type = '010'.
      WHEN gc_type-blob.
        lv_type = '011'.
      WHEN gc_type-ref_d.
        lv_type = '111'.
      WHEN OTHERS.
        _raise 'Unexpected object type while encoding pack'.
    ENDCASE.

    lv_x4 = xstrlen( is_object-data ).
    DO 32 TIMES.
      GET BIT sy-index OF lv_x4 INTO lv_c.
      CONCATENATE lv_bits lv_c INTO lv_bits.
    ENDDO.

    IF lv_bits(28) = '0000000000000000000000000000'.
      CONCATENATE '0' lv_type lv_bits+28(4) INTO lv_result.
    ELSEIF lv_bits(21) = '000000000000000000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+21(7) INTO lv_result.
    ELSEIF lv_bits(14) = '00000000000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+21(7) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+14(7) INTO lv_result.
    ELSEIF lv_bits(7) = '0000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+21(7) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+14(7) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+7(7) INTO lv_result.
    ELSE.
* todo, this IF can be refactored, use shifting?
      _raise 'Todo, encoding length'.
    ENDIF.

* convert bit string to xstring
    CLEAR lv_x.
    DO strlen( lv_result ) TIMES.
      lv_offset = sy-index - 1.
      IF lv_result+lv_offset(1) = '1'.
        SET BIT ( lv_offset MOD 8 ) + 1 OF lv_x.
      ENDIF.
      IF ( lv_offset + 1 ) MOD 8 = 0.
        CONCATENATE rv_xstring lv_x INTO rv_xstring IN BYTE MODE.
        CLEAR lv_x.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "type_and_length

  METHOD get_length.

    DATA: lv_x           TYPE x,
          lv_length_bits TYPE string,
          lv_bitbyte     TYPE ty_bitbyte.


    lv_x = cv_data(1).
    IF c_debug_pack = abap_true.
      WRITE: / 'A:', lv_x, '(hex)'.                         "#EC NOTEXT
    ENDIF.
    lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
    IF c_debug_pack = abap_true.
      WRITE: lv_bitbyte.
    ENDIF.

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      IF c_debug_pack = abap_true.
        WRITE: / 'x:', lv_x, '(hex)'.                       "#EC NOTEXT
      ENDIF.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      IF c_debug_pack = abap_true.
        WRITE: lv_bitbyte.
      ENDIF.
      cv_data = cv_data+1.
      CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
    ENDWHILE.

    ev_length = lcl_convert=>bitbyte_to_int( lv_length_bits ).

  ENDMETHOD.                    "get_length

  METHOD encode_tree.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lt_nodes   LIKE it_nodes,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lt_nodes[] = it_nodes[].
* following has to be done, or unpack will fail on server side
    SORT lt_nodes BY name ASCENDING.

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_node>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.                    "encode_tree

  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_parent_lower = is_commit-parent.
    TRANSLATE lv_parent_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space. "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.                     "#EC NOTEXT
      CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    CONCATENATE lv_string gc_newline is_commit-body INTO lv_string.

    rv_data = lcl_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.                    "encode_commit

  METHOD get_type.

    DATA: lv_char3   TYPE c LENGTH 3,
          lv_bitbyte TYPE ty_bitbyte.


    lv_bitbyte = lcl_convert=>x_to_bitbyte( iv_x ).
    lv_char3 = lv_bitbyte+1.

    CASE lv_char3.
      WHEN '001'.
        rv_type = gc_type-commit.
      WHEN '010'.
        rv_type = gc_type-tree.
      WHEN '011'.
        rv_type = gc_type-blob.
      WHEN '111'.
        rv_type = gc_type-ref_d.
      WHEN OTHERS.
        _raise 'Todo, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "get_type

  METHOD decode_commit.

    DATA: lv_string TYPE string,
          lv_mode   TYPE string,
          lv_len    TYPE i,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = lcl_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT gc_newline INTO TABLE lt_string.

    lv_mode = 'tree'.                                       "#EC NOTEXT
    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_len = strlen( lv_mode ).

      IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
        CASE lv_mode.
          WHEN 'tree'.
            rs_commit-tree = <lv_string>+5.
            lv_mode = 'parent'.                             "#EC NOTEXT
          WHEN 'parent'.
            rs_commit-parent = <lv_string>+7.
            lv_mode = 'author'.                             "#EC NOTEXT
          WHEN 'author'.
            rs_commit-author = <lv_string>+7.
            lv_mode = 'committer'.                          "#EC NOTEXT
          WHEN 'committer'.
            rs_commit-committer = <lv_string>+10.
            CLEAR lv_mode.
        ENDCASE.
      ELSEIF lv_mode = 'parent' AND <lv_string>(6) = 'author'. "#EC NOTEXT
* first commit doesnt have parent
        rs_commit-author = <lv_string>+7.
        lv_mode = 'committer'.                              "#EC NOTEXT
      ELSE.
* body
        CONCATENATE rs_commit-body <lv_string> INTO rs_commit-body
          SEPARATED BY gc_newline.
      ENDIF.
    ENDLOOP.

* strip first newline
    IF strlen( rs_commit-body ) >= 2.
      rs_commit-body = rs_commit-body+2.
    ENDIF.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      _raise 'multiple parents? not supported'.
    ENDIF.

  ENDMETHOD.                    "decode_commit

  METHOD delta_header.

    DATA: lv_bitbyte TYPE ty_bitbyte,
          lv_bits    TYPE string,
          lv_x       TYPE x.


    lv_bits = ''.
    DO.
      lv_x = cv_delta(1).
      cv_delta = cv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    ev_header = lcl_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.                    "delta_header

  METHOD delta.

    DATA: lv_delta   TYPE xstring,
          lv_base    TYPE xstring,
          lv_result  TYPE xstring,
          lv_bitbyte TYPE ty_bitbyte,
          lv_offset  TYPE i,
          lv_message TYPE string,
          lv_sha1    TYPE ty_sha1,
          ls_object  LIKE LINE OF ct_objects,
          lv_len     TYPE i,
          lv_x       TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    lv_delta = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object> WITH KEY sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      CONCATENATE 'Base not found,' is_object-sha1 INTO lv_message
        SEPARATED BY space.                                 "#EC NOTEXT
      _raise lv_message.
    ELSE.
      lv_base = <ls_object>-data.
    ENDIF.

* sanity check
    IF <ls_object>-type = gc_type-ref_d.
      _raise 'Delta, base eq delta'.
    ENDIF.

* skip the 2 headers
    delta_header( CHANGING cv_delta = lv_delta ).
    delta_header( CHANGING cv_delta = lv_delta ).

    WHILE xstrlen( lv_delta ) > 0.

      lv_x = lv_delta(1).
      lv_delta = lv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).

      IF lv_bitbyte(1) = '1'. " MSB

        lv_offset = 0.
        IF lv_bitbyte+7(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_x.
        ENDIF.
        IF lv_bitbyte+6(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 256.
        ENDIF.
        IF lv_bitbyte+5(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 65536.
        ENDIF.
        IF lv_bitbyte+4(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_bitbyte+3(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_x.
        ENDIF.
        IF lv_bitbyte+2(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_len + lv_x * 256.
        ENDIF.
        IF lv_bitbyte+1(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_len + lv_x * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len) INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_x.
        CONCATENATE lv_result lv_delta(lv_len) INTO lv_result IN BYTE MODE.
        lv_delta = lv_delta+lv_len.
      ENDIF.

    ENDWHILE.

    lv_sha1 = lcl_hash=>sha1( iv_type = <ls_object>-type iv_data = lv_result ).

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    APPEND ls_object TO ct_objects.

  ENDMETHOD.                    "delta

  METHOD decode_deltas.

    DATA: ls_object LIKE LINE OF ct_objects,
          lt_deltas LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object WHERE type = gc_type-ref_d.
      DELETE ct_objects INDEX sy-tabix.
      APPEND ls_object TO lt_deltas.
    ENDLOOP.

    LOOP AT lt_deltas INTO ls_object.
      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.                    "decode_deltas

  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE string,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          ls_node    TYPE ty_node,
          lv_start   TYPE i.


    DO.
      IF lv_cursor >= xstrlen( iv_data ).
        EXIT. " current loop
      ENDIF.

      IF iv_data+lv_cursor(1) = lc_null.
        lv_len = lv_cursor - lv_start.
        lv_xstring = iv_data+lv_start(lv_len).

        lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
        SPLIT lv_string AT space INTO lv_chmod lv_name.

        lv_offset = lv_cursor + 1.

        CLEAR ls_node.
        ls_node-chmod = lv_chmod.
        IF ls_node-chmod <> gc_chmod-dir AND ls_node-chmod <> gc_chmod-file.
          _raise 'Unknown chmod'.
        ENDIF.

        ls_node-name = lv_name.
        ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
        TRANSLATE ls_node-sha1 TO LOWER CASE.
        APPEND ls_node TO rt_nodes.

        lv_start = lv_cursor + 1 + lc_sha_length.
        lv_cursor = lv_start.
      ELSE.
        lv_cursor = lv_cursor + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "decode_tree

  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE ty_sha1,
          lv_ref_delta      TYPE ty_sha1,
          lv_adler32        TYPE lcl_hash=>ty_adler32,
          lv_compressed     TYPE xstring,
          lv_compressed_len TYPE i,
          lv_decompress_len TYPE i,
          lv_decompressed   TYPE xstring,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects,
          ls_data           TYPE lcl_zlib=>ty_decompress.


    lv_data = iv_data.

* header
    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> c_pack_start.
      _raise 'Unexpected pack header'.
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      _raise 'Version not supported'.
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = lcl_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = gc_type-ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        _raise 'Unexpected zlib header'.
      ENDIF.
      lv_data = lv_data+2.

*******************************

      IF lv_zlib = c_zlib.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_data
          IMPORTING
            raw_out     = lv_decompressed
            raw_out_len = lv_decompress_len ).

        IF lv_expected <> lv_decompress_len.
          _raise 'Decompression falied'.
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          _raise 'Compressed data doesnt match'.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.
        lv_data = lv_data+4. " skip adler checksum

      ELSEIF lv_zlib = c_zlib_hmm.
* this takes some processing. When time permits: implement DEFLATE algorithm
* cl_abap_gzip copmression works for '789C', but does not produce the same
* result when '7801'
* compressed data might be larger than origial so add 10, adding 10 is safe
* as package always ends with SHA1 checksum
*        DO lv_expected + 10 TIMES.
*          lv_compressed_len = sy-index.
*
*          cl_abap_gzip=>decompress_binary(
*            EXPORTING
*              gzip_in     = lv_data
*              gzip_in_len = lv_compressed_len
*            IMPORTING
*              raw_out     = lv_decompressed
*              raw_out_len = lv_decompress_len ).
*
*          IF lv_decompress_len = lv_expected.
*            EXIT.
*          ELSE.
*            CLEAR lv_compressed_len.
*          ENDIF.
*        ENDDO.
        ls_data = lcl_zlib=>decompress( lv_data ).
        lv_compressed_len = ls_data-compressed_len.
        lv_decompressed = ls_data-raw.

        IF lv_compressed_len IS INITIAL.
          _raise 'Decompression falied :o/'.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.

        lv_adler32 = lcl_hash=>adler32( lv_decompressed ).
        IF lv_data(4) <> lv_adler32.
          lv_data = lv_data+1.
        ENDIF.
        IF lv_data(4) <> lv_adler32.
          lv_data = lv_data+1.
        ENDIF.
        IF lv_data(4) <> lv_adler32.
          _raise 'Wrong Adler checksum'.
        ENDIF.

        lv_data = lv_data+4. " skip adler checksum

      ENDIF.

*************************

      CLEAR ls_object.
      IF lv_type = gc_type-ref_d.
        ls_object-sha1 = lv_ref_delta.
        TRANSLATE ls_object-sha1 TO LOWER CASE.
      ELSE.
        ls_object-sha1 = lcl_hash=>sha1( iv_type = lv_type iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      APPEND ls_object TO rt_objects.

      IF c_debug_pack = abap_true.
        WRITE: /.
      ENDIF.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = lcl_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      _raise 'SHA1 at end of pack doesnt match'.
    ENDIF.

  ENDMETHOD.                    "decode

  METHOD encode.

    DATA: lv_sha1       TYPE x LENGTH 20,
          lv_adler32    TYPE lcl_hash=>ty_adler32,
          lv_len        TYPE i,
          lv_compressed TYPE xstring,
          lv_xstring    TYPE xstring.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_len = lines( it_objects ).
    lv_xstring = lcl_convert=>int_to_xstring( iv_i      = lv_len
                                              iv_length = 4 ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    LOOP AT it_objects ASSIGNING <ls_object>.
      lv_xstring = type_and_length( <ls_object> ).
      CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in   = <ls_object>-data
        IMPORTING
          gzip_out = lv_compressed ).

      CONCATENATE rv_data c_zlib lv_compressed INTO rv_data IN BYTE MODE.

      lv_adler32 = lcl_hash=>adler32( <ls_object>-data ).
      CONCATENATE rv_data lv_adler32 INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = to_upper( lcl_hash=>sha1_raw( rv_data ) ).
    CONCATENATE rv_data lv_sha1 INTO rv_data IN BYTE MODE.

  ENDMETHOD.                    "encode

ENDCLASS.                    "lcl_pack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_persistence DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence DEFINITION FINAL.

* there is currently no locking, so it is theoretically possible
* for a user to overwrite another users changes.
* the risk is minimized by always reading before updating

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_repo_persi,
             url         TYPE string,
             branch_name TYPE string,
             sha1        TYPE ty_sha1,
             package     TYPE devclass,
             offline     TYPE sap_bool,
           END OF ty_repo_persi.
    TYPES: ty_repos_persi_tt TYPE STANDARD TABLE OF ty_repo_persi WITH DEFAULT KEY.

    METHODS list
      RETURNING VALUE(rt_repos) TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS update
      IMPORTING iv_url         TYPE ty_repo_persi-url
                iv_branch_name TYPE ty_repo_persi-branch_name
                iv_branch      TYPE ty_sha1
      RAISING   lcx_exception.

    METHODS add
      IMPORTING iv_url         TYPE string
                iv_branch_name TYPE string
                iv_branch      TYPE ty_sha1 OPTIONAL
                iv_package     TYPE devclass
                iv_offline     TYPE sap_bool DEFAULT abap_false
      RAISING   lcx_exception.

    METHODS delete
      IMPORTING iv_url         TYPE ty_repo_persi-url
                iv_branch_name TYPE ty_repo_persi-branch_name
      RAISING   lcx_exception.

  PRIVATE SECTION.
    METHODS read_text_online
      RETURNING VALUE(rt_repos) TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS save_text_online
      IMPORTING it_repos TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS header_online
      RETURNING VALUE(rs_header) TYPE thead.

    METHODS read_text_offline
      RETURNING VALUE(rt_repos) TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS save_text_offline
      IMPORTING it_repos TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS header_offline
      RETURNING VALUE(rs_header) TYPE thead.

    METHODS read_text
      IMPORTING is_header       TYPE thead
      RETURNING VALUE(rt_lines) TYPE tlinetab
      RAISING   lcx_exception.

    METHODS save_text
      IMPORTING is_header TYPE thead
                it_lines  TYPE tlinetab
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_persistence DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_persistence IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence IMPLEMENTATION.

  METHOD save_text.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header   = is_header
      TABLES
        lines    = it_lines
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      _raise 'error from SAVE_TEXT'.
    ENDIF.

  ENDMETHOD.                    "save_text

  METHOD header_online.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = gc_english.
    rs_header-tdname   = 'ZABAPGIT'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header

  METHOD header_offline.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = gc_english.
    rs_header-tdname   = 'ZABAPGIT_OFFLINE'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header_offline

  METHOD delete.

    DATA: lt_repos TYPE ty_repos_persi_tt.


    lt_repos = list( ).

    DELETE lt_repos WHERE url = iv_url AND branch_name = iv_branch_name.
    IF sy-subrc <> 0.
      _raise 'repo not found, delete'.
    ENDIF.

    save_text_online( lt_repos ).
    save_text_offline( lt_repos ).

  ENDMETHOD.                    "delete

  METHOD save_text_online.

    DATA: lt_lines  TYPE TABLE OF tline.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo> WHERE offline = abap_false.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-branch_name.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-sha1.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-package.
    ENDLOOP.

    save_text( is_header = header_online( )
               it_lines  = lt_lines ).

    COMMIT WORK.

  ENDMETHOD.                    "save_text

  METHOD save_text_offline.

    DATA: lt_lines  TYPE TABLE OF tline.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo> WHERE offline = abap_true.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-package.
    ENDLOOP.

    save_text( is_header = header_offline( )
               it_lines  = lt_lines ).

    COMMIT WORK.

  ENDMETHOD.                    "save_text_offline

  METHOD add.

    DATA: lt_repos TYPE ty_repos_persi_tt.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    ASSERT NOT iv_url IS INITIAL.
    ASSERT NOT iv_package IS INITIAL.

    lt_repos = list( ).

    READ TABLE lt_repos WITH KEY url = iv_url branch_name = iv_branch_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      _raise 'already inserted'.
    ENDIF.

    APPEND INITIAL LINE TO lt_repos ASSIGNING <ls_repo>.
    <ls_repo>-url         = iv_url.
    <ls_repo>-branch_name = iv_branch_name.
    <ls_repo>-sha1        = iv_branch.
    <ls_repo>-package     = iv_package.
    <ls_repo>-offline     = iv_offline.

    save_text_online( lt_repos ).
    save_text_offline( lt_repos ).

  ENDMETHOD.                    "insert

  METHOD update.

    DATA: lt_repos TYPE ty_repos_persi_tt.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    IF iv_branch IS INITIAL.
      _raise 'update, sha empty'.
    ENDIF.

    lt_repos = list( ).

    READ TABLE lt_repos ASSIGNING <ls_repo>
      WITH KEY url = iv_url branch_name = iv_branch_name.
    IF sy-subrc <> 0.
      _raise 'persist update, repo not found'.
    ENDIF.

    <ls_repo>-sha1 = iv_branch.

    save_text_online( lt_repos ).

  ENDMETHOD.                    "update

  METHOD list.
    CLEAR rt_repos.
    APPEND LINES OF read_text_online( ) TO rt_repos.
    APPEND LINES OF read_text_offline( ) TO rt_repos.
  ENDMETHOD.                    "list

  METHOD read_text.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = is_header-tdid
        language                = is_header-tdspras
        name                    = is_header-tdname
        object                  = is_header-tdobject
      TABLES
        lines                   = rt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 4.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'Error from READ_TEXT'.
    ENDIF.

  ENDMETHOD.                    "read_text

  METHOD read_text_online.

    DATA: lt_lines TYPE TABLE OF tline,
          lv_step  TYPE i,
          ls_repo  TYPE ty_repo_persi.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lt_lines = read_text( header_online( ) ).
    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    IF lines( lt_lines ) MOD 4 <> 0.
* if this happens, delete text ZABAPGIT in SO10 or edit the text
* manually, so it contains the right information
      _raise 'Persistence, text broken'.
    ENDIF.

    CLEAR ls_repo.
    LOOP AT lt_lines ASSIGNING <ls_line>.
      lv_step = lv_step + 1.
      CASE lv_step.
        WHEN 4.
          ls_repo-package = <ls_line>-tdline.

          IF ls_repo-url IS INITIAL OR ls_repo-branch_name IS INITIAL.
            _raise 'Persistence, text broken 2'.
          ENDIF.
          APPEND ls_repo TO rt_repos.
          CLEAR ls_repo.
          lv_step = 0.
        WHEN 3.
          ls_repo-sha1 = <ls_line>-tdline.
        WHEN 2.
          ls_repo-branch_name = <ls_line>-tdline.
        WHEN 1.
          ls_repo-url = <ls_line>-tdline.
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "list

  METHOD read_text_offline.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_repo  TYPE ty_repo_persi.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lt_lines = read_text( header_offline( ) ).
    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    IF lines( lt_lines ) MOD 2 <> 0.
* if this happens, delete text ZABAPGIT in SO10 or edit the text
* manually, so it contains the right information
      _raise 'Persistence, text broken'.
    ENDIF.

    CLEAR ls_repo.
    LOOP AT lt_lines ASSIGNING <ls_line>.
      IF <ls_line>-tdline IS INITIAL.
        _raise 'Persistence, text broken'.
      ENDIF.
      IF ls_repo-url IS INITIAL.
        ls_repo-url = <ls_line>-tdline.
        CONTINUE. " current loop
      ENDIF.

      ls_repo-package = <ls_line>-tdline.
      ls_repo-offline = abap_true.
      APPEND ls_repo TO rt_repos.
      CLEAR ls_repo.
    ENDLOOP.

  ENDMETHOD.                    "list

ENDCLASS.                    "lcl_persistence IMPLEMENTATION

CLASS lcl_repo DEFINITION ABSTRACT.

  PUBLIC SECTION.
    TYPES: ty_key TYPE i.

    METHODS:
      constructor
        IMPORTING iv_key  TYPE ty_key
                  is_data TYPE lcl_persistence=>ty_repo_persi,
      get_key
        RETURNING VALUE(rv_key) TYPE ty_key,
      get_name
        RETURNING VALUE(rv_name) TYPE string
        RAISING   lcx_exception,
      get_package
        RETURNING VALUE(rv_package) TYPE lcl_persistence=>ty_repo_persi-package,
      delete
        RAISING lcx_exception,
      add
        RAISING lcx_exception,
      refresh
        RAISING lcx_exception,
      render
        RETURNING VALUE(rv_html) TYPE string
        RAISING   lcx_exception.

  PROTECTED SECTION.
    DATA: mv_key  TYPE ty_key,
          ms_data TYPE lcl_persistence=>ty_repo_persi.

ENDCLASS.

CLASS lcl_repo_online DEFINITION INHERITING FROM lcl_repo FINAL.

  PUBLIC SECTION.
    METHODS:
      render REDEFINITION,
      refresh REDEFINITION,
      constructor
        IMPORTING iv_key  TYPE ty_key
                  is_data TYPE lcl_persistence=>ty_repo_persi
        RAISING   lcx_exception,
      get_url
        RETURNING VALUE(rv_url) TYPE lcl_persistence=>ty_repo_persi-url,
      get_branch_name
        RETURNING VALUE(rv_name) TYPE lcl_persistence=>ty_repo_persi-branch_name,
      get_sha1_local
        RETURNING VALUE(rv_sha1) TYPE lcl_persistence=>ty_repo_persi-sha1,
      get_sha1_remote
        RETURNING VALUE(rv_sha1) TYPE lcl_persistence=>ty_repo_persi-sha1,
      get_files
        RETURNING VALUE(rt_files) TYPE ty_files_tt,
      get_objects
        RETURNING VALUE(rt_objects) TYPE lcl_git_pack=>ty_objects_tt,
      deserialize
        RAISING lcx_exception,
      status
        RETURNING VALUE(rt_results) TYPE lcl_file_status=>ty_results_tt
        RAISING   lcx_exception,
      push
        IMPORTING is_comment TYPE ty_comment
                  it_files   TYPE ty_files_tt
        RAISING   lcx_exception.

  PRIVATE SECTION.
    DATA:
      mt_files   TYPE ty_files_tt,
      mt_objects TYPE lcl_git_pack=>ty_objects_tt,
      mv_branch  TYPE ty_sha1.

    METHODS:
      set_sha1
        IMPORTING iv_sha1 TYPE ty_sha1
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_repo_offline DEFINITION INHERITING FROM lcl_repo FINAL.

  PUBLIC SECTION.
    METHODS:
      render REDEFINITION.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_porcelain DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS pull
      IMPORTING io_repo    TYPE REF TO lcl_repo_online
      EXPORTING et_files   TYPE ty_files_tt
                et_objects TYPE lcl_git_pack=>ty_objects_tt
                ev_branch  TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS push
      IMPORTING io_repo          TYPE REF TO lcl_repo_online
                is_comment       TYPE ty_comment
                it_files         TYPE ty_files_tt
      RETURNING VALUE(rv_branch) TYPE ty_sha1
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS walk
      IMPORTING it_objects TYPE lcl_git_pack=>ty_objects_tt
                iv_sha1    TYPE ty_sha1
                iv_path    TYPE string
      CHANGING  ct_files   TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS root_tree
      IMPORTING it_objects      TYPE lcl_git_pack=>ty_objects_tt
                iv_branch       TYPE ty_sha1
      RETURNING VALUE(rt_nodes) TYPE lcl_git_pack=>ty_nodes_tt
      RAISING   lcx_exception.

    CLASS-METHODS receive_pack
      IMPORTING is_comment       TYPE ty_comment
                io_repo          TYPE REF TO lcl_repo_online
                it_nodes         TYPE lcl_git_pack=>ty_nodes_tt
                it_files         TYPE ty_files_tt
                iv_branch        TYPE ty_sha1
      RETURNING VALUE(rv_branch) TYPE ty_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_porcelain DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING lcx_exception.

    CLASS-METHODS on_event
                  FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

    CLASS-METHODS render_repo_online
      IMPORTING io_repo        TYPE REF TO lcl_repo_online
      RETURNING VALUE(rv_html) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS render_repo_offline
      IMPORTING io_repo        TYPE REF TO lcl_repo_offline
      RETURNING VALUE(rv_html) TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

    CLASS-METHODS render
      RETURNING VALUE(rv_html) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS render_css
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_header
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_menu
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_footer
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS install
      IMPORTING iv_url TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS newoffline
      RAISING lcx_exception.

    CLASS-METHODS add
      IMPORTING is_item TYPE ty_item
                iv_key  TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS uninstall
      IMPORTING iv_key TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS remove
      IMPORTING iv_key TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS pull
      IMPORTING iv_key TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS commit
      IMPORTING iv_key TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS diff
      IMPORTING is_result TYPE lcl_file_status=>ty_result
                iv_key    TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS render_diff
      IMPORTING is_result TYPE lcl_file_status=>ty_result
                io_diff   TYPE REF TO lcl_diff.

    CLASS-METHODS file_encode
      IMPORTING iv_key           TYPE lcl_repo=>ty_key
                is_file          TYPE lcl_file_status=>ty_result
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS file_decode
      IMPORTING iv_string TYPE clike
      EXPORTING ev_key    TYPE lcl_repo=>ty_key
                es_file   TYPE lcl_file_status=>ty_result
      RAISING   lcx_exception.

    CLASS-METHODS popup_comment
      RETURNING VALUE(rs_comment) TYPE ty_comment
      RAISING   lcx_exception.

    CLASS-METHODS get_logo_src
      RETURNING VALUE(rv_src) TYPE string.

    CLASS-METHODS zipexport
      RAISING lcx_exception.

ENDCLASS.                    "lcl_gui DEFINITION

CLASS lcl_repo_offline IMPLEMENTATION.

  METHOD render.
    rv_html = lcl_gui=>render_repo_offline( me ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_repo_srv DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: ty_repo_tt TYPE STANDARD TABLE OF REF TO lcl_repo WITH DEFAULT KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS list
      RETURNING VALUE(rt_list) TYPE ty_repo_tt
      RAISING   lcx_exception.

    CLASS-METHODS refresh
      IMPORTING iv_show_progress TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

    CLASS-METHODS new_online
      IMPORTING iv_url         TYPE string
                iv_branch_name TYPE string
                iv_package     TYPE devclass
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo_online
      RAISING   lcx_exception.

    CLASS-METHODS new_offline
      IMPORTING iv_url         TYPE string
                iv_package     TYPE devclass
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo_offline
      RAISING   lcx_exception.

    CLASS-METHODS add
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   lcx_exception.

    CLASS-METHODS get
      IMPORTING iv_key         TYPE lcl_repo=>ty_key
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo.

  PRIVATE SECTION.

    CLASS-DATA: gv_init        TYPE abap_bool VALUE abap_false,
                go_persistence TYPE REF TO lcl_persistence,
                gt_list        TYPE ty_repo_tt.

    CLASS-METHODS:
      validate_package
        IMPORTING
          iv_package TYPE devclass
        RAISING
          lcx_exception,
      show_progress
        IMPORTING
          iv_current TYPE i
          iv_total   TYPE i
          iv_text    TYPE string.

ENDCLASS.

CLASS lcl_repo_online IMPLEMENTATION.

  METHOD constructor.

    super->constructor( iv_key  = iv_key
                        is_data = is_data ).

    refresh( ).

  ENDMETHOD.

  METHOD status.

    rt_results = lcl_file_status=>status( it_files   = mt_files
                                         iv_package = get_package( ) ).

  ENDMETHOD.

  METHOD deserialize.

    lcl_objects=>deserialize( it_files   = mt_files
                              iv_package = get_package( ) ).

    lcl_repo_srv=>add( me ).

    set_sha1( mv_branch ).

  ENDMETHOD.

  METHOD refresh.

    lcl_git_porcelain=>pull( EXPORTING io_repo    = me
                             IMPORTING et_files   = mt_files
                                       et_objects = mt_objects
                                       ev_branch  = mv_branch ).

  ENDMETHOD.

  METHOD get_sha1_remote.
    rv_sha1 = mv_branch.
  ENDMETHOD.

  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.

  METHOD get_objects.
    rt_objects = mt_objects.
  ENDMETHOD.

  METHOD render.
    rv_html = lcl_gui=>render_repo_online( me ).
  ENDMETHOD.

  METHOD get_url.
    rv_url = ms_data-url.
  ENDMETHOD.

  METHOD get_branch_name.
    rv_name = ms_data-branch_name.
  ENDMETHOD.

  METHOD get_sha1_local.
    rv_sha1 = ms_data-sha1.
  ENDMETHOD.

  METHOD push.

    DATA: lv_branch TYPE ty_sha1.


    lv_branch = lcl_git_porcelain=>push( is_comment = is_comment
                                         io_repo    = me
                                         it_files   = it_files ).

    set_sha1( lv_branch ).

    refresh( ).

  ENDMETHOD.

  METHOD set_sha1.

    DATA: lo_persistence TYPE REF TO lcl_persistence.


    CREATE OBJECT lo_persistence.

    lo_persistence->update( iv_url         = ms_data-url
                            iv_branch_name = ms_data-branch_name
                            iv_branch      = iv_sha1 ).

    ms_data-sha1 = iv_sha1.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_repo IMPLEMENTATION.

  METHOD constructor.
    ms_data = is_data.
    mv_key = iv_key.
  ENDMETHOD.

  METHOD render.

* to be implemented in subclass
    ASSERT 1 = 0.

  ENDMETHOD.

  METHOD delete.

    DATA: lo_persistence TYPE REF TO lcl_persistence.


    CREATE OBJECT lo_persistence.

    lo_persistence->delete(
      iv_url         = ms_data-url
      iv_branch_name = ms_data-branch_name ).

  ENDMETHOD.

  METHOD refresh.

* redefined in LCL_REPO_ONLINE
    RETURN.

  ENDMETHOD.

  METHOD add.

    DATA: lo_persistence TYPE REF TO lcl_persistence.


    CREATE OBJECT lo_persistence.

    lo_persistence->add(
      iv_url         = ms_data-url
      iv_branch_name = ms_data-branch_name
      iv_branch      = ms_data-sha1
      iv_package     = ms_data-package
      iv_offline     = ms_data-offline ).

  ENDMETHOD.

  METHOD get_package.
    rv_package = ms_data-package.
  ENDMETHOD.

  METHOD get_key.
    rv_key = mv_key.
  ENDMETHOD.

  METHOD get_name.

    IF ms_data-offline = abap_true.
      rv_name = ms_data-url.
    ELSE.
      rv_name = lcl_url=>name( ms_data-url ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_repo_srv IMPLEMENTATION.

  METHOD class_constructor.
    CREATE OBJECT go_persistence.
  ENDMETHOD.

  METHOD list.

    IF gv_init = abap_false.
      refresh( ).
    ENDIF.

    rt_list = gt_list.

  ENDMETHOD.

  METHOD get.

    FIELD-SYMBOLS: <lo_list> LIKE LINE OF gt_list.


    LOOP AT gt_list ASSIGNING <lo_list>.
      IF <lo_list>->get_key( ) = iv_key.
        ro_repo = <lo_list>.
        RETURN.
      ENDIF.
    ENDLOOP.

    ASSERT 1 = 0.

  ENDMETHOD.

  METHOD refresh.

    DATA: lt_list    TYPE lcl_persistence=>ty_repos_persi_tt,
          lv_index   TYPE i,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_offline TYPE REF TO lcl_repo_offline.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CLEAR gt_list.

    lt_list = go_persistence->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      lv_index = sy-tabix.

      IF iv_show_progress = abap_true.
        show_progress( iv_current = sy-tabix
                       iv_total   = lines( lt_list )
                       iv_text    = <ls_list>-url ).
      ENDIF.

      IF <ls_list>-offline = abap_false.
        CREATE OBJECT lo_online
          EXPORTING
            iv_key  = lv_index
            is_data = <ls_list>.
        APPEND lo_online TO gt_list.
      ELSE.
        CREATE OBJECT lo_offline
          EXPORTING
            iv_key  = lv_index
            is_data = <ls_list>.
        APPEND lo_offline TO gt_list.
      ENDIF.
    ENDLOOP.

    gv_init = abap_true.

  ENDMETHOD.

  METHOD new_online.

    DATA: ls_repo_persi TYPE lcl_persistence=>ty_repo_persi.


    validate_package( iv_package ).

    ls_repo_persi-url         = iv_url.
    ls_repo_persi-branch_name = iv_branch_name.
    ls_repo_persi-package     = iv_package.

    CREATE OBJECT ro_repo
      EXPORTING
        iv_key  = lines( gt_list ) + 1
        is_data = ls_repo_persi.

  ENDMETHOD.

  METHOD new_offline.

    DATA: ls_repo_persi TYPE lcl_persistence=>ty_repo_persi.


    validate_package( iv_package ).

    ls_repo_persi-url     = iv_url.
    ls_repo_persi-package = iv_package.
    ls_repo_persi-offline = abap_true.

    CREATE OBJECT ro_repo
      EXPORTING
        iv_key  = lines( gt_list ) + 1
        is_data = ls_repo_persi.

    add( ro_repo ).

  ENDMETHOD.

  METHOD add.

    DATA: lo_repo LIKE LINE OF gt_list.


    LOOP AT gt_list INTO lo_repo.
      IF lo_repo->get_key( ) = io_repo->get_key( ).
        IF lo_repo = io_repo.
          RETURN.
        ENDIF.
        _raise 'identical keys'.
      ENDIF.
    ENDLOOP.

    io_repo->add( ).

    APPEND io_repo TO gt_list.

  ENDMETHOD.

  METHOD show_progress.

    DATA: lv_text TYPE c LENGTH 100,
          lv_pct  TYPE i,
          lv_f    TYPE f.


    lv_text = iv_text.
    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.
    IF lv_pct = 100.
      lv_pct = 99.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = lv_text.

  ENDMETHOD.

  METHOD validate_package.

    DATA: lv_devclass TYPE tdevc-devclass,
          lt_repos    TYPE lcl_persistence=>ty_repos_persi_tt.


    IF iv_package IS INITIAL.
      _raise 'add, package empty'.
    ENDIF.

    IF iv_package = '$TMP'.
      _raise 'not possible to use $TMP, create new (local) package'.
    ENDIF.

    SELECT SINGLE devclass FROM tdevc INTO lv_devclass
      WHERE devclass = iv_package
      AND as4user <> 'SAP'.                             "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'package not found or not allowed'.
    ENDIF.

* make sure its not already in use for a different repository
    lt_repos = go_persistence->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      _raise 'Package already in use'.
    ENDIF.

  ENDMETHOD.

  METHOD delete.

    io_repo->delete( ).

    DELETE TABLE gt_list FROM io_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_transport DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_transport DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_branch_list,
             sha1 TYPE ty_sha1,
             name TYPE string,
           END OF ty_branch_list.
    TYPES: ty_branch_list_tt TYPE STANDARD TABLE OF ty_branch_list WITH DEFAULT KEY.

* remote to local
    CLASS-METHODS upload_pack
      IMPORTING io_repo   TYPE REF TO lcl_repo_online
      EXPORTING ev_pack   TYPE xstring
                ev_branch TYPE ty_sha1
      RAISING   lcx_exception.

* local to remote
    CLASS-METHODS receive_pack
      IMPORTING io_repo   TYPE REF TO lcl_repo_online
                iv_commit TYPE ty_sha1
                iv_pack   TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS branches
      IMPORTING iv_url                TYPE string
      RETURNING VALUE(rt_branch_list) TYPE ty_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    CLASS-DATA: gv_agent TYPE string.

    CONSTANTS: BEGIN OF c_service,
                 receive TYPE string VALUE 'receive',       "#EC NOTEXT
                 upload  TYPE string VALUE 'upload',        "#EC NOTEXT
               END OF c_service.

    CLASS-METHODS branch_list
      IMPORTING iv_url         TYPE string
                iv_service     TYPE string
      EXPORTING ei_client      TYPE REF TO if_http_client
                et_branch_list TYPE ty_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS pkt_string
      IMPORTING iv_string     TYPE string
      RETURNING VALUE(rv_pkt) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS find_branch
      IMPORTING io_repo    TYPE REF TO lcl_repo_online
                iv_service TYPE string
      EXPORTING ei_client  TYPE REF TO if_http_client
                ev_branch  TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS parse
      EXPORTING ev_pack TYPE xstring
      CHANGING  cv_data TYPE xstring.

    CLASS-METHODS length_utf8_hex
      IMPORTING iv_data       TYPE xstring
      RETURNING VALUE(rv_len) TYPE i.

    CLASS-METHODS parse_branch_list
      IMPORTING iv_data        TYPE string
      RETURNING VALUE(rt_list) TYPE ty_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS set_headers
      IMPORTING io_repo    TYPE REF TO lcl_repo_online
                iv_service TYPE string
                ii_client  TYPE REF TO if_http_client
      RAISING   lcx_exception.

    CLASS-METHODS check_http_200
      IMPORTING ii_client TYPE REF TO if_http_client
      RAISING   lcx_exception.

    CLASS-METHODS get_null
      RETURNING VALUE(rv_c) TYPE char1.

ENDCLASS.                    "lcl_transport DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_transport IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_transport IMPLEMENTATION.

  METHOD class_constructor.

* bitbucket require agent prefix = "git/"
    gv_agent = 'git/abapGit ' && gc_abap_version.

  ENDMETHOD.

  METHOD set_headers.

    DATA: lv_value TYPE string.


    ii_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = lcl_url=>path_name( io_repo->get_url( ) ) &&
      '.git/git-' &&
      iv_service &&
      '-pack'.
    ii_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.         "#EC NOTEXT
    ii_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).                                 "#EC NOTEXT

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.          "#EC NOTEXT
    ii_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).                                 "#EC NOTEXT

  ENDMETHOD.                    "set_headers

  METHOD get_null.

    DATA: lv_x(4) TYPE x VALUE '00000000',
          lv_z(2) TYPE c.

    FIELD-SYMBOLS <lv_y> TYPE c.


    ASSIGN lv_x TO <lv_y> CASTING.
    lv_z = <lv_y>.
    rv_c = lv_z(1).

  ENDMETHOD.                    "get_null

  METHOD check_http_200.

    DATA: lv_code TYPE i.


    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 302.
        _raise 'HTTP redirect, check URL'.
      WHEN 401.
        _raise 'HTTP 401, unauthorized'.
      WHEN 403.
        _raise 'HTTP 403, forbidden'.
      WHEN 404.
        _raise 'HTTP 404, not found'.
      WHEN 415.
        _raise 'HTTP 415, unsupported media type'.
      WHEN OTHERS.
        _raise 'HTTP error code'.
    ENDCASE.

  ENDMETHOD.                                                "http_200

  METHOD parse_branch_list.

    DATA: lt_result TYPE TABLE OF string,
          lv_hash   TYPE ty_sha1,
          lv_name   TYPE string,
          lv_foo    TYPE string ##needed,
          lv_char   TYPE c,
          lv_data   LIKE LINE OF lt_result.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF rt_list.


    SPLIT iv_data AT gc_newline INTO TABLE lt_result.
    LOOP AT lt_result INTO lv_data.
      IF sy-tabix = 1.
        CONTINUE. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = get_null( ).
        SPLIT lv_name AT lv_char INTO lv_name lv_foo.
      ELSEIF sy-tabix > 2 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        _raise 'No branches, create branch manually by adding file'.
      ELSE.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1 = lv_hash.
      <ls_branch>-name = lv_name.
    ENDLOOP.

  ENDMETHOD.                    "parse_branch_list

  METHOD find_branch.

    DATA: lt_branch_list TYPE ty_branch_list_tt,
          ls_branch_list LIKE LINE OF lt_branch_list.


    branch_list(
      EXPORTING
        iv_url          = io_repo->get_url( )
        iv_service      = iv_service
      IMPORTING
        ei_client       = ei_client
        et_branch_list  = lt_branch_list ).

    IF io_repo->get_branch_name( ) IS INITIAL.
      _raise 'branch empty'.
    ENDIF.

    READ TABLE lt_branch_list INTO ls_branch_list
      WITH KEY name = io_repo->get_branch_name( ).
    IF sy-subrc <> 0.
      _raise 'Branch not found'.
    ENDIF.

    ev_branch = ls_branch_list-sha1.

  ENDMETHOD.                    "find_branch

  METHOD branches.

    DATA: li_client TYPE REF TO if_http_client.


    lcl_git_transport=>branch_list(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
      IMPORTING
        ei_client      = li_client
        et_branch_list = rt_branch_list ).
    li_client->close( ).

  ENDMETHOD.

  METHOD branch_list.

    DATA: lv_data TYPE string,
          lv_uri  TYPE string,
          lv_text TYPE string.

    STATICS: sv_authorization TYPE string.


    cl_http_client=>create_by_url(
      EXPORTING
        url    = lcl_url=>host( iv_url )
        ssl_id = 'ANONYM'
      IMPORTING
        client = ei_client ).

    ei_client->request->set_cdata( '' ).
    ei_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    ei_client->request->set_header_field(
        name  = 'user-agent'
        value = gv_agent ).                                 "#EC NOTEXT
    lv_uri = lcl_url=>path_name( iv_url ) &&
             '.git/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    ei_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).
    IF NOT sv_authorization IS INITIAL.
* note this will only work if all repositories uses the same login
      ei_client->request->set_header_field(
          name  = 'authorization'
          value = sv_authorization ).                       "#EC NOTEXT
      ei_client->propertytype_logon_popup = ei_client->co_disabled.
    ENDIF.
    ei_client->send( ).
    ei_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
* make sure:
* a) SSL is setup properly in STRUST
* b) no firewalls
* check trace file in transaction SMICM
          lv_text = 'HTTP Communication Failure'.           "#EC NOTEXT
        WHEN 2.
          lv_text = 'HTTP Invalid State'.                   "#EC NOTEXT
        WHEN 3.
          lv_text = 'HTTP Processing failed'.               "#EC NOTEXT
        WHEN OTHERS.
          lv_text = 'Another error occured'.                "#EC NOTEXT
      ENDCASE.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          iv_text = lv_text.
    ENDIF.

    check_http_200( ei_client ).
    sv_authorization = ei_client->request->get_header_field(
                                                  'authorization' ). "#EC NOTEXT

    lv_data = ei_client->response->get_cdata( ).
    et_branch_list = parse_branch_list( lv_data ).

  ENDMETHOD.                    "ref_discovery

  METHOD receive_pack.

    DATA: li_client  TYPE REF TO if_http_client,
          lv_cmd_pkt TYPE string,
          lv_line    TYPE string,
          lv_tmp     TYPE xstring,
          lv_xstring TYPE xstring,
          lv_string  TYPE string,
          lv_buffer  TYPE string,
          lv_branch  TYPE ty_sha1.


    find_branch(
      EXPORTING
        io_repo    = io_repo
        iv_service = c_service-receive
      IMPORTING
        ei_client  = li_client
        ev_branch  = lv_branch ).

    set_headers(
        io_repo    = io_repo
        iv_service = c_service-receive
        ii_client  = li_client ).

    lv_line = lv_branch &&
              ` ` &&
              iv_commit &&
              ` ` &&
              io_repo->get_branch_name( ) &&
              get_null( ) &&
              ` ` &&
              'report-status agent=' && gv_agent &&
              gc_newline.                                   "#EC NOTEXT
    lv_cmd_pkt = pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = lcl_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    li_client->request->set_data( lv_xstring ).

    li_client->send( ).
    li_client->receive( ).
    check_http_200( li_client ).

    lv_xstring = li_client->response->get_data( ).
    li_client->close( ).

    lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
    IF NOT lv_string CP '*unpack ok*'.
      _raise 'unpack not ok'.
    ELSEIF lv_string CP '*pre-receive hook declined*'.
      _raise 'pre-receive hook declined'.
    ENDIF.

  ENDMETHOD.                    "receive_pack

  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_string  TYPE string,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2,
          lo_obj     TYPE REF TO cl_abap_conv_in_ce,
          lv_len     TYPE int4.

* hmm, can this be done easier?

    lv_xstring = iv_data(4).

    lo_obj = cl_abap_conv_in_ce=>create(
        input    = lv_xstring
        encoding = 'UTF-8' ).
    lv_len = xstrlen( lv_xstring ).

    lo_obj->read( EXPORTING n    = lv_len
                  IMPORTING data = lv_string ).

    lv_char4 = lv_string.
    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.                    "length_utf8_hex

  METHOD parse.

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = length_utf8_hex( cv_data ).

      lv_contents = cv_data(lv_len).
      IF lv_len = 0.
        cv_data = cv_data+4.
        CONTINUE.
      ELSE.
        cv_data = cv_data+lv_len.
      ENDIF.

      lv_contents = lv_contents+4.

      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = lc_band1.
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.                    "parse

  METHOD upload_pack.

    DATA: li_client  TYPE REF TO if_http_client,
          lv_buffer  TYPE string,
          lv_xstring TYPE xstring,
          lv_line    TYPE string,
          lv_pkt1    TYPE string,
          lv_pkt2    TYPE string.


    find_branch(
      EXPORTING
        io_repo    = io_repo
        iv_service = c_service-upload
      IMPORTING
        ei_client  = li_client
        ev_branch  = ev_branch ).

    set_headers(
        io_repo    = io_repo
        iv_service = c_service-upload
        ii_client  = li_client ).

    lv_line = 'want' &&
              ` ` &&
              ev_branch &&
              ` ` &&
              'side-band-64k no-progress agent=' && gv_agent
              && gc_newline.                                "#EC NOTEXT
    lv_pkt1 = pkt_string( lv_line ).

    lv_pkt2 = pkt_string( 'deepen 1' && gc_newline ).       "#EC NOTEXT

    lv_buffer = lv_pkt1
             && lv_pkt2
             && '0000'
             && '0009done' && gc_newline.

* do not use set_cdata as it modifies the Content-Type header field
    li_client->request->set_data( lcl_convert=>string_to_xstring_utf8( lv_buffer ) ).
    li_client->send( ).
    li_client->receive( ).
    check_http_200( li_client ).
    lv_xstring = li_client->response->get_data( ).
    li_client->close( ).

    parse( IMPORTING ev_pack = ev_pack
           CHANGING cv_data = lv_xstring ).

  ENDMETHOD.                    "upload_pack

  METHOD pkt_string.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.


    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
      _raise 'PKT, todo'.
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = rv_pkt && '00' && lv_x && iv_string.

  ENDMETHOD.                    "pkt

ENDCLASS.                    "lcl_transport IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_zip DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS import
      IMPORTING iv_key TYPE lcl_repo=>ty_key
      RAISING   lcx_exception.

    CLASS-METHODS export_key
      IMPORTING iv_key TYPE lcl_repo=>ty_key
                iv_zip TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

    CLASS-METHODS export
      IMPORTING iv_package TYPE devclass
                iv_zip     TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS show_progress
      IMPORTING iv_current  TYPE i
                iv_total    TYPE i
                iv_obj_name TYPE tadir-obj_name.

    CLASS-METHODS file_upload
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS unzip_file
      IMPORTING iv_xstr         TYPE xstring
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS filename
      IMPORTING iv_str             TYPE string
      RETURNING VALUE(rv_filename) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS file_download
      IMPORTING iv_package TYPE devclass
                iv_xstr    TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS files_commit
      IMPORTING it_files TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE ty_files_tt
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS get_message
      RETURNING VALUE(rv_message) TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_zip DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zip IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip IMPLEMENTATION.

  METHOD show_progress.

    DATA: lv_pct TYPE i,
          lv_f   TYPE f.


    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_obj_name.

  ENDMETHOD.                    "show_progress

  METHOD get_message.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Commit message'.                "#EC NOTEXT
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Enter commit message'            "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      _raise 'cancelled'.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rv_message = <ls_field>-value.

  ENDMETHOD.                    "get_message

  METHOD file_download.

    DATA: lt_rawdata  TYPE solix_tab,
          lv_action   TYPE i,
          lv_filename TYPE string,
          lv_default  TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.


    CONCATENATE iv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = 'Export ZIP'
        default_extension    = 'zip'
        default_file_name    = lv_default
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_fullpath
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).                         "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from file_save_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      _raise 'cancelled'.
    ENDIF.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstr ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_xstr )
        filename                  = lv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      _raise 'error from gui_download'.
    ENDIF.

  ENDMETHOD.                    "file_download

  METHOD encode_files.

    DATA: lo_zip      TYPE REF TO cl_abap_zip,
          lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    CREATE OBJECT lo_zip.

    LOOP AT it_files ASSIGNING <ls_file>.
      CONCATENATE <ls_file>-path <ls_file>-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-data ).
    ENDLOOP.

    rv_xstr = lo_zip->save( ).

  ENDMETHOD.                    "encode_files

  METHOD filename.

    DATA: lv_path TYPE string.                              "#EC NEEDED


    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES lv_path rv_filename.
      IF sy-subrc <> 0.
        _raise 'Malformed path'.
      ENDIF.
    ELSE.
      rv_filename = iv_str.
    ENDIF.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

  METHOD file_upload.

    DATA: lt_data       TYPE TABLE OF x255,
          lt_file_table TYPE filetable,
          ls_file_table LIKE LINE OF lt_file_table,
          lv_action     TYPE i,
          lv_string     TYPE string,
          lv_rc         TYPE i,
          lv_length     TYPE i.


    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Import ZIP'
        default_extension       = 'ZIP'
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).                      "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from file_open_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      _raise 'cancelled'.
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    lv_string = ls_file_table-filename.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_string
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      _raise 'error from gui_upload'.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.                    "file_upload

  METHOD unzip_file.

    DATA: lo_zip    TYPE REF TO cl_abap_zip,
          lv_xstr   TYPE xstring,
          lt_splice TYPE cl_abap_zip=>t_splice_entries.

    FIELD-SYMBOLS: <ls_splice> LIKE LINE OF lt_splice,
                   <ls_file>   LIKE LINE OF rt_files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    IF sy-subrc <> 0.
      _raise 'error from zip'.
    ENDIF.

    lt_splice = cl_abap_zip=>splice( iv_xstr ).

    LOOP AT lt_splice ASSIGNING <ls_splice>.
      lo_zip->get(
        EXPORTING
          name                    = <ls_splice>-name
        IMPORTING
          content                 = lv_xstr
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        _raise 'error from zip get'.
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      <ls_file>-path     = '/'.
      <ls_file>-filename = filename( <ls_splice>-name ).
      <ls_file>-data     = lv_xstr.

    ENDLOOP.

  ENDMETHOD.                    "decode_files

  METHOD export_key.

    DATA: lo_repo  TYPE REF TO lcl_repo.


    lo_repo = lcl_repo_srv=>get( iv_key ).

    export( iv_package = lo_repo->get_package( )
            iv_zip = iv_zip ).

  ENDMETHOD.

  METHOD import.

    DATA: lt_files TYPE ty_files_tt,
          lo_repo  TYPE REF TO lcl_repo.


    lo_repo = lcl_repo_srv=>get( iv_key ).

    lt_files = unzip_file( file_upload( ) ).

    lcl_objects=>deserialize( it_files   = lt_files
                              iv_package = lo_repo->get_package( ) ).

  ENDMETHOD.                    "import

  METHOD export.

    DATA: lt_tadir TYPE lcl_tadir=>ty_tadir_tt,
          ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt,
          lt_zip   TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_file>  LIKE LINE OF lt_files,
                   <ls_tadir> LIKE LINE OF lt_tadir.


    lt_tadir = lcl_tadir=>read( iv_package ).

    IF lt_tadir IS INITIAL.
      _raise 'Package is empty'.
    ENDIF.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      show_progress( iv_current  = sy-tabix
                     iv_total    = lines( lt_tadir )
                     iv_obj_name = <ls_tadir>-obj_name ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      lt_files = lcl_objects=>serialize( ls_item ).

      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_tadir>-path.
      ENDLOOP.

      APPEND LINES OF lt_files TO lt_zip.
    ENDLOOP.

    IF iv_zip = abap_true.
      file_download( iv_package = iv_package
                     iv_xstr = encode_files( lt_zip ) ).
    ELSE.
      files_commit( lt_zip ).
    ENDIF.

  ENDMETHOD.                    "export

  METHOD files_commit.

    DATA: lv_folder   TYPE string,
          lv_filename TYPE string,
          lv_par      TYPE string,
          lv_message  TYPE string,
          lt_rawdata  TYPE solix_tab.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Select folder'
      CHANGING
        selected_folder      = lv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).                         "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from directory_browser'.
    ENDIF.

    IF lv_folder IS INITIAL.
      RETURN.
    ENDIF.

    lv_message = get_message( ).

    LOOP AT it_files ASSIGNING <ls_file>.
      lt_rawdata = cl_bcs_convert=>xstring_to_solix( <ls_file>-data ).

      CONCATENATE lv_folder <ls_file>-path <ls_file>-filename INTO lv_filename.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize            = xstrlen( <ls_file>-data )
          filename                = lv_filename
          filetype                = 'BIN'
        CHANGING
          data_tab                = lt_rawdata
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24 ).
      IF sy-subrc <> 0.
        _raise 'error from gui_download'.
      ENDIF.

    ENDLOOP.

* assumption: git command is in PATH
    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = 'git'
        default_directory      = lv_folder
        synchronous            = 'X'
        parameter              = 'add *'
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).                      "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from execute'.
    ENDIF.

* make sure to set git user.email and user.name manually
    lv_par = 'commit -m "' && lv_message && '"'.            "#EC NOTEXT
    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = 'git'
        default_directory      = lv_folder
        synchronous            = 'X'
        parameter              = lv_par
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      _raise 'error from execute'.
    ENDIF.

  ENDMETHOD.                    "files_commit

ENDCLASS.                    "lcl_zip IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_porcelain IMPLEMENTATION.

  METHOD receive_pack.

    DATA: lv_tree    TYPE xstring,
          lv_time    TYPE lcl_time=>ty_unixtime,
          lv_commit  TYPE xstring,
          lt_objects TYPE lcl_git_pack=>ty_objects_tt,
          lv_pack    TYPE xstring,
          ls_object  LIKE LINE OF lt_objects,
          ls_commit  TYPE lcl_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    lv_tree = lcl_git_pack=>encode_tree( it_nodes ).

* new commit
    lv_time = lcl_time=>get( ).
    ls_commit-tree      = lcl_hash=>sha1( iv_type = gc_type-tree iv_data = lv_tree ).
    ls_commit-parent    = iv_branch.
    CONCATENATE is_comment-username space '<' is_comment-email '>' space lv_time
      INTO ls_commit-author RESPECTING BLANKS.
    ls_commit-committer = ls_commit-author.
    ls_commit-body      = is_comment-comment.
    lv_commit = lcl_git_pack=>encode_commit( ls_commit ).

    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-commit iv_data = lv_commit ).
    ls_object-type = gc_type-commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO lt_objects.
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-tree iv_data = lv_tree ).
    ls_object-type = gc_type-tree.
    ls_object-data = lv_tree.
    APPEND ls_object TO lt_objects.
    LOOP AT it_files ASSIGNING <ls_file>.
      CLEAR ls_object.
      ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_file>-data ).
      ls_object-type = gc_type-blob.
      ls_object-data = <ls_file>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    lv_pack = lcl_git_pack=>encode( lt_objects ).

    rv_branch = lcl_hash=>sha1( iv_type = gc_type-commit iv_data = lv_commit ).
    lcl_git_transport=>receive_pack( io_repo   = io_repo
                                     iv_commit = rv_branch
                                     iv_pack   = lv_pack ).

  ENDMETHOD.                    "receive_pack

  METHOD push.

* todo, only works with root files

    DATA:
      lt_nodes TYPE lcl_git_pack=>ty_nodes_tt,
      lt_files LIKE it_files,
      lv_sha1  TYPE ty_sha1,
      lv_index TYPE i.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files,
                   <ls_node> LIKE LINE OF lt_nodes.


    lt_nodes = root_tree( it_objects = io_repo->get_objects( )
                          iv_branch  = io_repo->get_sha1_remote( ) ).

    lt_files[] = it_files[].

    LOOP AT lt_files ASSIGNING <ls_file>.
      lv_index = sy-tabix.
      READ TABLE lt_nodes ASSIGNING <ls_node> WITH KEY name = <ls_file>-filename.
      IF sy-subrc <> 0.
* new files
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = gc_chmod-file.
        <ls_node>-name = <ls_file>-filename.
      ENDIF.

      lv_sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_file>-data ).
      IF <ls_node>-sha1 <> lv_sha1.
        <ls_node>-sha1 = lv_sha1.
      ELSE.
        DELETE lt_files INDEX lv_index.
      ENDIF.
    ENDLOOP.

    IF lt_files[] IS INITIAL.
      _raise 'no files'.
    ENDIF.

    rv_branch = receive_pack( is_comment = is_comment
                              io_repo    = io_repo
                              it_nodes   = lt_nodes
                              it_files   = lt_files
                              iv_branch  = io_repo->get_sha1_remote( ) ).

  ENDMETHOD.                    "push

  METHOD root_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE lcl_git_pack=>ty_commit.


    READ TABLE it_objects INTO ls_object WITH KEY sha1 = iv_branch type = gc_type-commit.
    IF sy-subrc <> 0.
      _raise 'commit not found'.
    ENDIF.
    ls_commit = lcl_git_pack=>decode_commit( ls_object-data ).

    READ TABLE it_objects INTO ls_object
      WITH KEY sha1 = ls_commit-tree type = gc_type-tree.
    IF sy-subrc <> 0.
      _raise 'tree not found'.
    ENDIF.
    rt_nodes = lcl_git_pack=>decode_tree( ls_object-data ).

  ENDMETHOD.                    "root_tree

  METHOD pull.

    DATA: ls_object LIKE LINE OF et_objects,
          ls_commit TYPE lcl_git_pack=>ty_commit,
          lv_pack   TYPE xstring.


    CLEAR et_files.
    CLEAR et_objects.
    CLEAR ev_branch.

    lcl_git_transport=>upload_pack( EXPORTING io_repo = io_repo
                                    IMPORTING ev_pack = lv_pack
                                              ev_branch = ev_branch ).

    IF lv_pack IS INITIAL.
      _raise 'empty pack'.
    ENDIF.

    et_objects = lcl_git_pack=>decode( lv_pack ).

    lcl_git_pack=>decode_deltas( CHANGING ct_objects = et_objects ).

    READ TABLE et_objects INTO ls_object WITH KEY sha1 = ev_branch type = gc_type-commit.
    IF sy-subrc <> 0.
      _raise 'Commit/branch not found'.
    ENDIF.
    ls_commit = lcl_git_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = et_objects
                    iv_sha1 = ls_commit-tree
                    iv_path = '/'
          CHANGING ct_files = et_files ).

  ENDMETHOD.                    "pull

  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree> WITH KEY sha1 = iv_sha1 type = gc_type-tree.
    IF sy-subrc <> 0.
      _raise 'Walk, tree not found'.
    ENDIF.

    lt_nodes = lcl_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = gc_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY sha1 = <ls_node>-sha1 type = gc_type-blob.
        IF sy-subrc <> 0.
          _raise 'Walk, blob not found'.
        ENDIF.

        CLEAR ls_file.
        ls_file-path     = iv_path.
        ls_file-filename = <ls_node>-name.
        ls_file-data     = <ls_blob>-data.
        APPEND ls_file TO ct_files.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = gc_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.
      walk( EXPORTING it_objects = it_objects
                      iv_sha1 = <ls_node>-sha1
                      iv_path = lv_path
            CHANGING ct_files = ct_files ).
    ENDLOOP.

  ENDMETHOD.                    "walk

ENDCLASS.                    "lcl_porcelain IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD zipexport.

    DATA: lv_returncode TYPE c,
          lv_package    TYPE devclass,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TDEVC'.
    <ls_field>-fieldname = 'DEVCLASS'.
    <ls_field>-fieldtext = 'Package'.                       "#EC NOTEXT

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Export package to ZIP'           "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.

    lcl_zip=>export( lv_package ).

  ENDMETHOD.                    "zipexport

  METHOD render_header.

    rv_html = '<html>'
          && gc_newline &&
          '<head>'
          && gc_newline &&
          '<title>abapGit</title>'
          && gc_newline &&
          render_css( )
          && gc_newline &&
          '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
          && gc_newline &&
          '<script>'
          && gc_newline &&
          'function goBack() {'
          && gc_newline &&
          '  window.history.back();'
          && gc_newline &&
          '}'
          && gc_newline &&
          '</script>'
          && gc_newline &&
          '</head>'
          && gc_newline &&
          '<body style="background: rgba(222, 241, 242, 1);">'
          && gc_newline.                                    "#EC NOTEXT

  ENDMETHOD.                    "render_head

  METHOD diff.

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_tt,
          ls_item   TYPE ty_item,
          lo_repo   TYPE REF TO lcl_repo_online,
          lo_diff   TYPE REF TO lcl_diff.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local>  LIKE LINE OF lt_local.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).

    lt_remote = lo_repo->get_files( ).

    ls_item-obj_type = is_result-obj_type.
    ls_item-obj_name = is_result-obj_name.

    lt_local = lcl_objects=>serialize( ls_item ).

    READ TABLE lt_remote ASSIGNING <ls_remote>
      WITH KEY filename = is_result-filename.
    IF sy-subrc <> 0.
      _raise 'not found remotely'.
    ENDIF.
    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY filename = is_result-filename.
    IF sy-subrc <> 0.
      _raise 'not found locally'.
    ENDIF.

    CREATE OBJECT lo_diff
      EXPORTING
        iv_local  = <ls_local>-data
        iv_remote = <ls_remote>-data.

    render_diff( is_result = is_result
                 io_diff   = lo_diff ).

  ENDMETHOD.                    "diff

  METHOD render_diff.

    DATA: lv_html    TYPE string,
          lv_local   TYPE string,
          lv_remote  TYPE string,
          lv_clocal  TYPE string,
          lv_cremote TYPE string,
          ls_count   TYPE lcl_diff=>ty_count,
          lt_diffs   TYPE lcl_diff=>ty_diffs_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF lt_diffs.


    lv_html = render_header( ) &&
              '<h1>diff</h1>&nbsp;<a href="javascript:goBack()">Back</a>' &&
              '<hr><h3>' &&
              is_result-obj_type && '&nbsp;' &&
              is_result-obj_name && '&nbsp;' &&
              is_result-filename && '</h3><br><br>'.

    ls_count = io_diff->stats( ).
    lv_html = lv_html &&
              '<table border="1">' && gc_newline &&
              '<tr>'               && gc_newline &&
              '<td>Insert</td>'    && gc_newline &&
              '<td>'               &&
              ls_count-insert      &&
              '</td>'              && gc_newline &&
              '</tr>'              && gc_newline &&
              '<tr>'               && gc_newline &&
              '<td>Delete</td>'    && gc_newline &&
              '<td>'               &&
              ls_count-delete      &&
              '</td>'              && gc_newline &&
              '</tr>'              && gc_newline &&
              '<tr>'               && gc_newline &&
              '<td>Update</td>'    && gc_newline &&
              '<td>'               &&
              ls_count-update      &&
              '</td>'              && gc_newline &&
              '</tr>'              && gc_newline &&
              '</table><br>'       && gc_newline.

    lv_html = lv_html &&
              '<table border="0">'       && gc_newline &&
              '<tr>'                     && gc_newline &&
              '<td><h2>Local</h2></td>'  && gc_newline &&
              '<td></td>'                && gc_newline &&
              '<td><h2>Remote</h2></td>' && gc_newline &&
              '</tr>'.

    lt_diffs = io_diff->get( ).
    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      lv_local = escape( val = <ls_diff>-local format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-remote format = cl_abap_format=>e_html_attr ).

      CASE <ls_diff>-result.
        WHEN lcl_diff=>c_diff-insert.
          lv_clocal = ' style="background:lightgreen;"'.    "#EC NOTEXT
          lv_cremote = ''.
        WHEN lcl_diff=>c_diff-delete.
          lv_clocal = ''.
          lv_cremote = ' style="background:lightpink;"'.    "#EC NOTEXT
        WHEN lcl_diff=>c_diff-update.
          lv_clocal = ' style="background:lightgreen;"'.    "#EC NOTEXT
          lv_cremote = ' style="background:lightpink;"'.    "#EC NOTEXT
        WHEN OTHERS.
          lv_clocal = ''.
          lv_cremote = ''.
      ENDCASE.

      lv_html = lv_html &&
        '<tr>' && gc_newline &&
        '<td' && lv_clocal && '><pre>' && lv_local && '</pre></td>' &&
        gc_newline &&
        '<td>&nbsp;' && <ls_diff>-result && '&nbsp;</td>' &&
        gc_newline &&
        '<td' && lv_cremote && '><pre>' && lv_remote && '</pre></td>' &&
        gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.
    lv_html = lv_html && '</table>' && gc_newline.

    lv_html = lv_html && render_footer( ).
    view( lv_html ).

  ENDMETHOD.                    "render_diff

  METHOD popup_comment.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'BAPIRTEXT'.
    <ls_field>-fieldname = 'TEXT'.
    <ls_field>-fieldtext = 'Username'.                      "#EC NOTEXT
    <ls_field>-field_obl = abap_true.
    <ls_field>-value = lcl_user=>get_username( ).

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'BAPIRTEXT1'.
    <ls_field>-fieldname = 'TEXT'.
    <ls_field>-fieldtext = 'E-Mail'.                        "#EC NOTEXT
    <ls_field>-field_obl = abap_true.
    <ls_field>-value = lcl_user=>get_email( ).

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Comment'.                       "#EC NOTEXT
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Enter Git username and email'    "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      CLEAR rs_comment.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_comment-username = <ls_field>-value.
    lcl_user=>set_username( rs_comment-username ).

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_comment-email = <ls_field>-value.
    lcl_user=>set_email( rs_comment-email ).

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_comment-comment = <ls_field>-value.

  ENDMETHOD.                    "popup_commit

  METHOD pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).

    lo_repo->refresh( ).

    lo_repo->deserialize( ).

    view( render( ) ).

  ENDMETHOD.                    "pull

  METHOD commit.

    DATA: lt_results TYPE lcl_file_status=>ty_results_tt,
          lt_push    TYPE ty_files_tt,
          ls_item    TYPE ty_item,
          ls_comment TYPE ty_comment,
          lo_repo    TYPE REF TO lcl_repo_online,
          lt_files   TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).
    lt_results = lo_repo->status( ).

    LOOP AT lt_results ASSIGNING <ls_result>
        WHERE match = abap_false
        AND filename <> ''.
      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      lt_files = lcl_objects=>serialize( ls_item ).
      APPEND LINES OF lt_files TO lt_push.
    ENDLOOP.

    IF lt_push[] IS INITIAL.
      _raise 'no changes'.
    ENDIF.

    ls_comment = popup_comment( ).
    IF ls_comment IS INITIAL.
      RETURN.
    ENDIF.

    lo_repo->push( is_comment = ls_comment
                   it_files   = lt_push ).

    view( render( ) ).

  ENDMETHOD.                    "commit

  METHOD file_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields,
                   <lg_any>   TYPE any.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    LOOP AT lt_fields ASSIGNING <ls_field>.
      ASSIGN COMPONENT <ls_field>-name OF STRUCTURE es_file TO <lg_any>.
      IF sy-subrc <> 0.
        CONTINUE. " more structures might be encoded in same string
      ENDIF.

      <lg_any> = <ls_field>-value.
    ENDLOOP.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'KEY'.
    IF sy-subrc = 0.
      ev_key = <ls_field>-value.
    ELSE.
      CLEAR ev_key.
    ENDIF.

  ENDMETHOD.                    "struct_decode

  METHOD file_encode.

    DATA: lt_fields    TYPE tihttpnvp,
          lo_descr_ref TYPE REF TO cl_abap_structdescr,
          ls_field     LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lo_descr_ref->components,
                   <lg_any>  TYPE any.


    lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( is_file ).

    LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.

      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE is_file TO <lg_any>.
      ASSERT sy-subrc = 0.

      ls_field-name = <ls_comp>-name.
      ls_field-value = <lg_any>.
      APPEND ls_field TO lt_fields.
    ENDLOOP.

    ls_field-name = 'KEY'.
    ls_field-value = iv_key.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "encode_struct

  METHOD on_event.

    DATA: lx_exception TYPE REF TO lcx_exception,
          ls_result    TYPE lcl_file_status=>ty_result,
          lv_url       TYPE string,
          lv_key       TYPE lcl_repo=>ty_key,
          ls_item      TYPE ty_item.


    TRY.
        CASE action.
          WHEN 'install'.
            lv_url = getdata.
            install( lv_url ).
          WHEN 'explore'.
            go_html_viewer->show_url( 'http://larshp.github.io/abapGit/explore.html' ).
          WHEN 'abapgithome'.
            cl_gui_frontend_services=>execute(
                 document = 'https://github.com/larshp/abapGit' ).
          WHEN 'add'.
            file_decode( EXPORTING iv_string = getdata
                         IMPORTING ev_key = lv_key
                                   es_file = ls_result ).
            CLEAR ls_item.
            MOVE-CORRESPONDING ls_result TO ls_item.
            add( is_item = ls_item
                 iv_key  = lv_key ).
          WHEN 'uninstall'.
            lv_key = getdata.
            uninstall( lv_key ).
          WHEN 'remove'.
            lv_key = getdata.
            remove( lv_key ).
          WHEN 'refresh'.
            lcl_repo_srv=>refresh( ).
            view( render( ) ).
          WHEN 'commit'.
            lv_key = getdata.
            commit( lv_key ).
          WHEN 'diff'.
            file_decode( EXPORTING iv_string = getdata
                         IMPORTING ev_key    = lv_key
                                   es_file   = ls_result ).
            diff( is_result = ls_result
                  iv_key    = lv_key ).
          WHEN 'jump'.
            file_decode( EXPORTING iv_string = getdata
                         IMPORTING ev_key = lv_key
                                   es_file = ls_result ).
            CLEAR ls_item.
            MOVE-CORRESPONDING ls_result TO ls_item.
            lcl_objects=>jump( ls_item ).
          WHEN 'pull'.
            lv_key = getdata.
            pull( lv_key ).
          WHEN 'newoffline'.
            newoffline( ).
          WHEN 'zipimport'.
            lv_key = getdata.
            lcl_zip=>import( lv_key ).
            view( render( ) ).
          WHEN 'zipexport'.
            lv_key = getdata.
            lcl_zip=>export_key( lv_key ).
            view( render( ) ).
          WHEN 'files_commit'.
            lv_key = getdata.
            lcl_zip=>export_key( iv_key = lv_key
                                 iv_zip = abap_false ).
            view( render( ) ).
          WHEN 'zipexport_gui'.
            zipexport( ).
          WHEN OTHERS.
            _raise 'Unknown action'.
        ENDCASE.
      CATCH lcx_exception INTO lx_exception.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD uninstall.

    DATA: lt_tadir    TYPE lcl_tadir=>ty_tadir_tt,
          lv_count    TYPE c LENGTH 3,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_repo_srv=>get( iv_key ).
    lv_package = lo_repo->get_package( ).

    lt_tadir = lcl_tadir=>read( lv_package ).

    IF lines( lt_tadir ) > 0.
      lv_count = lines( lt_tadir ).

      CONCATENATE 'This will delete all objects in package' lv_package
        INTO lv_question
        SEPARATED BY space.                                 "#EC NOTEXT

      CONCATENATE lv_question '(' lv_count 'objects)'
        INTO lv_question
        SEPARATED BY space.                                 "#EC NOTEXT

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Uninstall'
          text_question         = lv_question
          text_button_1         = 'Delete'
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.                        "#EC NOTEXT
      IF sy-subrc <> 0.
        _raise 'error from POPUP_TO_CONFIRM'.
      ENDIF.

      IF lv_answer = '2'.
        RETURN.
      ENDIF.

      lcl_objects=>delete( lt_tadir ).

    ENDIF.

    lcl_repo_srv=>delete( lo_repo ).

    view( render( ) ).

  ENDMETHOD.                    "uninstall

  METHOD remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_repo_srv=>get( iv_key ).
    lv_package = lo_repo->get_package( ).

    CONCATENATE 'This will remove the repository reference to the package'
      lv_package
      INTO lv_question
      SEPARATED BY space.                                   "#EC NOTEXT

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Remove'
        text_question         = lv_question
        text_button_1         = 'Remove'
        icon_button_1         = 'ICON_WF_UNLINK'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                          "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from POPUP_TO_CONFIRM'.
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_repo_srv=>delete( lo_repo ).

    view( render( ) ).

  ENDMETHOD.                    "remove

  METHOD add.

    DATA: lt_files    TYPE ty_files_tt,
          ls_comment  TYPE ty_comment,
          lo_repo     TYPE REF TO lcl_repo_online,
          lv_package  TYPE devclass,
          lv_obj_name TYPE tadir-obj_name.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).
    lv_package = lo_repo->get_package( ).

    IF is_item-obj_type = 'SICF'.
      CONCATENATE is_item-obj_name '%' INTO lv_obj_name.
    ELSE.
      lv_obj_name = is_item-obj_name.
    ENDIF.

    SELECT SINGLE obj_name FROM tadir
      INTO lv_obj_name
      WHERE pgmid = 'R3TR'
      AND object = is_item-obj_type
      AND obj_name LIKE lv_obj_name
      AND devclass = lv_package.                        "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'Object not found or in wrong package'.
    ENDIF.

    lt_files = lcl_objects=>serialize( is_item ).

    ls_comment = popup_comment( ).
    IF ls_comment IS INITIAL.
      RETURN.
    ENDIF.

    lo_repo->push( is_comment = ls_comment
                   it_files   = lt_files ).

    view( render( ) ).

  ENDMETHOD.                    "add

  METHOD newoffline.

    DATA: lv_returncode TYPE c,
          lv_url        TYPE string,
          lv_package    TYPE devclass,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Name'.                          "#EC NOTEXT

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TDEVC'.
    <ls_field>-fieldname = 'DEVCLASS'.
    <ls_field>-fieldtext = 'Package'.                       "#EC NOTEXT

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'New Offline Project'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_url = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.

    lcl_repo_srv=>new_offline(
      iv_url     = lv_url
      iv_package = lv_package ).

    view( render( ) ).

  ENDMETHOD.                    "newoffline

  METHOD install.

    DATA: lv_returncode  TYPE c,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lv_branch_name TYPE string,
          lv_icon_ok     TYPE icon-name,
          lv_icon_br     TYPE icon-name,
          lo_repo        TYPE REF TO lcl_repo_online,
          lt_fields      TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Git Clone Url'.                 "#EC NOTEXT
    <ls_field>-value     = iv_url.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TDEVC'.
    <ls_field>-fieldname = 'DEVCLASS'.
    <ls_field>-fieldtext = 'Target Package'.                "#EC NOTEXT

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TEXTL'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Branch'.                        "#EC NOTEXT
    <ls_field>-value     = 'refs/heads/master'.             "#EC NOTEXT
    <ls_field>-field_attr = '05'.

    lv_icon_ok = icon_okay.
    lv_icon_br = icon_workflow_fork.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
      EXPORTING
        popup_title       = 'Clone'
        programname       = sy-repid
        formname          = 'BRANCH_POPUP'
        ok_pushbuttontext = 'OK'
        icon_ok_push      = lv_icon_ok
        first_pushbutton  = 'Select branch'
        icon_button_1     = lv_icon_br
      IMPORTING
        returncode        = lv_returncode
      TABLES
        fields            = lt_fields
      EXCEPTIONS
        error_in_fields   = 1
        OTHERS            = 2.                              "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_url = <ls_field>-value.
    lcl_url=>name( lv_url ).         " validate

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_branch_name = <ls_field>-value.

    lo_repo = lcl_repo_srv=>new_online(
      iv_url         = lv_url
      iv_branch_name = lv_branch_name
      iv_package     = lv_package ).
    lo_repo->status( ). " check for errors
    lo_repo->deserialize( ).

    view( render( ) ).

  ENDMETHOD.                    "install

  METHOD render_css.

    rv_html = '<style type="text/css">' && gc_newline &&
          'body {'                      && gc_newline &&    "#EC NOTEXT
          '  font-family: Arial,Helvetica,sans-serif;' && gc_newline && "#EC NOTEXT
          '  background: #DEF1F2;'      && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a:link {'                    && gc_newline &&    "#EC NOTEXT
          '  color: blue;'              && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a:visited {'                 && gc_newline &&    "#EC NOTEXT
          '  color: blue;'              && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.grey:link {'               && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.grey:visited {'            && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.plain:link {'              && gc_newline &&    "#EC NOTEXT
          '  color: black;'             && gc_newline &&    "#EC NOTEXT
          '  text-decoration: none;'    && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.plain:visited {'           && gc_newline &&    "#EC NOTEXT
          '  color: black;'             && gc_newline &&    "#EC NOTEXT
          '  text-decoration: none;'    && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.white:link {'              && gc_newline &&    "#EC NOTEXT
          '  color: white;'             && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.white:visited {'           && gc_newline &&    "#EC NOTEXT
          '  color: white;'             && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h1 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h2 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h3 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-weight:normal;'       && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'pre {'                       && gc_newline &&
          '  display: inline;'          && gc_newline &&
          '}'                           && gc_newline &&
          'table, th, td {'             && gc_newline &&
          '  border: 1px solid black;'  && gc_newline &&
          '  border-collapse: collapse;' && gc_newline &&
          '}'                           && gc_newline &&
          'th, td {'                    && gc_newline &&
          '  padding: 5px;'             && gc_newline &&
          '}'                           && gc_newline &&
          'th {'                        && gc_newline &&
          '  background: #e5e5e5;'      && gc_newline &&
          '}'                           && gc_newline &&
          'td {'                        && gc_newline &&
           ' background: #F8FCFC;'      && gc_newline &&
          '}'                           && gc_newline &&
          '</style>'                    && gc_newline.

  ENDMETHOD.                    "render_css

  METHOD render_menu.
    rv_html =
      |<img src="{ get_logo_src( ) }" height="50px">|
      && gc_newline &&
      '<h1>abapGit</h1>&nbsp;'                                  && gc_newline &&
      '<a href="sapevent:refresh">Refresh</a>&nbsp;'            && gc_newline &&
      '<a href="sapevent:install">Clone</a>&nbsp;'              && gc_newline &&
      '<a href="sapevent:explore">Explore</a>&nbsp;'            && gc_newline &&
      '<a href="sapevent:abapgithome">abapGit@GitHub</a>&nbsp;' && gc_newline &&
      '<a href="sapevent:newoffline">New offline project</a>&nbsp;' && gc_newline &&
      '<hr>'                                                    && gc_newline.

  ENDMETHOD.                    "render_menu

  METHOD render.

    DATA: lt_repos TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo  LIKE LINE OF lt_repos.


    lt_repos = lcl_repo_srv=>list( ).

    rv_html = render_header( ) && render_menu( ).

    LOOP AT lt_repos INTO lo_repo.
      rv_html = rv_html &&
        '<a href="#' && lo_repo->get_name( ) &&'" class="grey">' &&
        lo_repo->get_name( ) &&
        '</a>&nbsp;'.
    ENDLOOP.

    IF lt_repos[] IS INITIAL.
      rv_html = rv_html && '<br><a href="sapevent:explore">Explore</a> new projects'.
    ELSE.
      rv_html = rv_html && '<br><br><br>'.

      LOOP AT lt_repos INTO lo_repo.
        rv_html = rv_html && lo_repo->render( ).
      ENDLOOP.
    ENDIF.

    rv_html = rv_html &&
              render_footer( ).

  ENDMETHOD.                    "render

  METHOD render_footer.

    rv_html = rv_html &&
      '<br><br><hr><center><h3>abapGit Version:&nbsp;' &&
      gc_abap_version &&
      '&nbsp;<a href="sapevent:zipexport_gui" class="white">e</a>' &&
      '</h3></center>'.                                     "#EC NOTEXT

    rv_html = rv_html &&
      '<center>' &&
      |<img src="{ get_logo_src( ) }" >| &&
      '</center>'.                                          "#EC NOTEXT

    rv_html = rv_html && '</body></html>'.

  ENDMETHOD.                    "render_footer

  METHOD render_repo_offline.

    DATA: lt_tadir TYPE lcl_tadir=>ty_tadir_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    rv_html = rv_html &&
      '<a id="' && io_repo->get_name( ) && '"></a>' &&
      '<h2>' && io_repo->get_name( ) && '</h2>&nbsp;' &&
      '<h3>' && io_repo->get_package( ) && '</h3>&nbsp;&nbsp;' &&
      '<br>' &&
      '<a href="sapevent:remove?' &&
      io_repo->get_key( ) &&
      '" class="grey">' &&
      'remove' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:uninstall?' &&
      io_repo->get_key( ) &&
      '" class="grey">' &&
      'uninstall' &&
      '</a><br><br>'.                                       "#EC NOTEXT

    rv_html = rv_html && '<table border="1">' && gc_newline &&
      '<tr>'                                  && gc_newline &&
      '<th><u>Local object</u></th>'          && gc_newline &&
      '</tr>'                                 && gc_newline.

    lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
* todo, add jump link like in online rendering
      rv_html = rv_html && '<tr>' &&
        '<td>' && <ls_tadir>-object &&
        '&nbsp;' && <ls_tadir>-obj_name &&
        '</td>' && gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.

    rv_html = rv_html && '</table>' && gc_newline.

    rv_html = rv_html && '<a href="sapevent:zipimport?' &&
      io_repo->get_key( ) &&
      '">' && 'Import ZIP' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:zipexport?' &&
      io_repo->get_key( ) &&
      '">' && 'Export ZIP' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:files_commit?' &&
      io_repo->get_key( ) &&
      '">' && 'Export files and commit' &&
      '</a>&nbsp;' &&
      '<br><br><br>'.                                       "#EC NOTEXT

  ENDMETHOD.                    "render_repo_offline

  METHOD render_repo_online.

    DATA: lv_link        TYPE string,
          lv_status      TYPE string,
          lv_package     TYPE string,
          lv_object      TYPE string,
          lv_index       LIKE sy-tabix,
          lv_file_encode TYPE string,
          lv_span        TYPE i,
          lt_results     TYPE lcl_file_status=>ty_results_tt,
          ls_next        LIKE LINE OF lt_results,
          ls_item        TYPE ty_item,
          lv_supported   TYPE abap_bool.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    rv_html = rv_html &&
      '<a id="' && io_repo->get_name( ) && '"></a>' &&
      '<h2>' && io_repo->get_name( ) && '</h2>&nbsp;' &&
      '<h3>' && io_repo->get_url( ) && '</h3>&nbsp;&nbsp;' &&
      '<h3>' && io_repo->get_branch_name( ) && '</h3>&nbsp;&nbsp;' &&
      '<h3>' && io_repo->get_package( ) && '</h3>&nbsp;&nbsp;' &&
      '<br>' &&
      '<a href="sapevent:remove?' &&
      io_repo->get_key( ) &&
      '" class="grey">' &&
      'remove' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:uninstall?' &&
      io_repo->get_key( ) &&
      '" class="grey">' &&
      'uninstall' &&
      '</a><br>'.                                           "#EC NOTEXT

    rv_html = rv_html && '<br>'.

    lt_results = io_repo->status( ).
    IF io_repo->get_sha1_remote( ) <> io_repo->get_sha1_local( ).
      lv_status = 'pull'.                                   "#EC NOTEXT
    ELSE.
      READ TABLE lt_results WITH KEY match = abap_false TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_status = 'commit'.                               "#EC NOTEXT
      ELSE.
        lv_status = 'match'.                                "#EC NOTEXT
      ENDIF.
    ENDIF.

    rv_html = rv_html && '<table border="1">' && gc_newline &&
      '<tr>'                                  && gc_newline &&
      '<th><u>Local object</u></th>'          && gc_newline &&
      '<th><u>Package</u></th>'               && gc_newline &&
      '<th><u>Path</u></th>'                  && gc_newline &&
      '<th><u>Remote file</u></th>'           && gc_newline &&
      '<th></th>'                             && gc_newline &&
      '</tr>'                                 && gc_newline.

    LOOP AT lt_results ASSIGNING <ls_result>.
      lv_index = sy-tabix.
      lv_file_encode = file_encode( iv_key  = io_repo->get_key( )
                                    is_file = <ls_result> ).

      CLEAR lv_link.
      IF lv_status = 'match' AND <ls_result>-filename IS INITIAL.
        MOVE-CORRESPONDING <ls_result> TO ls_item.
        lv_supported = lcl_objects=>is_supported( ls_item ).
        IF lv_supported = abap_true.
          lv_link = '<a href="sapevent:add?' && lv_file_encode && '">add</a>'.
        ELSE.
          lv_link = |Object type <b>{ ls_item-obj_type }</b> not supported|.
        ENDIF.
      ELSEIF <ls_result>-match = abap_false.
        lv_link = '<a href="sapevent:diff?' && lv_file_encode && '">diff</a>'.
      ENDIF.

      IF lv_span = 0.
        READ TABLE lt_results INTO ls_next INDEX lv_index.
        ASSERT sy-subrc = 0.
        WHILE ls_next-obj_type = <ls_result>-obj_type
            AND ls_next-obj_name = <ls_result>-obj_name.
          lv_span  = lv_span + 1.
          lv_index = lv_index + 1.
          READ TABLE lt_results INTO ls_next INDEX lv_index.
          IF sy-subrc <> 0.
            EXIT. " current loop.
          ENDIF.
        ENDWHILE.

        IF <ls_result>-obj_type IS INITIAL.
          lv_object = '<td rowspan="' && lv_span && '" valign="top">&nbsp;</td>'.

          lv_package = lv_object.
        ELSE.
          lv_object = '<td rowspan="' &&
            lv_span &&
            '" valign="top"><a href="sapevent:jump?' &&
            lv_file_encode &&
            '" class="plain">' &&
            <ls_result>-obj_type &&
            '&nbsp;' &&
            <ls_result>-obj_name  &&
            '</a></td>'.

          lv_package = '<td rowspan="' &&
            lv_span &&
            '" valign="top">' &&
            <ls_result>-package &&
            '</td>'.
        ENDIF.
      ELSE.
        CLEAR lv_object.
        CLEAR lv_package.
      ENDIF.

      rv_html = rv_html &&
        '<tr>'                                    && gc_newline &&
        lv_object                                 && gc_newline &&
        lv_package                                && gc_newline &&
        '<td>' && <ls_result>-path && '</td>'     && gc_newline &&
        '<td>' && <ls_result>-filename && '</td>' && gc_newline &&
        '<td>' && lv_link && '</td>'              && gc_newline &&
        '</tr>'                                   && gc_newline.

      lv_span = lv_span - 1.
    ENDLOOP.
    rv_html = rv_html && '</table>' && gc_newline.

    CASE lv_status.
      WHEN 'commit'.
        rv_html = rv_html && '<a href="sapevent:commit?'
                  && io_repo->get_key( ) && '">commit</a>'.
      WHEN 'pull'.
        rv_html = rv_html && '<a href="sapevent:pull?'
                  && io_repo->get_key( ) && '">pull</a>'.
    ENDCASE.

    lv_status = lcl_sap_package=>check( it_results = lt_results
                                        iv_top     = io_repo->get_package( ) ).
    rv_html = rv_html && lv_status && '<br><br><br>'.

  ENDMETHOD.                    "render_repo

  METHOD run.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.


    CREATE OBJECT go_html_viewer
      EXPORTING
        parent = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = go_html_viewer->m_id_sapevent.
    ls_event-appl_event = 'x'.
    APPEND ls_event TO lt_events.
    go_html_viewer->set_registered_events( lt_events ).

    SET HANDLER lcl_gui=>on_event FOR go_html_viewer.

    view( render( ) ).

  ENDMETHOD.                    "init

  METHOD view.

    DATA: lt_data TYPE TABLE OF text200,
          lv_html TYPE string,
          lv_url  TYPE text200.


    lv_html = iv_html.

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) < 200.
        APPEND lv_html TO lt_data.
        CLEAR lv_html.
      ELSE.
        APPEND lv_html(200) TO lt_data.
        lv_html = lv_html+200.
      ENDIF.
    ENDWHILE.

    go_html_viewer->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_data ).

    go_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "view

  METHOD get_logo_src.
    rv_src =
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAX8AAAF/CAMAAACWmjlVAAAA' &&
      'M1BMVEX////wUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUD' &&
      'PwUDP3eUwZAAAAEHRSTlMA8DAQ0KDAQGCA4CCQUHCw+BUOAQAACQ5JREFUeF7s1VFKw0AA' &&
      'RVHTpmkbGzr7X63gn6D1K7ko521g4J5h5u2vbrsv6/jctDy285FH2/Uyja97bgCO2m0d32' &&
      'y9AThip2X8sPkKYPfdx4s9AOy783O83LzrRyz/PH7Z/A4gzD/GdAIQ5A8A5A8A5M8A5E8B' &&
      '5E8B5E8B5E8B5E8B5E8B5E8B5E8B5E8B5E8B5G8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O' &&
      '8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B9AcAAIABAGAAugEAYAAA' &&
      'GAAABgCAAQDw//sCaK/3BUD7ugBI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8w' &&
      'NI8wNI8wNI8wNI8wNI8wMI83+wcy85EsIwAEQDJAH6A3X/086yZ20hlWTZN6CewPIGKAA3' &&
      'fwH4+QvAz18Afv4C8PMXgJ+/APz8cYA5P+d5jtkLIJY/DrCM43+b/boL4Kn8MCLURy+AeP' &&
      '44wG/2WQCB/M8BtL0XQCD/cwDtLIBffgNgfRXAA/lh1M+7zfxxgDZ8AD8/BaDmVwG6DODn' &&
      'dwE2dwn7+W2AFRHAz+8DXCaAn98H6CKAn98H2BEB/Pw+wBQB/Pw+wI4AkDE/fL0XwAfw8y' &&
      '9bqP+BB+Dn9xfAhgCQMD/fFptbAEiYn827wXwAP39vwVkRANLl59OigwCQLj9nuH8XANLl' &&
      'Zw/3n3gAfn6//wcBIFt+4k96IgBky0/z+vsAfn42r78P4OdnF/vHAfz8fv8bASBbfo5w/4' &&
      'kH4Of3768FASBbfu5o/jcIANnys0T7f/EA/Pz+ATYQAPLl5wr2fyEA5MtPD69fD8DP73+A' &&
      'VgEgY35GKwAxP7wLwMzPaAUg5oe1AMz8zFYAYn64CsDMD2sBmPlZtgIQ80MvADM/BbD1P/' &&
      'buNudxFQjCqAk2Nvgj7H+19/4raTTzjoKGFFQ3K0DnkRNsJ2oGuwcAP3kVe1/CfH4/hoKf' &&
      'v9JpMQD4+euKBgOAn7/CFocPoMiPtVu5AsA/hf9LLAD45/BPh1QA8M/iX5UCgH8ef6EA4J' &&
      '/IXycA+GfylwkA/qn8VQKAfy5/kQDgn8xfIwD4Z/OXCAD+6fwVAoB/Pn+BAOCf0F8gAPgn' &&
      '9BcIAP4J/QUCgH9Cf4EA4J/QXyAA+Cf0FwgA/gn9BQKAX8B/ogDgF/CfLwD4BfznCwB+Af' &&
      '+mAOuWj/3/decrEQKAf2b/xgDp+iXpfhVCAPBr+CNAw3ptXwsAfj3/9gDLeX0nAPgl/dsD' &&
      'LGf6QgDwq/q3B1iO0DsA+IX92wO81r4BwC/t3x4gpp4BwK/uX48Bx9mCX8N/ugDg1/dHAP' &&
      '5I87PHoCC+f68Aoed/mXM14o8A/GGGCSesquPfLcDVb6xNEvLvFiCWXnM9jirk3y/A0esT' &&
      'qCj5dwxQ+sxV3Ks1/3qMdAFs9vwD/wLAN0Cw57/Rj0DYyF3t+b/o4zxxIWZ7/gGk/A+gZM' &&
      '//PcxjuPosxZ7/A1H2CWhbqj1/1OM/BLLoH8eZ5xws+i/j+FeD/mVhztNz/2TeH8v9A5HR' &&
      '/ddPkvrn//rvz58XkdHPPxf1+Zuf/48lEhn9/vdclrc5/9zq//QYbHnTFP3552Py/XsYZp' &&
      'x5tPT7E6x7kPdf2yenWn//u3WaKrpb8w9xiN/gXnixb8u/PiO8/SoR99XG/MsIb993c/+/' &&
      'wMr8w//TMJZAxj9E9qf/BnwEkPdvvwd79+JHAH3/9t/g3t34EcCUf3gxBieBnxuA71/XSJ' &&
      'uCsDVEVvNHAD4/AhjyRwA+PwIY8kcAPj8CWPKvYWd89RICsP3bb4Rz/Q4/Apjyr+uPl8C+' &&
      'fpMfAfT9sdIfC5zv+mV+BND3x/r92M47VQI/Auj7Y4XtOBesJd5bqAR+BLDij1VSzs++Pz' &&
      'mnAjQCPwII+PMX+AkB3B/8hADuD35CAPcHPyGA+4OfEMD9wU8I4P7gJwRwf/ATArg/+AkB' &&
      '3B/8hADuD35CAPcHPyGA+4OfEMD9wU8I4P7gJwRwf/ATArg/+AkB3B/8hADuD35CAPcHPy' &&
      'GA+4OfEMD9wU8I4P7glw5QchzSH/zKAUL+YVzsewh+5QDv+OMG98LnFw4Q7r9tMF5sfuEA' &&
      '69kwMZnALxogxVF3CH7hANuwOwS/cIA07A7BLxxgjbRJFXx+foDw2dCoh8GvHOAmjEpg8/' &&
      'MDtM+MOgOBXzZAOD/eYCbwywbIvGl1fH5+gBAb/A8Cv2iAq2l/gcCvGeBs8s8EfskAa9vu' &&
      'TgK/ZICHOC6Hz88PcDb6XwR+wQCFOK+Fz88P8G7eG4FfMEBu9i8Efr0Ad7N/IvDrBdib/S' &&
      '9xfgQY0z8T+PUCLCP6/8feHaQ2DEMBEK0tWZGiOu79T9tFF39RqCHwGYomJ4jfBInwkQXw' &&
      'AwFwf56fD7AD/gA/EABY/wF+KgDv30B+PgD//6uC/HwA/nmz+NcKUN/+Okn8qwUo4H3JPD' &&
      '8foL/pf3H8fAD+oQvIzwfgF6AO8vMB+AWocvx8AH4C+QD5+QD8DtwS+FcN8Czgz5/n5wMM' &&
      'cvbL8/MBdnD0wvPzAWoBVx+enw9wEKdfgt8Ag72v1wAd5jfAP+B3D5hp/AZo5RbgTNx6Db' &&
      'Cdt6//kT91HtDmHwD9KX/6POCYN/ryJ88DWi+/AOYIffnTZ8LXeATAPD/rcsNG/nzA1n4+' &&
      'QSY/cUBD/vsA8hsA4DeA/BFAfgMA/AaQPwLIbwCA3wDyRwD5DQDwGwDiN4D8EUB+AwD8Bp' &&
      'A/AshvAIDfAPJHAPkNAPAbAOA3AMBvAIDfABy/AXh+A/D8BpDf65INIP9K1yUbgOc3AM9v' &&
      'AJ7fADy/AXh+A/D8BuD5DcDzG+D4bu9eUhiGYTAIJ7ai9BFM73/aQpeFamcPheG/wTdeGl' &&
      'QBGADmNwDMbwCY3wAwvwFgfgPA/AaQ3yM+BgD5DQDzG0B+dM+QH10PmN8AML8BYH4DwPwG' &&
      'gPkNAPMbAOY3AMxvAJjfADC/AWB+A8D8BoD5DQDzGwDmNwDMbwCY3wAwvwFgfgPA/AaQH1' &&
      '3Pmn9INHn3Qv9x6jN9LX/xX12dFTuzOMvsFqwdX98TY/j2166NIz8RIq/b3z79N8fTabrp' &&
      'KtbiAAAAAElFTkSuQmCC'.

  ENDMETHOD.                    "base64_logo
ENDCLASS.                    "lcl_gui IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception,
        lv_ind       TYPE t000-ccnocliind.


  IF sy-langu <> gc_english.
    WRITE: / 'Use English as logon language'.               "#EC NOTEXT
    RETURN.
  ENDIF.

  SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
    WHERE mandt = sy-mandt.
  IF sy-subrc = 0
      AND lv_ind <> ' '
      AND lv_ind <> '1'. " check changes allowed
    WRITE: / 'Wrong client, changes to repository objects not allowed'. "#EC NOTEXT
    RETURN.
  ENDIF.

  TRY.
      lcl_gui=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

  CALL SELECTION-SCREEN 1001. " trigger screen

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  branch_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TT_FIELDS      text
*      -->PV_CODE        text
*      -->CS_ERROR       text
*      -->CV_SHOW_POPUP  text
*      -->RAISING        text
*      -->LCX_EXCEPTION  text
*      -->##CALLED       text
*      -->##NEEDED       text
*----------------------------------------------------------------------*
FORM branch_popup TABLES   tt_fields STRUCTURE sval
                  USING    pv_code
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING lcx_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lv_url       TYPE string,
        lv_answer    TYPE c,
        lx_error     TYPE REF TO lcx_exception,
        lt_selection TYPE TABLE OF spopli,
        lt_branches  TYPE lcl_git_transport=>ty_branch_list_tt.

  FIELD-SYMBOLS: <ls_fbranch> LIKE LINE OF tt_fields,
                 <ls_branch>  LIKE LINE OF lt_branches,
                 <ls_sel>     LIKE LINE OF lt_selection,
                 <ls_furl>    LIKE LINE OF tt_fields.


  CLEAR cs_error.

  IF pv_code = 'COD1'.
    cv_show_popup = abap_true.

    READ TABLE tt_fields ASSIGNING <ls_furl> WITH KEY tabname = 'ABAPTXT255'.
    IF sy-subrc <> 0 OR <ls_furl>-value IS INITIAL.
      MESSAGE 'Fill URL' TYPE 'S' DISPLAY LIKE 'E'.         "#EC NOTEXT
      RETURN.
    ENDIF.
    lv_url = <ls_furl>-value.

    TRY.
        lt_branches = lcl_git_transport=>branches( lv_url ).
      CATCH lcx_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    LOOP AT lt_branches ASSIGNING <ls_branch>.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = <ls_branch>-name.
    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        textline1          = 'Select branch'
        titel              = 'Select branch'
      IMPORTING
        answer             = lv_answer
      TABLES
        t_spopli           = lt_selection
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.                             "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_TO_DECIDE_LIST'.
    ENDIF.

    IF lv_answer = 'A'. " cancel
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TEXTL'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = <ls_sel>-varoption.

  ENDIF.

ENDFORM.                    "branch_popup

CLASS ltcl_convert DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS convert_int FOR TESTING RAISING lcx_exception.

ENDCLASS.

CLASS ltcl_convert IMPLEMENTATION.

  METHOD convert_int.

    DATA: lv_xstring TYPE xstring,
          lv_input   TYPE i,
          lv_result  TYPE i.


    DO 1000 TIMES.
      lv_input = sy-index.
      lv_xstring = lcl_convert=>int_to_xstring( iv_i      = lv_input
                                                iv_length = 4 ).
      lv_result = lcl_convert=>xstring_to_int( lv_xstring ).

      cl_abap_unit_assert=>assert_equals(
          exp = lv_input
          act = lv_result ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_diff DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_diff DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_local    TYPE TABLE OF string,
          mt_remote   TYPE TABLE OF string,
          mt_expected TYPE lcl_diff=>ty_diffs_tt,
          ms_expected LIKE LINE OF mt_expected.

    METHODS: setup.
    METHODS: test.

    METHODS:
      diff01 FOR TESTING,
      diff02 FOR TESTING,
      diff03 FOR TESTING,
      diff04 FOR TESTING,
      diff05 FOR TESTING,
      diff06 FOR TESTING.

ENDCLASS.                    "ltcl_diff DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_diff IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_diff IMPLEMENTATION.

  DEFINE _local.
    append &1 to mt_local.
  END-OF-DEFINITION.

  DEFINE _remote.
    append &1 to mt_remote.
  END-OF-DEFINITION.

  DEFINE _expected.
    clear ms_expected.
    ms_expected-local = &1.
    ms_expected-result = &2.
    ms_expected-remote = &3.
    append ms_expected to mt_expected.
  END-OF-DEFINITION.

  METHOD setup.
    CLEAR mt_local.
    CLEAR mt_remote.
    CLEAR mt_expected.
  ENDMETHOD.                    "setup

  METHOD test.

    DATA: lv_local   TYPE string,
          lv_xlocal  TYPE xstring,
          lv_remote  TYPE string,
          lv_xremote TYPE xstring,
          lo_diff    TYPE REF TO lcl_diff,
          lt_diff    TYPE lcl_diff=>ty_diffs_tt.


    CONCATENATE LINES OF mt_local  INTO lv_local SEPARATED BY gc_newline.
    CONCATENATE LINES OF mt_remote INTO lv_remote SEPARATED BY gc_newline.

    lv_xlocal  = lcl_convert=>string_to_xstring_utf8( lv_local ).
    lv_xremote = lcl_convert=>string_to_xstring_utf8( lv_remote ).

    CREATE OBJECT lo_diff
      EXPORTING
        iv_local  = lv_xlocal
        iv_remote = lv_xremote.

    lt_diff = lo_diff->get( ).

    cl_abap_unit_assert=>assert_equals( act = lt_diff
                                        exp = mt_expected ).


  ENDMETHOD.                    "test

  METHOD diff01.

* insert
    _local '1'.
    _expected '1' lcl_diff=>c_diff-insert ''.
    test( ).

  ENDMETHOD.                    "diff01

  METHOD diff02.

* identical
    _local '1'.
    _remote '1'.
    _expected '1' '' '1'.
    test( ).

  ENDMETHOD.                    "diff02

  METHOD diff03.

* delete
    _remote '1'.
    _expected '' lcl_diff=>c_diff-delete '1'.
    test( ).

  ENDMETHOD.                    "diff03

  METHOD diff04.

* update
    _local '1+'.
    _remote '1'.
    _expected '1+' lcl_diff=>c_diff-update '1'.
    test( ).

  ENDMETHOD.                    "diff04

  METHOD diff05.

* identical
    _local '1'.
    _local '2'.
    _remote '1'.
    _remote '2'.
    _expected '1' '' '1'.
    _expected '2' '' '2'.
    test( ).

  ENDMETHOD.                    "diff05

  METHOD diff06.

    _local '1'.
    _local '2'.
    _local 'inserted'.
    _local '3'.
    _local '4 update'.

    _remote '1'.
    _remote '2'.
    _remote '3'.
    _remote '4'.

    _expected '1' '' '1'.
    _expected '2' '' '2'.
    _expected 'inserted' lcl_diff=>c_diff-insert ''.
    _expected '3' '' '3'.
    _expected '4 update' lcl_diff=>c_diff-update '4'.

    test( ).

  ENDMETHOD.                    "diff06

ENDCLASS.                    "ltcl_diff IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_git_pack DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      tree FOR TESTING
        RAISING lcx_exception,
      commit FOR TESTING
        RAISING lcx_exception,
      pack_short FOR TESTING
        RAISING lcx_exception,
      pack_long FOR TESTING
        RAISING lcx_exception,
      pack_multiple FOR TESTING
        RAISING lcx_exception.

    METHODS:
      object_blob
        IMPORTING iv_data          TYPE xstring
        RETURNING VALUE(rs_object) TYPE lcl_git_pack=>ty_object
        RAISING   lcx_exception.

ENDCLASS.                    "test DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_url DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_url DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS repo_url FOR TESTING RAISING lcx_exception.
    METHODS repo_error FOR TESTING.

ENDCLASS.                    "ltcl_url DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS serialize_tabl FOR TESTING RAISING lcx_exception.
    METHODS serialize_enqu FOR TESTING RAISING lcx_exception.
    METHODS serialize_shlp FOR TESTING RAISING lcx_exception.
    METHODS serialize_view FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_serialize DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_xml DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS xml FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_xml IMPLEMENTATION.

  METHOD xml.

    DATA: lo_xml           TYPE REF TO lcl_xml,
          lv_xml           TYPE string,
          ls_component_in  TYPE wdy_component_metadata,
          ls_component_out TYPE wdy_component_metadata.


    ls_component_in-comp_metadata-definition-component_name = 'FOOBAR'.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_component_in ).
    lv_xml = lo_xml->xml_render( ).

    CREATE OBJECT lo_xml
      EXPORTING
        iv_xml = lv_xml.
    lo_xml->structure_read( CHANGING cg_structure = ls_component_out ).

    cl_abap_unit_assert=>assert_equals( act = ls_component_out
                                        exp = ls_component_in ).

  ENDMETHOD.                    "xml

ENDCLASS.                    "ltcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "serialize_table

ENDCLASS.                    "ltcl_serialize IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_url IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_url IMPLEMENTATION.

  METHOD repo_error.

    TRY.
        lcl_url=>host( 'not a real url' ).                  "#EC NOTEXT
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_exception.                              "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "repo_error

  METHOD repo_url.

    DATA: lv_host TYPE string.

    lv_host = lcl_url=>host( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'https://github.com'
        act = lv_host ).

  ENDMETHOD.                    "repo_url

ENDCLASS.                    "ltcl_url IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_abap_unit IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_git_pack IMPLEMENTATION.

  METHOD pack_multiple.

    CONSTANTS: lc_data TYPE x LENGTH 15 VALUE '123456789ABCDEF545794254754554',
               lc_sha  TYPE ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_objects TYPE lcl_git_pack=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_nodes   TYPE lcl_git_pack=>ty_nodes_tt,
          ls_node    LIKE LINE OF lt_nodes,
          ls_commit  TYPE lcl_git_pack=>ty_commit,
          lt_result  TYPE lcl_git_pack=>ty_objects_tt,
          lv_data    TYPE xstring.


* blob
    lv_data = lc_data.
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = lv_data ).
    ls_object-type = gc_type-blob.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* commit
    CLEAR ls_commit.
    ls_commit-tree      = lc_sha.
    ls_commit-parent    = lc_sha.
    ls_commit-author    = 'John Foobar'.
    ls_commit-committer = 'John Foobar'.
    ls_commit-body      = 'body'.
    lv_data = lcl_git_pack=>encode_commit( ls_commit ).
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-commit iv_data = lv_data ).
    ls_object-type = gc_type-commit.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* tree
    CLEAR ls_node.
    ls_node-chmod     = '12456'.
    ls_node-name      = 'foobar.abap'.
    ls_node-sha1      = lc_sha.
    APPEND ls_node TO lt_nodes.
    lv_data = lcl_git_pack=>encode_tree( lt_nodes ).
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-tree iv_data = lv_data ).
    ls_object-type = gc_type-tree.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.


    CLEAR lv_data.
    lv_data = lcl_git_pack=>encode( lt_objects ).
    lt_result = lcl_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack_multiple

  METHOD object_blob.

    rs_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob
                                     iv_data = iv_data ).
    rs_object-type = gc_type-blob.
    rs_object-data = iv_data.

  ENDMETHOD.

  METHOD pack_short.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE lcl_git_pack=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE lcl_git_pack=>ty_objects_tt,
          lv_data    TYPE xstring.


    lv_data = lc_data.

    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = lcl_git_pack=>encode( lt_objects ).
    lt_result = lcl_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack

  METHOD pack_long.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE lcl_git_pack=>ty_objects_tt,
          ls_object  LIKE LINE OF lt_objects,
          lv_xstring TYPE xstring,
          lt_result  TYPE lcl_git_pack=>ty_objects_tt,
          lv_data    TYPE xstring.


    lv_xstring = lc_data.
    DO 20 TIMES.
      CONCATENATE lv_xstring lv_data INTO lv_data IN BYTE MODE.
    ENDDO.

    ls_object = object_blob( lv_data ).
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = lcl_git_pack=>encode( lt_objects ).
    lt_result = lcl_git_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack_long

  METHOD tree.

    CONSTANTS: lc_sha TYPE ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_nodes  TYPE lcl_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE lcl_git_pack=>ty_nodes_tt.

    CLEAR ls_node.
    ls_node-chmod = gc_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = lc_sha.
    APPEND ls_node TO lt_nodes.

    lv_data = lcl_git_pack=>encode_tree( lt_nodes ).
    lt_result = lcl_git_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.

  METHOD commit.

    CONSTANTS: lc_tree   TYPE ty_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc',
               lc_parent TYPE ty_sha1 VALUE '1236cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: ls_commit TYPE lcl_git_pack=>ty_commit,
          ls_result TYPE lcl_git_pack=>ty_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = lc_tree.
    ls_commit-parent    = lc_parent.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.

    lv_data = lcl_git_pack=>encode_commit( ls_commit ).
    ls_result = lcl_git_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.

ENDCLASS.                    "lcl_abap_unit IMPLEMENTATION
