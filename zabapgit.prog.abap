REPORT zabapgit.

* See http://www.abapgit.org

CONSTANTS: gc_xml_version  TYPE string VALUE 'v1.0.0',      "#EC NOTEXT
           gc_abap_version TYPE string VALUE 'v1.9.7'.      "#EC NOTEXT

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2014 abapGit Contributors
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

TYPES: BEGIN OF ty_file_item,
         file TYPE ty_file,
         item TYPE ty_item,
       END OF ty_file_item.
TYPES: ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY.

TYPES: BEGIN OF ty_metadata,
         class      TYPE string,
         version    TYPE string,
         late_deser TYPE string,
       END OF ty_metadata.

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
  RAISE EXCEPTION TYPE lcx_exception
    EXPORTING
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
      IMPORTING iv_text     TYPE string
                ix_previous TYPE REF TO cx_root OPTIONAL.

  PRIVATE SECTION.
    DATA mx_previous TYPE REF TO cx_root.

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
    mx_previous = previous.
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

CLASS lcl_progress DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      show
        IMPORTING
          iv_key            TYPE string
          VALUE(iv_current) TYPE i
          iv_total          TYPE i
          iv_text           TYPE csequence.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_stack,
             key     TYPE string,
             current TYPE i,
             total   TYPE i,
             text    TYPE string,
           END OF ty_stack.

    CLASS-DATA:
      gt_stack TYPE STANDARD TABLE OF ty_stack WITH DEFAULT KEY.

    CLASS-METHODS:
      calc_pct
        RETURNING VALUE(rv_pct) TYPE i,
      build_text
        RETURNING VALUE(rv_text) TYPE string.

ENDCLASS.

CLASS lcl_progress IMPLEMENTATION.

  METHOD show.

    DATA: lv_pct  TYPE i,
          lv_text TYPE string.

    FIELD-SYMBOLS: <ls_stack> LIKE LINE OF gt_stack.

* assumption:
* all callers must end with calling this method with iv_current = iv_total
* to clear the progress of that sub element
    ASSERT lines( gt_stack ) < 10.

    READ TABLE gt_stack INDEX lines( gt_stack ) ASSIGNING <ls_stack>.
    IF sy-subrc <> 0 OR <ls_stack>-key <> iv_key.
      APPEND INITIAL LINE TO gt_stack ASSIGNING <ls_stack>.
    ENDIF.
    <ls_stack>-key     = iv_key.
    <ls_stack>-current = iv_current.
    <ls_stack>-total   = iv_total.
    <ls_stack>-text    = iv_text.

    lv_pct = calc_pct( ).
    lv_text = build_text( ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = lv_text.

    IF iv_current = iv_total.
      DELETE gt_stack INDEX lines( gt_stack ).
    ENDIF.

  ENDMETHOD.

  METHOD build_text.

    FIELD-SYMBOLS: <ls_stack> LIKE LINE OF gt_stack.


    LOOP AT gt_stack ASSIGNING <ls_stack>.
      IF sy-tabix = 1.
        rv_text = |{ <ls_stack>-key } { <ls_stack>-text }|.
      ELSE.
        rv_text = |{ rv_text } - { <ls_stack>-key } { <ls_stack>-text }|.

        IF <ls_stack>-current <> 1 AND <ls_stack>-total <> 1.
          rv_text = |{ rv_text } ({ <ls_stack>-current }/{ <ls_stack>-total })|.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD calc_pct.

    DATA: lv_f TYPE f.

    FIELD-SYMBOLS: <ls_stack> LIKE LINE OF gt_stack.


    READ TABLE gt_stack ASSIGNING <ls_stack> INDEX 1.
    ASSERT sy-subrc = 0.

    lv_f = ( <ls_stack>-current / <ls_stack>-total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_html_helper DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS c_indent_size TYPE i VALUE 2.

    DATA mv_html   TYPE string READ-ONLY.
    DATA mv_indent TYPE i READ-ONLY.

    METHODS add IMPORTING iv_chunk TYPE any.
    METHODS reset.

  PRIVATE SECTION.
    METHODS _add_str IMPORTING iv_str  TYPE csequence.
    METHODS _add_htm IMPORTING io_html TYPE REF TO lcl_html_helper.

ENDCLASS.                    "lcl_html_helper DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_helper IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_helper IMPLEMENTATION.
  METHOD add.
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_html TYPE REF TO lcl_html_helper.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_chunk ).

    CASE lo_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_char
          OR cl_abap_typedescr=>typekind_string.
        IF strlen( iv_chunk ) = 0.
          RETURN.
        ENDIF.
        _add_str( iv_chunk ).
      WHEN cl_abap_typedescr=>typekind_oref.
        ASSERT iv_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= iv_chunk.
          CATCH cx_sy_move_cast_error.
            ASSERT 1 = 0. " Dev mistake
        ENDTRY.
        _add_htm( lo_html ).
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

  ENDMETHOD.  " add

  METHOD reset.
    CLEAR: me->mv_html, me->mv_indent.
  ENDMETHOD.                    "reset

  METHOD _add_str.
    CONSTANTS lc_single_tags_re TYPE string " HTML5 singleton tags
      VALUE '<(area|base|br|col|command|embed|hr|img|input|link|meta|param|source|!)'.

    DATA lv_tags        TYPE i.
    DATA lv_tags_open   TYPE i.
    DATA lv_tags_close  TYPE i.
    DATA lv_tags_single TYPE i.
    DATA lv_close_offs  TYPE i.
    DATA lv_shift_back  TYPE i.

    FIND FIRST OCCURRENCE OF '</' IN iv_str MATCH OFFSET lv_close_offs.
    IF sy-subrc = 0 AND lv_close_offs = 0 AND mv_indent > 0. " Found close tag @beginning
      lv_shift_back = 1.
    ENDIF.

    mv_html =   mv_html
            &&  repeat( val = ` ` occ = ( mv_indent - lv_shift_back ) * c_indent_size )
            &&  iv_str
            &&  gc_newline.

    FIND ALL OCCURRENCES OF '<'  IN iv_str MATCH COUNT lv_tags.
    FIND ALL OCCURRENCES OF '</' IN iv_str MATCH COUNT lv_tags_close.
    FIND ALL OCCURRENCES OF REGEX lc_single_tags_re IN iv_str MATCH COUNT lv_tags_single.

    lv_tags_open = lv_tags - lv_tags_close - lv_tags_single.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF lv_tags_open > lv_tags_close.
      mv_indent = mv_indent + 1.
    ELSEIF lv_tags_open < lv_tags_close AND mv_indent > 0.
      mv_indent = mv_indent - 1.
    ENDIF.

  ENDMETHOD.                    "_add_str

  METHOD _add_htm.
    DATA lv_indent_str  TYPE string.
    DATA lv_temp_str    TYPE string.

    lv_indent_str = repeat( val = ` ` occ = mv_indent * c_indent_size ).
    lv_temp_str   = io_html->mv_html.

    IF me->mv_indent > 0.
      REPLACE ALL OCCURRENCES OF gc_newline IN lv_temp_str
        WITH gc_newline && lv_indent_str.
      SHIFT lv_temp_str RIGHT DELETING TRAILING space.
      SHIFT lv_temp_str LEFT  DELETING LEADING space.
    ENDIF.

    mv_html   = mv_html && lv_indent_str && lv_temp_str.
    mv_indent = mv_indent + io_html->mv_indent.

  ENDMETHOD.                    "_add_htm

ENDCLASS.                    "lcl_html_helper IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add    IMPORTING iv_txt TYPE string
                             iv_sub TYPE REF TO lcl_html_toolbar OPTIONAL
                             iv_cmd TYPE string OPTIONAL.
    METHODS render IMPORTING iv_as_droplist_with_label  TYPE string OPTIONAL
                             ib_no_separator            TYPE abap_bool OPTIONAL
                   RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

  PRIVATE SECTION.
    TYPES:  BEGIN OF ty_item,
              txt TYPE string,
              cmd TYPE string,
              sub TYPE REF TO lcl_html_toolbar,
            END OF ty_item.
    TYPES:  tt_items TYPE STANDARD TABLE OF ty_item.

    DATA    mt_items TYPE tt_items.

ENDCLASS. "lcl_html_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar IMPLEMENTATION.

  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_cmd IS INITIAL AND iv_sub IS NOT INITIAL
      OR   iv_cmd IS NOT INITIAL AND iv_sub IS INITIAL. " Only one supplied

    ls_item-txt = iv_txt.
    ls_item-cmd = iv_cmd.
    ls_item-sub = iv_sub.
    APPEND ls_item TO mt_items.
  ENDMETHOD.

  METHOD render.
    DATA          lo_html   TYPE REF TO lcl_html_helper.
    DATA          lv_class  TYPE string.
    DATA          lb_last   TYPE abap_bool.
    FIELD-SYMBOLS <item>    TYPE ty_item.

    CREATE OBJECT lo_html.

    IF iv_as_droplist_with_label IS INITIAL.
      lv_class = 'menu'.
    ELSE.
      lv_class = 'dropdown'.
      IF ib_no_separator = abap_true.
        lv_class = lv_class && ' menu_end'.
      ENDIF.
    ENDIF.

    lo_html->add( |<div class="{ lv_class }">| ).

    IF iv_as_droplist_with_label IS NOT INITIAL.
      lo_html->add( |<button class="dropbtn">{ iv_as_droplist_with_label }</button>| ).
      lo_html->add( '<div class="dropdown_content">' ).
    ENDIF.

    LOOP AT mt_items ASSIGNING <item>.
      lb_last = boolc( sy-tabix = lines( mt_items ) ).

      IF <item>-sub IS INITIAL.
        CLEAR lv_class.
        IF lb_last = abap_true AND iv_as_droplist_with_label IS INITIAL.
          lv_class = ' class="menu_end"'.
        ENDIF.
        lo_html->add( |<a{ lv_class } href="{ <item>-cmd }">{ <item>-txt }</a>| ).
      ELSE.
        lo_html->add( <item>-sub->render( iv_as_droplist_with_label = <item>-txt
                                          ib_no_separator           = lb_last ) ).
      ENDIF.

    ENDLOOP.

    IF iv_as_droplist_with_label IS NOT INITIAL.
      lo_html->add( '</div>' ).
    ENDIF.

    lo_html->add( '</div>' ).
    ro_html = lo_html.

  ENDMETHOD.

ENDCLASS. "lcl_html_toolbar IMPLEMENTATION


CLASS lcl_log DEFINITION.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING
          iv_msgv1 TYPE csequence
          iv_msgv2 TYPE csequence OPTIONAL
          iv_msgv3 TYPE csequence OPTIONAL
          iv_msgv4 TYPE csequence OPTIONAL,
      count
        RETURNING VALUE(rv_count) TYPE i,
      to_html
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      show.

  PRIVATE SECTION.
    DATA: mt_log TYPE rs_t_msg.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD to_html.

    DATA: lv_string TYPE string.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    CREATE OBJECT ro_html.

    IF count( ) = 0.
      RETURN.
    ENDIF.

    ro_html->add( '<br>' ).
    LOOP AT mt_log ASSIGNING <ls_log>.
      CONCATENATE <ls_log>-msgv1
        <ls_log>-msgv2
        <ls_log>-msgv3
        <ls_log>-msgv4 INTO lv_string SEPARATED BY space.
      ro_html->add( lv_string ).
      ro_html->add( '<br>' ).
    ENDLOOP.
    ro_html->add( '<br>' ).

  ENDMETHOD.

  METHOD add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msgty = 'W'.
    <ls_log>-msgid = '00'.
    <ls_log>-msgno = '001'.
    <ls_log>-msgv1 = iv_msgv1.
    <ls_log>-msgv2 = iv_msgv2.
    <ls_log>-msgv3 = iv_msgv3.
    <ls_log>-msgv4 = iv_msgv4.

  ENDMETHOD.

  METHOD show.
    CALL FUNCTION 'RSDC_SHOW_MESSAGES_POPUP'
      EXPORTING
        i_t_msg           = mt_log
        i_txt             = 'Warning'
        i_with_s_on_empty = abap_false
        i_one_msg_direct  = abap_false
        i_one_msg_type_s  = abap_false
        ##no_text.
  ENDMETHOD.

  METHOD count.
    rv_count = lines( mt_log ).
  ENDMETHOD.

ENDCLASS.

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

CLASS lcl_persistence_migrate DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run RAISING lcx_exception.

  PRIVATE SECTION.
    CONSTANTS:
      c_text TYPE string VALUE 'Generated by abapGit' ##NO_TEXT.

    CLASS-METHODS:
      migrate_repo RAISING lcx_exception,
      migrate_user RAISING lcx_exception,
      table_create RAISING lcx_exception,
      table_exists
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      lock_create RAISING lcx_exception,
      lock_exists
        RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_user DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user DEFINITION FINAL FRIENDS lcl_persistence_migrate.

* this class is obsolete, use LCL_PERSISTENCE_USER instead

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_user,
             user     LIKE sy-uname,
             username TYPE string,
             email    TYPE string,
           END OF ty_user.

    TYPES: ty_user_tt TYPE STANDARD TABLE OF ty_user WITH DEFAULT KEY.

    CLASS-METHODS set_username
      IMPORTING iv_user     TYPE xubname DEFAULT sy-uname
                iv_username TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_username
      IMPORTING iv_user            TYPE xubname DEFAULT sy-uname
      RETURNING VALUE(rv_username) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS set_email
      IMPORTING iv_user  TYPE xubname DEFAULT sy-uname
                iv_email TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_email
      IMPORTING iv_user         TYPE xubname DEFAULT sy-uname
      RETURNING VALUE(rv_email) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS list
      RETURNING VALUE(rt_data) TYPE ty_user_tt
      RAISING   lcx_exception.

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

* this class is obsolete, use LCL_PERSISTENCE_USER instead

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
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
      _raise 'error from SAVE_TEXT'.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.                    "change

  METHOD set_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' iv_user INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_username ).

  ENDMETHOD.                    "set_username

  METHOD get_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' iv_user INTO lv_name.

    rv_username = read( lv_name ).

  ENDMETHOD.                    "get_username

  METHOD set_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' iv_user INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_email ).

  ENDMETHOD.                    "set_email

  METHOD list.

    DATA: lt_stxh TYPE STANDARD TABLE OF stxh WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF rt_data,
                   <ls_stxh>   LIKE LINE OF lt_stxh.


    SELECT * FROM stxh INTO TABLE lt_stxh
      WHERE tdobject = 'TEXT'
      AND tdname LIKE 'ZABAPGIT_USERNAME_%'.

    LOOP AT lt_stxh ASSIGNING <ls_stxh>.
      APPEND INITIAL LINE TO rt_data ASSIGNING <ls_output>.

      <ls_output>-user     = <ls_stxh>-tdname+18.
      <ls_output>-username = get_username( <ls_output>-user ).
      <ls_output>-email    = get_email( <ls_output>-user ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' iv_user INTO lv_name.

    rv_email = read( lv_name ).

  ENDMETHOD.                    "get_email

ENDCLASS.                    "lcl_user IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor.

  PROTECTED SECTION.
    DATA: mi_ixml     TYPE REF TO if_ixml,
          mi_xml_doc  TYPE REF TO if_ixml_document,
          ms_metadata TYPE ty_metadata.

    CONSTANTS: c_abapgit_tag             TYPE string VALUE 'abapGit' ##NO_TEXT,
               c_attr_version            TYPE string VALUE 'version' ##NO_TEXT,
               c_attr_serializer         TYPE string VALUE 'serializer' ##NO_TEXT,
               c_attr_serializer_version TYPE string VALUE 'serializer_version' ##NO_TEXT.

    METHODS to_xml
      IMPORTING iv_normalize  TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS parse
      IMPORTING iv_normalize TYPE abap_bool DEFAULT abap_true
                iv_xml       TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   lcx_exception.

    METHODS display_xml_error
      RAISING lcx_exception.

ENDCLASS.                    "lcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.

  METHOD constructor.
    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).
  ENDMETHOD.                    "constructor

  METHOD parse.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_element        TYPE REF TO if_ixml_element,
          li_version        TYPE REF TO if_ixml_node,
          li_parser         TYPE REF TO if_ixml_parser.


    ASSERT NOT iv_xml IS INITIAL.

    li_stream_factory = mi_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->set_normalizing( iv_normalize ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).


    li_element = mi_xml_doc->find_from_name_ns( depth = 0 name = c_abapgit_tag ).
    li_version = li_element->if_ixml_node~get_attributes(
      )->get_named_item_ns( c_attr_version ) ##no_text.
    IF li_version->get_value( ) <> gc_xml_version.
      display_xml_error( ).
    ENDIF.

* buffer serializer metadata. Git node will be removed lateron
    ms_metadata-class = li_element->get_attribute_ns( c_attr_serializer ).
    ms_metadata-version = li_element->get_attribute_ns( c_attr_serializer_version ).

  ENDMETHOD.                    "parse

  METHOD display_xml_error.

    DATA: lv_version TYPE string.


    lv_version = |abapGit version: { gc_abap_version }|.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'abapGit XML version mismatch'
        txt1  = 'abapGit XML version mismatch'
        txt2  = 'See https://github.com/larshp/abapGit/wiki/XML-Mismatch'
        txt3  = lv_version.                                 "#EC NOTEXT

    _raise 'XML error'.

  ENDMETHOD.                    "display_xml_error

  METHOD to_xml.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( iv_normalize ).

    li_renderer->render( ).

  ENDMETHOD.                    "to_xml

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

ENDCLASS.                    "lcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_output DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_output DEFINITION FINAL INHERITING FROM lcl_xml CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING iv_name TYPE clike
                  ig_data TYPE any
        RAISING   lcx_exception,
      set_raw
        IMPORTING ii_raw TYPE REF TO if_ixml_element,
      render
        IMPORTING iv_normalize  TYPE sap_bool DEFAULT abap_true
                  is_metadata   TYPE ty_metadata OPTIONAL
        RETURNING VALUE(rv_xml) TYPE string.

  PRIVATE SECTION.
    DATA: mi_raw  TYPE REF TO if_ixml_element.

ENDCLASS.                    "lcl_xml_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_output IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_output IMPLEMENTATION.

  METHOD set_raw.
    mi_raw = ii_raw.
  ENDMETHOD.                    "set_raw

  METHOD add.

    DATA: li_node TYPE REF TO if_ixml_node,
          li_doc  TYPE REF TO if_ixml_document,
          lt_stab TYPE abap_trans_srcbind_tab.

    FIELD-SYMBOLS: <ls_stab> LIKE LINE OF lt_stab.


    ASSERT NOT iv_name IS INITIAL.

    APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.
    <ls_stab>-name = iv_name.
    GET REFERENCE OF ig_data INTO <ls_stab>-value.

    li_doc = cl_ixml=>create( )->create_document( ).

    CALL TRANSFORMATION id
      SOURCE (lt_stab)
      RESULT XML li_doc.

    li_node = mi_xml_doc->get_root( )->get_first_child( ).
    IF li_node IS BOUND.
      mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child(
        li_doc->get_root( )->get_first_child( )->get_first_child( )->get_first_child( ) ).
    ELSE.
      mi_xml_doc->get_root( )->append_child( li_doc->get_root( )->get_first_child( ) ).
    ENDIF.

  ENDMETHOD.                    "add

  METHOD render.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    IF mi_raw IS INITIAL.
      li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
      mi_xml_doc->get_root( )->remove_child( li_abap ).
    ELSE.
      li_abap = mi_raw.
    ENDIF.

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = c_attr_version value = gc_xml_version ). "#EC NOTEXT
    IF NOT is_metadata IS INITIAL.
      li_git->set_attribute( name  = c_attr_serializer
                             value = is_metadata-class ).   "#EC NOTEXT
      li_git->set_attribute( name  = c_attr_serializer_version
                             value = is_metadata-version ). "#EC NOTEXT
    ENDIF.
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.                    "render

ENDCLASS.                    "lcl_xml_output IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_input DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_input DEFINITION FINAL INHERITING FROM lcl_xml CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_xml TYPE clike
        RAISING   lcx_exception,
      read
        IMPORTING iv_name TYPE clike
        CHANGING  cg_data TYPE any
        RAISING   lcx_exception,
      get_raw
        RETURNING VALUE(ri_raw) TYPE REF TO if_ixml_node,
      get_metadata
        RETURNING VALUE(rs_metadata) TYPE ty_metadata.

  PRIVATE SECTION.
    METHODS: fix_xml.

ENDCLASS.                    "lcl_xml_input DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_input IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_input IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    parse( iv_xml ).
    fix_xml( ).

  ENDMETHOD.                    "constructor

  METHOD get_raw.
    ri_raw = mi_xml_doc->get_root_element( ).
  ENDMETHOD.                    "get_raw

  METHOD fix_xml.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_node.


    li_git ?= mi_xml_doc->find_from_name_ns( depth = 0 name = c_abapgit_tag ).
    li_abap = li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

  ENDMETHOD.                    "fix_xml

  METHOD read.

    DATA: lv_text  TYPE string,
          lx_error TYPE REF TO cx_transformation_error,
          lt_rtab  TYPE abap_trans_resbind_tab.

    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF lt_rtab.


    ASSERT NOT iv_name IS INITIAL.

    APPEND INITIAL LINE TO lt_rtab ASSIGNING <ls_rtab>.
    <ls_rtab>-name = iv_name.
    GET REFERENCE OF cg_data INTO <ls_rtab>-value.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML mi_xml_doc
          RESULT (lt_rtab) ##no_text.
      CATCH cx_transformation_error INTO lx_error.
        lv_text = lx_error->if_message~get_text( ).
        _raise lv_text.
    ENDTRY.

  ENDMETHOD.                    "read

  METHOD get_metadata.
    rs_metadata = ms_metadata.
  ENDMETHOD.                    "get_metadata

ENDCLASS.                    "lcl_xml_input IMPLEMENTATION

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

CLASS lcl_xml_pretty DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: print
      IMPORTING iv_xml        TYPE string
      RETURNING VALUE(rv_xml) TYPE string.

ENDCLASS.

CLASS ltcl_dot_abapgit DEFINITION DEFERRED.

CLASS lcl_dot_abapgit DEFINITION CREATE PRIVATE FRIENDS ltcl_dot_abapgit.

  PUBLIC SECTION.
    CLASS-METHODS:
      build_default
        IMPORTING iv_master_language    TYPE spras
        RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_dot_abapgit,
      deserialize
        IMPORTING iv_xstr               TYPE xstring
        RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_dot_abapgit
        RAISING   lcx_exception.

    METHODS:
      serialize
        RETURNING VALUE(rv_xstr) TYPE xstring,
      add_ignore
        IMPORTING iv_path     TYPE string
                  iv_filename TYPE string,
      is_ignored
        IMPORTING iv_path           TYPE string
                  iv_filename       TYPE string
        RETURNING VALUE(rv_ignored) TYPE abap_bool,
      remove_ignore
        IMPORTING iv_path     TYPE string
                  iv_filename TYPE string,
      get_starting_folder
        RETURNING VALUE(rv_path) TYPE string,
      set_starting_folder
        IMPORTING iv_path TYPE string,
      get_master_language
        RETURNING VALUE(rv_language) TYPE spras,
      set_master_language
        IMPORTING iv_language TYPE spras.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_dot_abapgit,
             master_language TYPE spras,
             starting_folder TYPE string,
             ignore          TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           END OF ty_dot_abapgit.

    CONSTANTS: c_data TYPE string VALUE 'DATA'.

    DATA: ms_data TYPE ty_dot_abapgit.

    METHODS:
      constructor
        IMPORTING is_data TYPE ty_dot_abapgit.

    CLASS-METHODS:
      to_xml
        IMPORTING is_data       TYPE ty_dot_abapgit
        RETURNING VALUE(rv_xml) TYPE string,
      from_xml
        IMPORTING iv_xml         TYPE string
        RETURNING VALUE(rs_data) TYPE ty_dot_abapgit.

ENDCLASS.

CLASS lcl_dot_abapgit IMPLEMENTATION.

  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.

  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE ty_dot_abapgit.


    lv_xml = lcl_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD serialize.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( ms_data ).

    rv_xstr = lcl_convert=>string_to_xstring_utf8( lv_xml ).

  ENDMETHOD.

  METHOD build_default.

    DATA: ls_data TYPE ty_dot_abapgit.


    ls_data-master_language = iv_master_language.
    ls_data-starting_folder = '/'.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE (c_data) = is_data
      RESULT XML rv_xml.

    rv_xml = lcl_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF '<?xml version="1.0" encoding="utf-16"?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_xml
      RESULT (c_data) = rs_data ##NO_TEXT.
  ENDMETHOD.

  METHOD add_ignore.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <ls_ignore> LIKE LINE OF ms_data-ignore.


    lv_name = iv_path && iv_filename.

    READ TABLE ms_data-ignore FROM lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO ms_data-ignore ASSIGNING <ls_ignore>.
    <ls_ignore> = lv_name.

  ENDMETHOD.

  METHOD is_ignored.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    READ TABLE ms_data-ignore FROM lv_name
      TRANSPORTING NO FIELDS.

    rv_ignored = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD remove_ignore.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    DELETE TABLE ms_data-ignore FROM lv_name.

  ENDMETHOD.

  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.

  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.

  METHOD get_master_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.

  METHOD set_master_language.
    ms_data-master_language = iv_language.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_dot_abapgit DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      identity FOR TESTING
        RAISING lcx_exception,
      ignore FOR TESTING.

ENDCLASS.

CLASS ltcl_dot_abapgit IMPLEMENTATION.

  METHOD identity.

    DATA: lo_dot    TYPE REF TO lcl_dot_abapgit,
          ls_before TYPE lcl_dot_abapgit=>ty_dot_abapgit,
          ls_after  TYPE lcl_dot_abapgit=>ty_dot_abapgit.


    lo_dot = lcl_dot_abapgit=>build_default( gc_english ).
    ls_before = lo_dot->ms_data.

    lo_dot = lcl_dot_abapgit=>deserialize( lo_dot->serialize( ) ).
    ls_after = lo_dot->ms_data.

    cl_abap_unit_assert=>assert_equals(
      act = ls_after
      exp = ls_before ).

  ENDMETHOD.

  METHOD ignore.

    CONSTANTS: lc_path     TYPE string VALUE '/',
               lc_filename TYPE string VALUE 'foobar.txt'.

    DATA: lv_ignored TYPE abap_bool,
          lo_dot     TYPE REF TO lcl_dot_abapgit.


    lo_dot = lcl_dot_abapgit=>build_default( gc_english ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

    lo_dot->add_ignore( iv_path = lc_path iv_filename = lc_filename ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_true ).

    lo_dot->remove_ignore( iv_path = lc_path iv_filename = lc_filename ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.

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
             local_line  TYPE c LENGTH 6,
             local       TYPE string,
             result      TYPE c LENGTH 1,
             remote_line TYPE c LENGTH 6,
             remote      TYPE string,
             short       TYPE abap_bool,
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

    CLASS-METHODS:
      unpack
        IMPORTING iv_local  TYPE xstring
                  iv_remote TYPE xstring
        EXPORTING et_local  TYPE abaptxt255_tab
                  et_remote TYPE abaptxt255_tab,
      render
        IMPORTING it_local       TYPE abaptxt255_tab
                  it_remote      TYPE abaptxt255_tab
                  it_delta       TYPE vxabapt255_tab
        RETURNING VALUE(rt_diff) TYPE ty_diffs_tt,
      compute
        IMPORTING it_local        TYPE abaptxt255_tab
                  it_remote       TYPE abaptxt255_tab
        RETURNING VALUE(rt_delta) TYPE vxabapt255_tab.

    METHODS:
      calculate_line_num,
      shortlist.

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

  METHOD shortlist.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    IF lines( mt_diff ) < 500.
      LOOP AT mt_diff ASSIGNING <ls_diff>.
        <ls_diff>-short = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT mt_diff TRANSPORTING NO FIELDS
          WHERE NOT result IS INITIAL AND short = abap_false.
        lv_index = sy-tabix.

        DO 20 TIMES. " Backward
          READ TABLE mt_diff INDEX ( lv_index - sy-index ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0 OR <ls_diff>-short = abap_true. " tab bound or prev marker
            EXIT.
          ENDIF.
          <ls_diff>-short = abap_true.
*          lv_index = lv_index - 1.
        ENDDO.

        DO 20 TIMES. " Forward
*          lv_index = lv_index + 1.
          READ TABLE mt_diff INDEX ( lv_index + sy-index - 1 ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0. " tab bound reached
            EXIT.
          ENDIF.
          CHECK <ls_diff>-short = abap_false. " skip marked
          <ls_diff>-short = abap_true.
        ENDDO.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_line_num.

    DATA: lv_local  TYPE i VALUE 1,
          lv_remote TYPE i VALUE 1.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    LOOP AT mt_diff ASSIGNING <ls_diff>.
      <ls_diff>-local_line = lv_local.
      <ls_diff>-remote_line = lv_remote.

      CASE <ls_diff>-result.
        WHEN c_diff-delete.
          lv_remote = lv_remote + 1.
          CLEAR <ls_diff>-local_line.
        WHEN c_diff-insert.
          lv_local = lv_local + 1.
          CLEAR <ls_diff>-remote_line.
        WHEN OTHERS.
          lv_local = lv_local + 1.
          lv_remote = lv_remote + 1.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

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

    calculate_line_num( ).

    shortlist( ).

  ENDMETHOD.                    "diff

  METHOD render.

    DEFINE _append.
      CLEAR ls_diff.
      ls_diff-local = &1.
      ls_diff-result = &2.
      ls_diff-remote = &3.
      APPEND ls_diff TO rt_diff.
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
            ASSERT 0 = 1.
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

  ENDMETHOD.

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

*----------------------------------------------------------------------*
*       CLASS lcl_objects_activation DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

ENDCLASS.                    "lcl_objects_activation DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_activation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_activation IMPLEMENTATION.

  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.                    "add_item

  METHOD clear.
    CLEAR: gt_ddic,
           gt_programs.
  ENDMETHOD.                    "clear

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
      WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'INDX' OR 'TTYP'
        OR 'VIEW' OR 'SHLP' OR 'ENQU'
        OR 'SFSW' OR 'SFBF' OR 'SFBS'.
* todo also insert_into_working_area?
        APPEND INITIAL LINE TO gt_ddic ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN 'REPS' OR 'DYNP' OR 'CUAD' OR 'REPT' OR 'INTF'
          OR 'FUNC' OR 'ENHO' OR 'TYPE' OR 'XSLT'.
* these seem to go into the workarea automatically
        APPEND INITIAL LINE TO gt_programs ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN OTHERS.
        _raise 'activate, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "activate

ENDCLASS.                    "lcl_objects_activation IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_files DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_files DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_item TYPE ty_item,
      add_string
        IMPORTING iv_extra  TYPE clike OPTIONAL
                  iv_ext    TYPE string
                  iv_string TYPE string
        RAISING   lcx_exception,
      read_string
        IMPORTING iv_extra         TYPE clike OPTIONAL
                  iv_ext           TYPE string
        RETURNING VALUE(rv_string) TYPE string
        RAISING   lcx_exception,
      add_xml
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO lcl_xml_output
                  iv_normalize TYPE sap_bool DEFAULT abap_true
                  is_metadata  TYPE ty_metadata OPTIONAL
        RAISING   lcx_exception,
* needed since type-check during dynamic call fails even if the object is compatible
      add_xml_from_plugin
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO object
                  iv_normalize TYPE sap_bool DEFAULT abap_true
        RAISING   lcx_exception ##called,
      read_xml
        IMPORTING iv_extra      TYPE clike OPTIONAL
        RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml_input
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

ENDCLASS.                    "lcl_objects_files DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_object.

  METHODS:
    serialize
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception,
    deserialize
      IMPORTING iv_package TYPE devclass
                io_xml     TYPE REF TO lcl_xml_input
      RAISING   lcx_exception,
    delete
      RAISING lcx_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool
      RAISING   lcx_exception,
    jump
      RAISING lcx_exception,
    get_metadata
      RETURNING VALUE(rs_metadata) TYPE ty_metadata.

  DATA: mo_files TYPE REF TO lcl_objects_files.

ENDINTERFACE.                    "lif_object DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_files IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_files IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.                    "constructor

  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.                    "add

  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.                    "get_files

  METHOD set_files.
    mt_files = it_files.
  ENDMETHOD.                    "set_files

  METHOD read_string.

    DATA: lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_html> LIKE LINE OF mt_files.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).            "#EC NOTEXT

    READ TABLE mt_files ASSIGNING <ls_html> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'html not found'.
    ENDIF.

    rv_string = lcl_convert=>xstring_to_string_utf8( <ls_html>-data ).

  ENDMETHOD.                    "read_string

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

  METHOD add_string.

    DATA: ls_file TYPE ty_file.


    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).       "#EC NOTEXT
    ls_file-data = lcl_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "add_string

  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE ty_file.


    lv_xml = io_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
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

  ENDMETHOD.                    "read_xml

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

  METHOD add_xml_from_plugin.
*    this method wraps add_xml as in the plugin. This is necessary as the wrapped
*    xml-object in the plugin can only be typed to object.
*    ABAP does not perform implicit type casts (also if compatible) in signatures,
*    therefore this method's signature is typed ref to object
    DATA lo_xml TYPE REF TO lcl_xml_output.

    lo_xml ?= io_xml.

    me->add_xml(
      iv_extra     = iv_extra
      io_xml       = lo_xml
      iv_normalize = iv_normalize ).

  ENDMETHOD.                    "add_xml_from_plugin

ENDCLASS.                    "lcl_objects_files IMPLEMENTATION

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
          is_item     TYPE ty_item
          iv_language TYPE spras.

  PROTECTED SECTION.

    DATA: ms_item     TYPE ty_item,
          mv_language TYPE spras.

    METHODS:
      get_metadata
        RETURNING VALUE(rs_metadata) TYPE ty_metadata,
      corr_insert
        IMPORTING iv_package TYPE devclass
        RAISING   lcx_exception,
      jump_se11
        IMPORTING iv_radio TYPE string
                  iv_field TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_objects_super DEFINITION

**********************************************************************
* Enable plugins

CLASS lcl_objects_bridge DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING is_item TYPE ty_item
      RAISING   cx_sy_create_object_error.

    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    DATA: mo_plugin TYPE REF TO object.

    TYPES: BEGIN OF ty_s_objtype_map,
             obj_typ      TYPE trobjtype,
             plugin_class TYPE seoclsname,
           END OF ty_s_objtype_map,
           ty_t_objtype_map TYPE SORTED TABLE OF ty_s_objtype_map WITH UNIQUE KEY obj_typ.

    CLASS-DATA gt_objtype_map TYPE ty_t_objtype_map.

ENDCLASS.                    "lcl_objects_bridge DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_bridge IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_bridge IMPLEMENTATION.

  METHOD lif_object~get_metadata.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = rs_metadata.

  ENDMETHOD.                    "lif_object~get_metadata

  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = gc_english ).

*    determine the responsible plugin
    READ TABLE gt_objtype_map INTO ls_objtype_map
      WITH TABLE KEY obj_typ = is_item-obj_type.
    IF sy-subrc = 0.
      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).

      CALL METHOD mo_plugin->('SET_ITEM')
        EXPORTING
          iv_obj_type = is_item-obj_type
          iv_obj_name = is_item-obj_name.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_create_object_error
        EXPORTING
          classname = 'LCL_OBJECTS_BRIDGE'.
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD lif_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            ix_previous = lx_plugin
            iv_text     = lx_plugin->get_text( ).
    ENDTRY.
  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            ix_previous = lx_plugin
            iv_text     = lx_plugin->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~JUMP').

  ENDMETHOD.                    "lif_object~jump

  METHOD class_constructor.

    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
    DATA lo_plugin          TYPE REF TO object.
    DATA lt_plugin_obj_type TYPE objtyptable.
    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.


    SELECT ext~clsname
      FROM vseoextend AS ext
      INTO TABLE lt_plugin_class
      WHERE ext~refclsname LIKE 'ZCL_ABAPGIT_OBJECT%'
      AND ext~version = '1'.                              "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
* have the generic plugin only as fallback
      TRY.
          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
        CATCH cx_sy_create_object_error.
          CONTINUE. ">>>>>>>>>>>>>>
      ENDTRY.

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        IMPORTING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
        IF sy-subrc <> 0.
* No exception in class-contructor possible.
* Anyway, a shortdump is more appropriate in this case
          ASSERT 'There must not be' =
            |multiple ABAPGit-Plugins for the same object type {
            ls_objtype_map-obj_typ }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP. "at plugins

* and the same for the generic plugin if exists
* have the generic plugin only as fallback
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line = 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        RECEIVING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
      ENDLOOP.
    ENDLOOP. "at plugins

  ENDMETHOD.                    "class_constructor

ENDCLASS.                    "lcl_objects_bridge IMPLEMENTATION

**********************************************************************

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

    METHODS serialize_program
      IMPORTING io_xml     TYPE REF TO lcl_xml_output OPTIONAL
                is_item    TYPE ty_item
                io_files   TYPE REF TO lcl_objects_files
                iv_program TYPE programm OPTIONAL
                iv_extra   TYPE clike OPTIONAL
      RAISING   lcx_exception.

    METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   lcx_exception.

  PROTECTED SECTION.
    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_tpool.
        INCLUDE TYPE textpool.
    TYPES:   split TYPE c LENGTH 8.
    TYPES: END OF ty_tpool.

    TYPES: ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_dynpro,
             header     TYPE rpy_dyhead,
             containers TYPE dycatt_tab,
             fields     TYPE dyfatc_tab,
             flow_logic TYPE swydyflow,
             spaces     TYPE ty_spaces_tt,
           END OF ty_dynpro.

    TYPES: ty_dynpro_tt TYPE STANDARD TABLE OF ty_dynpro WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_cua,
             adm TYPE rsmpe_adm,
             sta TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY,
             fun TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY,
             men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY,
             mtx TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY,
             act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY,
             but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY,
             pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY,
             set TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY,
             doc TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY,
             tit TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY,
             biv TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY,
           END OF ty_cua.

    METHODS serialize_dynpros
      IMPORTING iv_program_name  TYPE programm
      RETURNING VALUE(rt_dynpro) TYPE ty_dynpro_tt
      RAISING   lcx_exception.

    METHODS serialize_cua
      IMPORTING iv_program_name TYPE programm
      RETURNING VALUE(rs_cua)   TYPE ty_cua
      RAISING   lcx_exception.

    METHODS deserialize_dynpros
      IMPORTING it_dynpros TYPE ty_dynpro_tt
      RAISING   lcx_exception.

    METHODS deserialize_cua
      IMPORTING is_cua TYPE ty_cua
      RAISING   lcx_exception.

    CLASS-METHODS:
      add_tpool
        IMPORTING it_tpool        TYPE textpool_table
        RETURNING VALUE(rt_tpool) TYPE ty_tpool_tt,
      read_tpool
        IMPORTING it_tpool        TYPE ty_tpool_tt
        RETURNING VALUE(rt_tpool) TYPE ty_tpool_tt.

  PRIVATE SECTION.
    METHODS:
      condense_flow
        EXPORTING et_spaces TYPE ty_spaces_tt
        CHANGING  ct_flow   TYPE swydyflow,
      uncondense_flow
        IMPORTING it_flow        TYPE swydyflow
                  it_spaces      TYPE ty_spaces_tt
        RETURNING VALUE(rt_flow) TYPE swydyflow.


ENDCLASS.                    "lcl_objects_program DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_program IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_program IMPLEMENTATION.

  METHOD condense_flow.

    DATA: lv_spaces LIKE LINE OF et_spaces.

    FIELD-SYMBOLS: <ls_flow> LIKE LINE OF ct_flow.


    CLEAR et_spaces.

    LOOP AT ct_flow ASSIGNING <ls_flow>.
      lv_spaces = 0.

      WHILE NOT <ls_flow>-line IS INITIAL AND <ls_flow>-line(1) = space.
        lv_spaces = lv_spaces + 1.
        <ls_flow>-line = <ls_flow>-line+1.
      ENDWHILE.

      APPEND lv_spaces TO et_spaces.
    ENDLOOP.

  ENDMETHOD.

  METHOD uncondense_flow.

    DATA: lv_spaces LIKE LINE OF it_spaces.

    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
                   <ls_output> LIKE LINE OF rt_flow.


    LOOP AT it_flow ASSIGNING <ls_flow>.
      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
      <ls_output>-line = <ls_flow>-line.

      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
      IF sy-subrc = 0.
        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          lo_xml          TYPE REF TO lcl_xml_output.

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

    IF io_xml IS BOUND.
      lo_xml = io_xml.
    ELSE.
      CREATE OBJECT lo_xml.
    ENDIF.

    lo_xml->add( iv_name = 'PROGDIR'
                 ig_data = ls_progdir ).
    IF ls_progdir-subc = '1'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      lo_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      lo_xml->add( iv_name = 'CUA'
                   ig_data = ls_cua ).
    ENDIF.

    IF lines( lt_tpool ) = 1.
      READ TABLE lt_tpool INDEX 1 INTO ls_tpool.
      ASSERT sy-subrc = 0.
      IF ls_tpool-id = 'R' AND ls_tpool-key = '' AND ls_tpool-length = 0.
        DELETE lt_tpool INDEX 1.
      ENDIF.
    ENDIF.

    lo_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF NOT io_xml IS BOUND.
      io_files->add_xml( iv_extra = iv_extra
                         io_xml   = lo_xml ).
    ENDIF.

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
        master_language     = mv_language
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
          LANGUAGE mv_language
          STATE 'I'.
        IF sy-subrc <> 0.
          _raise 'error from INSERT TEXTPOOL'.
        ENDIF.
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

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = mv_language
        state           = 'A'
      IMPORTING
        adm             = rs_cua-adm
      TABLES
        sta             = rs_cua-sta
        fun             = rs_cua-fun
        men             = rs_cua-men
        mtx             = rs_cua-mtx
        act             = rs_cua-act
        but             = rs_cua-but
        pfk             = rs_cua-pfk
        set             = rs_cua-set
        doc             = rs_cua-doc
        tit             = rs_cua-tit
        biv             = rs_cua-biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_FETCH'.
    ENDIF.

  ENDMETHOD.                    "serialize_cua

  METHOD serialize_dynpros.

    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          lt_d020s                TYPE TABLE OF d020s.

    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
                   <lv_outputstyle> TYPE scrpostyle,
                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
                   <ls_dynpro>      LIKE LINE OF rt_dynpro.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      _raise 'error from screen_list'.
    ENDIF.

* loop dynpros and skip generated selection screens
    LOOP AT lt_d020s ASSIGNING <ls_d020s> WHERE type <> 'S'.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_d020s>-dnum
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

      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
          CLEAR <lv_outputstyle>.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
      <ls_dynpro>-header     = ls_header.
      <ls_dynpro>-containers = lt_containers.
      <ls_dynpro>-fields     = lt_fields_to_containers.

      condense_flow( IMPORTING et_spaces = <ls_dynpro>-spaces
                     CHANGING ct_flow = lt_flow_logic ).
      <ls_dynpro>-flow_logic = lt_flow_logic.

    ENDLOOP.

  ENDMETHOD.                    "serialize_dynpros


  METHOD deserialize_dynpros.

    DATA: lv_name   TYPE dwinactiv-obj_name,
          ls_dynpro LIKE LINE OF it_dynpros.


* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since it_dynpros cannot be changed
    LOOP AT it_dynpros INTO ls_dynpro.

      ls_dynpro-flow_logic = uncondense_flow(
        it_flow = ls_dynpro-flow_logic
        it_spaces = ls_dynpro-spaces ).

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
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

      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
        INTO lv_name RESPECTING BLANKS.
      ASSERT NOT lv_name IS INITIAL.

      lcl_objects_activation=>add( iv_type = 'DYNP'
                                   iv_name = lv_name ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_dynpros

  METHOD add_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        <ls_tpool_out>-split = <ls_tpool_out>-entry.
        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "add_tpool

  METHOD read_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
          INTO <ls_tpool_out>-entry
          RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "read_tpool

  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey.


    IF is_cua-adm IS INITIAL.
      RETURN.
    ENDIF.

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

    sy-tcode = 'SE41' ##write_ok. " evil hack, workaround to handle fixes in note 2159455
    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = ms_item-obj_name
        language  = mv_language
        tr_key    = ls_tr_key
        adm       = is_cua-adm
        state     = 'I'
      TABLES
        sta       = is_cua-sta
        fun       = is_cua-fun
        men       = is_cua-men
        mtx       = is_cua-mtx
        act       = is_cua-act
        but       = is_cua-but
        pfk       = is_cua-pfk
        set       = is_cua-set
        doc       = is_cua-doc
        tit       = is_cua-tit
        biv       = is_cua-biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_WRITE'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'CUAD'
                                 iv_name = ms_item-obj_name ).

  ENDMETHOD.                    "deserialize_cua

ENDCLASS.                    "lcl_objects_program IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.                    "constructor

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
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.                                                "jump_se11

  METHOD get_metadata.
    rs_metadata-class =
      cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).
    rs_metadata-version = 'v1.0.0' ##no_text.
  ENDMETHOD.                    "get_metadata

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = ms_item-obj_type.
    ls_object-objname = ms_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        devclass            = iv_package
        master_language     = mv_language
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

*----------------------------------------------------------------------*
*       CLASS lcl_object_acid DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_acid DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS: create_object
      RETURNING VALUE(ro_aab) TYPE REF TO cl_aab_id
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_acid DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_acid IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_acid IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

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

  ENDMETHOD.                    "create_object

  METHOD lif_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_id,
          lv_description TYPE aab_id_descript.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript( IMPORTING ex_descript = lv_description ).

    io_xml->add( iv_name = 'DESCRIPTION'
                 ig_data = lv_description ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING cg_data = lv_description ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript( lv_description ).
    lo_aab->save( ).

  ENDMETHOD.                    "lif_object~deserialize

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

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_state TYPE flag,
          lo_aab   TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).

    lo_aab->get_state(
      IMPORTING
        ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    _raise 'todo, jump, ACID'.
  ENDMETHOD.                    "lif_object~jump

ENDCLASS.                    "lcl_object_acid IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_auth DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_auth DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_auth DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_auth IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_auth IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~serialize.

    DATA: ls_authx TYPE authx.


    SELECT SINGLE * FROM authx INTO ls_authx
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'AUTHX'
                 ig_data = ls_authx ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.
* see include LSAUT_FIELDF02

    DATA: ls_authx TYPE authx,
          lo_auth  TYPE REF TO cl_auth_tools.


    io_xml->read( EXPORTING iv_name = 'AUTHX'
                  CHANGING cg_data = ls_authx ).

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

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: lv_fieldname TYPE authx-fieldname.


    lv_fieldname = ms_item-obj_name.

* there is a bug in SAP standard, the TADIR entries are not deleted
* when the AUTH object is deleted in transaction SU20
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

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_fieldname TYPE authx-fieldname.


    SELECT SINGLE fieldname FROM authx
      INTO lv_fieldname
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    _raise 'todo, AUTH jump'.

  ENDMETHOD.                    "lif_object~jump

ENDCLASS.                    "lcl_object_auth IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_xslt DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_xslt DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_xslt DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_xslt IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_xslt IMPLEMENTATION.

  METHOD lif_object~serialize.

    DATA: lv_name       TYPE cxsltdesc,
          lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_source     TYPE string,
          ls_attributes TYPE o2xsltattr.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = lo_xslt
      EXCEPTIONS
        not_existing       = 1
        permission_failure = 2
        OTHERS             = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ls_attributes = lo_xslt->get_attributes( ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lv_source = lo_xslt->get_source_string( ).

    mo_files->add_string( iv_extra  = 'source'
                          iv_ext    = 'xml'
                          iv_string = lv_source ) ##NO_TEXT.

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lv_source     TYPE string,
          lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_len        TYPE i,
          ls_attributes TYPE o2xsltattr.


    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).

    ls_attributes-devclass = iv_package.

    lv_source = mo_files->read_string( iv_extra = 'source'
                                       iv_ext   = 'xml' ) ##NO_TEXT.

* workaround: somewhere additional linefeeds are added
    lv_len = strlen( lv_source ) - 2.
    IF lv_source+lv_len(2) = cl_abap_char_utilities=>cr_lf.
      lv_source = lv_source(lv_len).
    ENDIF.

    cl_o2_api_xsltdesc=>create_new_from_string(
      EXPORTING
        p_source                = lv_source
        p_attr                  = ls_attributes
      IMPORTING
        p_obj                   = lo_xslt
      EXCEPTIONS
        action_cancelled        = 1
        error_occured           = 2
        not_authorized          = 3
        object_already_existing = 4
        undefined_name          = 5
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      _raise 'error from cl_o2_api_xsltdesc=>create_new_from_string'.
    ENDIF.

    lo_xslt->save( ).

    lo_xslt->set_changeable( abap_false ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: lo_xslt TYPE REF TO cl_o2_api_xsltdesc,
          lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = lo_xslt
      EXCEPTIONS
        error_occured      = 1
        not_existing       = 2
        permission_failure = 3
        version_not_found  = 4
        OTHERS             = 5 ).
    IF sy-subrc <> 0.
      _raise 'error from cl_o2_api_xsltdesc=>load'.
    ENDIF.

    lo_xslt->set_changeable( abap_true ).
    lo_xslt->delete( ).
    lo_xslt->save( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    rv_bool = cl_o2_api_xsltdesc=>exists( lv_name ).
    IF rv_bool = '1'.
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    _raise 'XSLT, jump, todo'.
  ENDMETHOD.                    "lif_object~jump

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

ENDCLASS.                    "lcl_object_xslt IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_doma DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_domname TYPE dd01l-domname.


    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
          lt_dd07v TYPE TABLE OF dd07v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
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

    io_xml->add( iv_name = 'DD01V'
                 ig_data = ls_dd01v ).
    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: ls_dd01v TYPE dd01v,
          lv_name  TYPE ddobjname,
          lt_dd07v TYPE TABLE OF dd07v.


    io_xml->read( EXPORTING iv_name = 'DD01V'
                  CHANGING cg_data = ls_dd01v ).
    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING cg_data = lt_dd07v ).

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
*       CLASS lcl_object_iarp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iarp DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      read
        EXPORTING es_attr       TYPE w3resoattr
                  et_parameters TYPE w3resopara_tabletype
        RAISING   lcx_exception,
      save
        IMPORTING is_attr       TYPE w3resoattr
                  it_parameters TYPE w3resopara_tabletype
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_iarp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iarp IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD read.

    DATA: li_resource TYPE REF TO if_w3_api_resource,
          ls_name     TYPE w3resokey.


    ls_name = ms_item-obj_name.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ls_name
      IMPORTING
        p_resource          = li_resource
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      _raise 'error from w3api_resource~load'.
    ENDIF.

    li_resource->get_attributes( IMPORTING p_attributes = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    li_resource->get_parameters( IMPORTING p_parameters = et_parameters ).

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD save.

    DATA: li_resource TYPE REF TO if_w3_api_resource.


    cl_w3_api_resource=>if_w3_api_resource~create_new(
      EXPORTING p_resource_data = is_attr
      IMPORTING p_resource = li_resource ).

    li_resource->set_attributes( is_attr ).
    li_resource->set_parameters( it_parameters ).

    li_resource->if_w3_api_object~save( ).

  ENDMETHOD.                    "save

  METHOD lif_object~deserialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attr       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: li_resource TYPE REF TO if_w3_api_resource,
          ls_name     TYPE w3resokey.


    ls_name = ms_item-obj_name.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ls_name
      IMPORTING
        p_resource          = li_resource
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      _raise 'error from if_w3_api_resource~load'.
    ENDIF.

    li_resource->if_w3_api_object~set_changeable( abap_true ).
    li_resource->if_w3_api_object~delete( ).
    li_resource->if_w3_api_object~save( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: ls_name TYPE w3resokey.


    ls_name = ms_item-obj_name.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ls_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc = 1.
      rv_bool = abap_false.
    ELSEIF sy-subrc <> 0.
      _raise 'error from w3_api_resource~load'.
    ELSE.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    _raise 'todo, IARP, jump'.
  ENDMETHOD.                    "lif_object~jump

ENDCLASS.                    "lcl_object_iarp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_iasp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iasp DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      read
        EXPORTING es_attr       TYPE w3servattr
                  et_parameters TYPE w3servpara_tabletype
        RAISING   lcx_exception,
      save
        IMPORTING is_attr       TYPE w3servattr
                  it_parameters TYPE w3servpara_tabletype
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_iasp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iasp IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD read.

    DATA: li_service TYPE REF TO if_w3_api_service,
          lv_name    TYPE itsappl.


    lv_name = ms_item-obj_name.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name     = lv_name
      IMPORTING
        p_service          = li_service
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      _raise 'error from w3api_service~load'.
    ENDIF.

    li_service->get_attributes( IMPORTING p_attributes = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    li_service->get_parameters( IMPORTING p_parameters = et_parameters ).

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: ls_attr       TYPE w3servattr,
          lt_parameters TYPE w3servpara_tabletype.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD save.

    DATA: li_service TYPE REF TO if_w3_api_service.


    cl_w3_api_service=>if_w3_api_service~create_new(
      EXPORTING p_service_data = is_attr
      IMPORTING p_service = li_service ).

    li_service->set_attributes( is_attr ).
    li_service->set_parameters( it_parameters ).

    li_service->if_w3_api_object~save( ).

  ENDMETHOD.                    "save

  METHOD lif_object~deserialize.

    DATA: ls_attr       TYPE w3servattr,
          lt_parameters TYPE w3servpara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attr       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: li_service TYPE REF TO if_w3_api_service,
          lv_name    TYPE itsappl.


    lv_name = ms_item-obj_name.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name      = lv_name
      IMPORTING
        p_service           = li_service
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      _raise 'error from if_w3_api_service~load'.
    ENDIF.

    li_service->if_w3_api_object~set_changeable( abap_true ).
    li_service->if_w3_api_object~delete( ).
    li_service->if_w3_api_object~save( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_name TYPE itsappl.


    lv_name = ms_item-obj_name.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name      = lv_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc = 1.
      rv_bool = abap_false.
    ELSEIF sy-subrc <> 0.
      _raise 'error from w3_api_service~load'.
    ELSE.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    _raise 'todo, IASP, jump'.
  ENDMETHOD.                    "lif_object~jump

ENDCLASS.                    "lcl_object_iasp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_iatu DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iatu DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      read
        EXPORTING es_attr   TYPE w3tempattr
                  ev_source TYPE string
        RAISING   lcx_exception,
      save
        IMPORTING is_attr   TYPE w3tempattr
                  iv_source TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_iatu DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_iatu IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iatu IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD read.

    DATA: li_template TYPE REF TO if_w3_api_template,
          lt_source   TYPE w3htmltabtype,
          ls_name     TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    cl_w3_api_template=>if_w3_api_template~load(
      EXPORTING
        p_template_name     = ls_name
      IMPORTING
        p_template          = li_template
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      _raise 'error from w3api_template~load'.
    ENDIF.

    li_template->get_attributes( IMPORTING p_attributes = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    li_template->get_source( IMPORTING p_source = lt_source ).

    CONCATENATE LINES OF lt_source INTO ev_source RESPECTING BLANKS.

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: ls_attr   TYPE w3tempattr,
          lv_source TYPE string.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr   = ls_attr
                    ev_source = lv_source ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).

    mo_files->add_string( iv_ext    = 'html'
                          iv_string = lv_source ) ##NO_TEXT.

  ENDMETHOD.                    "lif_object~serialize

  METHOD save.

    DATA: lt_source   TYPE w3htmltabtype,
          lv_source   TYPE string,
          li_template TYPE REF TO if_w3_api_template.


    cl_w3_api_template=>if_w3_api_template~create_new(
      EXPORTING p_template_data = is_attr
                p_program_name = is_attr-programm
      IMPORTING p_template = li_template ).

    li_template->set_attributes( is_attr ).

    lv_source = iv_source.
    WHILE strlen( lv_source ) >= 255.
      APPEND lv_source(255) TO lt_source.
      lv_source = lv_source+255.
    ENDWHILE.
    IF NOT lv_source IS INITIAL.
      APPEND lv_source TO lt_source.
    ENDIF.

    li_template->set_source( lt_source ).

    li_template->if_w3_api_object~save( ).

  ENDMETHOD.                    "save

  METHOD lif_object~deserialize.

    DATA: ls_attr   TYPE w3tempattr,
          lv_source TYPE string.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).

    lv_source = mo_files->read_string( 'html' ) ##NO_TEXT.

    ls_attr-devclass = iv_package.
    save( is_attr   = ls_attr
          iv_source = lv_source ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: li_template TYPE REF TO if_w3_api_template,
          ls_name     TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    cl_w3_api_template=>if_w3_api_template~load(
      EXPORTING
        p_template_name     = ls_name
      IMPORTING
        p_template          = li_template
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      _raise 'error from if_w3_api_template~load'.
    ENDIF.

    li_template->if_w3_api_object~set_changeable( abap_true ).
    li_template->if_w3_api_object~delete( ).
    li_template->if_w3_api_object~save( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: ls_name TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    cl_w3_api_template=>if_w3_api_template~load(
      EXPORTING
        p_template_name     = ls_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc = 1.
      rv_bool = abap_false.
    ELSEIF sy-subrc <> 0.
      _raise 'error from w3_api_template~load'.
    ELSE.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    _raise 'todo, IATU, jump'.
  ENDMETHOD.                    "lif_object~jump

ENDCLASS.                    "lcl_object_iatu IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.


    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
          ls_tpara TYPE tpara.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
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

    io_xml->add( iv_name = 'DD04V'
                 ig_data = ls_dd04v ).
    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname,
          ls_tpara TYPE tpara.


    io_xml->read( EXPORTING iv_name = 'DD04V'
                  CHANGING cg_data = ls_dd04v ).
    io_xml->read( EXPORTING iv_name = 'TPARA'
                  CHANGING cg_data = ls_tpara ).

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
CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_program.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    DATA mv_skip_testclass TYPE abap_bool.

    METHODS deserialize_abap
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS deserialize_textpool
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.

    METHODS deserialize_docu
      IMPORTING io_xml TYPE REF TO lcl_xml_input
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

    METHODS read_include
      IMPORTING is_clskey        TYPE seoclskey
                iv_type          TYPE seop_include_ext_app
      RETURNING VALUE(rt_source) TYPE seop_source_string.

    METHODS serialize_testclasses
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_macros
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.

    METHODS remove_signatures
      CHANGING ct_source TYPE ty_string_tt.

    METHODS reduce
      CHANGING ct_source TYPE ty_string_tt.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_intf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_intf DEFINITION INHERITING FROM lcl_object_clas FINAL.
* todo, CLAS + INTF to be refactored, see:
* https://github.com/larshp/abapGit/issues/21
ENDCLASS.                    "lcl_object_intf DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

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

  ENDMETHOD.                    "lif_object~exists

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

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_imp ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_local

  METHOD serialize_locals_def.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_def ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_locals_def

  METHOD read_include.

    DATA: ls_include TYPE progstruc.


    ASSERT iv_type = seop_ext_class_locals_def
      OR iv_type = seop_ext_class_locals_imp
      OR iv_type = seop_ext_class_macros
      OR iv_type = seop_ext_class_testclasses.

    ls_include-rootname = is_clskey-clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_type(1).
    ls_include-codea = iv_type+1(4).

* it looks like there is an issue in function module SEO_CLASS_GET_INCLUDE_SOURCE
* on 750 kernels, where the READ REPORT without STATE addition does not
* return the active version, this method is a workaround for this issue
    READ REPORT ls_include INTO rt_source STATE 'A'.

  ENDMETHOD.

  METHOD serialize_testclasses.

    DATA: lv_line1 LIKE LINE OF rt_source,
          lv_line2 LIKE LINE OF rt_source.


    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_testclasses ).

* when creating classes in Eclipse it automatically generates the
* testclass include, but it is not needed, so skip to avoid
* creating an extra file in the repository.
* Also remove it if the content is manually removed, but
* the class still thinks it contains tests
    mv_skip_testclass = abap_false.
    IF lines( rt_source ) = 2.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      READ TABLE rt_source INDEX 2 INTO lv_line2.
      ASSERT sy-subrc = 0.
      IF lv_line1(3) = '*"*' AND lv_line2 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 1.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      IF lv_line1(3) = '*"*' OR lv_line1 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 0.
      mv_skip_testclass = abap_true.
    ENDIF.

  ENDMETHOD.                    "serialize_test

  METHOD serialize_macros.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_macros ).

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

    serialize_xml( io_xml ).

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

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        io_xml->add( iv_name = 'VSEOCLASS'
                     ig_data = ls_vseoclass ).

        lv_cp = cl_oo_classname_service=>get_classpool_name( ls_clskey-clsname ).
        READ TEXTPOOL lv_cp INTO lt_tpool LANGUAGE mv_language. "#EC CI_READ_REP
        io_xml->add( iv_name = 'TPOOL'
                     ig_data = add_tpool( lt_tpool ) ).
      WHEN 'INTF'.
        io_xml->add( iv_name = 'VSEOINTERF'
                     ig_data = ls_vseointerf ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    lv_object = ls_clskey-clsname.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = mv_language
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
      io_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

  ENDMETHOD.                    "serialize_xml

  METHOD lif_object~deserialize.

* function group SEOK
* function group SEOQ
* function group SEOP
* class CL_OO_CLASSNAME_SERVICE
* class CL_OO_SOURCE

    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    IF ms_item-obj_type = 'CLAS'.
      deserialize_textpool( io_xml ).
    ENDIF.

    deserialize_docu( io_xml ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.


    io_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lt_lines[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'CL'
        langu    = mv_language
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

    DATA: lv_cp        TYPE program,
          lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE ty_tpool_tt,
          lt_tpool     TYPE textpool_table.


    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lt_tpool[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.
    lv_cp = cl_oo_classname_service=>get_classpool_name( lv_clsname ).

    INSERT TEXTPOOL lv_cp
      FROM lt_tpool
      LANGUAGE mv_language
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
        io_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                      CHANGING cg_data = ls_vseoclass ).

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
        io_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                      CHANGING cg_data = ls_vseointerf ).

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
        ASSERT 0 = 1.
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
    ALIASES mo_files FOR lif_object~mo_files.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    TRY.
        get_url_for_io( ).
        rv_bool = abap_true.
      CATCH lcx_not_found.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "lif_object~exists

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

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'FOLDER'
                 ig_data = lv_folder ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lv_content  TYPE xstring,
          lv_filename TYPE skwf_filnm,
          lv_io       TYPE sdok_docid,
          ls_skwf_io  TYPE skwf_io,
          li_api      TYPE REF TO if_mr_api.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'FOLDER'
                  CHANGING cg_data = lv_folder ).

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
    _raise 'todo, SMIM, jump'.
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
    ALIASES mo_files FOR lif_object~mo_files.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: ls_icfservice TYPE icfservice.


    read( IMPORTING es_icfservice = ls_icfservice ).
    rv_bool = boolc( NOT ls_icfservice IS INITIAL ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
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

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'ICFSERVICE'
                 ig_data = ls_icfservice ).
    io_xml->add( iv_name = 'ICFDOCU'
                 ig_data = ls_icfdocu ).
    io_xml->add( iv_name = 'ICFHANDLER_TABLE'
                 ig_data = lt_icfhandler ).

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
        icf_langu         = mv_language
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

    DATA: ls_icfservice TYPE icfservice,
          ls_read       TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lt_icfhandler TYPE TABLE OF icfhandler.


    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'ICFSERVICE'
                  CHANGING cg_data = ls_icfservice ).
    io_xml->read( EXPORTING iv_name = 'ICFDOCU'
                  CHANGING cg_data = ls_icfdocu ).
    io_xml->read( EXPORTING iv_name = 'ICFHANDLER_TABLE'
                  CHANGING cg_data = lt_icfhandler ).

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
        doculang                  = mv_language
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
        doculang                  = mv_language
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
    _raise 'todo, SICF, jump'.
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
    ALIASES mo_files FOR lif_object~mo_files.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_stylename TYPE stxsadm-stylename.


    SELECT SINGLE stylename FROM stxsadm INTO lv_stylename
      WHERE stylename = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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

    DATA: lv_style_name TYPE tdssname,
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
        i_style_language         = mv_language
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

    io_xml->add( iv_name = 'HEADER'
                 ig_data = ls_header ).
    io_xml->add( ig_data = lt_paragraphs
                 iv_name = 'SSFPARAS' ).
    io_xml->add( ig_data = lt_strings
                 iv_name = 'SSFSTRINGS' ).
    io_xml->add( ig_data = lt_tabstops
                 iv_name = 'STXSTAB' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see fm SSF_UPLOAD_STYLE

    DATA: ls_header     TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'SSFPARAS'
                  CHANGING cg_data = lt_paragraphs ).
    io_xml->read( EXPORTING iv_name = 'SSFSTRINGS'
                  CHANGING cg_data = lt_strings ).
    io_xml->read( EXPORTING iv_name = 'STXSTAB'
                  CHANGING cg_data = lt_tabstops ).

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_suso DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdyn DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdyn DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_component_name TYPE wdy_component-component_name.


    SELECT SINGLE component_name FROM wdy_component
      INTO lv_component_name
      WHERE component_name = ms_item-obj_name
      AND version = 'A'.                                "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
          ASSERT 0 = 1.
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

    DATA: ls_component TYPE wdy_component_metadata.


    ls_component = read( ).

    io_xml->add( iv_name = 'COMPONENT'
                 ig_data = ls_component ).
    io_xml->add( ig_data = mt_components
                 iv_name = 'COMPONENTS' ).
    io_xml->add( ig_data = mt_sources
                 iv_name = 'SOURCES' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: ls_component TYPE wdy_component_metadata.

    FIELD-SYMBOLS: <ls_view>       LIKE LINE OF ls_component-view_metadata,
                   <ls_controller> LIKE LINE OF ls_component-ctlr_metadata.


    io_xml->read( EXPORTING iv_name = 'COMPONENT'
                  CHANGING cg_data = ls_component ).
    io_xml->read( EXPORTING iv_name  = 'COMPONENTS'
                  CHANGING cg_data = mt_components ).
    io_xml->read( EXPORTING iv_name  = 'SOURCES'
                  CHANGING cg_data = mt_sources ).

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
*       CLASS lcl_object_wdya DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wdya DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

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

  ENDMETHOD.                    "lif_object~exists

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

    DATA: ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.


    read( IMPORTING es_app        = ls_app
                    et_properties = lt_properties ).

    io_xml->add( iv_name = 'APP'
                 ig_data = ls_app ).
    io_xml->add( iv_name = 'PROPERTIES'
                 ig_data = lt_properties ).

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

    DATA: ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.


    io_xml->read( EXPORTING iv_name = 'APP'
                  CHANGING cg_data = ls_app ).
    io_xml->read( EXPORTING iv_name = 'PROPERTIES'
                  CHANGING cg_data = lt_properties ).

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_susc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_suso IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_suso IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_objct TYPE tobj-objct.


    SELECT SINGLE objct FROM tobj INTO lv_objct
      WHERE objct = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: ls_tobj       TYPE tobj,
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
      AND langu = mv_language.                          "#EC CI_GENBUFF
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

    io_xml->add( iv_name = 'TOBJ'
                 ig_data = ls_tobj ).
    io_xml->add( iv_name = 'TOBJT'
                 ig_data = ls_tobjt ).
    io_xml->add( iv_name = 'TOBJVORFLG'
                 ig_data = ls_tobjvorflg ).
    io_xml->add( ig_data = lt_tactz
                 iv_name = 'TACTZ' ).
    io_xml->add( ig_data = lt_tobjvordat
                 iv_name = 'TOBJVORDAT' ).
    io_xml->add( ig_data = lt_tobjvor
                 iv_name = 'TOBJVOR' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see function group SUSA

    DATA: lv_objectname TYPE e071-obj_name,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    ASSERT NOT ms_item-obj_name IS INITIAL.

    io_xml->read( EXPORTING iv_name = 'TOBJ'
                  CHANGING cg_data = ls_tobj ).
    ls_tobj-bname = sy-uname.
    io_xml->read( EXPORTING iv_name = 'TOBJT'
                  CHANGING cg_data = ls_tobjt ).
    io_xml->read( EXPORTING iv_name = 'TOBJVORFLG'
                  CHANGING cg_data = ls_tobjvorflg ).
    io_xml->read( EXPORTING iv_name = 'TACTZ'
                  CHANGING  cg_data = lt_tactz ).
    io_xml->read( EXPORTING iv_name = 'TOBJVORDAT'
                  CHANGING  cg_data = lt_tobjvordat ).
    io_xml->read( EXPORTING iv_name = 'TOBJVOR'
                  CHANGING  cg_data = lt_tobjvor ).

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_oclss TYPE tobc-oclss.


    SELECT SINGLE oclss FROM tobc INTO lv_oclss
      WHERE oclss = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: ls_tobc  TYPE tobc,
          ls_tobct TYPE tobct.


    SELECT SINGLE * FROM tobc INTO ls_tobc
      WHERE oclss = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tobct INTO ls_tobct
      WHERE oclss = ms_item-obj_name
      AND langu = mv_language.
    IF sy-subrc <> 0.
      _raise 'TOBCT no english description'.
    ENDIF.

    io_xml->add( iv_name = 'TOBC'
                 ig_data = ls_tobc ).
    io_xml->add( iv_name = 'TOBCT'
                 ig_data = ls_tobct ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see function group SUSA

    DATA: ls_tobc       TYPE tobc,
          lv_objectname TYPE e071-obj_name,
          ls_tobct      TYPE tobct.


    io_xml->read( EXPORTING iv_name = 'TOBC'
                  CHANGING cg_data = ls_tobc ).
    io_xml->read( EXPORTING iv_name = 'TOBCT'
                  CHANGING cg_data = ls_tobct ).

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
    ALIASES mo_files FOR lif_object~mo_files.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    TRY.
        read( ).
        rv_bool = abap_true.
      CATCH lcx_not_found.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "lif_object~exists

  METHOD read.

    DATA: lv_typdname  TYPE rsedd0-typegroup,
          lt_psmodisrc TYPE TABLE OF smodisrc,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_ptrdir    TYPE TABLE OF trdir.


    SELECT SINGLE ddtext FROM ddtypet
      INTO ev_ddtext
      WHERE typegroup = ms_item-obj_name
      AND ddlanguage = mv_language.
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

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    TRY.
        read( IMPORTING
                ev_ddtext = lv_ddtext
                et_source = lt_source ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    io_xml->add( iv_name = 'DDTEXT'
                 ig_data = lv_ddtext ).

    mo_files->add_abap( lt_source ).

  ENDMETHOD.                    "serialize

  METHOD create.

    DATA: lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.

    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

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

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    io_xml->read( EXPORTING iv_name = 'DDTEXT'
                  CHANGING cg_data = lv_ddtext ).

    lt_source = mo_files->read_abap( ).

    create( iv_ddtext   = lv_ddtext
            it_source   = lt_source
            iv_devclass = iv_package ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'G'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4
        dialog_needed        = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      _raise 'error deleting TYPE'.
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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_para DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_para IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_para IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_paramid TYPE tpara-paramid.


    SELECT SINGLE paramid FROM tpara INTO lv_paramid
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tparat INTO ls_tparat
      WHERE paramid = ms_item-obj_name
      AND sprache = mv_language.                        "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'PARA no english description'.
    ENDIF.

    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).
    io_xml->add( iv_name = 'TPARAT'
                 ig_data = ls_tparat ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see fm RS_PARAMETER_ADD and RS_PARAMETER_EDIT

    DATA: lv_mode   TYPE c LENGTH 1,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc = 0.
      lv_mode = 'M'.
    ELSE.
      lv_mode = 'I'.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TPARA'
                  CHANGING cg_data = ls_tpara ).
    io_xml->read( EXPORTING iv_name = 'TPARAT'
                  CHANGING cg_data = ls_tparat ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ms_item-obj_name
        object_class        = 'PARA'
        mode                = lv_mode
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = mv_language
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
*       CLASS lcl_object_splo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_splo DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_splo DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_splo IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_splo IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~serialize.

    DATA: ls_tsp1t TYPE tsp1t,
          ls_tsp1d TYPE tsp1d,
          ls_tsp0p TYPE tsp0p.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tsp1t INTO ls_tsp1t
      WHERE papart = ms_item-obj_name
      AND spras = mv_language.            "#EC CI_GENBUFF "#EC CI_SUBRC
    SELECT SINGLE * FROM tsp1d INTO ls_tsp1d
      WHERE papart = ms_item-obj_name.                    "#EC CI_SUBRC
    SELECT SINGLE * FROM tsp0p INTO ls_tsp0p
      WHERE pdpaper = ms_item-obj_name.                   "#EC CI_SUBRC

    CLEAR: ls_tsp1d-chgname1,
           ls_tsp1d-chgtstmp1,
           ls_tsp1d-chgsaprel1,
           ls_tsp1d-chgsapsys1.

    io_xml->add( iv_name = 'TSPLT'
                 ig_data = ls_tsp1t ).
    io_xml->add( iv_name = 'TSPLD'
                 ig_data = ls_tsp1d ).
    io_xml->add( iv_name = 'TSP0P'
                 ig_data = ls_tsp0p ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lv_obj_name TYPE e071-obj_name,
          ls_tsp1t    TYPE tsp1t,
          ls_tsp1d    TYPE tsp1d,
          ls_tsp0p    TYPE tsp0p.


    io_xml->read( EXPORTING iv_name = 'TSPLT'
                  CHANGING cg_data = ls_tsp1t ).
    io_xml->read( EXPORTING iv_name = 'TSPLD'
                  CHANGING cg_data = ls_tsp1d ).
    io_xml->read( EXPORTING iv_name = 'TSP0P'
                  CHANGING cg_data = ls_tsp0p ).

    MODIFY tsp1t FROM ls_tsp1t.                           "#EC CI_SUBRC
    MODIFY tsp1d FROM ls_tsp1d.                           "#EC CI_SUBRC
    MODIFY tsp0p FROM ls_tsp0p.                           "#EC CI_SUBRC

    lv_obj_name = ms_item-obj_name.

    CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
      EXPORTING
        wi_e071_pgmid     = 'R3TR'
        wi_e071_object    = ms_item-obj_type
        wi_e071_obj_name  = lv_obj_name
        wi_tadir_devclass = iv_package.

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DELETE FROM tsp1t WHERE papart = ms_item-obj_name. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM tsp1d WHERE papart = ms_item-obj_name.    "#EC CI_SUBRC
    DELETE FROM tsp0p WHERE pdpaper = ms_item-obj_name.   "#EC CI_SUBRC

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry = abap_true
        wi_tadir_pgmid        = 'R3TR'
        wi_tadir_object       = ms_item-obj_type
        wi_tadir_obj_name     = ms_item-obj_name
        wi_test_modus         = abap_false.

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_papart TYPE tsp1d-papart.


    SELECT SINGLE papart INTO lv_papart FROM tsp1d
      WHERE papart = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    _raise 'todo, jump, SPLO'.
  ENDMETHOD.                    "lif_object~jump

ENDCLASS.                    "lcl_object_splo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ssfo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssfo DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssfo IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_formname TYPE stxfadm-formname.


    SELECT SINGLE formname FROM stxfadm INTO lv_formname
      WHERE formname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

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
          lv_name     TYPE string,
          li_node     TYPE REF TO if_ixml_node,
          li_element  TYPE REF TO if_ixml_element,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          li_attr     TYPE REF TO if_ixml_named_node_map,
          lv_formname TYPE tdsfname,
          li_ixml     TYPE REF TO if_ixml,
          li_xml_doc  TYPE REF TO if_ixml_document.


    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    CREATE OBJECT lo_sf.
    lv_formname = ms_item-obj_name. " convert type
    TRY.
        lo_sf->load( im_formname = lv_formname
                     im_language = '' ).
      CATCH cx_ssf_fb.
* the smartform is not present in system, or other error occured
        RETURN.
    ENDTRY.

    lo_sf->xml_download( EXPORTING parent   = li_xml_doc
                         CHANGING  document = li_xml_doc ).

    li_iterator = li_xml_doc->create_iterator( ).
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

    li_element = li_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ). "#EC NOTEXT
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).              "#EC NOTEXT

    io_xml->set_raw( li_xml_doc->get_root_element( ) ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.
* see function module FB_UPLOAD_FORM

    DATA: li_node     TYPE REF TO if_ixml_node,
          lv_formname TYPE tdsfname,
          lv_name     TYPE string,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lo_res      TYPE REF TO cl_ssf_fb_smart_form.


    CREATE OBJECT lo_sf.

* set "created by" and "changed by" to current user
    li_iterator = io_xml->get_raw( )->create_iterator( ).
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

* todo, iv_package?
    lv_formname = ms_item-obj_name.
    lo_sf->enqueue( suppress_corr_check = space
                    master_language     = mv_language
                    mode                = 'INSERT'
                    formname            = lv_formname ).

    lo_sf->xml_upload( EXPORTING dom      = io_xml->get_raw( )
                                 formname = lv_formname
                                 language = mv_language
                       CHANGING  sform    = lo_res ).

    lo_res->store( im_formname = lo_res->header-formname
                   im_language = mv_language
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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tabl IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.


    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
        langu         = mv_language
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

* XML output assumes correct field content
      IF <ls_dd03p>-routputlen = '      '.
        CLEAR <ls_dd03p>-routputlen.
      ENDIF.
    ENDLOOP.

    io_xml->add( iv_name = 'DD02V'
                 ig_data = ls_dd02v ).
    io_xml->add( iv_name = 'DD09L'
                 ig_data = ls_dd09l ).
    io_xml->add( ig_data = lt_dd03p
                 iv_name = 'DD03P_TABLE' ).
    io_xml->add( ig_data = lt_dd05m
                 iv_name = 'DD05M_TABLE' ).
    io_xml->add( ig_data = lt_dd08v
                 iv_name = 'DD08V_TABLE' ).
    io_xml->add( iv_name = 'DD12V'
                 ig_data = lt_dd12v ).
    io_xml->add( iv_name = 'DD17V'
                 ig_data = lt_dd17v ).
    io_xml->add( ig_data = lt_dd35v
                 iv_name = 'DD35V_TALE' ).
    io_xml->add( iv_name = 'DD36M'
                 ig_data = lt_dd36m ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name      TYPE ddobjname,
          lv_tname     TYPE trobj_name,
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


    io_xml->read( EXPORTING iv_name = 'DD02V'
                  CHANGING cg_data = ls_dd02v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                  CHANGING cg_data = lt_dd03p ).
    io_xml->read( EXPORTING iv_name = 'DD05M_TABLE'
                  CHANGING cg_data = lt_dd05m ).
    io_xml->read( EXPORTING iv_name = 'DD08V_TABLE'
                  CHANGING cg_data = lt_dd08v ).
    io_xml->read( EXPORTING iv_name = 'DD12V'
                  CHANGING cg_data = lt_dd12v ).
    io_xml->read( EXPORTING iv_name = 'DD17V'
                  CHANGING cg_data = lt_dd17v ).
    io_xml->read( EXPORTING iv_name = 'DD35V_TALE'
                  CHANGING cg_data = lt_dd35v ).
    io_xml->read( EXPORTING iv_name = 'DD36M'
                  CHANGING cg_data = lt_dd36m ).

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
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS deserialize_badi
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.
    METHODS deserialize_hook
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_badi
      IMPORTING io_xml      TYPE REF TO lcl_xml_output
                iv_tool     TYPE enhtooltype
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   lcx_exception.
    METHODS serialize_hook
      IMPORTING io_xml      TYPE REF TO lcl_xml_output
                iv_tool     TYPE enhtooltype
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_enho DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

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
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_enh_id   TYPE enhname,
          lv_tool     TYPE enhtooltype,
          li_enh_tool TYPE REF TO if_enh_tool.


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
        serialize_badi( io_xml = io_xml
                        iv_tool = lv_tool
                        ii_enh_tool = li_enh_tool ).
      WHEN cl_enh_tool_hook_impl=>tooltype.
        serialize_hook( io_xml = io_xml
                        iv_tool = lv_tool
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

    DATA: lv_tool TYPE enhtooltype.

    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    CASE lv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        deserialize_badi( io_xml     = io_xml
                          iv_package = iv_package ).
      WHEN cl_enh_tool_hook_impl=>tooltype.
        deserialize_hook( io_xml     = io_xml
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


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'SPOT_NAME'
                  CHANGING cg_data = lv_spot_name ).
    io_xml->read( EXPORTING iv_name = 'IMPL'
                  CHANGING cg_data = lt_impl ).

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

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data = ls_original_object ).
    io_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data = lt_enhancements ).

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
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    io_xml->add( iv_name = 'TOOL'
                 ig_data = iv_tool ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( iv_name = 'SPOT_NAME'
                 ig_data = lv_spot_name ).
    io_xml->add( iv_name = 'IMPL'
                 ig_data = lt_impl ).

  ENDMETHOD.                    "serialize_badi

  METHOD serialize_hook.

    DATA: lv_shorttext       TYPE string,
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

    io_xml->add( iv_name = 'TOOL'
                 ig_data = iv_tool ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( ig_data = ls_original_object
                 iv_name = 'ORIGINAL_OBJECT' ).
    io_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enqu IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE TABLE OF dd27p.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
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

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( ig_data = lt_dd26e
                 iv_name = 'DD26E_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE TABLE OF dd27p.


    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD26E_TABLE'
                  CHANGING cg_data = lt_dd26e ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_shlp IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_shlpname TYPE dd30l-shlpname.


    SELECT SINGLE shlpname FROM dd30l INTO lv_shlpname
      WHERE shlpname = ms_item-obj_name
      AND as4local = 'A'.                               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
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

    io_xml->add( iv_name = 'DD30V'
                 ig_data = ls_dd30v ).
    io_xml->add( ig_data = lt_dd31v
                 iv_name = 'DD31V_TABLE' ).
    io_xml->add( ig_data = lt_dd32p
                 iv_name = 'DD32P_TABLE' ).
    io_xml->add( ig_data = lt_dd33v
                 iv_name = 'DD33V_TABLE' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    io_xml->read( EXPORTING iv_name = 'DD30V'
                  CHANGING cg_data = ls_dd30v ).
    io_xml->read( EXPORTING iv_name = 'DD31V_TABLE'
                  CHANGING cg_data = lt_dd31v ).
    io_xml->read( EXPORTING iv_name = 'DD32P_TABLE'
                  CHANGING cg_data = lt_dd32p ).
    io_xml->read( EXPORTING iv_name = 'DD33V_TABLE'
                  CHANGING cg_data = lt_dd33v ).

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
*       CLASS lcl_object_tran DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tran DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.

    CONSTANTS: c_oo_program(9)    VALUE '\PROGRAM=',
               c_oo_class(7)      VALUE '\CLASS=',
               c_oo_method(8)     VALUE '\METHOD=',
               c_oo_tcode         TYPE tcode VALUE 'OS_APPLICATION',
               c_oo_frclass(30)   VALUE 'CLASS',
               c_oo_frmethod(30)  VALUE 'METHOD',
               c_oo_frupdtask(30) VALUE 'UPDATE_MODE',
               c_oo_synchron      VALUE 'S',
               c_oo_asynchron     VALUE 'U',
               c_true             TYPE c VALUE 'X',
               c_false            TYPE c VALUE space.

    METHODS:
      split_parameters
        CHANGING ct_rsparam TYPE s_param
                 cs_rsstcd  TYPE rsstcd
                 cs_tstcp   TYPE tstcp
                 cs_tstc    TYPE tstc,
      split_parameters_comp
        IMPORTING iv_type  TYPE any
                  iv_param TYPE any
        CHANGING  cg_value TYPE any.

ENDCLASS.                    "lcl_object_TRAN DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_msag IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tran IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD split_parameters_comp.
    DATA: lv_off TYPE i.

    IF iv_param CS iv_type.
      lv_off = sy-fdpos + strlen( iv_type ).
      cg_value = iv_param+lv_off.
      IF cg_value CA '\'.
        CLEAR cg_value+sy-fdpos.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "split_parameters_comp

  METHOD split_parameters.
* see subroutine split_parameters in include LSEUKF01

    DATA: lv_off       TYPE i,
          lv_param_beg TYPE i,
          lv_length    TYPE i,
          ls_param     LIKE LINE OF ct_rsparam.

    FIELD-SYMBOLS <lg_f> TYPE any.


    CLEAR cs_rsstcd-s_vari.

    IF cs_tstcp-param(1) = '\'.             " OO-Transaktion ohne FR
      split_parameters_comp( EXPORTING iv_type = c_oo_program
                                       iv_param = cs_tstcp-param
                             CHANGING  cg_value = cs_tstc-pgmna ).
      split_parameters_comp( EXPORTING iv_type = c_oo_class
                                       iv_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-classname ).
      split_parameters_comp( EXPORTING iv_type = c_oo_method
                                       iv_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-method ).

      IF NOT cs_tstc-pgmna IS INITIAL.
        cs_rsstcd-s_local = c_true.
      ENDIF.
      RETURN.
    ELSEIF cs_tstcp-param(1) = '@'.         " Transaktionsvariante
      cs_rsstcd-s_vari = c_true.
      IF cs_tstcp-param(2) = '@@'.
        cs_rsstcd-s_ind_vari = c_true.
        lv_off = 2.
      ELSE.
        CLEAR cs_rsstcd-s_ind_vari.
        lv_off = 1.
      ENDIF.
      sy-fdpos = sy-fdpos - lv_off.
      IF sy-fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+lv_off(sy-fdpos).
        sy-fdpos = sy-fdpos + 1 + lv_off.
        cs_rsstcd-variant = cs_tstcp-param+sy-fdpos.
      ENDIF.
    ELSEIF cs_tstcp-param(1) = '/'.
      cs_rsstcd-st_tcode = c_true.
      cs_rsstcd-st_prog  = space.
      IF cs_tstcp-param+1(1) = '*'.
        cs_rsstcd-st_skip_1 = c_true.
      ELSE.
        CLEAR cs_rsstcd-st_skip_1.
      ENDIF.
      lv_param_beg = sy-fdpos + 1.
      sy-fdpos = sy-fdpos - 2.
      IF sy-fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+2(sy-fdpos).
      ENDIF.
      SHIFT cs_tstcp-param BY lv_param_beg PLACES.
    ELSE.
      cs_rsstcd-st_tcode = space.
      cs_rsstcd-st_prog  = c_true.
    ENDIF.

    DO 254 TIMES.
      IF cs_tstcp-param = space.
        EXIT.
      ENDIF.
      CLEAR ls_param.
      IF cs_tstcp-param CA '='.
        CHECK sy-fdpos <> 0.
        ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
        ls_param-field = <lg_f>.
        IF ls_param-field(1) = space.
          SHIFT ls_param-field.
        ENDIF.
        sy-fdpos = sy-fdpos + 1.
        SHIFT cs_tstcp-param BY sy-fdpos PLACES.
        IF cs_tstcp-param CA ';'.
          IF sy-fdpos <> 0.
            ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
            ls_param-value = <lg_f>.
            IF ls_param-value(1) = space.
              SHIFT ls_param-value.
            ENDIF.
          ENDIF.
          sy-fdpos = sy-fdpos + 1.
          SHIFT cs_tstcp-param BY sy-fdpos PLACES.
          APPEND ls_param TO ct_rsparam.
        ELSE.
          lv_length = strlen( cs_tstcp-param ).
          CHECK lv_length > 0.
          ASSIGN cs_tstcp-param(lv_length) TO <lg_f>.
          ls_param-value = <lg_f>.
          IF ls_param-value(1) = space.
            SHIFT ls_param-value.
          ENDIF.
          lv_length = lv_length + 1.
          SHIFT cs_tstcp-param BY lv_length PLACES.
          APPEND ls_param TO ct_rsparam.
        ENDIF.
      ENDIF.
    ENDDO.
* oo-Transaktion mit Framework
    IF cs_rsstcd-call_tcode = c_oo_tcode.
      cs_rsstcd-s_trframe = c_true.
      LOOP AT ct_rsparam INTO ls_param.
        CASE ls_param-field.
          WHEN c_oo_frclass.
            cs_rsstcd-classname = ls_param-value.
          WHEN c_oo_frmethod.
            cs_rsstcd-method   = ls_param-value.
          WHEN c_oo_frupdtask.
            IF ls_param-value = c_oo_synchron.
              cs_rsstcd-s_upddir  = c_true.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_false.
            ELSEIF ls_param-value = c_oo_asynchron.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_true.
              cs_rsstcd-s_updlok  = c_false.
            ELSE.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_true.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "split_parameters

  METHOD lif_object~exists.

    DATA: lv_tcode TYPE tstc-tcode.


    SELECT SINGLE tcode FROM tstc INTO lv_tcode
      WHERE tcode = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
        OTHERS                = 4
        ##fm_subrc_ok
      .                                                   "#EC CI_SUBRC

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

    DATA: lv_dynpro       TYPE d020s-dnum,
          ls_tstc         TYPE tstc,
          lv_type         TYPE rglif-docutype,
          ls_tstct        TYPE tstct,
          ls_tstcc        TYPE tstcc,
          ls_tstcp        TYPE tstcp,
          lt_param_values TYPE TABLE OF rsparam,
          ls_rsstcd       TYPE rsstcd.

    io_xml->read( EXPORTING iv_name = 'TSTC'
                  CHANGING cg_data = ls_tstc ).
    io_xml->read( EXPORTING iv_name = 'TSTCC'
                  CHANGING cg_data = ls_tstcc ).
    io_xml->read( EXPORTING iv_name = 'TSTCT'
                  CHANGING cg_data = ls_tstct ).
    io_xml->read( EXPORTING iv_name = 'TSTCP'
                  CHANGING cg_data = ls_tstcp ).

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

    IF ls_tstcp IS NOT INITIAL.
      split_parameters(
        CHANGING
          ct_rsparam = lt_param_values
          cs_rsstcd  = ls_rsstcd
          cs_tstcp   = ls_tstcp
          cs_tstc    = ls_tstc ).
    ENDIF.

    CALL FUNCTION 'RPY_TRANSACTION_INSERT'
      EXPORTING
        transaction             = ls_tstc-tcode
        program                 = ls_tstc-pgmna
        dynpro                  = lv_dynpro
        language                = mv_language
        development_class       = iv_package
        transaction_type        = lv_type
        shorttext               = ls_tstct-ttext
        called_transaction      = ls_rsstcd-call_tcode
        called_transaction_skip = ls_rsstcd-st_skip_1
        variant                 = ls_rsstcd-variant
        cl_independend          = ls_rsstcd-s_ind_vari
        html_enabled            = ls_tstcc-s_webgui
        java_enabled            = ls_tstcc-s_platin
        wingui_enabled          = ls_tstcc-s_win32
      TABLES
        param_values            = lt_param_values
      EXCEPTIONS
        cancelled               = 1
        already_exist           = 2
        permission_error        = 3
        name_not_allowed        = 4
        name_conflict           = 5
        illegal_type            = 6
        object_inconsistent     = 7
        db_access_error         = 8
        OTHERS                  = 9.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_INSERT'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          lt_tcodes      TYPE TABLE OF tstc,
          ls_tcode       LIKE LINE OF lt_tcodes,
          ls_tstct       TYPE tstct,
          ls_tstcp       TYPE tstcp,
          lt_gui_attr    TYPE TABLE OF tstcc,
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
    ELSEIF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_READ'.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = mv_language
      AND tcode = lv_transaction.                       "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'Transaction description not found'.
    ENDIF.

    SELECT SINGLE * FROM tstcp INTO ls_tstcp
      WHERE tcode = lv_transaction.       "#EC CI_SUBRC "#EC CI_GENBUFF

    READ TABLE lt_tcodes INDEX 1 INTO ls_tcode.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO ls_gui_attr.
    ASSERT sy-subrc = 0.

    io_xml->add( iv_name = 'TSTC'
                 ig_data = ls_tcode ).
    io_xml->add( iv_name = 'TSTCC'
                 ig_data = ls_gui_attr ).
    io_xml->add( iv_name = 'TSTCT'
                 ig_data = ls_tstct ).
    IF ls_tstcp IS NOT INITIAL.
      io_xml->add( iv_name = 'TSTCP'
                   ig_data = ls_tstcp ).
    ENDIF.

  ENDMETHOD.                    "serialize

ENDCLASS.                    "lcl_object_tran IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_tobj DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tobj DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_tobj DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_tobj IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tobj IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-late_deser = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_objectname TYPE objh-objectname,
          lv_type_pos   TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    SELECT SINGLE objectname FROM objh INTO lv_objectname
      WHERE objectname = ms_item-obj_name(lv_type_pos)
      AND objecttype = ms_item-obj_name+lv_type_pos.    "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: ls_objh     TYPE objh,
          ls_objt     TYPE objt,
          lt_objs     TYPE tt_objs,
          lt_objsl    TYPE tt_objsl,
          lt_objm     TYPE tt_objm,
          lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    ls_objh-objectname = ms_item-obj_name(lv_type_pos).
    ls_objh-objecttype = ms_item-obj_name+lv_type_pos.

    CALL FUNCTION 'CTO_OBJECT_GET'
      EXPORTING
        iv_objectname      = ls_objh-objectname
        iv_objecttype      = ls_objh-objecttype
        iv_language        = mv_language
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

    io_xml->add( iv_name = 'OBJH'
                 ig_data = ls_objh ).
    io_xml->add( iv_name = 'OBJT'
                 ig_data = ls_objt ).
    io_xml->add( iv_name = 'OBJS'
                 ig_data = lt_objs ).
    io_xml->add( iv_name = 'OBJSL'
                 ig_data = lt_objsl ).
    io_xml->add( iv_name = 'OBJM'
                 ig_data = lt_objm ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm.


    io_xml->read( EXPORTING iv_name = 'OBJH'
                  CHANGING cg_data = ls_objh ).
    io_xml->read( EXPORTING iv_name = 'OBJT'
                  CHANGING cg_data = ls_objt ).
    io_xml->read( EXPORTING iv_name = 'OBJS'
                  CHANGING cg_data = lt_objs ).
    io_xml->read( EXPORTING iv_name = 'OBJSL'
                  CHANGING cg_data = lt_objsl ).
    io_xml->read( EXPORTING iv_name = 'OBJM'
                  CHANGING cg_data = lt_objm ).

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
* TOBJ has to be saved/generated after the DDIC tables have been
* activated - fixed with late deserialization
      _raise 'error from OBJ_GENERATE'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: ls_objh     TYPE objh,
          lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    ls_objh-objectname = ms_item-obj_name(lv_type_pos).
    ls_objh-objecttype = ms_item-obj_name+lv_type_pos.

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_msag DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_msag IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_arbgb TYPE t100a-arbgb.


    SELECT SINGLE arbgb FROM t100a INTO lv_arbgb
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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

    DATA: ls_t100a TYPE t100a,
          ls_t100t TYPE t100t,
          ls_t100u TYPE t100u,
          lt_t100  TYPE TABLE OF t100.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


    io_xml->read( EXPORTING iv_name = 'T100A'
                  CHANGING cg_data = ls_t100a ).
    io_xml->read( EXPORTING iv_name = 'T100'
                  CHANGING cg_data = lt_t100 ).

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

    ls_t100a-masterlang = mv_language.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    ls_t100t-sprsl = mv_language.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_inf    TYPE t100a,
          lt_source TYPE TABLE OF t100.


    lv_msg_id = ms_item-obj_name.

    SELECT SINGLE * FROM t100a INTO ls_inf
      WHERE arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_inf-respuser.

    SELECT * FROM t100 INTO TABLE lt_source
      WHERE sprsl = mv_language
      AND arbgb = lv_msg_id
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    io_xml->add( iv_name = 'T100A'
                 ig_data = ls_inf ).
    io_xml->add( ig_data = lt_source
                 iv_name = 'T100' ).

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
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    TYPES: ty_rs38l_incl_tt TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_function,
             funcname      TYPE rs38l_fnam,
             include       TYPE progname,
             global_flag   TYPE rs38l-global,
             remote_call   TYPE rs38l-remote,
             update_task   TYPE rs38l-utask,
             short_text    TYPE tftit-stext,
             remote_basxml TYPE rs38l-basxml_enabled,
             import        TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY,
             changing      TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY,
             export        TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY,
             tables        TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY,
             exception     TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY,
             documentation TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY,
           END OF ty_function.

    TYPES: ty_function_tt TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY.

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
      RETURNING VALUE(rt_functions) TYPE ty_function_tt
      RAISING   lcx_exception.

    METHODS deserialize_functions
      IMPORTING it_functions TYPE ty_function_tt
      RAISING   lcx_exception.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.

    METHODS deserialize_xml
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_includes
      RAISING lcx_exception.

    METHODS deserialize_includes
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

*    METHODS deserialize_dynpros
*      IMPORTING it_dynpros TYPE ty_dynpro_tt
*      RAISING   lcx_exception.
*
*    METHODS deserialize_cua
*      IMPORTING is_cua TYPE ty_cua
*      RAISING   lcx_exception.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_pool  TYPE tlibg-area.


    lv_pool = ms_item-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD deserialize_functions.

    DATA: lv_include TYPE rs38l-include,
          lv_area    TYPE rs38l-area,
          lt_source  TYPE TABLE OF rssource.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF it_functions.


    LOOP AT it_functions ASSIGNING <ls_func>.

      lt_source = mo_files->read_abap( iv_extra = <ls_func>-funcname ).

      lv_area = ms_item-obj_name.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = <ls_func>-funcname
        IMPORTING
          include            = lv_include
        EXCEPTIONS
          function_not_exist = 1.
      IF sy-subrc = 0.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_func>-funcname
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
          funcname                = <ls_func>-funcname
          function_pool           = lv_area
          interface_global        = <ls_func>-global_flag
          remote_call             = <ls_func>-remote_call
          short_text              = <ls_func>-short_text
*         NAMESPACE               = ' ' todo
          remote_basxml_supported = <ls_func>-remote_basxml
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = <ls_func>-import
          export_parameter        = <ls_func>-export
          tables_parameter        = <ls_func>-tables
          changing_parameter      = <ls_func>-changing
          exception_list          = <ls_func>-exception
          parameter_docu          = <ls_func>-documentation
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
                                   iv_name = <ls_func>-funcname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_functions

  METHOD deserialize_includes.

    DATA: lo_xml       TYPE REF TO lcl_xml_input,
          ls_progdir   TYPE ty_progdir,
          lt_includes  TYPE rso_t_objnm,
          lt_tpool     TYPE textpool_table,
          lt_tpool_ext TYPE ty_tpool_tt,
          lt_source    TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    io_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      lt_source = mo_files->read_abap( iv_extra = <lv_include> ).

      lo_xml = mo_files->read_xml( <lv_include> ).

      lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                    CHANGING cg_data = ls_progdir ).

      lo_xml->read( EXPORTING iv_name = 'TPOOL'
                    CHANGING cg_data = lt_tpool_ext ).
      lt_tpool = read_tpool( lt_tpool_ext ).

      deserialize_program( is_progdir = ls_progdir
                           it_source  = lt_source
                           it_tpool   = lt_tpool
                           iv_package = iv_package ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_includes

  METHOD deserialize_xml.

    DATA: lv_complete  TYPE rs38l-area,
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

    io_xml->read( EXPORTING iv_name = 'AREAT'
                  CHANGING cg_data = lv_areat ).
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

    DATA: lt_functab  TYPE ty_rs38l_incl_tt,
          lt_includes TYPE rso_t_objnm,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = mv_language
      AND area = ms_item-obj_name.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in TLIBT'.
    ENDIF.

    lt_functab = functions( ).
    lt_includes = includes( ).

* todo, dynpros

    io_xml->add( iv_name = 'AREAT'
                 ig_data = lv_areat ).
    io_xml->add( iv_name = 'INCLUDES'
                 ig_data = lt_includes ).

  ENDMETHOD.                    "serialize_xml

  METHOD includes.

    DATA: lv_program TYPE program,
          lv_cnam    TYPE reposrc-cnam,
          lv_tabix   LIKE sy-tabix,
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


    LOOP AT rt_includes ASSIGNING <lv_include>.
      lv_tabix = sy-tabix.

* skip SAP standard includes
      SELECT SINGLE cnam FROM reposrc INTO lv_cnam
        WHERE progname = <lv_include>
        AND r3state = 'A'
        AND cnam = 'SAP'.
      IF sy-subrc = 0.
        DELETE rt_includes INDEX lv_tabix.
        CONTINUE.
      ENDIF.

* also make sure the include exists
      SELECT SINGLE cnam FROM reposrc INTO lv_cnam
        WHERE progname = <lv_include>
        AND r3state = 'A'.
      IF sy-subrc <> 0.
        DELETE rt_includes INDEX lv_tabix.
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

    DATA:
      lt_source     TYPE TABLE OF rssource,
      lt_functab    TYPE ty_rs38l_incl_tt,
      lt_new_source TYPE rsfb_source.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF lt_functab,
                   <ls_ret>  LIKE LINE OF rt_functions.


    lt_functab = functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      APPEND INITIAL LINE TO rt_functions ASSIGNING <ls_ret>.
      MOVE-CORRESPONDING <ls_func> TO <ls_ret>.

      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = <ls_ret>-global_flag
          remote_call             = <ls_ret>-remote_call
          update_task             = <ls_ret>-update_task
          short_text              = <ls_ret>-short_text
          remote_basxml_supported = <ls_ret>-remote_basxml
        TABLES
          import_parameter        = <ls_ret>-import
          changing_parameter      = <ls_ret>-changing
          export_parameter        = <ls_ret>-export
          tables_parameter        = <ls_ret>-tables
          exception_list          = <ls_ret>-exception
          documentation           = <ls_ret>-documentation
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

    DATA: lt_functions    TYPE ty_function_tt,
          ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_xml( io_xml ).

    lt_functions = serialize_functions( ).
    io_xml->add( iv_name = 'FUNCTIONS'
                 ig_data = lt_functions ).

    serialize_includes( ).

    lv_program_name = main_name( ).
    ls_progdir = read_progdir( lv_program_name ).

    IF ls_progdir-subc = 'F'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      io_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      io_xml->add( iv_name = 'CUA'
                   ig_data = ls_cua ).
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lt_functions TYPE ty_function_tt,
          lt_dynpros   TYPE ty_dynpro_tt,
          ls_cua       TYPE ty_cua.


    deserialize_xml(
      io_xml     = io_xml
      iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'FUNCTIONS'
                  CHANGING cg_data = lt_functions ).
    deserialize_functions( lt_functions ).

    deserialize_includes(
      io_xml     = io_xml
      iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data = lt_dynpros ).
    deserialize_dynpros( lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data = ls_cua ).
    deserialize_cua( ls_cua ).

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_view IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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

    DATA: lv_name  TYPE ddobjname,
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
        langu         = mv_language
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

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( iv_name = 'DD09L'
                 ig_data = ls_dd09l ).
    io_xml->add( ig_data = lt_dd26v
                 iv_name = 'DD26V_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).
    io_xml->add( ig_data = lt_dd28j
                 iv_name = 'DD28J_TABLE' ).
    io_xml->add( ig_data = lt_dd28v
                 iv_name = 'DD28V_TABLE' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.


    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name = 'DD26V_TABLE'
                  CHANGING cg_data = lt_dd26v ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).
    io_xml->read( EXPORTING iv_name = 'DD28J_TABLE'
                  CHANGING cg_data = lt_dd28j ).
    io_xml->read( EXPORTING iv_name = 'DD28V_TABLE'
                  CHANGING cg_data = lt_dd28v ).

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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_nrob DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_object TYPE tnro-object.


    SELECT SINGLE object FROM tnro INTO lv_object
      WHERE object = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_object     TYPE tnro-object,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    lv_object = ms_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
      EXPORTING
        language          = mv_language
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

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).
    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_text ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING cg_data = ls_text ).

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
        wi_tadir_masterlang = mv_language
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
        language           = mv_language
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
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ttyp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ttyp IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_typename TYPE dd40l-typename.


    SELECT SINGLE typename FROM dd40l INTO lv_typename
      WHERE typename = ms_item-obj_name
      AND as4local = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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

    DATA: lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
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

    io_xml->add( iv_name = 'DD40V'
                 ig_data = ls_dd40v ).
    io_xml->add( iv_name = 'DD42V'
                 ig_data = lt_dd42v ).
    io_xml->add( iv_name = 'DD43V'
                 ig_data = lt_dd43v ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    io_xml->read( EXPORTING iv_name = 'DD40V'
                  CHANGING cg_data = ls_dd40v ).
    io_xml->read( EXPORTING iv_name = 'DD42V'
                  CHANGING cg_data = lt_dd42v ).
    io_xml->read( EXPORTING iv_name = 'DD43V'
                  CHANGING cg_data = lt_dd43v ).

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
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.

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

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_progname TYPE reposrc-progname.


    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

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
      LANGUAGE mv_language
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = ms_item-obj_name ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD lif_object~serialize.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = mo_files ).

  ENDMETHOD.                    "lif_serialize~serialize

  METHOD lif_object~deserialize.

    DATA: ls_progdir   TYPE ty_progdir,
          lt_tpool     TYPE textpool_table,
          lt_dynpros   TYPE ty_dynpro_tt,
          lt_tpool_ext TYPE ty_tpool_tt,
          ls_cua       TYPE ty_cua,
          lt_source    TYPE abaptxt255_tab.


    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data = ls_progdir ).
    deserialize_program( is_progdir = ls_progdir
                         it_source  = lt_source
                         it_tpool   = lt_tpool
                         iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data = lt_dynpros ).
    deserialize_dynpros( lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data = ls_cua ).
    deserialize_cua( ls_cua ).

    deserialize_textpool( lt_tpool ).

  ENDMETHOD.                    "lif_serialize~deserialize

ENDCLASS.                    "lcl_object_prog IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_vcls DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_vcls DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_vcls DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_vcls IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INCLUDE mtobjcon.
*----------------------------------------------------------------------*
*       CLASS lcl_object_vcls IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_vcls IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.
    DATA lv_vclname TYPE vcl_name.

    SELECT SINGLE vclname INTO lv_vclname FROM vcldir WHERE vclname = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_vclname      TYPE vcl_name,
          ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_vclname = ms_item-obj_name.

    CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
      EXPORTING
        vclname                = lv_vclname
      IMPORTING
        vcldir_entry           = ls_vcldir_entry
      TABLES
        vclstruc_tab           = lt_vclstruc
        vclstrudep_tab         = lt_vclstrudep
        vclmf_tab              = lt_vclmf
      EXCEPTIONS
        viewcluster_not_found  = 1
        incomplete_viewcluster = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      _raise 'error in VIEWCLUSTER_GET_DEFINITION'.
    ENDIF.

    CLEAR ls_vcldir_entry-author.

    io_xml->add( iv_name = 'VCLDIR'
                 ig_data = ls_vcldir_entry ).
    io_xml->add( iv_name = 'VLCSTRUC_TAB'
                 ig_data = lt_vclstruc ).
    io_xml->add( iv_name = 'VCLSTRUDEP_TAB'
                 ig_data = lt_vclstrudep ).
    io_xml->add( iv_name = 'VCLMF_TAB'
                 ig_data = lt_vclmf ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf,
          lv_objectname   TYPE ob_object.


    io_xml->read( EXPORTING iv_name = 'VCLDIR'
                  CHANGING cg_data = ls_vcldir_entry ).
    io_xml->read( EXPORTING iv_name = 'VLCSTRUC_TAB'
                  CHANGING cg_data = lt_vclstruc ).
    io_xml->read( EXPORTING iv_name = 'VCLSTRUDEP_TAB'
                  CHANGING cg_data = lt_vclstrudep ).
    io_xml->read( EXPORTING iv_name = 'lt_vclstrudep'
                  CHANGING cg_data = lt_vclmf ).

    ls_vcldir_entry-author = sy-uname.

    CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
      EXPORTING
        vcldir_entry   = ls_vcldir_entry
      TABLES
        vclstruc_tab   = lt_vclstruc
        vclstrudep_tab = lt_vclstrudep
        vclmf_tab      = lt_vclmf.

    lv_objectname = ls_vcldir_entry-vclname.
    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = lv_objectname
        iv_objecttype         = gc_cluster_type
        iv_maint_mode         = gc_mode_insert
        iv_devclass           = iv_package
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _raise 'error in OBJ_GENERATE for VCLS'.
    ENDIF.

*    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.
* Do the same as in VIEWCLUSTER_SAVE_DEFINITION
    DATA: lv_vclname TYPE vcl_name.


    lv_vclname = ms_item-obj_name.

    DELETE FROM vcldir WHERE vclname = lv_vclname.        "#EC CI_SUBRC
    DELETE FROM vcldirt WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstruc WHERE vclname = lv_vclname.      "#EC CI_SUBRC
    DELETE FROM vclstruct WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstrudep WHERE vclname = lv_vclname.    "#EC CI_SUBRC
    DELETE FROM vclmf WHERE vclname = lv_vclname.         "#EC CI_SUBRC

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    DATA: lv_vclname      TYPE  vcl_name.

    lv_vclname = ms_item-obj_name.
    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
      EXPORTING
        viewcluster_name             = lv_vclname
        maintenance_action           = 'S'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        viewcluster_not_found        = 3
        viewcluster_is_inconsistent  = 4
        missing_generated_function   = 5
        no_upd_auth                  = 6
        no_show_auth                 = 7
        object_not_found             = 8
        no_tvdir_entry               = 9
        no_clientindep_auth          = 10
        invalid_action               = 11
        saving_correction_failed     = 12
        system_failure               = 13
        unknown_field_in_dba_sellist = 14
        missing_corr_number          = 15
        OTHERS                       = 16.
    IF sy-subrc <> 0.
      _raise 'error in VIEWCLUSTER_MAINTENANCE_CALL'.
    ENDIF.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_vcls IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_sfsw DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfsw DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_sfsw DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_sfsw IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfsw IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: ls_tadir     TYPE tadir,
          lv_switch_id TYPE sfw_switch_id.

    lv_switch_id = ms_item-obj_name.
    IF cl_sfw_sw=>check_existence( lv_switch_id ) = abap_false.
      RETURN.
    ENDIF.

    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_switch_id TYPE sfw_switch_id,
          lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_switch_id = ms_item-obj_name.

    TRY.
        lo_switch = cl_sfw_sw=>get_switch_from_db( lv_switch_id ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        _raise 'Error from CL_SFW_SW=>GET_SWITCH'.
    ENDTRY.

    ls_header = lo_switch->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_switch->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_parent_bf = lo_switch->get_parent_bf( ).
    lt_conflicts = lo_switch->get_conflicts( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_parent_bf
                 iv_name = 'PARENT_BF' ).
    io_xml->add( ig_data = lt_conflicts
                 iv_name = 'CONFLICTS' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          lv_switch_id TYPE sfw_switch_id,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'PARENT_BF'
                  CHANGING cg_data = lt_parent_bf ).
    io_xml->read( EXPORTING iv_name = 'CONFLICTS'
                  CHANGING cg_data = lt_conflicts ).

    lv_switch_id = ms_item-obj_name.
    TRY.
        lo_switch = cl_sfw_sw=>create_switch( lv_switch_id ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        _raise 'error in CL_SFW_SW=>CREATE_SWITCH'.
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_switch->set_header_data( ls_header ).

    lo_switch->set_texts( p_32 = lv_name_32
                          p_80 = lv_name_80 ).

    lo_switch->set_parent_bf( lt_parent_bf ).
    lo_switch->set_conflicts( lt_conflicts ).

    lo_switch->save_all(
      EXCEPTIONS
        not_saved = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      _raise 'error in CL_SFW_SW->SAVE_ALL'.
    ENDIF.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_switch_id TYPE sfw_switch_id,
          lo_switch    TYPE REF TO cl_sfw_sw.


    lv_switch_id = ms_item-obj_name.
    TRY.
        lo_switch = cl_sfw_sw=>get_switch( lv_switch_id ).
        lo_switch->set_delete_flag( lv_switch_id ).
        lo_switch->save_all( ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        _raise 'Error deleting Switch'.
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'SFSW'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_sfsw IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBF DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbf DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_SFBF DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBF IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbf IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir,
          lv_bf    TYPE sfw_bfunction.

    lv_bf = ms_item-obj_name.
    IF cl_sfw_bf=>check_existence( lv_bf ) = abap_false.
      RETURN.
    ENDIF.

    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_bf                TYPE sfw_bfunction,
          lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_bf = ms_item-obj_name.

    TRY.
* make sure to clear cache, method GET_BF_FROM_DB does not exist in 702
        lo_bf = cl_sfw_bf=>get_bf( lv_bf ).
        lo_bf->free( ).
        lo_bf = cl_sfw_bf=>get_bf( lv_bf ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        _raise 'Error from CL_SFW_BF=>GET_BF'.
    ENDTRY.

    ls_header = lo_bf->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bf->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_switches = lo_bf->get_assigned_switches( ).
    lt_dependancies = lo_bf->get_excluded_bf( ).
    lo_bf->get_content_data(
      IMPORTING
        ex_sfw_bfc_kw = ls_sfw_bfc_kw
        ex_sfw_bfc_tc = ls_sfw_bfc_tc
        ex_sfw_bfc_rn = ls_sfw_bfc_rn ).
    lt_parent_bfs = lo_bf->get_parent_bfs( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_switches
                 iv_name = 'ASSIGNED_SWITCHES' ).
    io_xml->add( ig_data = lt_dependancies
                 iv_name = 'DEPENDANCIES' ).
    io_xml->add( ig_data = ls_sfw_bfc_kw
                 iv_name = 'CONTENT_KW' ).
    io_xml->add( ig_data = ls_sfw_bfc_tc
                 iv_name = 'CONTENT_TC' ).
    io_xml->add( ig_data = ls_sfw_bfc_rn
                 iv_name = 'CONTENT_RN' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_bf                TYPE sfw_bfunction,
          lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_SWITCHES'
                  CHANGING cg_data = lt_assigned_switches ).
    io_xml->read( EXPORTING iv_name = 'DEPENDANCIES'
                  CHANGING cg_data = lt_dependancies ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_KW'
                  CHANGING cg_data = ls_sfw_bfc_kw ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_TC'
                  CHANGING cg_data = ls_sfw_bfc_tc ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_RN'
                  CHANGING cg_data = ls_sfw_bfc_rn ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    lv_bf = ms_item-obj_name.
    TRY.
        lo_bf = cl_sfw_bf=>create_bf( lv_bf ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        _raise 'error in CL_SFW_BF=>CREATE_BF'.
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_bf->set_header_data( ls_header ).

    lo_bf->set_texts( p_32 = lv_name_32
                      p_80 = lv_name_80 ).

    lo_bf->set_assigned_switches( lt_assigned_switches ).
    lo_bf->set_excluded_bf( lt_dependancies ).
    lo_bf->set_content_data(
        im_sfw_bfc_kw = ls_sfw_bfc_kw
        im_sfw_bfc_rn = ls_sfw_bfc_rn
        im_sfw_bfc_tc = ls_sfw_bfc_tc ).
    lo_bf->set_parent_bfs( lt_parent_bfs ).

    lo_bf->save_all( ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_bf TYPE sfw_bfunction,
          lo_bf TYPE REF TO cl_sfw_bf.


    lv_bf = ms_item-obj_name.
    TRY.
        lo_bf = cl_sfw_bf=>get_bf( lv_bf ).
        lo_bf->set_delete_flag( lv_bf ).
        lo_bf->save_all( ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        _raise 'Error deleting BF'.
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'SFBF'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_SFBF IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbs DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.                    "lcl_object_SFBS DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbs IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir,
          lv_bfset TYPE sfw_bset.

    lv_bfset = ms_item-obj_name.
    IF cl_sfw_bfs=>check_existence( lv_bfset ) = abap_false.
      RETURN.
    ENDIF.

    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_bfset       TYPE sfw_bset,
          lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_bfset = ms_item-obj_name.

    TRY.
        lo_bfs = cl_sfw_bfs=>get_bfs( lv_bfset ).
        lo_bfs->free( ).
        lo_bfs = cl_sfw_bfs=>get_bfs( lv_bfset ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        _raise 'Error from CL_SFW_BFS=>GET_BFS'.
    ENDTRY.

    ls_header = lo_bfs->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bfs->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_bf = lo_bfs->get_assigned_bf( ).
    lt_nested_bfs = lo_bfs->get_nested_bfs( ).
    lt_parent_bfs = lo_bfs->get_nested_parent( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_bf
                 iv_name = 'ASSIGNED_BF' ).
    io_xml->add( ig_data = lt_nested_bfs
                 iv_name = 'NESTED_BFS' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_bfset       TYPE sfw_bset,
          lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_BF'
                  CHANGING cg_data = lt_assigned_bf ).
    io_xml->read( EXPORTING iv_name = 'NESTED_BFS'
                  CHANGING cg_data = lt_nested_bfs ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    lv_bfset = ms_item-obj_name.
    TRY.
        lo_bfs = cl_sfw_bfs=>create_bfs( lv_bfset ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        _raise 'error in CL_SFW_BFS=>CREATE_BFS'.
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_bfs->set_header_data( ls_header ).

    lo_bfs->set_texts( p_32 = lv_name_32
                       p_80 = lv_name_80 ).

    lo_bfs->set_assigned_bf( lt_assigned_bf ).
    lo_bfs->set_assigned_bfs( lt_nested_bfs ).
    lo_bfs->set_nested_parent( lt_parent_bfs ).

    lo_bfs->save_all( ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_bfset TYPE sfw_bset,
          lo_bfs   TYPE REF TO cl_sfw_bfs.


    lv_bfset = ms_item-obj_name.
    TRY.
        lo_bfs = cl_sfw_bfs=>get_bfs( lv_bfset ).
        lo_bfs->set_delete_flag( lv_bfset ).
        lo_bfs->save_all( ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        _raise 'Error deleting BF'.
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'SFBS'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_SFBS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3SUPER DEFINITION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (super class)
*----------------------------------------------------------------------*
CLASS lcl_object_w3super DEFINITION INHERITING FROM lcl_objects_super ABSTRACT.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS init_key RETURNING VALUE(rs_key) TYPE wwwdatatab.

ENDCLASS. "lcl_object_W3SUPER DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3SUPER IMPLEMENTATION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (super class)
*----------------------------------------------------------------------*
CLASS lcl_object_w3super IMPLEMENTATION.

  METHOD init_key.
    rs_key-relid = ms_item-obj_type+2(2).
    rs_key-objid = ms_item-obj_name.
  ENDMETHOD.                    " init_key

  METHOD lif_object~jump.
    " No idea how to just to SMW0
    _raise 'Please go to SMW0 for W3MI object'.
  ENDMETHOD.                    "jump

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx EXISTS
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~exists.
    DATA ls_key   TYPE wwwdatatab.

    ls_key = init_key( ).

    SELECT SINGLE objid INTO ls_key-objid
      FROM wwwdata
      WHERE relid = ls_key-relid
      AND   objid = ls_key-objid
      AND   srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.                    "lif_object~exists

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx SERIALIZE
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~serialize.
    DATA ls_key       TYPE wwwdatatab.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA ls_wwwparam  LIKE LINE OF lt_w3params.
    DATA lv_size      TYPE int4.
    DATA lv_base64str TYPE string.
    DATA lo_utility   TYPE REF TO cl_http_utility.

    ls_key = init_key( ).

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_key
      FROM wwwdata
      WHERE relid = ls_key-relid
      AND   objid = ls_key-objid
      AND   srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot read W3xx data'.
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ls_key-relid
        objid            = ls_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot read W3xx data'.
    ENDIF.

    READ TABLE lt_w3params INTO ls_wwwparam WITH KEY name = 'filesize' ##NO_TEXT.
    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot read W3xx filesize'.
    ENDIF.

    lv_size = ls_wwwparam-value.

    CASE ls_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_size
          IMPORTING
            buffer       = lv_xstring
          TABLES
            binary_tab   = lt_w3mime
          EXCEPTIONS
            failed       = 1.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
          IMPORTING
            buffer   = lv_xstring
          TABLES
            text_tab = lt_w3html
          EXCEPTIONS
            failed   = 1.
      WHEN OTHERS.
        _raise 'Wrong W3xx type'.
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot convert W3xx to xstring'.
    ENDIF.

    CREATE OBJECT lo_utility.
    lv_base64str = lo_utility->encode_x_base64( lv_xstring ).

    io_xml->add( iv_name = 'NAME'
                 ig_data = ls_key-objid ).

    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_key-text ).

    io_xml->add( iv_name = 'DATA'
                 ig_data = lv_base64str ).

    io_xml->add( iv_name = 'PARAMS'
                 ig_data = lt_w3params ).

  ENDMETHOD.                    "serialize

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx DESERIALIZE
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~deserialize.

    DATA ls_key       TYPE wwwdatatab.
    DATA lv_base64str TYPE string.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lo_utility   TYPE REF TO cl_http_utility.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lv_size      TYPE int4.
    DATA lv_tadir_obj TYPE tadir-object.

    ls_key = init_key( ).

    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING  cg_data = ls_key-text ).

    io_xml->read( EXPORTING iv_name = 'DATA'
                  CHANGING  cg_data = lv_base64str ).

    io_xml->read( EXPORTING iv_name = 'PARAMS'
                  CHANGING  cg_data = lt_w3params ).

    CREATE OBJECT lo_utility.
    lv_xstring = lo_utility->decode_x_base64( lv_base64str ).

    CASE ls_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.

        CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
          EXPORTING
            input_length  = lv_size
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime
            text_tab      = lt_w3html
          EXCEPTIONS
            failed        = 1.
        IF sy-subrc IS NOT INITIAL.
          _raise 'Cannot update W3xx params'.
        ENDIF.

        CLEAR lt_w3mime.
      WHEN OTHERS.
        _raise 'Wrong W3xx type'.
    ENDCASE.

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot update W3xx params'.
    ENDIF.

    ls_key-tdate    = sy-datum.
    ls_key-ttime    = sy-uzeit.
    ls_key-chname   = sy-uname.
    ls_key-devclass = iv_package.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot upload W3xx data'.
    ENDIF.

    CONCATENATE 'W3' ls_key-relid INTO lv_tadir_obj.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = lv_tadir_obj
        wi_tadir_devclass              = iv_package
        wi_tadir_obj_name              = ls_key-objid
        wi_test_modus                  = space
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 99.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot update TADIR for W3xx'.
    ENDIF.

  ENDMETHOD.                    "lif_object~deserialize

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx DELETE
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~delete.
    DATA ls_key TYPE wwwdatatab.

    ls_key = init_key( ).

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key               = ls_key
      EXCEPTIONS
        wrong_object_type = 1
        delete_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot delete W3xx data'.
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
      EXPORTING
        key          = ls_key
      EXCEPTIONS
        delete_error = 1.

    IF sy-subrc IS NOT INITIAL.
      _raise 'Cannot delete W3xx params'.
    ENDIF.

  ENDMETHOD.                    "lif_object~delete

ENDCLASS. "lcl_object_W3SUPER IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3MI DEFINITION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (binary data)
*----------------------------------------------------------------------*
CLASS lcl_object_w3mi DEFINITION INHERITING FROM lcl_object_w3super FINAL.
ENDCLASS.                    "lcl_object_W3MI DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3HT DEFINITION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (html data)
*----------------------------------------------------------------------*
CLASS lcl_object_w3ht DEFINITION INHERITING FROM lcl_object_w3super FINAL.
ENDCLASS.                    "lcl_object_W3HT DEFINITION

CLASS lcl_persistence_db DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      c_tabname TYPE tabname VALUE 'ZABAPGIT',
      c_lock    TYPE viewname VALUE 'EZABAPGIT'.

    TYPES: ty_type  TYPE c LENGTH 12.
    TYPES: ty_value TYPE c LENGTH 12.

    TYPES: BEGIN OF ty_content,
             type     TYPE ty_type,
             value    TYPE ty_value,
             data_str TYPE string,
           END OF ty_content,
           tt_content TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY value.

    METHODS list_by_type
      IMPORTING iv_type           TYPE ty_type
      RETURNING VALUE(rt_content) TYPE tt_content.

    METHODS list
      RETURNING VALUE(rt_content) TYPE tt_content.

    METHODS add
      IMPORTING iv_type  TYPE ty_type
                iv_value TYPE ty_content-value
                iv_data  TYPE ty_content-data_str
      RAISING   lcx_exception.

    METHODS delete
      IMPORTING iv_type  TYPE ty_type
                iv_value TYPE ty_content-value
      RAISING   lcx_exception.

    METHODS update
      IMPORTING iv_type  TYPE ty_type
                iv_value TYPE ty_content-value
                iv_data  TYPE ty_content-data_str
      RAISING   lcx_exception.

    METHODS modify
      IMPORTING iv_type  TYPE ty_type
                iv_value TYPE ty_content-value
                iv_data  TYPE ty_content-data_str
      RAISING   lcx_exception.

    METHODS read
      IMPORTING iv_type        TYPE ty_type
                iv_value       TYPE ty_content-value
      RETURNING VALUE(rv_data) TYPE ty_content-data_str
      RAISING   lcx_not_found.

    METHODS lock
      IMPORTING iv_mode  TYPE enqmode DEFAULT 'E'
                iv_type  TYPE ty_type
                iv_value TYPE ty_content-value
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_persistence_repo DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_file_checksum,
             path     TYPE string,
             filename TYPE string,
             sha1     TYPE ty_sha1,
           END OF ty_file_checksum.

    TYPES: ty_file_checksum_tt TYPE STANDARD TABLE OF ty_file_checksum WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_repo_xml,
             url             TYPE string,
             branch_name     TYPE string,
             sha1            TYPE ty_sha1,
             package         TYPE devclass,
             offline         TYPE sap_bool,
             after_last_pull TYPE ty_file_checksum_tt,
             master_language TYPE spras,
           END OF ty_repo_xml.

    TYPES: BEGIN OF ty_repo,
             key TYPE lcl_persistence_db=>ty_value.
        INCLUDE TYPE ty_repo_xml.
    TYPES: END OF ty_repo.
    TYPES: tt_repo TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY.

    METHODS constructor.

    METHODS list
      RETURNING VALUE(rt_repos) TYPE tt_repo
      RAISING   lcx_exception.

    METHODS update_sha1
      IMPORTING iv_key         TYPE ty_repo-key
                iv_branch_sha1 TYPE ty_sha1
      RAISING   lcx_exception.

    METHODS add
      IMPORTING iv_url         TYPE string
                iv_branch_name TYPE string
                iv_branch      TYPE ty_sha1 OPTIONAL
                iv_package     TYPE devclass
                iv_offline     TYPE sap_bool DEFAULT abap_false
      RETURNING VALUE(rv_key)  TYPE ty_repo-key
      RAISING   lcx_exception.

    METHODS delete
      IMPORTING iv_key TYPE ty_repo-key
      RAISING   lcx_exception.

    METHODS read
      IMPORTING iv_key         TYPE ty_repo-key
      RETURNING VALUE(rs_repo) TYPE ty_repo
      RAISING   lcx_exception
                lcx_not_found.

    METHODS lock
      IMPORTING iv_mode TYPE enqmode
                iv_key  TYPE ty_repo-key
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CONSTANTS c_type_repo TYPE lcl_persistence_db=>ty_type VALUE 'REPO'.

    DATA: mo_db TYPE REF TO lcl_persistence_db.

    METHODS from_xml
      IMPORTING iv_repo_xml_string TYPE string
      RETURNING VALUE(rs_repo)     TYPE ty_repo_xml
      RAISING   lcx_exception.

    METHODS to_xml
      IMPORTING is_repo                   TYPE ty_repo
      RETURNING VALUE(rv_repo_xml_string) TYPE string.

    METHODS get_next_id
      RETURNING VALUE(rv_next_repo_id) TYPE lcl_persistence_db=>ty_content-value
      RAISING   lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_repo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_data TYPE lcl_persistence_repo=>ty_repo,
      get_key
        RETURNING VALUE(rv_key) TYPE lcl_persistence_db=>ty_value,
      get_name
        RETURNING VALUE(rv_name) TYPE string
        RAISING   lcx_exception,
      get_files_local
        IMPORTING io_log          TYPE REF TO lcl_log OPTIONAL
        RETURNING VALUE(rt_files) TYPE ty_files_item_tt
        RAISING   lcx_exception,
      get_files_remote
        RETURNING VALUE(rt_files) TYPE ty_files_tt
        RAISING   lcx_exception,
      get_package
        RETURNING VALUE(rv_package) TYPE lcl_persistence_repo=>ty_repo-package,
      get_master_language
        RETURNING VALUE(rv_language) TYPE spras,
      delete
        RAISING lcx_exception,
      get_dot_abapgit
        RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_dot_abapgit,
      deserialize
        RAISING lcx_exception,
      refresh
        RAISING lcx_exception,
      is_offline
        RETURNING VALUE(rv_offline) TYPE abap_bool
        RAISING   lcx_exception.

  PROTECTED SECTION.
    CONSTANTS: c_root        TYPE string VALUE '/',
               c_dot_abapgit TYPE string VALUE '.abapgit.xml'.

    DATA: mt_local       TYPE ty_files_item_tt,
          mt_remote      TYPE ty_files_tt,
          mo_dot_abapgit TYPE REF TO lcl_dot_abapgit,
          ms_data        TYPE lcl_persistence_repo=>ty_repo.

    METHODS: find_dot_abapgit RAISING lcx_exception.

ENDCLASS.                    "lcl_repo DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_status DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
      IMPORTING io_repo           TYPE REF TO lcl_repo
                io_log            TYPE REF TO lcl_log OPTIONAL
      RETURNING VALUE(rt_results) TYPE ty_results_tt
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS compare_files
      IMPORTING it_repo         TYPE ty_files_tt
                is_gen          TYPE ty_file
      RETURNING VALUE(rv_match) TYPE sap_bool
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_file_status DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_types_tt TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.

    CLASS-METHODS serialize
      IMPORTING is_item         TYPE ty_item
                iv_language     TYPE spras
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING io_repo TYPE REF TO lcl_repo
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

    CLASS-METHODS supported_list
      RETURNING VALUE(rt_types) TYPE ty_types_tt.

  PRIVATE SECTION.
    CLASS-METHODS create_object
      IMPORTING is_item       TYPE ty_item
                iv_language   TYPE spras
                is_metadata   TYPE ty_metadata OPTIONAL
      RETURNING VALUE(ri_obj) TYPE REF TO lif_object
      RAISING   lcx_exception.

    CLASS-METHODS
      prioritize_deser
        IMPORTING it_results        TYPE lcl_file_status=>ty_results_tt
        RETURNING VALUE(rt_results) TYPE lcl_file_status=>ty_results_tt.

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

  ENDMETHOD.                    "check_exists

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
          lv_message  TYPE string,
          lv_path     TYPE string,
          lv_category TYPE seoclassdf-category.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc,
                   <ls_tadir> LIKE LINE OF rt_tadir.


    SELECT * FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_tadir
      WHERE devclass = iv_package
      AND object <> 'DEVC'
      AND object <> 'SOTR'
      AND object <> 'SFB1'
      AND object <> 'SFB2'
      AND delflag = abap_false
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
* if abapGit project is installed in package ZZZ, all subpackages should be named
* ZZZ_something. This will define the folder name in the zip file to be "something",
* similarily with online projects
        lv_message = 'Unexpected package naming(' &&
          <ls_tdevc>-devclass && ')' ##no_text.
        MESSAGE lv_message TYPE 'I'.
        CONTINUE.
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

*----------------------------------------------------------------------*
*       CLASS lcl_package DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sap_package DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      check
        IMPORTING io_log     TYPE REF TO lcl_log
                  it_results TYPE lcl_file_status=>ty_results_tt
                  iv_top     TYPE devclass,
      create_local
        IMPORTING iv_package TYPE devclass
        RAISING   lcx_exception,
      create
        IMPORTING is_package TYPE scompkdtln
        RAISING   lcx_exception.

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
*       CLASS lcl_file_status IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_status IMPLEMENTATION.

  METHOD compare_files.

    READ TABLE it_repo WITH KEY
      path = is_gen-path
      filename = is_gen-filename
      data = is_gen-data
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      rv_match = abap_false.
    ELSE.
      rv_match = abap_true.
    ENDIF.

  ENDMETHOD.                    "compare_files

  METHOD status.

    DATA: lv_pre    TYPE tadir-obj_name,
          lt_files  TYPE ty_files_tt,
          ls_result LIKE LINE OF rt_results,
          lv_type   TYPE string,
          ls_item   TYPE ty_item,
          lt_tadir  TYPE lcl_tadir=>ty_tadir_tt,
          lt_local  TYPE ty_files_item_tt,
          ls_tadir  TYPE tadir,
          lt_remote TYPE ty_files_tt,
          lv_ext    TYPE string.

    FIELD-SYMBOLS: <ls_remote> TYPE ty_file,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <ls_result> LIKE LINE OF rt_results,
                   <ls_local>  LIKE LINE OF lt_local,
                   <ls_gen>    LIKE LINE OF lt_files.


    lt_remote = io_repo->get_files_remote( ).
    lt_local = io_repo->get_files_local( io_log ).

    LOOP AT lt_remote ASSIGNING <ls_remote>.
      lcl_progress=>show( iv_key     = 'Status'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_remote )
                          iv_text    = <ls_remote>-filename ) ##NO_TEXT.

      SPLIT <ls_remote>-filename AT '.' INTO lv_pre lv_type lv_ext.
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

      CLEAR lt_files.
      LOOP AT lt_local ASSIGNING <ls_local> WHERE item = ls_item.
        APPEND <ls_local>-file TO lt_files.
      ENDLOOP.

      IF lt_files[] IS INITIAL.
* item does not exist locally
        ls_result-filename = <ls_remote>-filename.
        APPEND ls_result TO rt_results.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT lt_files ASSIGNING <ls_gen>.
        ls_result-filename = <ls_gen>-filename.
        ls_result-match = compare_files( it_repo = lt_remote
                                         is_gen  = <ls_gen> ).
        APPEND ls_result TO rt_results.
      ENDLOOP.
    ENDLOOP.

* find files only existing remotely, including non abapGit related
    LOOP AT lt_remote ASSIGNING <ls_remote>.
      READ TABLE rt_results WITH KEY filename = <ls_remote>-filename
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-filename = <ls_remote>-filename.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* find objects only existing locally
    lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      READ TABLE rt_results
        WITH KEY obj_type = <ls_tadir>-object
        obj_name = <ls_tadir>-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_item-obj_type = <ls_tadir>-object.
        ls_item-obj_name = <ls_tadir>-obj_name.
        IF lcl_objects=>is_supported( ls_item ) = abap_false.
          CONTINUE.
        ENDIF.

        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-obj_type = <ls_tadir>-object.
        ls_result-obj_name = <ls_tadir>-obj_name.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* add path information for files
    LOOP AT lt_remote ASSIGNING <ls_remote>.
      READ TABLE rt_results ASSIGNING <ls_result> WITH KEY filename = <ls_remote>-filename.
      IF sy-subrc = 0.
        <ls_result>-path = <ls_remote>-path.
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

    lcl_sap_package=>check(
      io_log     = io_log
      it_results = rt_results
      iv_top     = io_repo->get_package( ) ).

  ENDMETHOD.                    "status

ENDCLASS.                    "lcl_file_status IMPLEMENTATION


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
        io_log->add( iv_msgv1 = 'Files for object'
                     iv_msgv2 = <ls_res1>-obj_type
                     iv_msgv3 = <ls_res1>-obj_name
                     iv_msgv4 = 'are not placed in the same folder' ) ##no_text.
        EXIT.
      ENDLOOP.
    ENDLOOP.

* check that objects are created in package corresponding to folder
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT package IS INITIAL AND NOT path IS INITIAL.
      lv_path = class_to_path( iv_top     = iv_top
                               iv_package = <ls_res1>-package ).
      IF lv_path <> <ls_res1>-path.
        io_log->add( iv_msgv1 = 'Package and path does not match for object,'
                     iv_msgv2 = <ls_res1>-obj_type
                     iv_msgv3 = <ls_res1>-obj_name ) ##no_text.
      ENDIF.
    ENDLOOP.

* check for multiple files with same filename
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT filename IS INITIAL.
      LOOP AT it_results ASSIGNING <ls_res2>
          WHERE filename = <ls_res1>-filename
          AND path <> <ls_res1>-path.
        io_log->add( iv_msgv1 = 'Multiple files with same filename,'
                     iv_msgv2 = <ls_res1>-filename ) ##no_text.
        EXIT.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "check

  METHOD create.

    DATA: lv_err     TYPE string,
          ls_package LIKE is_package,
          li_package TYPE REF TO if_package.


    ASSERT NOT is_package-devclass IS INITIAL.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = is_package-devclass
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc = 0.
      RETURN. "Package already exists. We assume this is fine
    ENDIF.

    ls_package = is_package.

    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object     = abap_true
*        i_suppress_dialog          = abap_true " does not exist in 730
      IMPORTING
        e_package                  = li_package
      CHANGING
        c_package_data             = ls_package
      EXCEPTIONS
        object_already_existing    = 1
        object_just_created        = 2
        not_authorized             = 3
        wrong_name_prefix          = 4
        undefined_name             = 5
        reserved_local_name        = 6
        invalid_package_name       = 7
        short_text_missing         = 8
        software_component_invalid = 9
        layer_invalid              = 10
        author_not_existing        = 11
        component_not_existing     = 12
        component_missing          = 13
        prefix_in_use              = 14
        unexpected_error           = 15
        intern_err                 = 16
        no_access                  = 17
*        invalid_translation_depth  = 18
*        wrong_mainpack_value       = 19
*        superpackage_invalid       = 20
*        error_in_cts_checks        = 21
        OTHERS                     = 18 ).
    IF sy-subrc <> 0.
      lv_err = |Package { is_package-devclass } could not be created|.
      _raise lv_err.
    ENDIF.

    li_package->save(
*      EXPORTING
*        i_suppress_dialog     = abap_true    " Controls whether popups can be transmitted
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_err.
      _raise lv_err.
    ENDIF.

  ENDMETHOD.

  METHOD create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = iv_package.
    ls_package-ctext     = iv_package.
    ls_package-pdevclass = '$TMP'.
    ls_package-component = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    create( ls_package ).

  ENDMETHOD.                    "create

ENDCLASS.                    "lcl_package IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects IMPLEMENTATION.

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

    TYPES: BEGIN OF ty_obj_serializer_map,
             item     LIKE is_item,
             metadata LIKE is_metadata,
           END OF ty_obj_serializer_map.

    STATICS st_obj_serializer_map
      TYPE SORTED TABLE OF ty_obj_serializer_map WITH UNIQUE KEY item.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF st_obj_serializer_map.

    READ TABLE st_obj_serializer_map INTO ls_obj_serializer_map WITH KEY item = is_item.
    IF sy-subrc = 0.
      lv_class_name = ls_obj_serializer_map-metadata-class.
    ELSEIF is_metadata IS NOT INITIAL.
*        Metadata is provided only on serialization
*        Once this has been triggered, the same serializer shall be used
*        for subsequent processes.
*        Thus, buffer the metadata afterwards
      ls_obj_serializer_map-item      = is_item.
      ls_obj_serializer_map-metadata  = is_metadata.
      INSERT ls_obj_serializer_map INTO TABLE st_obj_serializer_map.

      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        TRY.
* 2nd step, try looking for plugins
            CREATE OBJECT ri_obj TYPE lcl_objects_bridge
              EXPORTING
                is_item = is_item.
          CATCH cx_sy_create_object_error.
            CONCATENATE 'Object type' is_item-obj_type 'not supported, serialize'
              INTO lv_message
              SEPARATED BY space.                           "#EC NOTEXT
            _raise lv_message.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.                    "create_object

  METHOD is_supported.

    TRY.
        create_object( is_item = is_item
                       iv_language = gc_english ).
        rv_bool = abap_true.
      CATCH lcx_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "is_supported

  METHOD supported_list.

    DATA: lv_type  LIKE LINE OF rt_types,
          lt_snode TYPE TABLE OF snode.

    FIELD-SYMBOLS: <ls_snode> LIKE LINE OF lt_snode.


    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name              = 'PG_ZABAPGIT'
        without_crossreference = abap_true
        with_tcode_index       = abap_true
      TABLES
        p_tree                 = lt_snode.

    DELETE lt_snode WHERE type <> 'OPL'
      OR name NP 'LCL_OBJECT_++++'.

    LOOP AT lt_snode ASSIGNING <ls_snode>.
      lv_type = <ls_snode>-name+11.
      APPEND lv_type TO rt_types.
    ENDLOOP.

  ENDMETHOD.                    "supported_list

  METHOD exists.

    DATA: li_obj TYPE REF TO lif_object.


    TRY.
        li_obj = create_object( is_item = is_item
                                iv_language = gc_english ).
        rv_bool = li_obj->exists( ).
      CATCH lcx_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.                    "exists

  METHOD class_name.

    CONCATENATE 'LCL_OBJECT_' is_item-obj_type INTO rv_class_name. "#EC NOTEXT

  ENDMETHOD.                    "class_name

  METHOD jump.

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item = is_item
                            iv_language = gc_english ).
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
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '5500'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '5510'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '5520'.
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
      lcl_progress=>show( iv_key     = 'Delete'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_tadir )
                          iv_text    = <ls_tadir>-obj_name ) ##NO_TEXT.

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

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item = is_item
                            iv_language = gc_english ).
    li_obj->delete( ).

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lt_files TYPE ty_files_tt,
          li_obj   TYPE REF TO lif_object,
          lo_xml   TYPE REF TO lcl_xml_output,
          lo_files TYPE REF TO lcl_objects_files.


    CREATE OBJECT lo_files
      EXPORTING
        is_item = is_item.

    li_obj = create_object( is_item = is_item
                            iv_language = iv_language ).
    li_obj->mo_files = lo_files.
    CREATE OBJECT lo_xml.
    li_obj->serialize( lo_xml ).
    lo_files->add_xml( io_xml      = lo_xml
                       is_metadata = li_obj->get_metadata( ) ).

    rt_files = lo_files->get_files( ).

* check for duplicates
    lt_files[] = rt_files[].
    SORT lt_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path filename.
    IF lines( lt_files ) <> lines( rt_files ).
      _raise 'Duplicates'.
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD prioritize_deser.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

* PROG before internet services, as the services might use the screens
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ISAP has to be handled before ISRP
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IASP'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
        WHERE obj_type <> 'IASP' AND obj_type <> 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

  ENDMETHOD.                    "prioritize_deser

  METHOD deserialize.

    TYPES: BEGIN OF ty_late,
             obj TYPE REF TO lif_object,
             xml TYPE REF TO lcl_xml_input,
           END OF ty_late.

    DATA: ls_item    TYPE ty_item,
          lv_cancel  TYPE abap_bool,
          li_obj     TYPE REF TO lif_object,
          lt_remote  TYPE ty_files_tt,
          lo_files   TYPE REF TO lcl_objects_files,
          lo_xml     TYPE REF TO lcl_xml_input,
          lt_results TYPE lcl_file_status=>ty_results_tt,
          lt_late    TYPE TABLE OF ty_late.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_late>   LIKE LINE OF lt_late.


    lcl_objects_activation=>clear( ).

    lt_remote = io_repo->get_files_remote( ).

    lt_results = lcl_file_status=>status( io_repo ).
    DELETE lt_results WHERE match = abap_true.
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    lt_results = prioritize_deser( lt_results ).

    LOOP AT lt_results ASSIGNING <ls_result>.
      lcl_progress=>show( iv_key     = 'Deserialize'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_results )
                          iv_text    = <ls_result>-obj_name ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

      lv_cancel = check_warning( is_item    = ls_item
                                 iv_package = io_repo->get_package( ) ).
      IF lv_cancel = abap_true.
        RETURN.
      ENDIF.

      CREATE OBJECT lo_files
        EXPORTING
          is_item = ls_item.
      lo_files->set_files( lt_remote ).

* Analyze XML in order to instantiate the proper serializer
      lo_xml = lo_files->read_xml( ).

      li_obj = create_object( is_item     = ls_item
                              iv_language = io_repo->get_master_language( )
                              is_metadata = lo_xml->get_metadata( ) ).

      li_obj->mo_files = lo_files.

      IF li_obj->get_metadata( )-late_deser = abap_true.
        APPEND INITIAL LINE TO lt_late ASSIGNING <ls_late>.
        <ls_late>-obj = li_obj.
        <ls_late>-xml = lo_xml.
        CONTINUE.
      ENDIF.

      li_obj->deserialize( iv_package = io_repo->get_package( )
                           io_xml     = lo_xml ).

    ENDLOOP.

    lcl_objects_activation=>activate( ).

    LOOP AT lt_late ASSIGNING <ls_late>.
      <ls_late>-obj->deserialize( iv_package = io_repo->get_package( )
                                  io_xml     = <ls_late>-xml ).
    ENDLOOP.

    update_package_tree( io_repo->get_package( ) ).

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
* this IF can be refactored, use shifting?
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
    lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
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
      ASSERT NOT <ls_node>-chmod IS INITIAL.
      ASSERT NOT <ls_node>-name IS INITIAL.
      ASSERT NOT <ls_node>-sha1 IS INITIAL.

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

      ELSEIF lv_zlib = c_zlib_hmm.
* cl_abap_gzip copmression works for header '789C', but does not work for
* '7801', call custom implementation of DEFLATE algorithm.
* The custom implementation could handle both, but most likely the kernel
* implementation runs faster than the custom ABAP.
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

      ENDIF.

      lv_data = lv_data+4. " skip adler checksum

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
CLASS lcl_persistence DEFINITION FINAL FRIENDS lcl_persistence_migrate.

* this class is obsolete, use LCL_PERSISTENCE_REPO instead

  PRIVATE SECTION.
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
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
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

CLASS lcl_stage DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_method TYPE c LENGTH 1.

    CONSTANTS: BEGIN OF c_method,
                 add    TYPE ty_method VALUE 'A',
                 rm     TYPE ty_method VALUE 'R',
                 ignore TYPE ty_method VALUE 'I',
               END OF c_method.

    TYPES: BEGIN OF ty_stage,
             file   TYPE ty_file,
             method TYPE ty_method,
           END OF ty_stage.

    TYPES: ty_stage_tt TYPE SORTED TABLE OF ty_stage
      WITH UNIQUE KEY file-path file-filename.

    CLASS-METHODS:
      method_description
        IMPORTING iv_method             TYPE ty_method
        RETURNING VALUE(rv_description) TYPE string
        RAISING   lcx_exception.

    METHODS:
      add
        IMPORTING is_file TYPE ty_file
        RAISING   lcx_exception,
      reset
        IMPORTING is_file TYPE ty_file
        RAISING   lcx_exception,
      rm
        IMPORTING is_file TYPE ty_file
        RAISING   lcx_exception,
      ignore
        IMPORTING is_file TYPE ty_file
        RAISING   lcx_exception,
      count
        RETURNING VALUE(rv_count) TYPE i,
      lookup
        IMPORTING iv_path         TYPE ty_file-path
                  iv_filename     TYPE ty_file-filename
        RETURNING VALUE(rs_stage) TYPE ty_stage
        RAISING   lcx_not_found,
      get_all
        RETURNING VALUE(rt_stage) TYPE ty_stage_tt.

  PRIVATE SECTION.
    DATA: mt_stage TYPE ty_stage_tt.

    METHODS: append
      IMPORTING is_file   TYPE ty_file
                iv_method TYPE ty_method.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_repo_online DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_online DEFINITION INHERITING FROM lcl_repo FINAL.

  PUBLIC SECTION.
    METHODS:
      refresh REDEFINITION,
      constructor
        IMPORTING is_data TYPE lcl_persistence_repo=>ty_repo
        RAISING   lcx_exception,
      get_url
        RETURNING VALUE(rv_url) TYPE lcl_persistence_repo=>ty_repo-url,
      get_branch_name
        RETURNING VALUE(rv_name) TYPE lcl_persistence_repo=>ty_repo-branch_name,
      get_sha1_local
        RETURNING VALUE(rv_sha1) TYPE lcl_persistence_repo=>ty_repo-sha1,
      get_sha1_remote
        RETURNING VALUE(rv_sha1) TYPE lcl_persistence_repo=>ty_repo-sha1
        RAISING   lcx_exception,
      get_files_remote REDEFINITION,
      get_objects
        RETURNING VALUE(rt_objects) TYPE lcl_git_pack=>ty_objects_tt
        RAISING   lcx_exception,
      deserialize REDEFINITION,
      status
        IMPORTING io_log            TYPE REF TO lcl_log OPTIONAL
        RETURNING VALUE(rt_results) TYPE lcl_file_status=>ty_results_tt
        RAISING   lcx_exception,
      push
        IMPORTING is_comment TYPE ty_comment
                  io_stage   TYPE REF TO lcl_stage
        RAISING   lcx_exception.

  PRIVATE SECTION.
    DATA:
      mt_objects     TYPE lcl_git_pack=>ty_objects_tt,
      mv_branch      TYPE ty_sha1,
      mv_initialized TYPE abap_bool.

    METHODS:
      handle_stage_ignore
        IMPORTING io_stage TYPE REF TO lcl_stage
        RAISING   lcx_exception,
      initialize
        RAISING lcx_exception,
      set_sha1
        IMPORTING iv_sha1 TYPE ty_sha1
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_repo_online DEFINITION

CLASS lcl_stage_logic DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_stage_files,
             local  TYPE ty_files_item_tt,
             remote TYPE ty_files_tt,
           END OF ty_stage_files.

    CLASS-METHODS:
      get
        IMPORTING io_repo         TYPE REF TO lcl_repo_online
        RETURNING VALUE(rs_files) TYPE ty_stage_files
        RAISING   lcx_exception,
      count
        IMPORTING io_repo         TYPE REF TO lcl_repo_online
        RETURNING VALUE(rv_count) TYPE i
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      remove_ignored
        IMPORTING io_repo  TYPE REF TO lcl_repo_online
        CHANGING  cs_files TYPE ty_stage_files,
      remove_identical
        CHANGING cs_files TYPE ty_stage_files.

ENDCLASS.

CLASS lcl_stage_logic IMPLEMENTATION.

  METHOD get.
    rs_files-local = io_repo->get_files_local( ).
    rs_files-remote = io_repo->get_files_remote( ).
    remove_identical( CHANGING cs_files = rs_files ).
    remove_ignored( EXPORTING io_repo = io_repo
                    CHANGING cs_files = rs_files ).
  ENDMETHOD.

  METHOD count.

    DATA: ls_files TYPE ty_stage_files.

    ls_files = get( io_repo ).

    rv_count = lines( ls_files-remote ) + lines( ls_files-local ).

  ENDMETHOD.

  METHOD remove_ignored.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF cs_files-remote.


    LOOP AT cs_files-remote ASSIGNING <ls_remote>.
      lv_index = sy-tabix.

      IF io_repo->get_dot_abapgit( )->is_ignored(
          iv_path = <ls_remote>-path
          iv_filename = <ls_remote>-filename ) = abap_true.
        DELETE cs_files-remote INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD remove_identical.

    DATA: lv_index  TYPE i,
          ls_remote LIKE LINE OF cs_files-remote.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF cs_files-local.


    LOOP AT cs_files-local ASSIGNING <ls_local>.
      lv_index = sy-tabix.

      READ TABLE cs_files-remote INTO ls_remote
        WITH KEY path = <ls_local>-file-path
        filename = <ls_local>-file-filename.
      IF sy-subrc = 0.
        DELETE cs_files-remote INDEX sy-tabix.
        IF ls_remote-data = <ls_local>-file-data.
          DELETE cs_files-local INDEX lv_index.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_repo_offline DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_offline DEFINITION INHERITING FROM lcl_repo FINAL.

  PUBLIC SECTION.
    METHODS:
      set_files_remote
        IMPORTING it_files TYPE ty_files_tt
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_repo_offline DEFINITION

CLASS ltcl_git_porcelain DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_porcelain DEFINITION FINAL FRIENDS ltcl_git_porcelain.

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
                io_stage         TYPE REF TO lcl_stage
      RETURNING VALUE(rv_branch) TYPE ty_sha1
      RAISING   lcx_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_expanded,
             path TYPE string,
             name TYPE string,
             sha1 TYPE ty_sha1,
           END OF ty_expanded.

    TYPES: ty_expanded_tt TYPE STANDARD TABLE OF ty_expanded WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_tree,
             path TYPE string,
             data TYPE xstring,
             sha1 TYPE ty_sha1,
           END OF ty_tree.

    TYPES: ty_trees_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_folder,
             path  TYPE string,
             count TYPE i,
             sha1  TYPE ty_sha1,
           END OF ty_folder.

    TYPES: ty_folders_tt TYPE STANDARD TABLE OF ty_folder WITH DEFAULT KEY.

    CLASS-METHODS build_trees
      IMPORTING it_expanded     TYPE ty_expanded_tt
      RETURNING VALUE(rt_trees) TYPE ty_trees_tt
      RAISING   lcx_exception.

    CLASS-METHODS find_folders
      IMPORTING it_expanded       TYPE ty_expanded_tt
      RETURNING VALUE(rt_folders) TYPE ty_folders_tt.

    CLASS-METHODS walk
      IMPORTING it_objects TYPE lcl_git_pack=>ty_objects_tt
                iv_sha1    TYPE ty_sha1
                iv_path    TYPE string
      CHANGING  ct_files   TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS walk_tree
      IMPORTING it_objects         TYPE lcl_git_pack=>ty_objects_tt
                iv_tree            TYPE ty_sha1
                iv_base            TYPE string
      RETURNING VALUE(rt_expanded) TYPE ty_expanded_tt
      RAISING   lcx_exception.

    CLASS-METHODS full_tree
      IMPORTING it_objects         TYPE lcl_git_pack=>ty_objects_tt
                iv_branch          TYPE ty_sha1
      RETURNING VALUE(rt_expanded) TYPE ty_expanded_tt
      RAISING   lcx_exception.

    CLASS-METHODS receive_pack
      IMPORTING is_comment       TYPE ty_comment
                io_repo          TYPE REF TO lcl_repo_online
                it_trees         TYPE ty_trees_tt
                it_blobs         TYPE ty_files_tt
                iv_branch        TYPE ty_sha1
      RETURNING VALUE(rv_branch) TYPE ty_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_porcelain DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_gui_page DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_gui_page.

  METHODS:
    on_event
      IMPORTING iv_action      TYPE clike
                iv_frame       TYPE clike
                iv_getdata     TYPE clike
                it_postdata    TYPE cnht_post_data_tab
                it_query_table TYPE cnht_query_table
      RAISING   lcx_exception,
    render
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

ENDINTERFACE.

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS render
      RAISING lcx_exception.

    CLASS-METHODS back
      RETURNING VALUE(r_exit) TYPE xfeld
      RAISING   lcx_exception.

    CLASS-METHODS call_page
      IMPORTING ii_page TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    CLASS-METHODS set_page
      IMPORTING ii_page TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    CLASS-METHODS on_event
                  FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

    CLASS-METHODS show_url
      IMPORTING iv_url TYPE clike.


  PRIVATE SECTION.
    CLASS-DATA: gi_page        TYPE REF TO lif_gui_page,
                gt_stack       TYPE TABLE OF REF TO lif_gui_page,
                go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS startup
      RAISING lcx_exception.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_offline IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_offline IMPLEMENTATION.

  METHOD set_files_remote.

    mt_remote = it_files.

    find_dot_abapgit( ).

  ENDMETHOD.

ENDCLASS.                    "lcl_repo_offline IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_srv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_srv DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: ty_repo_tt TYPE STANDARD TABLE OF REF TO lcl_repo WITH DEFAULT KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS list
      RETURNING VALUE(rt_list) TYPE ty_repo_tt
      RAISING   lcx_exception.

    CLASS-METHODS refresh
      RAISING lcx_exception.

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

    CLASS-METHODS delete
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   lcx_exception.

    CLASS-METHODS get
      IMPORTING iv_key         TYPE lcl_persistence_db=>ty_value
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo.

  PRIVATE SECTION.

    CLASS-DATA: gv_init        TYPE abap_bool VALUE abap_false,
                go_persistence TYPE REF TO lcl_persistence_repo,
                gt_list        TYPE ty_repo_tt.

    CLASS-METHODS add
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   lcx_exception.

    CLASS-METHODS:
      validate_package
        IMPORTING
          iv_package TYPE devclass
        RAISING
          lcx_exception.

ENDCLASS.                    "lcl_repo_srv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_online IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_online IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_data ).

    mv_initialized = abap_false.

  ENDMETHOD.                    "constructor

  METHOD initialize.
    IF mv_initialized = abap_false.
      refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD status.

    initialize( ).

    rt_results = lcl_file_status=>status( io_repo = me
                                          io_log  = io_log ).

  ENDMETHOD.                    "status

  METHOD deserialize.

    initialize( ).

    super->deserialize( ).

    set_sha1( mv_branch ).

  ENDMETHOD.                    "deserialize

  METHOD refresh.

    super->refresh( ).

    lcl_progress=>show( iv_key     = 'Fetch'
                        iv_current = 1
                        iv_total   = 1
                        iv_text    = 'Remote files' ) ##NO_TEXT.

    lcl_git_porcelain=>pull( EXPORTING io_repo    = me
                             IMPORTING et_files   = mt_remote
                                       et_objects = mt_objects
                                       ev_branch  = mv_branch ).

    find_dot_abapgit( ).

    mv_initialized = abap_true.

  ENDMETHOD.                    "refresh

  METHOD get_sha1_remote.
    initialize( ).

    rv_sha1 = mv_branch.
  ENDMETHOD.                    "get_sha1_remote

  METHOD get_files_remote.
    initialize( ).

    rt_files = mt_remote.
  ENDMETHOD.                    "get_files

  METHOD get_objects.
    initialize( ).

    rt_objects = mt_objects.
  ENDMETHOD.                    "get_objects

  METHOD get_url.
    rv_url = ms_data-url.
  ENDMETHOD.                    "get_url

  METHOD get_branch_name.
    rv_name = ms_data-branch_name.
  ENDMETHOD.                    "get_branch_name

  METHOD get_sha1_local.
    rv_sha1 = ms_data-sha1.
  ENDMETHOD.                    "get_sha1_local

  METHOD push.

    DATA: lv_branch TYPE ty_sha1.


    handle_stage_ignore( io_stage ).

    lv_branch = lcl_git_porcelain=>push( is_comment = is_comment
                                         io_repo    = me
                                         io_stage   = io_stage ).

    set_sha1( lv_branch ).

    refresh( ).

  ENDMETHOD.                    "push

  METHOD handle_stage_ignore.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt,
          ls_file  TYPE ty_file.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = lcl_stage=>c_method-ignore.

      mo_dot_abapgit->add_ignore( iv_path = <ls_stage>-file-path iv_filename = <ls_stage>-file-filename ).
* remove it from the staging object, as the action is handled here

      CLEAR ls_file.
      ls_file-path = <ls_stage>-file-path.
      ls_file-filename = <ls_stage>-file-filename.
      io_stage->reset( ls_file ).

      CLEAR ls_file.
      ls_file-path     = c_root.
      ls_file-filename = c_dot_abapgit.
      ls_file-data     = mo_dot_abapgit->serialize( ).
      io_stage->add( ls_file ).
    ENDLOOP.

  ENDMETHOD.

  METHOD set_sha1.

    DATA: lo_persistence TYPE REF TO lcl_persistence_repo.


    CREATE OBJECT lo_persistence.

    lo_persistence->update_sha1( iv_key         = ms_data-key
                                 iv_branch_sha1 = iv_sha1 ).

    ms_data-sha1 = iv_sha1.

  ENDMETHOD.                    "set_sha1

ENDCLASS.                    "lcl_repo_online IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo IMPLEMENTATION.

  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.

  ENDMETHOD.                    "constructor

  METHOD find_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.


    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY path = c_root
      filename = c_dot_abapgit.
    IF sy-subrc = 0.
      mo_dot_abapgit = lcl_dot_abapgit=>deserialize( <ls_remote>-data ).
    ENDIF.

  ENDMETHOD.

  METHOD get_files_remote.
    rt_files = mt_remote.
  ENDMETHOD.

  METHOD deserialize.

    IF mo_dot_abapgit->get_master_language( ) <> sy-langu.
      _raise 'Current login language does not match master language'.
    ENDIF.

    lcl_objects=>deserialize( me ).

    CLEAR mt_local.

  ENDMETHOD.

  METHOD get_files_local.

    DATA: lt_tadir TYPE lcl_tadir=>ty_tadir_tt,
          ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF lt_files,
                   <ls_return> LIKE LINE OF rt_files,
                   <ls_tadir>  LIKE LINE OF lt_tadir.


    IF lines( mt_local ) > 0.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    lt_tadir = lcl_tadir=>read( get_package( ) ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lcl_progress=>show( iv_key     = 'Serialize'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_tadir )
                          iv_text    = <ls_tadir>-obj_name ) ##NO_TEXT.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      IF lcl_objects=>is_supported( ls_item ) = abap_false.
        IF NOT io_log IS INITIAL.
          io_log->add( iv_msgv1 = 'Object type ignored, not supported:'
                       iv_msgv2 = ls_item-obj_type
                       iv_msgv3 = '-'
                       iv_msgv4 = ls_item-obj_name ) ##no_text.
        ENDIF.
        CONTINUE.
      ENDIF.

      lt_files = lcl_objects=>serialize( is_item = ls_item
                                         iv_language = get_master_language( ) ).
      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = '/' && <ls_tadir>-path.

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
        <ls_return>-file = <ls_file>.
        <ls_return>-item = ls_item.
      ENDLOOP.
    ENDLOOP.

    IF mo_dot_abapgit IS INITIAL.
      mo_dot_abapgit = lcl_dot_abapgit=>build_default( ms_data-master_language ).
    ENDIF.
    APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
    <ls_return>-file-path     = c_root.
    <ls_return>-file-filename = c_dot_abapgit.
    <ls_return>-file-data     = mo_dot_abapgit->serialize( ).

    mt_local = rt_files.

  ENDMETHOD.

  METHOD get_dot_abapgit.
    ro_dot_abapgit = mo_dot_abapgit.
  ENDMETHOD.

  METHOD delete.

    DATA: lo_persistence TYPE REF TO lcl_persistence_repo.


    CREATE OBJECT lo_persistence.

    lo_persistence->delete( ms_data-key ).

  ENDMETHOD.                    "delete

  METHOD is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.

  METHOD refresh.
    CLEAR mt_local.
  ENDMETHOD.                    "refresh

  METHOD get_package.
    rv_package = ms_data-package.
  ENDMETHOD.                    "get_package

  METHOD get_master_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.

  METHOD get_key.
    rv_key = ms_data-key.
  ENDMETHOD.                    "get_key

  METHOD get_name.

    IF ms_data-offline = abap_true.
      rv_name = ms_data-url.
    ELSE.
      rv_name = lcl_url=>name( ms_data-url ).
    ENDIF.

  ENDMETHOD.                    "get_name

ENDCLASS.                    "lcl_repo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_srv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_srv IMPLEMENTATION.

  METHOD class_constructor.
    CREATE OBJECT go_persistence.
  ENDMETHOD.                    "class_constructor

  METHOD list.

    IF gv_init = abap_false.
      refresh( ).
    ENDIF.

    rt_list = gt_list.

  ENDMETHOD.                    "list

  METHOD get.

    FIELD-SYMBOLS: <lo_list> LIKE LINE OF gt_list.


    LOOP AT gt_list ASSIGNING <lo_list>.
      IF <lo_list>->get_key( ) = iv_key.
        ro_repo = <lo_list>.
        RETURN.
      ENDIF.
    ENDLOOP.

    ASSERT 1 = 0.

  ENDMETHOD.                    "get

  METHOD refresh.

    DATA: lt_list    TYPE lcl_persistence_repo=>tt_repo,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_offline TYPE REF TO lcl_repo_offline.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CLEAR gt_list.

    lt_list = go_persistence->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      IF <ls_list>-offline = abap_false.
        CREATE OBJECT lo_online
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_online TO gt_list.
      ELSE.
        CREATE OBJECT lo_offline
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_offline TO gt_list.
      ENDIF.
    ENDLOOP.

    gv_init = abap_true.

  ENDMETHOD.                    "refresh

  METHOD new_online.

    DATA: ls_repo TYPE lcl_persistence_repo=>ty_repo,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = go_persistence->add(
      iv_url         = iv_url
      iv_branch_name = iv_branch_name
      iv_package     = iv_package ).

    TRY.
        ls_repo = go_persistence->read( lv_key ).
      CATCH lcx_not_found.
        _raise 'new_online not found'.
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_online

  METHOD new_offline.

    DATA: ls_repo TYPE lcl_persistence_repo=>ty_repo,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = go_persistence->add(
      iv_url         = iv_url
      iv_branch_name = ''
      iv_package     = iv_package
      iv_offline     = abap_true ).

    TRY.
        ls_repo = go_persistence->read( lv_key ).
      CATCH lcx_not_found.
        _raise 'new_offline not found'.
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_offline

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

    APPEND io_repo TO gt_list.

  ENDMETHOD.                    "add

  METHOD validate_package.

    DATA: lv_devclass TYPE tdevc-devclass,
          lt_repos    TYPE lcl_persistence_repo=>tt_repo.


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

  ENDMETHOD.                    "validate_package

  METHOD delete.

    io_repo->delete( ).

    DELETE TABLE gt_list FROM io_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "delete

ENDCLASS.                    "lcl_repo_srv IMPLEMENTATION

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

  ENDMETHOD.                    "class_constructor

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

      IF lv_name CP 'refs/pull/*'.
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

  ENDMETHOD.                    "branches

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
      IMPORTING iv_key TYPE lcl_persistence_db=>ty_value
      RAISING   lcx_exception.

    CLASS-METHODS export_key
      IMPORTING iv_key TYPE lcl_persistence_db=>ty_value
                iv_zip TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

  PRIVATE SECTION.

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
      IMPORTING it_files TYPE ty_files_item_tt
      RAISING   lcx_exception.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE ty_files_item_tt
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

  METHOD get_message.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
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
      CONCATENATE <ls_file>-file-path+1 <ls_file>-file-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-file-data ).
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

    DATA: lo_repo TYPE REF TO lcl_repo,
          lo_log  TYPE REF TO lcl_log,
          lt_zip  TYPE ty_files_item_tt.


    lo_repo = lcl_repo_srv=>get( iv_key ).

    CREATE OBJECT lo_log.

    lt_zip = lo_repo->get_files_local( lo_log ).

    IF lo_log->count( ) > 0.
      lo_log->show( ).
    ENDIF.

    IF iv_zip = abap_true.
      file_download( iv_package = lo_repo->get_package( )
                     iv_xstr = encode_files( lt_zip ) ).
    ELSE.
      files_commit( lt_zip ).
    ENDIF.

  ENDMETHOD.                    "export_key

  METHOD import.

    DATA: lo_repo TYPE REF TO lcl_repo_offline.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).
    lo_repo->set_files_remote( unzip_file( file_upload( ) ) ).
    lo_repo->deserialize( ).

    lcl_gui=>render( ).

  ENDMETHOD.                    "import

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
      lt_rawdata = cl_bcs_convert=>xstring_to_solix( <ls_file>-file-data ).

      CONCATENATE lv_folder <ls_file>-file-path <ls_file>-file-filename INTO lv_filename.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize            = xstrlen( <ls_file>-file-data )
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

    DATA: lv_time    TYPE lcl_time=>ty_unixtime,
          lv_commit  TYPE xstring,
          lt_objects TYPE lcl_git_pack=>ty_objects_tt,
          lv_pack    TYPE xstring,
          ls_object  LIKE LINE OF lt_objects,
          ls_commit  TYPE lcl_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_trees,
                   <ls_blob> LIKE LINE OF it_blobs.


    lv_time = lcl_time=>get( ).

    READ TABLE it_trees ASSIGNING <ls_tree> WITH KEY path = '/'.
    ASSERT sy-subrc = 0.

* new commit
    ls_commit-tree      = <ls_tree>-sha1.
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

    LOOP AT it_trees ASSIGNING <ls_tree>.
      CLEAR ls_object.
      ls_object-sha1 = <ls_tree>-sha1.
      ls_object-type = gc_type-tree.
      ls_object-data = <ls_tree>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    LOOP AT it_blobs ASSIGNING <ls_blob>.
      CLEAR ls_object.
      ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_blob>-data ).
      ls_object-type = gc_type-blob.
      ls_object-data = <ls_blob>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    lv_pack = lcl_git_pack=>encode( lt_objects ).

    rv_branch = lcl_hash=>sha1( iv_type = gc_type-commit iv_data = lv_commit ).
    lcl_git_transport=>receive_pack( io_repo   = io_repo
                                     iv_commit = rv_branch
                                     iv_pack   = lv_pack ).

  ENDMETHOD.                    "receive_pack

  METHOD push.

    DATA: lt_expanded TYPE ty_expanded_tt,
          lt_blobs    TYPE ty_files_tt,
          lv_sha1     TYPE ty_sha1,
          lt_trees    TYPE ty_trees_tt,
          lt_stage    TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage,
                   <ls_exp>   LIKE LINE OF lt_expanded.


    lt_expanded = full_tree( it_objects = io_repo->get_objects( )
                             iv_branch  = io_repo->get_sha1_remote( ) ).

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.
      CASE <ls_stage>-method.
        WHEN lcl_stage=>c_method-add.
          APPEND <ls_stage>-file TO lt_blobs.

          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH KEY
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          IF sy-subrc <> 0. " new files
            APPEND INITIAL LINE TO lt_expanded ASSIGNING <ls_exp>.
            <ls_exp>-name = <ls_stage>-file-filename.
            <ls_exp>-path = <ls_stage>-file-path.
          ENDIF.

          lv_sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_stage>-file-data ).
          IF <ls_exp>-sha1 <> lv_sha1.
            <ls_exp>-sha1 = lv_sha1.
          ENDIF.
        WHEN lcl_stage=>c_method-rm.
          DELETE lt_expanded
            WHERE name = <ls_stage>-file-filename
            AND path = <ls_stage>-file-path.
          ASSERT sy-subrc = 0.
        WHEN OTHERS.
          _raise 'stage method not supported, todo'.
      ENDCASE.
    ENDLOOP.

    lt_trees = build_trees( lt_expanded ).

    rv_branch = receive_pack( is_comment = is_comment
                              io_repo    = io_repo
                              it_trees   = lt_trees
                              it_blobs   = lt_blobs
                              iv_branch  = io_repo->get_sha1_remote( ) ).

  ENDMETHOD.                    "push

  METHOD walk_tree.

    DATA: ls_object   LIKE LINE OF it_objects,
          lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects INTO ls_object
      WITH KEY sha1 = iv_tree
      type = gc_type-tree.
    IF sy-subrc <> 0.
      _raise 'tree not found'.
    ENDIF.
    lt_nodes = lcl_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN gc_chmod-file.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path = iv_base.
          <ls_exp>-name = <ls_node>-name.
          <ls_exp>-sha1 = <ls_node>-sha1.
        WHEN gc_chmod-dir.
          lt_expanded = walk_tree(
            it_objects = it_objects
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          _raise 'walk_tree: unknown chmod'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD full_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE lcl_git_pack=>ty_commit.


    READ TABLE it_objects INTO ls_object WITH KEY sha1 = iv_branch type = gc_type-commit.
    IF sy-subrc <> 0.
      _raise 'commit not found'.
    ENDIF.
    ls_commit = lcl_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

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

  METHOD find_folders.

    DATA: lt_paths TYPE TABLE OF string,
          lv_split TYPE string,
          lv_path  TYPE string.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF rt_folders,
                   <ls_new>    LIKE LINE OF rt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    LOOP AT it_expanded ASSIGNING <ls_exp>.
      READ TABLE rt_folders WITH KEY path = <ls_exp>-path TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_folder>.
        <ls_folder>-path = <ls_exp>-path.
      ENDIF.
    ENDLOOP.

* add empty folders
    LOOP AT rt_folders ASSIGNING <ls_folder>.
      SPLIT <ls_folder>-path AT '/' INTO TABLE lt_paths.

      CLEAR lv_path.
      LOOP AT lt_paths INTO lv_split.
        CONCATENATE lv_path lv_split '/' INTO lv_path.
        READ TABLE rt_folders WITH KEY path = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_new>.
          <ls_new>-path = lv_path.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT rt_folders ASSIGNING <ls_folder>.
      FIND ALL OCCURRENCES OF '/' IN <ls_folder>-path MATCH COUNT <ls_folder>-count.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_trees.

    DATA: lt_nodes   TYPE lcl_git_pack=>ty_nodes_tt,
          ls_tree    LIKE LINE OF rt_trees,
          lv_sub     TYPE string,
          lv_len     TYPE i,
          lt_folders TYPE ty_folders_tt.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF lt_folders,
                   <ls_node>   LIKE LINE OF lt_nodes,
                   <ls_sub>    LIKE LINE OF lt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    lt_folders = find_folders( it_expanded ).

* start with the deepest folders
    SORT lt_folders BY count DESCENDING.

    LOOP AT lt_folders ASSIGNING <ls_folder>.
      CLEAR lt_nodes.

* files
      LOOP AT it_expanded ASSIGNING <ls_exp> WHERE path = <ls_folder>-path.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = gc_chmod-file.
        <ls_node>-name = <ls_exp>-name.
        <ls_node>-sha1 = <ls_exp>-sha1.
      ENDLOOP.

* folders
      lv_sub = <ls_folder>-path && '+*'.
      LOOP AT lt_folders ASSIGNING <ls_sub>
          WHERE count = <ls_folder>-count + 1 AND path CP lv_sub.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = gc_chmod-dir.

* extract folder name, this can probably be done easier using regular expressions
        lv_len = strlen( <ls_folder>-path ).
        <ls_node>-name = <ls_sub>-path+lv_len.
        lv_len = strlen( <ls_node>-name ) - 1.
        <ls_node>-name = <ls_node>-name(lv_len).

        <ls_node>-sha1 = <ls_sub>-sha1.
      ENDLOOP.

      CLEAR ls_tree.
      ls_tree-path = <ls_folder>-path.
      ls_tree-data = lcl_git_pack=>encode_tree( lt_nodes ).
      ls_tree-sha1 = lcl_hash=>sha1( iv_type = gc_type-tree iv_data = ls_tree-data ).
      APPEND ls_tree TO rt_trees.

      <ls_folder>-sha1 = ls_tree-sha1.
    ENDLOOP.

  ENDMETHOD.

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

  METHOD show_url.

    go_html_viewer->show_url( iv_url ).

  ENDMETHOD.

  METHOD on_event.

    DATA: lx_exception TYPE REF TO lcx_exception.


    TRY.
        gi_page->on_event(
          iv_action      = action
          iv_frame       = frame
          iv_getdata     = getdata
          it_postdata    = postdata
          it_query_table = query_table ).
      CATCH lcx_exception INTO lx_exception.
        ROLLBACK WORK.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD back.

    DATA: lv_index TYPE i.


    lv_index = lines( gt_stack ).

    IF lv_index = 0.
      r_exit = 'X'.
      RETURN.
    ENDIF.

    READ TABLE gt_stack INDEX lv_index INTO gi_page.
    ASSERT sy-subrc = 0.

    DELETE gt_stack INDEX lv_index.
    ASSERT sy-subrc = 0.

    render( ).

  ENDMETHOD.

  METHOD call_page.

    IF NOT go_html_viewer IS BOUND.
      startup( ).
    ENDIF.

    IF NOT gi_page IS INITIAL.
      APPEND gi_page TO gt_stack.
    ENDIF.

    set_page( ii_page ).

  ENDMETHOD.

  METHOD set_page.

    gi_page = ii_page.
    render( ).

  ENDMETHOD.

  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.


    CREATE OBJECT go_html_viewer
      EXPORTING
        parent = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = go_html_viewer->m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.
    go_html_viewer->set_registered_events( lt_events ).

    SET HANDLER lcl_gui=>on_event FOR go_html_viewer.

  ENDMETHOD.                    "init

  METHOD render.

    view( gi_page->render( )->mv_html ).

  ENDMETHOD.

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

ENDCLASS.                    "lcl_gui IMPLEMENTATION

CLASS lcl_persistence_user DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_user TYPE xubname DEFAULT sy-uname.

    METHODS set_username
      IMPORTING iv_username TYPE string
      RAISING   lcx_exception.

    METHODS get_username
      RETURNING VALUE(rv_username) TYPE string
      RAISING   lcx_exception.

    METHODS set_email
      IMPORTING iv_email TYPE string
      RAISING   lcx_exception.

    METHODS get_email
      RETURNING VALUE(rv_email) TYPE string
      RAISING   lcx_exception.

    METHODS is_hidden
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(rv_hidden) TYPE abap_bool
      RAISING   lcx_exception.

    METHODS hide
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS unhide
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CONSTANTS c_type_user TYPE lcl_persistence_db=>ty_type VALUE 'USER'.

    DATA: mv_user TYPE xubname.

    TYPES:
      ty_repo_hidden_tt
        TYPE STANDARD TABLE OF lcl_persistence_repo=>ty_repo-key
        WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_user,
             username    TYPE string,
             email       TYPE string,
             repo_hidden TYPE ty_repo_hidden_tt,
           END OF ty_user.

    METHODS from_xml
      IMPORTING iv_xml         TYPE string
      RETURNING VALUE(rs_user) TYPE ty_user
      RAISING   lcx_exception.

    METHODS to_xml
      IMPORTING is_user       TYPE ty_user
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS read
      RETURNING VALUE(rs_user) TYPE ty_user
      RAISING   lcx_exception.

    METHODS update
      IMPORTING is_user TYPE ty_user
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_super DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_gui_page ALL METHODS ABSTRACT.

  PROTECTED SECTION.
    METHODS header
      IMPORTING io_include_style TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS footer
      IMPORTING io_include_script TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS title
      IMPORTING iv_page_title TYPE string
                io_menu TYPE REF TO lcl_html_toolbar OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS get_logo_src
      RETURNING VALUE(rv_src) TYPE string.

  PRIVATE SECTION.
    METHODS: styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_super IMPLEMENTATION.

  METHOD header.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                        "#EC NOTEXT
    ro_html->add( '<html>' ).                                 "#EC NOTEXT
    ro_html->add( '<head>' ).                                 "#EC NOTEXT
    ro_html->add( '<title>abapGit</title>' ).                 "#EC NOTEXT
    ro_html->add( styles( ) ).

    IF io_include_style IS BOUND.
      ro_html->add( '<style type="text/css">' ).              "#EC NOTEXT
      ro_html->add( io_include_style ).
      ro_html->add( '</style>' ).                             "#EC NOTEXT
    ENDIF.

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' )."#EC NOTEXT
    ro_html->add( '</head>' ).                                "#EC NOTEXT
    ro_html->add( '<body>' ).                                 "#EC NOTEXT

  ENDMETHOD.                    "render html header

  METHOD title.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="header">' ).                      "#EC NOTEXT
    ro_html->add( '<table class="mixed_height_bar"><tr>' ).   "#EC NOTEXT

    ro_html->add( '<td class="logo">' ).                      "#EC NOTEXT
    ro_html->add( '<a href="sapevent:abapgithome">' ).        "#EC NOTEXT
    ro_html->add( |<img src="{ me->get_logo_src( ) }"></a>| )."#EC NOTEXT
    ro_html->add( |<span class="page_title">::{ iv_page_title }</span>| )."#EC NOTEXT
    ro_html->add( '</td>' ).                                  "#EC NOTEXT

    IF io_menu IS BOUND.
      ro_html->add( '<td class="right">' ).                   "#EC NOTEXT
      ro_html->add( io_menu->render( ) ).
      ro_html->add( '</td>' ).                                "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr></table>' ).                          "#EC NOTEXT
    ro_html->add( '</div>' ).                                 "#EC NOTEXT

  ENDMETHOD.                    "render page title

  METHOD footer.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="footer">' ).                      "#EC NOTEXT
    ro_html->add( |<img src="{ get_logo_src( ) }" >| ).       "#EC NOTEXT
    ro_html->add( |<span class="version">{ gc_abap_version }</span>| )."#EC NOTEXT
    ro_html->add( '</div>' ).                                 "#EC NOTEXT
    ro_html->add( '</body>' ).                                "#EC NOTEXT

    IF io_include_script IS BOUND.
      ro_html->add( io_include_script ).
    ENDIF.

    ro_html->add( '</html>').                                 "#EC NOTEXT

  ENDMETHOD.                    "render html footer & logo

  METHOD styles.

    CREATE OBJECT ro_html.

    ro_html->add('<style type="text/css">').

    " Global styles
    ro_html->add('/* GLOBALS */').
    ro_html->add('body {').
    ro_html->add('  font-family: Arial,Helvetica,sans-serif;').
    ro_html->add('  background: #E8E8E8;').
    ro_html->add('}').
    ro_html->add('a, a:visited {').
    ro_html->add('  color:            #4078c0;').
    ro_html->add('  text-decoration:  none;').
    ro_html->add('}').
    ro_html->add('a:hover, a:active { text-decoration:  underline; }').
    ro_html->add('img               { border: 0px; }').

    " Structure div styles: header, footer, toc
    ro_html->add('/* STRUCTURE DIVS */').
    ro_html->add('div#header {').
    ro_html->add('  display:          block;').
    ro_html->add('  margin-top:       0.5em;').
    ro_html->add('  border-bottom:    3px double lightgrey;').
    ro_html->add('}').
    ro_html->add('div#toc {').
    ro_html->add('  display:          block;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding:          1em;').
    ro_html->add('}').
    ro_html->add('div#footer {').
    ro_html->add('  display:          block;').
    ro_html->add('  margin-bottom:    1em;').
    ro_html->add('  padding-top:      0.5em;').
    ro_html->add('  border-top:       3px double lightgrey;').
    ro_html->add('  color:            grey;').
    ro_html->add('  text-align:       center;').
    ro_html->add('}').

    " Header, footer and menu styles
    ro_html->add('/* HEADER, FOOTER & MENU */').
    ro_html->add('.mixed_height_bar {').
    ro_html->add('  width: 98%; /*IE7 compat5 mode workaround*/').
    ro_html->add('}').
    ro_html->add('div.menu  { display: inline; }').
    ro_html->add('.right    { text-align:right; }').
    ro_html->add('.menu_end { border-right: 0px !important; }').
    ro_html->add('.menu a {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('  padding-right: 0.5em;').
    ro_html->add('  border-right: 1px solid lightgrey;').
    ro_html->add('}').
    ro_html->add('span.version {').
    ro_html->add('  display: block;').
    ro_html->add('  margin-top: 0.3em;').
    ro_html->add('}').
    ro_html->add('span.page_title {').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  font-size: larger;').
    ro_html->add('  color: #bbb;').
    ro_html->add('  vertical-align: super;').
    ro_html->add('}').

    " Drop down styles
    ro_html->add('/*DROP DOWN*/').
    ro_html->add('.dropdown {').
    ro_html->add('    position: relative;').
    ro_html->add('    display: inline;').
    ro_html->add('    border-right: 1px solid lightgrey;').
    ro_html->add('}').
    ro_html->add('.dropbtn {').
    ro_html->add('    background-color: transparent;').
    ro_html->add('    color: #4078c0;').
    ro_html->add('    border: none;').
    ro_html->add('    padding-left: 0.5em;').
    ro_html->add('    padding-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.dropdown_content {').
    ro_html->add('    display: none;').
    ro_html->add('    position: absolute;').
    ro_html->add('    background-color: #f9f9f9;').
    ro_html->add('    right: 0;').
    ro_html->add('    top: 1.1em; /*IE7 woraround*/').
    ro_html->add('    border-right: 1px solid lightgrey;').
    ro_html->add('    border-bottom: 1px solid lightgrey;').
    ro_html->add('    min-width: 8em;').
    ro_html->add('}').
    ro_html->add('.dropdown_content a {').
    ro_html->add('    padding: 0.2em;').
    ro_html->add('    text-decoration: none;').
    ro_html->add('    border: 0px;').
    ro_html->add('    display: block;').
    ro_html->add('}').
    ro_html->add('.dropdown_content a:hover { background-color: #f1f1f1 }').
    ro_html->add('.dropdown:hover .dropdown_content { display: block; }').
    ro_html->add('.dropdown:hover .dropbtn { color: #79a0d2; }').

    " Other and outdated (?) styles
    ro_html->add('/* MISC AND REFACTOR */').
    ro_html->add('a.grey:link {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: smaller;').
    ro_html->add('}').
    ro_html->add('a.grey:visited {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: smaller;').
    ro_html->add('}').
    ro_html->add('a.plain:link {').
    ro_html->add('  color: black;').
    ro_html->add('  text-decoration: none;').
    ro_html->add('}').
    ro_html->add('a.plain:visited {').
    ro_html->add('  color: black;').
    ro_html->add('  text-decoration: none;').
    ro_html->add('}').
    ro_html->add('a.bkg:link {').
    ro_html->add('  color: #E8E8E8;').
    ro_html->add('}').
    ro_html->add('a.bkg:visited {').
    ro_html->add('  color: #E8E8E8;').
    ro_html->add('}').
    ro_html->add('h1 {').
    ro_html->add('  display: inline;').
    ro_html->add('}').
    ro_html->add('h2 {').
    ro_html->add('  display: inline;').
    ro_html->add('}').
    ro_html->add('h3 {').
    ro_html->add('  display: inline;').
    ro_html->add('  color: grey;').
    ro_html->add('  font-weight:normal;').
    ro_html->add('  font-size: smaller;').
    ro_html->add('}').
    ro_html->add('pre {').
    ro_html->add('  display: inline;').
    ro_html->add('}').

    ro_html->add('</style>').

  ENDMETHOD.                    "common styles

  METHOD get_logo_src.

    rv_src =
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAKMAAAAoCAYAAACSG0qbAAAABHNCSVQICAgIfAhk' &&
      'iAAAAAlwSFlzAAAEJQAABCUBprHeCQAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAA8' &&
      'VSURBVHic7Zx7cJzVeYef31nJAtvYko1JjM3FYHlXimwZkLWyLEMcwIGQcEkDJWmTltLStGkoDC' &&
      'kzwBAuCemUlksDNCkhJTTTljJpZhIuBQxxAWPvyuYiW7UkG8IlUByIsS1sLEu75+0fu5JXu9/et' &&
      'AJz0TOzM/rOec85765+37m+3yczY8w0NU3qrwv9npfaHfx02pPPd469sgk+7misYnyjpWXy5IOG' &&
      '7kd8ZjjNjEtr13TdOm7eTfCxwo2lUJAQASRu2dnRfMn4uDbBx42yxZhPiMNMCHKCsVK2GGuqqqo' &&
      'QUwrZTAhygrFQshjfaGmZ/M7yxQtmxGL9/qDqzwLxQvYTgpygXEoS4/DQ7LE1O05atLBu1YZdE4' &&
      'KcYLwpupoOmCO+5Z2dXPfExk07Tm2ZroGhBwX1wAygKqiOiVX2Rw9Jam/gyH0wuGGzvTEudRYSY' &&
      '4HFyogghxN2n7SwIendvcCioLoOtCCXNeqohOf0oDwPq9f3Wt/77dOHlWhYzUj/BRybTnrGEnZO' &&
      '5wv2m0rqDezJoOiqeZbzegzpk6TVPPWJTT39y5svMogF1ZcesjlQgkwYp4F+EJQXwv4E+MiLUZJ' &&
      'aF7AIcRq4hWZ2mMRhQD/oZcErXv7FScaja3rt/wpU9E/sFyLACQq57wB/XIl/gWIstn2TxpHVre' &&
      '7ZW71p8sFDeQscSEHKu3pTBadNH2Lq61VT57iwNazLgaNSqYaUaWXLDZCJIbBog3tK2A2xHns0o' &&
      'Mrm3CRrqdTPnAVMiUIEmLlz2XGLMxNmH7YrifFcoUIHalHj8f8p6UfAO+932weStno1zghps6Q7' &&
      'GBFiUYRxopkeaZ2vIwLyfxtQ4vV8lbWHNScacf+T/vwqn90oMZYhRADJ+bv725vmj6Q8tHWffPK' &&
      'UD6IgO/tsfawneRHYd97Pdg8kSyJaZiGtBY4pYPYOkH84C0Cyv8tKSiK7OZ99EpYAJ2V8AhkRY5' &&
      'lCHGaxhaq+BLCzY/EXd5y0aOG0td1vf1AFCWCw7/1u80DQEtahQvcB03MyjQfM7Hwnmxfv9dPiv' &&
      'X5SssqOwuzPSqk71mN3ymw5ZtdKdmVIdly8xx7JZ29yy0qptwrGLMRRCA6T1w93nLTo5Lq13Zv6' &&
      '25tOMRd6DLF4v0lWmQO8qPko45y7TWaHZyUnwa6M99mN2fYbuu1V4K5oxF1B4Z4UgFifrQHWFLN' &&
      'bvkh1QheV5DNNTZMqFWIGs5zX48M95PTqGa3TZ4erzbvj8/WUErf0L2++uNyGJLn2Js1oDeuYlk' &&
      'bNbmlRdeXup2hq0qS2es2VlHMDFaOlRdXL5uuwlnodG23QTEljCkbJV3d7WHOK+dXWqHqZnZebY' &&
      '1fGe3OFOArRU5GTGbSHNWdwUL8Epo1qIQ9V/bXu3HES4jCznNfjb7e1zZ8Ri/UD1MLzu05s/huM' &&
      'x4IKGNy4+8Tj/2Pqk8++Vaji86TQqxEuNNM5rWGtSCaokSDkgd0QjbidoPvN+5s7t9jz5TgdbdB' &&
      'MvLsG2cop6FgLUdUaZk804jYKuyrWa6vzlT2+XrOqQnxd6KwQOj5RhULpL9Yaxkcj7g3QT6zK39' &&
      '7ZbdtGtbtAZ+B0U3adkt0c67E7OyI6fFDuSpktC6HGpJjUGmZ3NOI2mdnVnX32eHZZ7903hGXfB' &&
      'G8mp3J7sd/B0DPCTgUmBf9O7lmMybk56or3Jn8foLVB7Q5dZ9Iy4OBsw2jYbUUk96fwQrzHf955' &&
      'iBZzsDA+aL9k1owZ20fNzaY/tfFXwK48ldQkSZ5YqJXmZk15JaJfmOmfgdOAmgCzWrCvyum5aIO' &&
      '+Uor3AIbOx7QV2TeBMPu3vKYASw091hbWt4PKRhu0oDqkmND1wAnk3vkOmAN2lRLa2hrWMVm5Te' &&
      'k2R3286YzWiK4eQltk9g1gMfsFMhVYKunR1obQddk+SXZqwLe8acMGe7fYb9HZk7wm3utrBmpsq' &&
      'iXsyClHMHK60hLWoRjHBfmLbP9K3bPYjFPIFWLaQeZnlZ8H4JyFflrMwcK4wG63v3/ycZnXOzqa' &&
      'lxE0mU7x9rvvVv93oVZqBtzNGGeU7Jbp9pZGzS7ReiVQVyDfmXRda4PaA9p5mBLmWGmmSronM0F' &&
      'ytUGGgjPTAi8UIeVk9u1og5YOJ0QbNBOjIac+Y22JPgLQ1WV7Ol+w36xebYnhtGpjFjBYTj3l4K' &&
      'Y9/dx6My4d74pN/Ki/Y9HpSG5HR/Nyh/1DHtO9OM6dvWFDwbtWslOykt6Us5VWZbOFnQtsyMqvc' &&
      '56Ty3T7NeBhLGAfDZDpe5nX6V5uXpbZ43K2NGQ2V9glwLas/I62hfrE8EWsJ3mFsGYs+OQqze+A' &&
      '1cBLgbmma4f/9AmOJGBe5vKVLYN1W6wnOWSHmdkVhexMPG6yC0x2AbmjoQ3njdh4uwrSw1Htmq5' &&
      'bd3Y0I3FLpQ5n0GTSQ7s6Fva70RPYTPbi+Pz0J7ryboRC+m5PnRfsJjVEAfp5bLNflTb52dKIBj' &&
      '36RWY5ZyX2WCLukvbX67ZYHFLHZtGw+1fD/jDL8qQljWpav9m6Uw3wKYzXgUNJTxsk+0Fssw0L6' &&
      'x+j4dCx6eF/BEtwDBkbx7Fe29gWCa0yrC2rvXXO26WZfrWG3V2kji8zWbm0QUev67GX5ZgZ8A0H' &&
      '121hXIIZNrxou9oW6m4b4m/z2aTP+fsAohF3PaNHROvssZ8ElRs5DnyPBAkovxDFF4oJESDeY9t' &&
      'JD4Ur5umgPSFm1Uy23Zk2SaM7e43p5Y4uxUMzu2f4H56+tuZmff2gfTqHrGEy5DkW6Abo7LH7gf' &&
      'sB2uo1LQGzBmoYFSwg57vNcjqqo4F1JXh2S7Zfx83TZZNqdD6MXkQkU369jONgcmfxe83MB7XQE' &&
      'dEhg1B0HzDk2ZHpy3vBqLPpMQhyi/f2AIA3WyPZG6KkeVpKiE925awEi7H6JRsAcqJDfIi9oayf' &&
      'W8ZB5dY/TFeX7YlGQg+RmgJkcnSQfWyr9QP92enmGcgeNCvx67mXbGdbxD1hjI5AklJ+ydgTUGz' &&
      '6iiZNXd09+gYGGIRlQgXn6wDesZYSRFsJOYES5QjSw7fqnu7qBqh7uqu7f3nzdw3uKFJszEIcpq' &&
      'VRs12SRuAYiTrJ1YXMzSGgS6iQnHmWyQWe70pySz/FMZagMWnMlaiTuTqTTih7s7IIHm1T1ncVI' &&
      '37l3BAAA4McAYF7iAvG17uxExi1U6Igd9XNDj+UmZA8qPrf3MDQbeSPIN8Ldub0JzeWLcT2I3Sw' &&
      'n8JFhr4VQnMze5uKnv0ugOHfUXa3ZhySedkR0eGDuMtbw/rTZCI1pA9PF0yWf4e3MnJ7YKXm0pO' &&
      'r6H03QRIIZeYnUj1njhid8aaRscKX/VGWSRLsCjnK2rcdC3njGUsQ5PSdv92yqJaMk5WBoRMpJs' &&
      'SnNgZufBdCkmsN60FgRbllK8PNzOlttT/qpz2sOUnpeWGHvq9ewcyc28/7XQCru213NOL+l6wgZ' &&
      '0kXAjnDcazP7gXuTdu41rCyxbgr3mt/P16+F6LgUVXtmq5bC237yNsNu5YtPBZgx4kLFznZ1XlM' &&
      'BzB/1liECBAN801yhfiq0HflbKXz1ojZ4qCylSBsbm6q/93wX0n0Q1Ir6UzWYXaZyZaFqqxeZn8' &&
      '13n4ZlhPWJWXMo00P5OTDF5c0qmm8fRlPip6bFhHk6Ti3ddfy5i3OXBemJQE2A5g/c/qaTasC8k' &&
      'rC0KdzE+3qWG/y6thmW7Vui/UkQ7w51vqDaGnRZFInPdlshNQ2C8oJh0oqaefF++zmzh5bu7bbX' &&
      'rBxjp88bp5qgZzNdyfWD/9t+B+TO4GW8/p+R0SHcGBxLWEFjiQlHeIXEaRIPZAVRMVCTDcQCUh8' &&
      'LfOyaqjgCcr+YpY7NRFa2VY/egsqtNtdw8ie5gjJoUTqicjofOYA2f/YgcR03s5MMBF4wlIa7rM' &&
      'r5mnUyru6xl0LZAeFvDG3l83DF5199mukoJO1FUMoviSi8Nh9Kg+Ru7qvUvCqPO+cMZsxbPsM4H' &&
      'XW9KcrEyKApTa7s9BVSyLaF3IkSbLSQros18RyInkkV2u5q+6zLaS+aCT0oJl/QVI78IWcsvDos' &&
      '1vtLYCE551QKNuCKW63+157g36cMOYI9yWhC3K+j4KDEHKxC9+t0altDaFHwL/kvVZIBJw761/u' &&
      'M5/MTJlU7S/ZN6hTBNlhZA0OPReNuGdM6nL4jR4G5ZnRusAtKmVHwg1Slcxe11nODZJKh1fJ6kw' &&
      'M3dQaVgOw3omjkGuL9/o/L/vFTzs7mi8pQZBpIT4f9PxE2bRFQncY9pdjKDoExDH7ebzPbgFobQ' &&
      'jdng48KBfvzZau77ORN61FI66PsW2N7ARiZnZTZ589BtAWCV1v5J1zF+JNVdui2CbLOcJsq1ejD' &&
      '2lVgCDL4e14r58J0N6k+cmEu0HYIssdrbxgnaGeeG9yJEg32hC6GbOix81ytrTsWLtiixpgQNLZ' &&
      '4yVEgCT++xSP0H7C0N1ZadVAh6SR3kRm2WfJO0H/XqTuQcn+IlOIAFjRVaZhus3g2az0WuA0wcI' &&
      'i5QP3DDNIIPtakBABYltts7AO4OEi9eTFYGCksSRzwM4LECKAM1gG9tVR5UP+RkqZN5s7a0yBnw' &&
      'UEOSDp7GlPPp83BH0srO+1PmQrDIIen9wOdnlnn31G5n9ZtDLL6ck2x3uTf6DUee8rASX6vNnyW' &&
      'I/dmZ0R77O7LNXLBkWy9CE7Pd6XvNihQkEQeZHZl9PBFtsDstebtyWFwv0B4r32UrzXn+6xDtBd' &&
      'wIslNL0N+JnMvravxiraFO/stm0y+xzQlcfkddCNCe/vGfP7GQH6lzdfbHAjqSCBHZK+PN5CzES' &&
      'SlixgnhMLzXAeXp+3hWfuM0sWL10abQv1CdtHixzvmtiYPhcvSFOTJk1NEPEQkWdPUry4oc96y2' &&
      'o3YJiWs5WxzbYq83THHHu9Y1N2kG45tDRqdsgzxxuznKPOGbsTsN2M7d6zfXhePJ5Ici1h6mUcA' &&
      'cw08Zo5fp35NoqKxAjwTrRhZmLSpPY9ySmPzV27dm+lTn9cKSTGA+XT+03Jq+l8HBLv2Q7cX9K+' &&
      'ygQTFGDcHhaaoGJyouDNV7JH+eGj4mF6gspoC+tzJt1ObsT4MDsF2zxs886+Ml5v/PogUvEwPUG' &&
      'FiE+SX4gAtQa1gkhV7onQR4oJMR5oxC6stDeghd7Dh6E+CPw/HL4vVO2fcpUAAAAASUVORK5CYII='.

  ENDMETHOD.                    "base64_logo


ENDCLASS.


CLASS lcl_gui_page_main DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA: go_user TYPE REF TO lcl_persistence_user.

    METHODS build_menu
      RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    CLASS-METHODS render_repo_online
      IMPORTING io_repo        TYPE REF TO lcl_repo_online
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    CLASS-METHODS render_top
      IMPORTING io_repo        TYPE REF TO lcl_repo_online
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    CLASS-METHODS render_explore
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    CLASS-METHODS render_repo_offline
      IMPORTING io_repo        TYPE REF TO lcl_repo_offline
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    CLASS-METHODS jump_link
      IMPORTING iv_obj_type    TYPE tadir-object
                iv_obj_name    TYPE tadir-obj_name
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS jump_encode
      IMPORTING iv_obj_type      TYPE tadir-object
                iv_obj_name      TYPE tadir-obj_name
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS jump_decode
      IMPORTING iv_string   TYPE clike
      EXPORTING ev_obj_type TYPE tadir-object
                ev_obj_name TYPE tadir-obj_name
      RAISING   lcx_exception.

    CLASS-METHODS render_error
      IMPORTING ix_error       TYPE REF TO lcx_exception
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    CLASS-METHODS render_toc
      IMPORTING it_list        TYPE lcl_repo_srv=>ty_repo_tt
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    CLASS-METHODS render_repo_menu
      IMPORTING iv_key         TYPE lcl_persistence_db=>ty_value
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    CLASS-METHODS install
      IMPORTING iv_url TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS newoffline
      RAISING lcx_exception.

    CLASS-METHODS uninstall
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS remove
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS pull
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS stage
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS diff
      IMPORTING is_result TYPE lcl_file_status=>ty_result
                iv_key    TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS file_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
                is_file          TYPE lcl_file_status=>ty_result
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS file_decode
      IMPORTING iv_string TYPE clike
      EXPORTING ev_key    TYPE lcl_persistence_repo=>ty_repo-key
                es_file   TYPE lcl_file_status=>ty_result
      RAISING   lcx_exception.

    CLASS-METHODS abapgit_installation
      RAISING lcx_exception.

    CLASS-METHODS is_repo_installed
      IMPORTING iv_url              TYPE string
                iv_target_package   TYPE devclass OPTIONAL
      RETURNING VALUE(rv_installed) TYPE abap_bool
      RAISING   lcx_exception.

    CLASS-METHODS needs_installation
      RETURNING VALUE(rv_not_completely_installed) TYPE abap_bool.

ENDCLASS.

CLASS lcl_gui_page_diff DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_local  TYPE ty_file
        is_remote TYPE ty_file.

    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mv_filename TYPE string,
          ms_stats    TYPE lcl_diff=>ty_count,
          mo_diff     TYPE REF TO lcl_diff.

    METHODS styles       RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_head  RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_diff  RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_diff IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mv_filename = is_local-filename.

    CREATE OBJECT mo_diff
      EXPORTING
        iv_local  = is_local-data
        iv_remote = is_remote-data.

  ENDMETHOD.

  METHOD styles.
    DATA lo_html TYPE REF TO lcl_html_helper.
    CREATE OBJECT lo_html.

    lo_html->add( '/* DIFF */' ).                           "#EC NOTEXT
    lo_html->add( 'div.diff {' ).                           "#EC NOTEXT
    lo_html->add( '  background-color: #f2f2f2;' ).         "#EC NOTEXT
    lo_html->add( '  padding: 0.7em    ' ).                 "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'div.diff_head {' ).                      "#EC NOTEXT
    lo_html->add( '  border-bottom: 1px solid #DDD;' ).     "#EC NOTEXT
    lo_html->add( '  padding-bottom: 0.7em;' ).             "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_name {' ).                     "#EC NOTEXT
    lo_html->add( '  padding-left: 0.5em;' ).               "#EC NOTEXT
    lo_html->add( '  color: grey;' ).                       "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_name strong {' ).              "#EC NOTEXT
    lo_html->add( '  color: #333;' ).                       "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_banner {' ).                   "#EC NOTEXT
    lo_html->add( '  border-style: solid;' ).               "#EC NOTEXT
    lo_html->add( '  border-width: 1px;' ).                 "#EC NOTEXT
    lo_html->add( '  border-radius: 3px;' ).                "#EC NOTEXT
    lo_html->add( '  padding-left: 0.3em;' ).               "#EC NOTEXT
    lo_html->add( '  padding-right: 0.3em;' ).              "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_ins {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #38e038;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #91ee91 !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_del {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #ff8093;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #ffb3be !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_upd {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #dada00;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #ffffb3 !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'div.diff_content {' ).                   "#EC NOTEXT
    lo_html->add( '  background: #fff;' ).                  "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab {' ).                     "#EC NOTEXT
    lo_html->add( '  width: 98%;' ).                        "#EC NOTEXT
    lo_html->add( '  border-collapse: collapse;' ).         "#EC NOTEXT
    lo_html->add( '  font-family: Consolas, Courier, monospace;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab th {' ).                  "#EC NOTEXT
    lo_html->add( '  color: grey;' ).                       "#EC NOTEXT
    lo_html->add( '  text-align: left;' ).                  "#EC NOTEXT
    lo_html->add( '  font-weight: normal;' ).               "#EC NOTEXT
    lo_html->add( '  padding: 0.5em;' ).                    "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td {' ).                  "#EC NOTEXT
    lo_html->add( '  color: #444;' ).                       "#EC NOTEXT
    lo_html->add( '  padding-left: 0.5em;' ).               "#EC NOTEXT
    lo_html->add( '  padding-right: 0.5em;' ).              "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td.num, th.num {' ).      "#EC NOTEXT
    lo_html->add( '  text-align: right;' ).                 "#EC NOTEXT
    lo_html->add( '  color: #ccc;' ).                       "#EC NOTEXT
    lo_html->add( '  border-left: 1px solid #eee;' ).       "#EC NOTEXT
    lo_html->add( '  border-right: 1px solid #eee;' ).      "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td.cmd, th.cmd {' ).      "#EC NOTEXT
    lo_html->add( '  font-size: smaller;' ).                "#EC NOTEXT
    lo_html->add( '  text-align: center !important;' ).     "#EC NOTEXT
    lo_html->add( '  white-space: nowrap;' ).               "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab tr.diff_nav_line {').     "#EC NOTEXT
    lo_html->add( '  background-color: #edf2f9;').          "#EC NOTEXT
    lo_html->add( '}').                                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab tr.diff_nav_line td {').  "#EC NOTEXT
    lo_html->add( '  color: #ccc;').                        "#EC NOTEXT
    lo_html->add( '}').                                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab code {' ).                "#EC NOTEXT
    lo_html->add( '  font-family: inherit;' ).              "#EC NOTEXT
    lo_html->add( '  font-size: normal;' ).                 "#EC NOTEXT
    lo_html->add( '  white-space: pre;' ).                  "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_head.
    DATA lo_html  TYPE REF TO lcl_html_helper.
    CREATE OBJECT lo_html.

    ms_stats = mo_diff->stats( ).

    lo_html->add( '<div class="diff_head">' ).              "#EC NOTEXT
    lo_html->add( |<span class="diff_banner diff_ins">+ { ms_stats-insert }</span>| ).
    lo_html->add( |<span class="diff_banner diff_del">- { ms_stats-delete }</span>| ).
    lo_html->add( |<span class="diff_banner diff_upd">~ { ms_stats-update }</span>| ).
    lo_html->add( '<span class="diff_name">' ).             "#EC NOTEXT
    lo_html->add( |{ mv_filename }| ).
    lo_html->add( '</span>' ).                              "#EC NOTEXT
    lo_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_diff.

    DATA lo_html         TYPE REF TO lcl_html_helper.
    DATA lt_diffs        TYPE lcl_diff=>ty_diffs_tt.
    DATA lv_local        TYPE string.
    DATA lv_remote       TYPE string.
    DATA lv_attr_local   TYPE string.
    DATA lv_attr_remote  TYPE string.
    DATA lv_anchor_count LIKE sy-tabix.
    DATA lv_href         TYPE string.
    DATA lb_insert_nav   TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.
    FIELD-SYMBOLS <ls_break> LIKE LINE OF lt_diffs.

    CREATE OBJECT lo_html.
    lt_diffs = mo_diff->get( ).

    lo_html->add( '<div class="diff_content">' ).           "#EC NOTEXT
    lo_html->add( '<table class="diff_tab">' ).             "#EC NOTEXT
    lo_html->add( '<tr>' ).                                 "#EC NOTEXT
    lo_html->add( '<th class="num"></th>' ).                "#EC NOTEXT
    lo_html->add( '<th>@LOCAL</th>' ).                      "#EC NOTEXT
    lo_html->add( '<th class="num"></th>' ).                "#EC NOTEXT
    lo_html->add( '<th>@REMOTE</th>' ).                     "#EC NOTEXT
    lo_html->add( '<th class="cmd"><a href=#diff_1>&#x25BC; 1</a></th>' ). "#EC NOTEXT
    lo_html->add( '</tr>' ).                                "#EC NOTEXT

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-short = abap_false.
        lb_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lb_insert_nav = abap_true. " Insert separator line with navigation
        lb_insert_nav = abap_false.
        lo_html->add( '<tr class="diff_nav_line"><td class="num"></td>' ).
        lo_html->add( |<td colspan="4">@@ { <ls_diff>-local_line }, { <ls_diff>-remote_line }</td>| ).
        lo_html->add( '</tr>' ).
      ENDIF.

      lv_local  = escape( val = <ls_diff>-local  format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-remote format = cl_abap_format=>e_html_attr ).

      CLEAR: lv_attr_local, lv_attr_remote. " Class for changed lines
      CASE <ls_diff>-result.
        WHEN lcl_diff=>c_diff-insert.
          lv_attr_local  = ' class="diff_ins"'.             "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          lv_attr_remote = ' class="diff_del"'.             "#EC NOTEXT
        WHEN lcl_diff=>c_diff-update.
          lv_attr_local  = ' class="diff_upd"'.             "#EC NOTEXT
          lv_attr_remote = ' class="diff_upd"'.             "#EC NOTEXT
      ENDCASE.

      CLEAR lv_href.  " Create link to next change
      IF <ls_diff>-result IS NOT INITIAL.
        lv_anchor_count = lv_anchor_count + 1.
        IF lv_anchor_count < ms_stats-insert + ms_stats-delete + ms_stats-update.
          lv_href = |<a name="diff_{ lv_anchor_count }"|
                 && |   href="#diff_{ lv_anchor_count + 1 }|
                 && |">&#x25BC; { lv_anchor_count + 1 }</a>|.
        ELSE.
          lv_href = |<a name="diff_{ lv_anchor_count }"></a>|.
        ENDIF.
      ENDIF.

      lo_html->add( '<tr>' ).                                               "#EC NOTEXT
      lo_html->add( |<td class="num">{ <ls_diff>-local_line }</td>| ).      "#EC NOTEXT
      lo_html->add( |<td{ lv_attr_local }><code>{ lv_local }</code></td>| ).  "#EC NOTEXT
      lo_html->add( |<td class="num">{ <ls_diff>-remote_line }</td>| ).     "#EC NOTEXT
      lo_html->add( |<td{ lv_attr_remote }><code>{ lv_remote }</code></td>| )."#EC NOTEXT
      lo_html->add( |<td class="cmd">{ lv_href }</td>| ).                   "#EC NOTEXT
      lo_html->add( '</tr>' ).                                              "#EC NOTEXT

    ENDLOOP.

    lo_html->add( '</table>' ).                             "#EC NOTEXT
    lo_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN OTHERS.
        _raise 'Unknown action'.                            "#EC NOTEXT
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( iv_page_title = 'DIFF' ) ).
    ro_html->add( '<div class="diff">' ).                   "#EC NOTEXT
    ro_html->add( render_head( ) ).
    ro_html->add( render_diff( ) ).
    ro_html->add( '</div>' ).                               "#EC NOTEXT
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_stage IMPLEMENTATION.

  METHOD lookup.
    READ TABLE mt_stage INTO rs_stage
      WITH KEY file-path = iv_path
      file-filename = iv_filename.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.

  METHOD append.

    DATA: ls_stage LIKE LINE OF mt_stage.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF mt_stage.


    READ TABLE mt_stage WITH KEY
      file-path = is_file-path
      file-filename = is_file-filename
      ASSIGNING <ls_stage>.
    IF sy-subrc = 0.
      <ls_stage>-file-data = is_file-data.
      <ls_stage>-method = iv_method.
    ELSE.
      ls_stage-file   = is_file.
      ls_stage-method = iv_method.
      INSERT ls_stage INTO TABLE mt_stage.
    ENDIF.

  ENDMETHOD.

  METHOD method_description.

    CASE iv_method.
      WHEN c_method-add.
        rv_description = 'add'.
      WHEN c_method-rm.
        rv_description = 'rm'.
      WHEN c_method-ignore.
        rv_description = 'ignore' ##NO_TEXT.
      WHEN OTHERS.
        _raise 'unknown staging method type'.
    ENDCASE.

  ENDMETHOD.

  METHOD add.
    append( is_file   = is_file
            iv_method = c_method-add ).
  ENDMETHOD.

  METHOD reset.
    DELETE mt_stage WHERE file-path = is_file-path
      AND file-filename = is_file-filename.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD rm.
    append( is_file   = is_file
            iv_method = c_method-rm ).
  ENDMETHOD.

  METHOD ignore.
    append( is_file   = is_file
            iv_method = c_method-ignore ).
  ENDMETHOD.

  METHOD count.
    rv_count = lines( mt_stage ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_commit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo  TYPE REF TO lcl_repo_online
                io_stage TYPE REF TO lcl_stage
      RAISING   lcx_exception.

    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    TYPES: BEGIN OF ty_fields,
             username TYPE string,
             email    TYPE string,
             comment  TYPE string,
             body     TYPE string,
           END OF ty_fields.

    METHODS:
      render_files
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      push
        IMPORTING it_postdata TYPE cnht_post_data_tab
        RAISING   lcx_exception,
      update_userdata
        IMPORTING is_fields TYPE ty_fields
        RAISING   lcx_exception,
      parse
        IMPORTING it_postdata      TYPE cnht_post_data_tab
        RETURNING VALUE(rs_fields) TYPE ty_fields.

ENDCLASS.

CLASS lcl_gui_page_commit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo  = io_repo.
    mo_stage = io_stage.
  ENDMETHOD.

  METHOD update_userdata.

    DATA: lo_user TYPE REF TO lcl_persistence_user.

    CREATE OBJECT lo_user.
    lo_user->set_username( is_fields-username ).
    lo_user->set_email( is_fields-email ).

  ENDMETHOD.

  METHOD push.

    DATA: ls_fields  TYPE ty_fields,
          ls_comment TYPE ty_comment.


    ls_fields = parse( it_postdata ).

    update_userdata( ls_fields ).

    IF ls_fields-username IS INITIAL.
      _raise 'empty username'.
    ENDIF.
    IF ls_fields-email IS INITIAL.
      _raise 'empty email'.
    ENDIF.
    IF ls_fields-comment IS INITIAL.
      _raise 'empty comment'.
    ENDIF.

    ls_comment-username = ls_fields-username.
    ls_comment-email    = ls_fields-email.
    ls_comment-comment  = ls_fields-comment.

    IF NOT ls_fields-body IS INITIAL.
      CONCATENATE ls_comment-comment gc_newline ls_fields-body
        INTO ls_comment-comment.
    ENDIF.

    mo_repo->push( is_comment = ls_comment
                   io_stage   = mo_stage ).

    COMMIT WORK.

    lcl_gui=>back( ).

  ENDMETHOD.

  METHOD parse.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    REPLACE ALL OCCURRENCES OF gc_newline IN lv_string WITH lc_replace.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'username' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-username = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'email' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-email = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'comment' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-comment = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'body' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-body = <ls_field>-value.
    REPLACE ALL OCCURRENCES OF lc_replace IN rs_fields-body WITH gc_newline.

  ENDMETHOD.

  METHOD render_files.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    CREATE OBJECT ro_html.

    lt_stage = mo_stage->get_all( ).

    ro_html->add( '<table>' ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.
      ro_html->add( '<tr>' ).
      ro_html->add( '<td>' ).
      ro_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td>' ).
      ro_html->add( lcl_stage=>method_description( <ls_stage>-method ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</table>' ).
    ro_html->add( '<br>' ).

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN 'post'.
        push( it_postdata ).
      WHEN 'cancel'.
        lcl_gui=>back( ).
      WHEN OTHERS.
        _raise 'Unknown action, commit'.
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lo_user  TYPE REF TO lcl_persistence_user,
          lv_user  TYPE string,
          lv_email TYPE string.


    CREATE OBJECT ro_html.

    CREATE OBJECT lo_user.
    lv_user = lo_user->get_username( ).
    lv_email = lo_user->get_email( ).

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    ro_html->add( header( ) ).

    "TODO refactor
    ro_html->add( '<div id="header">' ).
    ro_html->add( '<h1>Commit</h1>' ).
    ro_html->add( '<a href="sapevent:cancel">Cancel</a>' ).
    ro_html->add( '</div>' ).

    ro_html->add( '<div id="toc">' ).
    ro_html->add( render_files( ) ).
    ro_html->add( '<form method="post" action="sapevent:post">' ).
    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>username</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<input name="username" type="text" value="' && lv_user && '">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>email</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<input name="email" type="text" value="' && lv_email && '">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>comment</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<input name="comment" type="text" id="cmt" maxlength="50" size="50">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td colspan="2">' ).
    ro_html->add( 'body <br>' ) ##NO_TEXT.
    ro_html->add( '<textarea name="body" rows="10" cols="72"></textarea>' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td colspan="2" align="right">' ).
    ro_html->add( '<input type="submit" value="Push">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).
    ro_html->add( '</form>' ).
    ro_html->add( '<script>' ).
    ro_html->add( 'document.getElementById("cmt").focus();' ).
    ro_html->add( '</script>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_stage DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo_online
      RAISING   lcx_exception.

    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo   TYPE REF TO lcl_repo_online,
          mt_remote TYPE ty_files_tt,
          mo_stage  TYPE REF TO lcl_stage,
          mt_local  TYPE ty_files_item_tt.

    METHODS:
      file_encode
        IMPORTING is_file          TYPE ty_file
        RETURNING VALUE(rv_string) TYPE string,
      file_decode
        IMPORTING iv_string      TYPE clike
        RETURNING VALUE(rs_file) TYPE ty_file
        RAISING   lcx_exception,
      refresh
        RAISING lcx_exception,
      all
        RAISING lcx_exception,
      call_commit
        RAISING lcx_exception,
      render_local
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_remote
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_stage IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo = io_repo.
    refresh( ).
  ENDMETHOD.

  METHOD all.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF mt_local.


    ASSERT mo_stage->count( ) = 0.
    ASSERT lines( mt_local ) > 0.

    LOOP AT mt_local ASSIGNING <ls_file>.
      mo_stage->add( <ls_file>-file ).
    ENDLOOP.

    call_commit( ).

  ENDMETHOD.

  METHOD call_commit.

    DATA: lo_commit TYPE REF TO lcl_gui_page_commit.

    CREATE OBJECT lo_commit
      EXPORTING
        io_repo  = mo_repo
        io_stage = mo_stage.
    lcl_gui=>set_page( lo_commit ).

  ENDMETHOD.

  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.


    ls_field-name = 'PATH'.
    ls_field-value = is_file-path.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'FILENAME'.
    ls_field-value = is_file-filename.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.

  METHOD file_decode.

    DATA: lt_fields   TYPE tihttpnvp,
          lv_path     TYPE string,
          lv_filename TYPE string,
          ls_local    LIKE LINE OF mt_local,
          lv_string   TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'PATH'.
    ASSERT sy-subrc = 0.
    lv_path = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'FILENAME'.
    ASSERT sy-subrc = 0.
    lv_filename = <ls_field>-value.

* the file should exist in either mt_lcoal or mt_remote, not in both at same time
    READ TABLE mt_local INTO ls_local WITH KEY file-path = lv_path file-filename = lv_filename.
    IF sy-subrc = 0.
      rs_file = ls_local-file.
    ELSE.
      READ TABLE mt_remote INTO rs_file WITH KEY path = lv_path filename = lv_filename.
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD refresh.

    DATA: ls_files TYPE lcl_stage_logic=>ty_stage_files.

    CREATE OBJECT mo_stage.

    ls_files = lcl_stage_logic=>get( mo_repo ).
    mt_local = ls_files-local.
    mt_remote = ls_files-remote.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    DATA: ls_file   TYPE ty_file.


    CASE iv_action.
      WHEN 'add'.
        ls_file = file_decode( iv_getdata ).
        mo_stage->add( ls_file ).
        lcl_gui=>render( ).
      WHEN 'all'.
        all( ).
      WHEN 'reset'.
        ls_file = file_decode( iv_getdata ).
        mo_stage->reset( ls_file ).
        lcl_gui=>render( ).
      WHEN 'ignore'.
        ls_file = file_decode( iv_getdata ).
        mo_stage->ignore( ls_file ).
        lcl_gui=>render( ).
      WHEN 'rm'.
        ls_file = file_decode( iv_getdata ).
        mo_stage->rm( ls_file ).
        lcl_gui=>render( ).
      WHEN 'commit'.
        call_commit( ).
      WHEN OTHERS.
        _raise 'Unknown action, stage'.
    ENDCASE.

  ENDMETHOD.

  METHOD render_local.

    DATA: lv_encode TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF mt_local.


    CREATE OBJECT ro_html.

    IF lines( mt_local ) = 0.
      RETURN.
    ENDIF.

    ro_html->add( 'Local:<br>' ) ##NO_TEXT.

    ro_html->add( '<table>' ).
    LOOP AT mt_local ASSIGNING <ls_file>.
      ro_html->add( '<tr>' ).
      lv_encode = file_encode( <ls_file>-file ).

      ro_html->add( '<td>' ).
      ro_html->add( <ls_file>-file-path && <ls_file>-file-filename ).
      ro_html->add( '</td>' ).

      ro_html->add( '<td>' ).
      TRY.
          mo_stage->lookup( iv_path = <ls_file>-file-path iv_filename = <ls_file>-file-filename ).
          ro_html->add( '<a href="sapevent:reset?' && lv_encode && '">reset</a>' ).
        CATCH lcx_not_found.
          ro_html->add( '<a href="sapevent:add?' && lv_encode && '">add</a>' ).
      ENDTRY.
      ro_html->add( '</td>' ).

      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</table>' ).

  ENDMETHOD.

  METHOD render_remote.

    DATA: lv_encode TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF mt_remote.


    CREATE OBJECT ro_html.

    IF lines( mt_remote ) = 0.
      RETURN.
    ENDIF.

    ro_html->add( 'Remote:<br>' ) ##NO_TEXT.

    ro_html->add( '<table>' ).
    LOOP AT mt_remote ASSIGNING <ls_file>.
      ro_html->add( '<tr>' ).
      lv_encode = file_encode( <ls_file> ).

      ro_html->add( '<td>' ).
      ro_html->add( <ls_file>-path && <ls_file>-filename ).
      ro_html->add( '</td>' ).

      ro_html->add( '<td>' ).
      TRY.
          mo_stage->lookup( iv_path = <ls_file>-path iv_filename = <ls_file>-filename ).
          ro_html->add( '<a href="sapevent:reset?' && lv_encode && '">reset</a>' ).
        CATCH lcx_not_found.
          ro_html->add( '<a href="sapevent:ignore?' && lv_encode && '">ignore</a>' ).
          ro_html->add( '<a href="sapevent:rm?' && lv_encode && '">rm</a>' ).
      ENDTRY.
      ro_html->add( '</td>' ).

      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</table>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( iv_page_title = 'STAGE' ) ).

    ro_html->add( '<div id="toc">' ).
    ro_html->add( render_local( ) ).
    ro_html->add( '<br>' ).
    ro_html->add( render_remote( ) ).
    ro_html->add( '<br>' ).

    IF mo_stage->count( ) > 0.
      ro_html->add( '<a href="sapevent:commit">commit</a>' ).
    ELSEIF lines( mt_local ) > 0.
      ro_html->add( '<a href="sapevent:all">add all and commit</a>' ).
    ENDIF.

    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_db DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      delete
        IMPORTING is_key TYPE lcl_persistence_db=>ty_content
        RAISING   lcx_exception,
      delete_popup
        RETURNING VALUE(rv_continue) TYPE abap_bool
        RAISING   lcx_exception,
      key_encode
        IMPORTING is_key           TYPE lcl_persistence_db=>ty_content
        RETURNING VALUE(rv_string) TYPE string,
      key_decode
        IMPORTING iv_string     TYPE clike
        RETURNING VALUE(rs_key) TYPE lcl_persistence_db=>ty_content.

ENDCLASS.

CLASS lcl_gui_page_main IMPLEMENTATION.

  METHOD diff.

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_item_tt,
          lo_page   TYPE REF TO lcl_gui_page_diff,
          lo_repo   TYPE REF TO lcl_repo_online.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local>  LIKE LINE OF lt_local.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).

    lt_remote = lo_repo->get_files_remote( ).

    lt_local = lo_repo->get_files_local( ).

    READ TABLE lt_remote ASSIGNING <ls_remote>
      WITH KEY filename = is_result-filename
      path = is_result-path.
    IF sy-subrc <> 0.
      _raise 'file not found remotely'.
    ENDIF.

    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY file-filename = is_result-filename
      file-path = is_result-path.
    IF sy-subrc <> 0.
      _raise 'file not found locally'.
    ENDIF.

    CREATE OBJECT lo_page
      EXPORTING
        is_local  = <ls_local>-file
        is_remote = <ls_remote>.

    lcl_gui=>call_page( lo_page ).

  ENDMETHOD.                    "diff

  METHOD pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).

    lo_repo->refresh( ).

    lo_repo->deserialize( ).

    COMMIT WORK.

    lcl_gui=>render( ).

  ENDMETHOD.                    "pull

  METHOD stage.

    DATA: lo_repo  TYPE REF TO lcl_repo_online,
          lo_stage TYPE REF TO lcl_gui_page_stage.


    lo_repo ?= lcl_repo_srv=>get( iv_key ).

* force refresh on stage, to make sure the latest local and remote files are used
    lo_repo->refresh( ).

    CREATE OBJECT lo_stage
      EXPORTING
        io_repo = lo_repo.

    lcl_gui=>call_page( lo_stage ).

  ENDMETHOD.

  METHOD jump_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'TYPE'.
    IF sy-subrc = 0.
      ev_obj_type = <ls_field>-value.
    ELSE.
      CLEAR ev_obj_type.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'NAME'.
    IF sy-subrc = 0.
      ev_obj_name = <ls_field>-value.
    ELSE.
      CLEAR ev_obj_name.
    ENDIF.

  ENDMETHOD.

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

  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.


    ls_field-name = 'TYPE'.
    ls_field-value = iv_obj_type.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'NAME'.
    ls_field-value = iv_obj_name.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.

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

    COMMIT WORK.

    lcl_gui=>render( ).

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

    COMMIT WORK.

    lcl_gui=>render( ).

  ENDMETHOD.                    "remove

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

    COMMIT WORK.

    lcl_gui=>render( ).

  ENDMETHOD.                    "newoffline

  METHOD install.

    DATA: lv_returncode  TYPE c,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lv_branch_name TYPE string,
          lv_icon_ok     TYPE icon-name,
          lv_icon_br     TYPE icon-name,
          lv_icon_msg    TYPE icon-name,
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
    lv_icon_msg = icon_msg.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
      EXPORTING
        popup_title       = 'Clone'
        programname       = sy-repid
        formname          = 'BRANCH_POPUP'
        ok_pushbuttontext = 'OK'
        icon_ok_push      = lv_icon_ok
        first_pushbutton  = 'Select branch'
        icon_button_1     = lv_icon_br
        second_pushbutton = 'Create package'
        icon_button_2     = lv_icon_msg
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

    COMMIT WORK.

    lcl_gui=>render( ).

  ENDMETHOD.                    "install

  METHOD build_menu.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.
    DATA lo_betasub TYPE REF TO lcl_html_toolbar.
    CREATE OBJECT lo_toolbar.
    CREATE OBJECT lo_betasub.

    lo_betasub->add( iv_txt = 'Database util'    iv_cmd = 'sapevent:db' ).

    lo_toolbar->add( iv_txt = 'Refresh All'      iv_cmd = 'sapevent:refresh' ).
    lo_toolbar->add( iv_txt = 'Clone'            iv_cmd = 'sapevent:install' ).
    lo_toolbar->add( iv_txt = 'Explore'          iv_cmd = 'sapevent:explore' ).
    lo_toolbar->add( iv_txt = 'New Offline Repo' iv_cmd = 'sapevent:newoffline' ).
    IF needs_installation( ) = abap_true.
      lo_toolbar->add( iv_txt = 'Install'        iv_cmd = 'sapevent:abapgit_installation' ).
    ENDIF.
    lo_toolbar->add( iv_txt = '<b>&#x03b2;</b>'  iv_sub = lo_betasub ).

    ro_menu = lo_toolbar.

  ENDMETHOD.                    "build menu

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  display:          block;').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding:          0.7em    ').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  font-size: x-large;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: smaller;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-right:     1em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('  color: grey;').
    ro_html->add('}').
    ro_html->add('.repo_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  border-radius: 3px;').
    ro_html->add('  background: #ffffff;').
    ro_html->add('  margin-top: 1em;').
    ro_html->add('}').
    ro_html->add('.repo_tab tr.unsupported {').
    ro_html->add('  color: lightgrey;').
    ro_html->add('}').
    ro_html->add('.repo_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  vertical-align: top;').
    ro_html->add('  padding-top: 2px;').
    ro_html->add('  padding-bottom: 2px;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.icon {').
    ro_html->add('  padding-left: 10px;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.type {').
    ro_html->add('  width: 3.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.object {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.files {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('  padding-right: 0.5em;').
    ro_html->add('}').

  ENDMETHOD.

  METHOD render_repo_offline.

    DATA: lt_tadir TYPE lcl_tadir=>ty_tadir_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( '<a id="' && io_repo->get_name( ) && '"></a>' ).
    ro_html->add( '<table class="mixed_height_bar">' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="repo_name">' ).
    ro_html->add( '<span>' && io_repo->get_name( ) && '</span>' ).
    ro_html->add( '</td>' ).
    ro_html->add( '<td class="repo_attr right">' ).
    ro_html->add( '<span>' && io_repo->get_package( ) && '</span>' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).
    ro_html->add( render_repo_menu( io_repo->get_key( ) ) ).

    IF go_user->is_hidden( io_repo->get_key( ) ) = abap_false.

      lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).
      IF lines( lt_tadir ) = 0.
        ro_html->add( 'Empty package<br><br>' ) ##NO_TEXT.
      ELSE.
        ro_html->add( '<table class="repo_tab">' ).
        ro_html->add( '<tbody>' ).
        LOOP AT lt_tadir ASSIGNING <ls_tadir>.
          ro_html->add( '<tr>' ).
          ro_html->add( '<td>' ).
          ro_html->add(
            jump_link( iv_obj_type = <ls_tadir>-object
                       iv_obj_name = <ls_tadir>-obj_name ) ).
          ro_html->add( '</td>' ).
          ro_html->add( '</tr>' ).
        ENDLOOP.
        ro_html->add( '</tbody>' ).
        ro_html->add( '</table>' ).
      ENDIF.

      ro_html->add( '<a href="sapevent:zipimport?' &&
        io_repo->get_key( ) &&
        '">' && 'Import ZIP' &&
        '</a>' ) ##NO_TEXT.

      ro_html->add( '<a href="sapevent:zipexport?' &&
        io_repo->get_key( ) &&
        '">' && 'Export ZIP' &&
        '</a>' ) ##NO_TEXT.

      ro_html->add( '<a href="sapevent:files_commit?' &&
        io_repo->get_key( ) &&
        '">' && 'Export files and commit' &&
        '</a>' ) ##NO_TEXT.
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.                    "render_repo_offline

  METHOD render_repo_menu.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="mixed_height_bar right menu">' ).

    IF go_user->is_hidden( iv_key ) = abap_true.
      ro_html->add( '<a class="menu_end" href="sapevent:unhide?' && iv_key && '">Show</a>' ).
    ELSE.
      ro_html->add( '<a href="sapevent:remove?' && iv_key && '">Remove</a>' ).
      ro_html->add( '<a href="sapevent:uninstall?' && iv_key && '">Uninstall</a>' ).
      ro_html->add( '<a href="sapevent:refresh_single?' && iv_key && '">Refresh</a>' ).
      ro_html->add( '<a class="menu_end" href="sapevent:hide?' && iv_key && '">Hide</a>' ).
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD jump_link.

    DATA: lv_encode TYPE string.


    lv_encode = jump_encode(
      iv_obj_type = iv_obj_type
      iv_obj_name = iv_obj_name ).

    rv_html = iv_obj_type &&
       '&nbsp;' &&
       '<a href="sapevent:jump?' &&
       lv_encode &&
       '">' &&
       iv_obj_name  &&
       '</a>'.

  ENDMETHOD.

  METHOD render_top.

    CREATE OBJECT ro_html.

    ro_html->add( '<table class="mixed_height_bar">' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="repo_name">' ).
    ro_html->add( '<span>' && io_repo->get_name( ) && '</span>' ).
    ro_html->add( '</td>' ).
    ro_html->add( '<td class="repo_attr right">' ).
    ro_html->add( '<span>' && io_repo->get_package( ) && '</span>' ).
    ro_html->add( '<span>' && io_repo->get_branch_name( ) && '</span>' ).
    ro_html->add( '<span>' && io_repo->get_url( ) && '</span>' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).

  ENDMETHOD.

  METHOD render_repo_online.

    DATA: lv_link        TYPE string,
          lv_object      TYPE string,
          lv_index       LIKE sy-tabix,
          lv_file_encode TYPE string,
          lx_error       TYPE REF TO lcx_exception,
          lv_span        TYPE i,
          lv_trclass     TYPE string,
          lo_log         TYPE REF TO lcl_log,
          lt_results     TYPE lcl_file_status=>ty_results_tt,
          ls_next        LIKE LINE OF lt_results,
          ls_item        TYPE ty_item,
          lv_supported   TYPE abap_bool.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( '<a id="' && io_repo->get_name( ) && '"></a>' ).
    ro_html->add( render_top( io_repo ) ).
    ro_html->add( render_repo_menu( io_repo->get_key( ) ) ).

    IF go_user->is_hidden( io_repo->get_key( ) ) = abap_false.
      TRY.
          CREATE OBJECT lo_log.
          lt_results = io_repo->status( lo_log ).

          ro_html->add( '<table class="repo_tab">' ).
          ro_html->add( '<tbody>' ).

          LOOP AT lt_results ASSIGNING <ls_result>.
            lv_index = sy-tabix.
            lv_file_encode = file_encode( iv_key  = io_repo->get_key( )
                                          is_file = <ls_result> ).

            CLEAR lv_link.
            IF <ls_result>-filename IS INITIAL.
              lv_link = 'new'.
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
                lv_object = '<td rowspan="' && lv_span && '">&nbsp;</td>' &&
                  '<td rowspan="' && lv_span && '"></td>'.
                lv_trclass = ' class="unsupported"' ##NO_TEXT.
              ELSE.
                CLEAR lv_trclass.
                lv_object = '<td rowspan="' && lv_span && '">' &&
                  jump_link( iv_obj_type = <ls_result>-obj_type
                             iv_obj_name = <ls_result>-obj_name ) &&
                  '</td>' &&
                  '<td rowspan="' && lv_span && '">' &&
                  <ls_result>-package &&
                  '</td>'.
              ENDIF.
            ELSE.
              CLEAR lv_object.
            ENDIF.

            ro_html->add( '<tr' && lv_trclass && '>' ).
            ro_html->add( lv_object ).
            ro_html->add( '<td>' ).
            ro_html->add( <ls_result>-path && <ls_result>-filename ).
            ro_html->add( '</td>' ).
            ro_html->add( '<td>' && lv_link && '</td>' ).
            ro_html->add( '</tr>' ).

            lv_span = lv_span - 1.
          ENDLOOP.

          ro_html->add( '</tbody>' ).
          ro_html->add( '</table>' ).

          IF io_repo->get_sha1_remote( ) <> io_repo->get_sha1_local( ).
            ro_html->add( '<a href="sapevent:pull?' &&
              io_repo->get_key( ) &&
              '">pull</a>' ).
          ELSEIF lcl_stage_logic=>count( io_repo ) > 0.
            ro_html->add( '<a href="sapevent:stage?' &&
              io_repo->get_key( ) &&
              '">stage</a>' ).
          ENDIF.

          ro_html->add( lo_log->to_html( ) ).

        CATCH lcx_exception INTO lx_error.
          ro_html->add( render_error( lx_error ) ).
      ENDTRY.
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.                    "render_repo

  METHOD abapgit_installation.

    CONSTANTS lc_package_abapgit TYPE devclass VALUE '$ABAPGIT'.
    CONSTANTS lc_package_plugins TYPE devclass VALUE '$ABAPGIT_PLUGINS'.

    DATA lv_text            TYPE c LENGTH 100.
    DATA lv_answer          TYPE c LENGTH 1.
    DATA lo_repo            TYPE REF TO lcl_repo_online.
    DATA lv_url             TYPE string.
    DATA lv_target_package  TYPE devclass.

    lv_text = |Installing current version ABAPGit to package { lc_package_abapgit } |
            && |and plugins to { lc_package_plugins }|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Install abapGit'
        text_question         = lv_text
        text_button_1         = 'Continue'
        text_button_2         = 'Cancel'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer ##no_text.
    IF lv_answer <> '1'.
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_url            = 'https://github.com/larshp/abapGit.git'.
          lv_target_package = lc_package_abapgit.
        WHEN 2.
          lv_url            = 'https://github.com/larshp/abapGit-plugins.git' ##no_text.
          lv_target_package = lc_package_plugins.
      ENDCASE.

      IF abap_false = is_repo_installed(
          iv_url              = lv_url
          iv_target_package   = lv_target_package ).

        lcl_sap_package=>create_local( lv_target_package ).

        lo_repo = lcl_repo_srv=>new_online(
          iv_url         = lv_url
          iv_branch_name = 'refs/heads/master'
          iv_package     = lv_target_package ) ##NO_TEXT.

        lo_repo->status( ). " check for errors
        lo_repo->deserialize( ).
      ENDIF.
    ENDDO.

    COMMIT WORK.

    lcl_gui=>render( ).

  ENDMETHOD. "abapgit_installation

  METHOD is_repo_installed.

    DATA: lt_repo        TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo        TYPE REF TO lcl_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO lcl_repo_online,
          lv_err         TYPE string.


    lt_repo = lcl_repo_srv=>list( ).

* find abapgit and abapgit-plugins-repos and validate bindings
    LOOP AT lt_repo INTO lo_repo.
      TRY.
          lo_repo_online ?= lo_repo.
        CATCH cx_sy_move_cast_error.
          CONTINUE. "the repositories we're looking for are online-repositories
      ENDTRY.

      lv_url     = lo_repo_online->get_url( ).
      lv_package = lo_repo_online->get_package( ).
      IF to_upper( lv_url ) <> to_upper( iv_url ).
        CONTINUE.
      ENDIF.

      IF iv_target_package IS NOT INITIAL AND iv_target_package <> lv_package.
        lv_err = |Installation to package { lv_package } detected. Cancelling installation|.
        _raise lv_err.
      ENDIF.

      rv_installed = abap_true.
    ENDLOOP.

  ENDMETHOD. "is_repo_installed

  METHOD needs_installation.

    CONSTANTS:
      lc_abapgit TYPE string VALUE 'https://github.com/larshp/abapGit.git',
      lc_plugins TYPE string VALUE 'https://github.com/larshp/abapGit-plugins.git' ##NO_TEXT.

    TRY.
        IF is_repo_installed( lc_abapgit ) = abap_false
            OR is_repo_installed( lc_plugins ) = abap_false.
          rv_not_completely_installed = abap_true.
        ENDIF.
      CATCH lcx_exception.
* cannot be installed anyway in this case, e.g. no connection
        rv_not_completely_installed = abap_false.
    ENDTRY.
  ENDMETHOD.                    "needs_installation

  METHOD lif_gui_page~on_event.

    DATA: ls_result TYPE lcl_file_status=>ty_result,
          lv_url    TYPE string,
          lv_key    TYPE lcl_persistence_repo=>ty_repo-key,
          ls_item   TYPE ty_item,
          lo_db     TYPE REF TO lcl_gui_page_db.


    CASE iv_action.
      WHEN 'install'.
        lv_url = iv_getdata.
        install( lv_url ).
      WHEN 'explore'.
        lcl_gui=>show_url( 'http://larshp.github.io/abapGit/explore.html' ).
      WHEN 'abapgithome'.
        cl_gui_frontend_services=>execute( document = 'http://www.abapgit.org' ).
      WHEN 'uninstall'.
        lv_key = iv_getdata.
        uninstall( lv_key ).
      WHEN 'remove'.
        lv_key = iv_getdata.
        remove( lv_key ).
      WHEN 'refresh'.
        lcl_repo_srv=>refresh( ).
        lcl_gui=>render( ).
      WHEN 'refresh_single'.
        lv_key = iv_getdata.
        lcl_repo_srv=>get( lv_key )->refresh( ).
        lcl_gui=>render( ).
      WHEN 'hide'.
        lv_key = iv_getdata.
        go_user->hide( lv_key ).
        lcl_gui=>render( ).
      WHEN 'unhide'.
        lv_key = iv_getdata.
        go_user->unhide( lv_key ).
        lcl_gui=>render( ).
      WHEN 'stage'.
        lv_key = iv_getdata.
        stage( lv_key ).
      WHEN 'diff'.
        file_decode( EXPORTING iv_string = iv_getdata
                     IMPORTING ev_key    = lv_key
                               es_file   = ls_result ).
        diff( is_result = ls_result
              iv_key    = lv_key ).
      WHEN 'jump'.
        CLEAR ls_item.
        jump_decode( EXPORTING iv_string = iv_getdata
                     IMPORTING ev_obj_type = ls_item-obj_type
                               ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
      WHEN 'pull'.
        lv_key = iv_getdata.
        pull( lv_key ).
      WHEN 'newoffline'.
        newoffline( ).
      WHEN 'db'.
        CREATE OBJECT lo_db.
        lcl_gui=>call_page( lo_db ).
      WHEN 'zipimport'.
        lv_key = iv_getdata.
        lcl_zip=>import( lv_key ).
      WHEN 'zipexport'.
        lv_key = iv_getdata.
        lcl_zip=>export_key( lv_key ).
      WHEN 'files_commit'.
        lv_key = iv_getdata.
        lcl_zip=>export_key( iv_key = lv_key
                             iv_zip = abap_false ).
      WHEN 'abapgit_installation'.
        abapgit_installation( ).
      WHEN OTHERS.
        _raise 'Unknown action'.
    ENDCASE.

  ENDMETHOD.

  METHOD render_toc.

    DATA: lo_repo    LIKE LINE OF it_list,
          lo_toolbar TYPE REF TO lcl_html_toolbar,
          lv_class   TYPE string.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    IF lines( it_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_list INTO lo_repo.
      lo_toolbar->add( iv_txt = lo_repo->get_name( ) iv_cmd = |#{ lo_repo->get_name( ) }| ).
    ENDLOOP.

    ro_html->add( '<div id="toc">' ) ##NO_TEXT.
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_error.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( 'Error:<br>' ) ##NO_TEXT.
    ro_html->add( ix_error->mv_text ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_explore.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( '<a href="sapevent:explore">Explore</a> new projects' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lt_repos        TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo_online  TYPE REF TO lcl_repo_online,
          lo_repo_offline TYPE REF TO lcl_repo_offline,
          lx_error        TYPE REF TO lcx_exception,
          lo_repo         LIKE LINE OF lt_repos.


    CREATE OBJECT ro_html.

    CREATE OBJECT go_user.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( iv_page_title = 'MAIN' io_menu = build_menu( ) ) ).

    TRY.
        lt_repos = lcl_repo_srv=>list( ).
      CATCH lcx_exception INTO lx_error.
* if wrong meta data exists in database, make sure to still render the menu
* where it is possible to use the database tool
        ro_html->add( render_error( lx_error ) ).
    ENDTRY.

    ro_html->add( render_toc( lt_repos ) ).

    IF lines( lt_repos ) = 0.
      ro_html->add( render_explore( ) ).
    ELSE.
      LOOP AT lt_repos INTO lo_repo.
        lcl_progress=>show( iv_key     = 'Render'
                            iv_current = sy-tabix
                            iv_total   = lines( lt_repos )
                            iv_text    = lo_repo->get_name( ) ) ##NO_TEXT.

        IF lo_repo->is_offline( ) = abap_true.
          lo_repo_offline ?= lo_repo.
          ro_html->add( render_repo_offline( lo_repo_offline ) ).
        ELSE.
          lo_repo_online ?= lo_repo.
          ro_html->add( render_repo_online( lo_repo_online ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception,
        lo_main      TYPE REF TO lcl_gui_page_main,
        lv_ind       TYPE t000-ccnocliind.


  SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
    WHERE mandt = sy-mandt.
  IF sy-subrc = 0
      AND lv_ind <> ' '
      AND lv_ind <> '1'. " check changes allowed
    WRITE: / 'Wrong client, changes to repository objects not allowed'. "#EC NOTEXT
    RETURN.
  ENDIF.

  TRY.
      lcl_persistence_migrate=>run( ).

      CREATE OBJECT lo_main.
      lcl_gui=>call_page( lo_main ).
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
                  USING    pv_code TYPE clike
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING lcx_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lv_url          TYPE string,
        lv_answer       TYPE c,
        lx_error        TYPE REF TO lcx_exception,
        lt_selection    TYPE TABLE OF spopli,
        ls_package_data TYPE scompkdtln,
        lt_branches     TYPE lcl_git_transport=>ty_branch_list_tt.

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

  ELSEIF pv_code = 'COD2'.
    cv_show_popup = abap_true.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'PB_POPUP_PACKAGE_CREATE'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      _raise 'Function module PB_POPUP_PACKAGE_CREATE does not exist'.
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = ls_package_data
      EXCEPTIONS
        action_cancelled = 1.
    IF sy-subrc = 1.
      RETURN.
    ENDIF.

    lcl_sap_package=>create( ls_package_data ).
    COMMIT WORK.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TDEVC'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = ls_package_data-devclass.
  ENDIF.

ENDFORM.                    "branch_popup

CLASS ltcl_html_helper DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mo_html TYPE REF TO lcl_html_helper.

    METHODS:
      indent1 FOR TESTING RAISING lcx_exception,
      indent2 FOR TESTING RAISING lcx_exception,
      indent3 FOR TESTING RAISING lcx_exception,
      indent4 FOR TESTING RAISING lcx_exception.

    METHODS:
      setup,
      last_line
        RETURNING VALUE(rv_line) TYPE string.

ENDCLASS.

CLASS ltcl_html_helper IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_html.
  ENDMETHOD.

  METHOD indent1.

    mo_html->add( '<td>' ).
    mo_html->add( 'hello world' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD indent2.

    mo_html->add( '<td>' ).
    mo_html->add( '<input name="comment" type="text">' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD indent3.

    mo_html->add( '<td>' ).
    mo_html->add( '<textarea name="body" rows="10" cols="72"></textarea>' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD indent4.

    mo_html->add( '<td>' ).
    mo_html->add( 'foo<br>bar' ).
    mo_html->add( '</td>' ).

    cl_abap_unit_assert=>assert_equals(
      act = last_line( )
      exp = '</td>' ).

  ENDMETHOD.

  METHOD last_line.

    DATA: lt_strings TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    SPLIT mo_html->mv_html AT gc_newline INTO TABLE lt_strings.

    READ TABLE lt_strings INDEX lines( lt_strings ) INTO rv_line.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS convert_int FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

  ENDMETHOD.                    "convert_int

ENDCLASS.                    "ltcl_convert IMPLEMENTATION

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
    APPEND &1 TO mt_local.
  END-OF-DEFINITION.

  DEFINE _remote.
    APPEND &1 TO mt_remote.
  END-OF-DEFINITION.

  DEFINE _expected.
    CLEAR ms_expected.
    ms_expected-local = &1.
    ms_expected-result = &2.
    ms_expected-remote = &3.
    APPEND ms_expected TO mt_expected.
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

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF lt_diff.


    CONCATENATE LINES OF mt_local  INTO lv_local SEPARATED BY gc_newline.
    CONCATENATE LINES OF mt_remote INTO lv_remote SEPARATED BY gc_newline.

    lv_xlocal  = lcl_convert=>string_to_xstring_utf8( lv_local ).
    lv_xremote = lcl_convert=>string_to_xstring_utf8( lv_remote ).

    CREATE OBJECT lo_diff
      EXPORTING
        iv_local  = lv_xlocal
        iv_remote = lv_xremote.

    lt_diff = lo_diff->get( ).

    LOOP AT lt_diff ASSIGNING <ls_diff>.
      CLEAR <ls_diff>-local_line.
      CLEAR <ls_diff>-remote_line.
      CLEAR <ls_diff>-short.
    ENDLOOP.

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

    METHODS:
      check
        IMPORTING is_item TYPE ty_item
        RAISING   lcx_exception,
      serialize_tabl FOR TESTING RAISING lcx_exception,
      serialize_enqu FOR TESTING RAISING lcx_exception,
      serialize_shlp FOR TESTING RAISING lcx_exception,
      serialize_view FOR TESTING RAISING lcx_exception,
      serialize_auth FOR TESTING RAISING lcx_exception,
      serialize_clas FOR TESTING RAISING lcx_exception,
      serialize_doma FOR TESTING RAISING lcx_exception,
      serialize_dtel FOR TESTING RAISING lcx_exception,
      serialize_fugr FOR TESTING RAISING lcx_exception,
      serialize_msag FOR TESTING RAISING lcx_exception,
      serialize_prog FOR TESTING RAISING lcx_exception,
      serialize_tran FOR TESTING RAISING lcx_exception,
      serialize_ttyp FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_serialize DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    check( ls_item ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_table

  METHOD serialize_auth.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'AUTH'.
    ls_item-obj_name = 'AREA'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_auth

  METHOD serialize_clas.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_GUI_FRONTEND_SERVICES'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_clas

  METHOD serialize_doma.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_doma

  METHOD serialize_dtel.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_dtel

  METHOD serialize_fugr.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SRFC'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_fugr

  METHOD serialize_msag.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'MSAG'.
    ls_item-obj_name = '00'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_msag

  METHOD serialize_prog.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPLWBABAP'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_prog

  METHOD serialize_tran.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'TRAN'.
    ls_item-obj_name = 'SE38'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_tran

  METHOD serialize_ttyp.

    DATA: ls_item  TYPE ty_item.


    ls_item-obj_type = 'TTYP'.
    ls_item-obj_name = 'ABAPPROG'.

    check( ls_item ).

  ENDMETHOD.                    "serialize_ttyp

  METHOD check.

    DATA: lt_files TYPE ty_files_tt.

    lt_files = lcl_objects=>serialize( is_item     = is_item
                                       iv_language = gc_english ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "check

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

  ENDMETHOD.                    "object_blob

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

  ENDMETHOD.                    "tree

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

  ENDMETHOD.                    "commit

ENDCLASS.                    "lcl_abap_unit IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      is_supported FOR TESTING,
      not_exist FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_object_types DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_object_types IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_object_types IMPLEMENTATION.

  METHOD is_supported.

    DATA: ls_item      TYPE ty_item,
          lv_supported TYPE abap_bool,
          lt_types     TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = lcl_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.

      CLEAR ls_item.
      ls_item-obj_type = <lv_type>.
      lv_supported = lcl_objects=>is_supported( ls_item ).

      cl_abap_unit_assert=>assert_equals(
          act  = lv_supported
          exp  = abap_true
          msg  = ls_item-obj_type
          quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.                    "is_supported

  METHOD not_exist.

    DATA: ls_item   TYPE ty_item,
          lv_exists TYPE abap_bool,
          lt_types  TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = lcl_objects=>supported_list( ).

    LOOP AT lt_types ASSIGNING <lv_type>.
      CLEAR ls_item.
      ls_item-obj_name = 'ZABAPGIT_FOOBAR'.
      ls_item-obj_type = <lv_type>.
      lv_exists = lcl_objects=>exists( ls_item ).

      cl_abap_unit_assert=>assert_equals(
          act  = lv_exists
          exp  = abap_false
          msg  = ls_item-obj_type
          quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.                    "not_exist

ENDCLASS.                    "ltcl_object_types IMPLEMENTATION

CLASS ltcl_xml DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    METHODS:
      up FOR TESTING
        RAISING lcx_exception,
      down FOR TESTING
        RAISING lcx_exception.

    TYPES: BEGIN OF st_old,
             foo TYPE i,
             bar TYPE c LENGTH 1,
           END OF st_old.

    TYPES: BEGIN OF st_new,
             foo TYPE i,
             bar TYPE c LENGTH 1,
             moo TYPE f,
           END OF st_new.

ENDCLASS.

CLASS ltcl_xml IMPLEMENTATION.

  METHOD up.

    DATA: ls_old    TYPE st_old,
          ls_new    TYPE st_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO lcl_xml_input,
          lo_output TYPE REF TO lcl_xml_output.


    ls_old-foo = 2.
    ls_old-bar = 'A'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_old ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_new ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-foo
      exp = ls_old-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-bar
      exp = ls_old-bar ).

  ENDMETHOD.

  METHOD down.

    DATA: ls_old    TYPE st_old,
          ls_new    TYPE st_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO lcl_xml_input,
          lo_output TYPE REF TO lcl_xml_output.


    ls_new-foo = 2.
    ls_new-bar = 'A'.
    ls_new-moo = 5.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_new ).
    lv_xml = lo_output->render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->read( EXPORTING iv_name = 'DATA'
                    CHANGING cg_data = ls_old ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_old-foo
      exp = ls_new-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_old-bar
      exp = ls_new-bar ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_zlib DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_zlib DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      decompress FOR TESTING RAISING cx_dynamic_check.

ENDCLASS.                    "ltcl_zlib DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_zlib IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_zlib IMPLEMENTATION.

  METHOD decompress.

    DATA: ls_data TYPE lcl_zlib=>ty_decompress.

    CONSTANTS:
      lc_raw        TYPE xstring VALUE '48656C6C6F20576F726C64210D0A',
      lc_compressed TYPE xstring VALUE 'F348CDC9C95708CF2FCA4951E4E5020024E90455'.


    ls_data = lcl_zlib=>decompress( lc_compressed ).

    cl_abap_unit_assert=>assert_not_initial( ls_data-raw ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-raw
                                        exp = lc_raw ).

  ENDMETHOD.                    "decompress

ENDCLASS.                    "ltcl_zlib IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous DEFINITION FOR TESTING RISK LEVEL CRITICAL DURATION LONG FINAL.
* if this test class does not run, parameters in transaction SAUNIT_CLIENT_SETUP
* might need to be adjusted

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup.

    METHODS:
      run FOR TESTING
        RAISING lcx_exception,
      check_empty_package
        RAISING lcx_exception.

    CONSTANTS: c_package TYPE devclass VALUE '$ABAPGIT_UNIT_TEST'.

ENDCLASS.                    "ltcl_dangerous DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_dangerous IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_dangerous IMPLEMENTATION.

  METHOD class_setup.

    DATA: lv_text   TYPE c LENGTH 100,
          lv_answer TYPE c LENGTH 1.


    lv_text = 'Objects will be created and deleted, do not run in customer system!'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = lv_text
        text_button_1         = 'Run'
        text_button_2         = 'Cancel'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer.

    IF lv_answer = '2'.
      cl_abap_unit_assert=>fail( 'Cancelled' ).
    ENDIF.

  ENDMETHOD.                    "class_setup

  METHOD check_empty_package.

    DATA: lt_tadir TYPE lcl_tadir=>ty_tadir_tt.


    lt_tadir = lcl_tadir=>read( c_package ).
    IF lines( lt_tadir ) > 0.
      cl_abap_unit_assert=>fail( 'Prerequsite: package should be empty' ).
    ENDIF.

  ENDMETHOD.                    "check_empty_package

  METHOD run.

    DATA: lo_repo    TYPE REF TO lcl_repo_online,
          lt_tadir   TYPE lcl_tadir=>ty_tadir_tt,
          lv_msg     TYPE string,
          lt_results TYPE lcl_file_status=>ty_results_tt,
          lt_types   TYPE lcl_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <lv_type>   LIKE LINE OF lt_types.


    lcl_sap_package=>create_local( c_package ).

    lt_types = lcl_objects=>supported_list( ).

    lo_repo = lcl_repo_srv=>new_online(
      iv_url         = 'https://github.com/larshp/abapGit-Unit-Test.git'
      iv_branch_name = 'refs/heads/master'
      iv_package     = c_package ).
    lo_repo->status( ).
    lo_repo->deserialize( ).

    lt_tadir = lcl_tadir=>read( c_package ).
    LOOP AT lt_types ASSIGNING <lv_type>.
      READ TABLE lt_tadir WITH KEY object = <lv_type> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        lv_msg = |Missing object type { <lv_type> }|.
        cl_abap_unit_assert=>fail(
            msg   = lv_msg
            level = if_aunit_constants=>tolerable
            quit  = if_aunit_constants=>no ).
      ENDIF.
    ENDLOOP.

    lt_results = lo_repo->status( ).
    LOOP AT lt_results ASSIGNING <ls_result> WHERE match = abap_false.
      lv_msg = |Does not match { <ls_result>-obj_type } { <ls_result>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    lcl_objects=>delete( lt_tadir ).
    lt_tadir = lcl_tadir=>read( c_package ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lv_msg = |Not deleted properly { <ls_tadir>-object } { <ls_tadir>-obj_name }|.
      cl_abap_unit_assert=>fail(
          msg  = lv_msg
          quit = if_aunit_constants=>no ).
    ENDLOOP.

    lcl_repo_srv=>delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "run

ENDCLASS.                    "ltcl_dangerous IMPLEMENTATION

CLASS lcl_persistence_user IMPLEMENTATION.

  METHOD constructor.
    mv_user = iv_user.
  ENDMETHOD.

  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_xml
      RESULT (c_type_user) = rs_user ##NO_TEXT.
  ENDMETHOD.

  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE (c_type_user) = is_user
      RESULT XML rv_xml.
  ENDMETHOD.

  METHOD read.

    DATA: lv_xml TYPE string,
          lo_db  TYPE REF TO lcl_persistence_db.


    CREATE OBJECT lo_db.

    TRY.
        lv_xml = lo_db->read(
          iv_type  = c_type_user
          iv_value = mv_user ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    rs_user = from_xml( lv_xml ).

  ENDMETHOD.

  METHOD update.

    DATA: lv_xml TYPE string,
          lo_db  TYPE REF TO lcl_persistence_db.


    lv_xml = to_xml( is_user ).

    CREATE OBJECT lo_db.

    lo_db->modify(
      iv_type  = c_type_user
      iv_value = mv_user
      iv_data  = lv_xml ).

  ENDMETHOD.

  METHOD set_username.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).

    ls_user-username = iv_username.

    update( ls_user ).

  ENDMETHOD.

  METHOD get_username.

    rv_username = read( )-username.

  ENDMETHOD.

  METHOD is_hidden.

    DATA: lt_hidden TYPE ty_repo_hidden_tt.


    lt_hidden = read( )-repo_hidden.
    READ TABLE lt_hidden FROM iv_key TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_hidden = abap_true.
    ELSE.
      rv_hidden = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD hide.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    APPEND iv_key TO ls_user-repo_hidden.
    update( ls_user ).

  ENDMETHOD.

  METHOD unhide.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    DELETE TABLE ls_user-repo_hidden FROM iv_key.
    update( ls_user ).

  ENDMETHOD.

  METHOD set_email.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    ls_user-email = iv_email.
    update( ls_user ).

  ENDMETHOD.

  METHOD get_email.

    rv_email = read( )-email.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_persistence_db IMPLEMENTATION.

  METHOD list_by_type.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content
      WHERE type = iv_type.                               "#EC CI_SUBRC
  ENDMETHOD.

  METHOD list.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content.                              "#EC CI_SUBRC
  ENDMETHOD.

  METHOD lock.

    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = iv_mode
        type           = iv_type
        value          = iv_value
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      _raise 'Could not aquire lock'.
    ENDIF.

* trigger dummy update task to automatically release locks at commit
    CALL FUNCTION 'BANK_OBJ_WORKL_RELEASE_LOCKS'
      IN UPDATE TASK.

  ENDMETHOD.

  METHOD add.

    DATA ls_table TYPE ty_content.

    ls_table-type  = iv_type.
    ls_table-value = iv_value.
    ls_table-data_str = iv_data.

    INSERT (c_tabname) FROM ls_table.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD delete.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    DELETE FROM (c_tabname)
      WHERE type = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      _raise 'DB Delete failed'.
    ENDIF.

  ENDMETHOD.

  METHOD update.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    UPDATE (c_tabname) SET data_str = iv_data
      WHERE type = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      _raise 'DB update failed'.
    ENDIF.

  ENDMETHOD.

  METHOD modify.

    DATA: ls_content TYPE ty_content.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    ls_content-type  = iv_type.
    ls_content-value = iv_value.
    ls_content-data_str = iv_data.

    MODIFY (c_tabname) FROM ls_content.
    IF sy-subrc <> 0.
      _raise 'DB modify failed'.
    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT SINGLE data_str FROM (c_tabname) INTO rv_data
      WHERE type = iv_type
      AND value = iv_value.                               "#EC CI_SUBRC
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_persistence_repo IMPLEMENTATION.

  METHOD add.

    DATA: ls_repo        TYPE ty_repo,
          lv_repo_as_xml TYPE string.


    ls_repo-url          = iv_url.
    ls_repo-branch_name  = iv_branch_name.
    ls_repo-sha1         = iv_branch.
    ls_repo-package      = iv_package.
    ls_repo-offline      = iv_offline.
    ls_repo-master_language = sy-langu.

    lv_repo_as_xml = to_xml( ls_repo ).

    rv_key = get_next_id( ).

    mo_db->add( iv_type  = c_type_repo
                iv_value = rv_key
                iv_data  = lv_repo_as_xml ).

  ENDMETHOD.

  METHOD delete.

    mo_db->delete( iv_type  = c_type_repo
                   iv_value = iv_key ).

  ENDMETHOD.

  METHOD update_sha1.

    DATA: lt_content TYPE lcl_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


    IF iv_branch_sha1 IS INITIAL.
      _raise 'update, sha empty'.
    ENDIF.

    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH lcx_not_found.
        _raise 'key not found'.
    ENDTRY.

    ls_repo-sha1 = iv_branch_sha1.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.

  METHOD read.

    DATA lt_repo TYPE tt_repo.

    lt_repo = list( ).

    READ TABLE lt_repo INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.

  ENDMETHOD.

  METHOD get_next_id.

* todo: Lock the complete persistence in order to prevent concurrent repo-creation
* however the current approach will most likely work in almost all cases

    DATA: lt_content TYPE lcl_persistence_db=>tt_content.

    FIELD-SYMBOLS: <ls_content> LIKE LINE OF lt_content.


    rv_next_repo_id = 1.

    lt_content = mo_db->list_by_type( c_type_repo ).
    LOOP AT lt_content ASSIGNING <ls_content>.
      IF <ls_content>-value >= rv_next_repo_id.
        rv_next_repo_id = <ls_content>-value + 1.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = rv_next_repo_id
      IMPORTING
        output = rv_next_repo_id.

  ENDMETHOD.

  METHOD list.

    DATA: lt_content TYPE lcl_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.


    lt_content = mo_db->list_by_type( c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      MOVE-CORRESPONDING from_xml( ls_content-data_str ) TO ls_repo.
      ls_repo-key = ls_content-value.
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.

  ENDMETHOD.

  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_repo_xml_string
      RESULT (c_type_repo) = rs_repo ##NO_TEXT.

    IF rs_repo IS INITIAL.
      _raise 'Inconsistent repo metadata'.
    ENDIF.

* field master_language is new, so default it for old repositories
    IF rs_repo-master_language IS INITIAL.
      rs_repo-master_language = sy-langu.
    ENDIF.
  ENDMETHOD.

  METHOD to_xml.

    DATA: ls_xml TYPE ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE (c_type_repo) = ls_xml
      RESULT XML rv_repo_xml_string.
  ENDMETHOD.

  METHOD constructor.
    CREATE OBJECT mo_db.
  ENDMETHOD.

  METHOD lock.

    mo_db->lock( iv_mode  = iv_mode
                 iv_type  = c_type_repo
                 iv_value = iv_key ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_persistence_migrate IMPLEMENTATION.

  METHOD run.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).

      migrate_repo( ).
      migrate_user( ).
    ENDIF.

  ENDMETHOD.

  METHOD migrate_repo.

    DATA: lt_repo TYPE lcl_persistence=>ty_repos_persi_tt,
          lo_repo TYPE REF TO lcl_persistence,
          lo_new  TYPE REF TO lcl_persistence_repo,
          ls_repo LIKE LINE OF lt_repo.


    CREATE OBJECT lo_repo.
    CREATE OBJECT lo_new.

    lt_repo = lo_repo->list( ).

    LOOP AT lt_repo INTO ls_repo.
      lo_new->add( iv_url         = ls_repo-url
                   iv_branch_name = ls_repo-branch_name
                   iv_branch      = ls_repo-sha1
                   iv_package     = ls_repo-package
                   iv_offline     = ls_repo-offline ).
    ENDLOOP.
  ENDMETHOD.

  METHOD migrate_user.

    DATA: lo_user  TYPE REF TO lcl_persistence_user,
          lt_users TYPE lcl_user=>ty_user_tt.

    FIELD-SYMBOLS: <ls_user> LIKE LINE OF lt_users.


    lt_users = lcl_user=>list( ).
    LOOP AT lt_users ASSIGNING <ls_user>.
      CREATE OBJECT lo_user
        EXPORTING
          iv_user = <ls_user>-user.

      lo_user->set_username( <ls_user>-username ).
      lo_user->set_email( <ls_user>-email ).
    ENDLOOP.

  ENDMETHOD.

  METHOD lock_exists.

    DATA: lv_viewname TYPE dd25l-viewname.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = lcl_persistence_db=>c_lock.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lock_create.

    DATA: lv_obj_name TYPE tadir-obj_name,
          ls_dd25v    TYPE dd25v,
          lt_dd26e    TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
          lt_dd27p    TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd26e> LIKE LINE OF lt_dd26e,
                   <ls_dd27p> LIKE LINE OF lt_dd27p.


    ls_dd25v-viewname   = lcl_persistence_db=>c_lock.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = lcl_persistence_db=>c_tabname.
    ls_dd25v-ddlanguage = gc_english.
    ls_dd25v-ddtext     = c_text.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = lcl_persistence_db=>c_lock.
    <ls_dd26e>-tabname    = lcl_persistence_db=>c_tabname.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = lcl_persistence_db=>c_tabname.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = lcl_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'TYPE'.
    <ls_dd27p>-tabname   = lcl_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'TYPE'.
    <ls_dd27p>-keyflag   = abap_true.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = lcl_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0002'.
    <ls_dd27p>-viewfield = 'VALUE'.
    <ls_dd27p>-tabname   = lcl_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'VALUE'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = lcl_persistence_db=>c_lock
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
      _raise 'migrate, error from DDIF_ENQU_PUT'.
    ENDIF.

    lv_obj_name = lcl_persistence_db=>c_lock.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      _raise 'migrate, error from TR_TADIR_INTERFACE'.
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = lcl_persistence_db=>c_lock
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      _raise 'migrate, error from DDIF_ENQU_ACTIVATE'.
    ENDIF.

  ENDMETHOD.

  METHOD table_exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = lcl_persistence_db=>c_tabname.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD table_create.

    DATA: lv_obj_name TYPE tadir-obj_name,
          ls_dd02v    TYPE dd02v,
          ls_dd09l    TYPE dd09l,
          lt_dd03p    TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = lcl_persistence_db=>c_tabname.
    ls_dd02v-ddlanguage = gc_english.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = c_text.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname  = lcl_persistence_db=>c_tabname.
    ls_dd09l-as4local = 'A'.
    ls_dd09l-tabkat   = '1'.
    ls_dd09l-tabart   = 'APPL1'.
    ls_dd09l-bufallow = 'N'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = lcl_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'TYPE'.
    <ls_dd03p>-position  = '0001'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = lcl_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'VALUE'.
    <ls_dd03p>-position  = '0002'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = lcl_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'DATA_STR'.
    <ls_dd03p>-position  = '0003'.
    <ls_dd03p>-datatype  = 'STRG'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = lcl_persistence_db=>c_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'migrate, error from DDIF_TABL_PUT'.
    ENDIF.

    lv_obj_name = lcl_persistence_db=>c_tabname.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      _raise 'migrate, error from TR_TADIR_INTERFACE'.
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = lcl_persistence_db=>c_tabname
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      _raise 'migrate, error from DDIF_TABL_ACTIVATE'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_xml_pretty IMPLEMENTATION.

  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = li_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
* ignore errors
      rv_xml = iv_xml.
      RETURN.
    ENDIF.
    li_istream->close( ).


    li_ostream = li_stream_factory->create_ostream_cstring( rv_xml ).

    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( abap_true ).

    li_renderer->render( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_db_display DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

ENDCLASS.

CLASS lcl_gui_page_db_display IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN OTHERS.
        _raise 'Unknown action'.
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data TYPE lcl_persistence_db=>ty_content-data_str,
          lo_db   TYPE REF TO lcl_persistence_db.


    CREATE OBJECT ro_html.

    CREATE OBJECT lo_db.
    TRY.
        lv_data = lo_db->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    ro_html->add( header( ) ).
    ro_html->add( title( iv_page_title = 'CONFIG' ) ).

    ro_html->add( '<div id="toc">' ).
    ro_html->add( '<b>Type:</b><br>' ).
    ro_html->add( ms_key-type && '<br><br>' ).
    ro_html->add( '<b>Value:</b><br>' ).
    ro_html->add( ms_key-value && '<br><br>' ).
    ro_html->add( '<b>Data:</b><br>' ).
    ro_html->add( '<pre>' && lv_data && '</pre><br>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_db_edit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~on_event REDEFINITION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS: save
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_db_edit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD save.

    DATA: lv_string  TYPE string,
          ls_content TYPE lcl_persistence_db=>ty_content,
          lo_db      TYPE REF TO lcl_persistence_db,
          lt_fields  TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'type' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-type = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'value' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-value = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'xmldata' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-data_str = <ls_field>-value+1. " hmm

    CREATE OBJECT lo_db.

    lo_db->update(
      iv_type  = ls_content-type
      iv_value = ls_content-value
      iv_data  = ls_content-data_str ).

    COMMIT WORK.

    lcl_gui=>back( ).

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN 'post'.
        save( it_postdata ).
      WHEN OTHERS.
        _raise 'Unknown action'.
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data TYPE lcl_persistence_db=>ty_content-data_str,
          lo_db   TYPE REF TO lcl_persistence_db.


    CREATE OBJECT ro_html.

    CREATE OBJECT lo_db.
    TRY.
        lv_data = lo_db->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lo_db->lock(
      iv_type  = ms_key-type
      iv_value = ms_key-value ).

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    ro_html->add( header( ) ).

    "TODO refactor
    ro_html->add( '<div id="header">' ).
    ro_html->add( '<h1>Edit</h1>' ).
    ro_html->add( '</div>' ).

    ro_html->add( '<div id="toc">' ).
    ro_html->add( '<b>Type:</b><br>' ).
    ro_html->add( ms_key-type && '<br><br>' ).
    ro_html->add( '<b>Value:</b><br>' ).
    ro_html->add( ms_key-value && '<br><br>' ).
    ro_html->add( '<b>Data:</b><br>' ).
    ro_html->add( '<form method="post" action="sapevent:post">' ).
    ro_html->add( '<input type="hidden" name="type" value="' && ms_key-type && '">' ).
    ro_html->add( '<input type="hidden" name="value" value="' && ms_key-value && '">' ).
    ro_html->add( '<textarea rows="20" cols="100" name="xmldata">' ).
    ro_html->add( lv_data ).
    ro_html->add( '</textarea><br><input type="submit" value="Update"></form>' ).
    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_db IMPLEMENTATION.

  METHOD delete.

    DATA: lo_db       TYPE REF TO lcl_persistence_db,
          lv_continue TYPE abap_bool.


    lv_continue = delete_popup( ).
    IF lv_continue = abap_false.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_db.

    lo_db->delete( iv_type  = is_key-type
                   iv_value = is_key-value ).

    COMMIT WORK.

    lcl_gui=>render( ).

  ENDMETHOD.

  METHOD delete_popup.

    DATA: lv_answer TYPE c LENGTH 1.


    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = 'Delete?'
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

    IF lv_answer <> '2'.
      rv_continue = abap_true.
    ELSE.
      rv_continue = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD key_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.


    ls_field-name = 'TYPE'.
    ls_field-value = is_key-type.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'VALUE'.
    ls_field-value = is_key-value.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.

  METHOD key_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'TYPE'.
    IF sy-subrc = 0.
      rs_key-type = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'VALUE'.
    IF sy-subrc = 0.
      rs_key-value = <ls_field>-value.
    ENDIF.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    DATA: lo_display TYPE REF TO lcl_gui_page_db_display,
          lo_edit    TYPE REF TO lcl_gui_page_db_edit,
          ls_key     TYPE lcl_persistence_db=>ty_content.


    ls_key = key_decode( iv_getdata ).

    CASE iv_action.
      WHEN 'display'.
        CREATE OBJECT lo_display
          EXPORTING
            is_key = ls_key.
        lcl_gui=>call_page( lo_display ).
      WHEN 'edit'.
        CREATE OBJECT lo_edit
          EXPORTING
            is_key = ls_key.
        lcl_gui=>call_page( lo_edit ).
      WHEN 'delete'.
        delete( ls_key ).
      WHEN OTHERS.
        _raise 'Unknown action'.
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lt_data    TYPE lcl_persistence_db=>tt_content,
          lv_escaped TYPE string,
          lv_encode  TYPE string,
          lo_db      TYPE REF TO lcl_persistence_db.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    CREATE OBJECT lo_db.
    lt_data = lo_db->list( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( iv_page_title = 'DATABASE PERSISTENCY' ) ).

    ro_html->add( '<div id="toc">' ).
    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td><b>Type</b></td>' ).
    ro_html->add( '<td><b>Value</b></td>' ).
    ro_html->add( '<td><b>Data</b></td>' ).
    ro_html->add( '</tr>' ).

    LOOP AT lt_data ASSIGNING <ls_data>.
      lv_escaped = escape( val    = <ls_data>-data_str(150)
                           format = cl_abap_format=>e_html_attr ).

      lv_encode = key_encode( <ls_data> ).

      ro_html->add( '<tr>' ).
      ro_html->add( '<td valign="top">' && <ls_data>-type && '</td>' ).
      ro_html->add( '<td valign="top">' && <ls_data>-value && '</td>' ).
      ro_html->add( '<td><pre>' && lv_escaped && '</pre>' ).
      ro_html->add( '<br><br>' ).
      ro_html->add( '<a href="sapevent:display?' && lv_encode && '">Display</a>' ).
      ro_html->add( '<a href="sapevent:edit?' && lv_encode && '">Edit</a>' ).
      ro_html->add( '<a href="sapevent:delete?' && lv_encode && '">Delete</a></td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_git_porcelain DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      append
        IMPORTING iv_path TYPE string
                  iv_name TYPE string,
      single_file FOR TESTING
        RAISING lcx_exception,
      two_files_same_path FOR TESTING
        RAISING lcx_exception,
      root_empty FOR TESTING
        RAISING lcx_exception,
      sub FOR TESTING
        RAISING lcx_exception.

    DATA: mt_expanded TYPE lcl_git_porcelain=>ty_expanded_tt,
          mt_trees    TYPE lcl_git_porcelain=>ty_trees_tt.

ENDCLASS.

CLASS ltcl_git_porcelain IMPLEMENTATION.

  METHOD setup.
    CLEAR mt_expanded.
    CLEAR mt_trees.
  ENDMETHOD.

  METHOD append.

    FIELD-SYMBOLS: <ls_expanded> LIKE LINE OF mt_expanded.


    APPEND INITIAL LINE TO mt_expanded ASSIGNING <ls_expanded>.
    <ls_expanded>-path = iv_path.
    <ls_expanded>-name = iv_name.
    <ls_expanded>-sha1 = 'a'.

  ENDMETHOD.

  METHOD single_file.

    append( iv_path = '/'
            iv_name = 'foobar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD two_files_same_path.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 1 ).

  ENDMETHOD.

  METHOD sub.

    append( iv_path = '/'
            iv_name = 'foo.txt' ).

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

  METHOD root_empty.

    append( iv_path = '/sub/'
            iv_name = 'bar.txt' ).

    mt_trees = lcl_git_porcelain=>build_trees( mt_expanded ).

* so 2 total trees are expected: '/' and '/sub/'
    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_trees )
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.
  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND: 'CRET' TO lt_ucomm.  "Button Execute

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  CASE sy-ucomm.
    WHEN 'CBAC'.  "Back
      IF lcl_gui=>back( ) IS INITIAL.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.