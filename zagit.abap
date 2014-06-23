REPORT zdk8lahvp089.

TYPES: t_type    TYPE c LENGTH 6,
       t_bitbyte TYPE c LENGTH 8,
       t_adler32 TYPE x LENGTH 4,
       t_sha1    TYPE x LENGTH 20.

TYPES: BEGIN OF st_node,
         chmod     TYPE string,
         directory TYPE abap_bool,
         name      TYPE string,
         sha1      TYPE t_sha1,
       END OF st_node.
TYPES: tt_nodes TYPE STANDARD TABLE OF st_node WITH DEFAULT KEY.

TYPES: BEGIN OF st_object,
         sha1 TYPE t_sha1,
         type TYPE t_type,
         data TYPE xstring,
       END OF st_object.
TYPES: tt_objects TYPE STANDARD TABLE OF st_object WITH DEFAULT KEY.

TYPES: BEGIN OF st_latest,
         path     TYPE string,
         filename TYPE string,
         data     TYPE xstring,
       END OF st_latest.
TYPES: tt_latest TYPE STANDARD TABLE OF st_latest WITH DEFAULT KEY.

TYPES: BEGIN OF st_commit,
         tree      TYPE t_sha1,
         parent    TYPE t_sha1,
         author    TYPE string,
         committer TYPE string,
         body      TYPE string,
       END OF st_commit.

CONSTANTS: gc_commit TYPE t_type VALUE 'commit',
           gc_tree   TYPE t_type VALUE 'tree',
           gc_ref_d  TYPE t_type VALUE 'ref_d',
           gc_blob   TYPE t_type VALUE 'blob'.

******************

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS lcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS bitbyte_to_int IMPORTING iv_bits TYPE clike
                                 RETURNING value(rv_int) TYPE i.

    CLASS-METHODS x_to_bitbyte IMPORTING iv_x TYPE x
                               RETURNING value(rv_bitbyte) TYPE t_bitbyte.

    CLASS-METHODS string_to_xstring_utf8 IMPORTING iv_string TYPE string
                                RETURNING value(rv_xstring) TYPE xstring.

    CLASS-METHODS xstring_to_string_utf8 IMPORTING iv_data TYPE xstring
                                         RETURNING value(rv_string) TYPE string.

    CLASS-METHODS xstring_to_int IMPORTING iv_xstring TYPE xstring
                                 RETURNING value(rv_i) TYPE i.

ENDCLASS.                    "lcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert IMPLEMENTATION.

  METHOD xstring_to_int.

    DATA: lv_string TYPE string.

* todo, this can be done a lot easier

    lv_string = iv_xstring.
    WHILE strlen( lv_string ) > 0.
      rv_i = rv_i * 16.
      CASE lv_string(1).
        WHEN '0'.
        WHEN '1'.
          rv_i = rv_i + 1.
        WHEN '2'.
          rv_i = rv_i + 2.
        WHEN '3'.
          rv_i = rv_i + 3.
        WHEN '4'.
          rv_i = rv_i + 4.
        WHEN '5'.
          rv_i = rv_i + 5.
        WHEN '6'.
          rv_i = rv_i + 6.
        WHEN '7'.
          rv_i = rv_i + 7.
        WHEN '8'.
          rv_i = rv_i + 8.
        WHEN '9'.
          rv_i = rv_i + 9.
        WHEN 'A'.
          rv_i = rv_i + 10.
        WHEN 'B'.
          rv_i = rv_i + 11.
        WHEN 'C'.
          rv_i = rv_i + 12.
        WHEN 'D'.
          rv_i = rv_i + 13.
        WHEN 'E'.
          rv_i = rv_i + 14.
        WHEN 'F'.
          rv_i = rv_i + 15.
        WHEN OTHERS.
          BREAK-POINT.
      ENDCASE.
      lv_string = lv_string+1.
    ENDWHILE.

  ENDMETHOD.                    "xstring_to_int

  METHOD xstring_to_string_utf8.

    DATA: lv_len    TYPE i,
          lo_obj    TYPE REF TO cl_abap_conv_in_ce.


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
* todo?
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
* todo?
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
*       CLASS lcl_hash DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS adler32 IMPORTING iv_xstring TYPE xstring
                          RETURNING value(rv_checksum) TYPE t_adler32.

    CLASS-METHODS sha1 IMPORTING iv_type TYPE t_type
                                 iv_data TYPE xstring
                       RETURNING value(rv_sha1) TYPE t_sha1.

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

  ENDMETHOD.                    "adler32

  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_hash    TYPE hash160,
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

    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        data           = lv_xstring
      IMPORTING
        hash           = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    rv_sha1 = lv_hash.

  ENDMETHOD.                    "sha1

ENDCLASS.                    "lcl_hash IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pack DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS decode IMPORTING iv_data TYPE xstring
                         RETURNING value(rt_objects) TYPE tt_objects.

    CLASS-METHODS decode_tree IMPORTING iv_data TYPE xstring
                         RETURNING value(rt_nodes) TYPE tt_nodes.

    CLASS-METHODS decode_deltas CHANGING ct_objects TYPE tt_objects.

    CLASS-METHODS decode_commit IMPORTING iv_data TYPE xstring
                         RETURNING value(rs_commit) TYPE st_commit.

    CLASS-METHODS encode IMPORTING it_objects TYPE tt_objects
                         RETURNING value(rv_data) TYPE xstring.

    CLASS-METHODS sanity_checks IMPORTING it_objects TYPE tt_objects
                         RETURNING value(rt_latest) TYPE tt_latest.

    CLASS-METHODS latest IMPORTING it_objects TYPE tt_objects
                         RETURNING value(rt_latest) TYPE tt_latest.

    CLASS-METHODS: encode_tree IMPORTING it_nodes TYPE tt_nodes
                         RETURNING value(rv_data) TYPE xstring.

    CLASS-METHODS: encode_commit IMPORTING is_commit TYPE st_commit
                         RETURNING value(rv_data) TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: c_debug_pack TYPE abap_bool VALUE abap_false.

    CLASS-METHODS delta IMPORTING is_object TYPE st_object
                        CHANGING ct_objects TYPE tt_objects.

    CLASS-METHODS delta_header CHANGING cv_delta TYPE xstring.

    CLASS-METHODS get_type IMPORTING iv_bitbyte TYPE t_bitbyte
                           RETURNING value(rv_type) TYPE t_type.

    CLASS-METHODS walk IMPORTING it_objects TYPE tt_objects
                                 iv_sha1 TYPE t_sha1
                                 iv_path TYPE string
                       CHANGING ct_latest TYPE tt_latest.

ENDCLASS.                    "lcl_pack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_pack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pack IMPLEMENTATION.

  METHOD encode_tree.

    DATA: lv_string  TYPE string,
          lv_null    TYPE x,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lv_null = '00'.

    LOOP AT it_nodes ASSIGNING <ls_node>.
      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

      CONCATENATE rv_data lv_xstring lv_null <ls_node>-sha1 INTO rv_data IN BYTE MODE.
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

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      CONCATENATE 'parent' lv_parent_lower INTO lv_tmp  SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author INTO lv_tmp  SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    CONCATENATE lv_string cl_abap_char_utilities=>newline is_commit-body INTO lv_string.

*    BREAK-POINT.
    rv_data = lcl_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.                    "encode_commit

  METHOD walk.

    DATA: lv_path   TYPE string,
          ls_latest LIKE LINE OF ct_latest,
          lt_nodes  TYPE tt_nodes.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree> WITH KEY sha1 = iv_sha1 type = gc_tree.
    IF sy-subrc <> 0.
      WRITE: / 'Tree', iv_sha1, 'not found'.                "#EC NOTEXT
      BREAK-POINT.
      RETURN.
    ENDIF.

    lt_nodes = lcl_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
*      WRITE: / <ls_node>-sha1, <ls_node>-directory, <ls_node>-name.
      IF <ls_node>-directory = abap_false.
        READ TABLE it_objects ASSIGNING <ls_blob> WITH KEY sha1 = <ls_node>-sha1 type = gc_blob.
        IF sy-subrc <> 0.
          BREAK-POINT.
          CONTINUE.
        ENDIF.

        CLEAR ls_latest.
        ls_latest-path = iv_path.
        ls_latest-filename = <ls_node>-name.
        ls_latest-data = <ls_blob>-data.
        APPEND ls_latest TO ct_latest.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE directory = abap_true.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.
      walk( EXPORTING it_objects = it_objects
                      iv_sha1 = <ls_node>-sha1
                      iv_path = lv_path
            CHANGING ct_latest = ct_latest ).
    ENDLOOP.

  ENDMETHOD.                    "walk

  METHOD latest.

    DATA: ls_commit  TYPE st_commit,
          lv_sha1    TYPE t_sha1,
          lt_commits TYPE tt_objects.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    LOOP AT it_objects ASSIGNING <ls_object> WHERE type = gc_commit.
      APPEND <ls_object> TO lt_commits.
    ENDLOOP.

* find first commit
    LOOP AT lt_commits ASSIGNING <ls_object>.
      ls_commit = lcl_pack=>decode_commit( <ls_object>-data ).
      IF ls_commit-parent IS INITIAL.
        lv_sha1 = <ls_object>-sha1.
        EXIT. " current loop.
      ENDIF.
    ENDLOOP.

* todo, there must be faster/easier way to do this
* todo, assumes no branching
    DO lines( lt_commits ) TIMES.
      LOOP AT lt_commits ASSIGNING <ls_object>.
        ls_commit = lcl_pack=>decode_commit( <ls_object>-data ).
        IF ls_commit-parent = lv_sha1.
          lv_sha1 = <ls_object>-sha1.
          EXIT. " current loop
        ENDIF.
      ENDLOOP.
    ENDDO.

    READ TABLE lt_commits ASSIGNING <ls_object> WITH KEY sha1 = lv_sha1.
    IF sy-subrc <> 0.
      BREAK-POINT.
      RETURN.
    ENDIF.

    ls_commit = lcl_pack=>decode_commit( <ls_object>-data ).

    walk( EXPORTING it_objects = it_objects
                    iv_sha1 = ls_commit-tree
                    iv_path = '/'
          CHANGING ct_latest = rt_latest ).

  ENDMETHOD.                    "latest

  METHOD sanity_checks.

    DATA: ls_commit TYPE st_commit,
          lt_nodes  TYPE tt_nodes.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <ls_node>   LIKE LINE OF lt_nodes.


* check that parent exists for all commits
    LOOP AT it_objects ASSIGNING <ls_object> WHERE type = gc_commit.
      ls_commit = lcl_pack=>decode_commit( <ls_object>-data ).
      IF ls_commit-parent IS INITIAL.
        CONTINUE.
      ENDIF.
      READ TABLE it_objects WITH KEY sha1 = ls_commit-parent type = gc_commit
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        WRITE: / 'Commit', ls_commit-parent, 'not found'.   "#EC NOTEXT
      ENDIF.
    ENDLOOP.

* check that tree exists for all commits
    LOOP AT it_objects ASSIGNING <ls_object> WHERE type = gc_commit.
      ls_commit = lcl_pack=>decode_commit( <ls_object>-data ).
      READ TABLE it_objects WITH KEY sha1 = ls_commit-tree type = gc_tree
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        WRITE: / 'Tree', ls_commit-tree, 'not found'.       "#EC NOTEXT
      ENDIF.
    ENDLOOP.

    LOOP AT it_objects ASSIGNING <ls_object> WHERE type = gc_tree.
* check that blobs in trees exists
      lt_nodes = lcl_pack=>decode_tree( <ls_object>-data ).
      LOOP AT lt_nodes ASSIGNING <ls_node> WHERE directory = abap_false.
        READ TABLE it_objects WITH KEY sha1 = <ls_node>-sha1 type = gc_blob
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          WRITE: / 'Blob', <ls_node>-sha1, 'not found'.     "#EC NOTEXT
        ENDIF.
      ENDLOOP.

* check that directories/trees in trees exists
      LOOP AT lt_nodes ASSIGNING <ls_node> WHERE directory = abap_true.
        READ TABLE it_objects WITH KEY sha1 = <ls_node>-sha1 type = gc_tree
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          WRITE: / 'Tree', <ls_node>-sha1, 'not found'.     "#EC NOTEXT
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "sanity_checks

  METHOD get_type.

    DATA: lv_char3 TYPE c LENGTH 3.

    lv_char3 = iv_bitbyte+1.

    CASE lv_char3.
      WHEN '001'.
        rv_type = gc_commit.
      WHEN '010'.
        rv_type = gc_tree.
      WHEN '011'.
        rv_type = gc_blob.
      WHEN '111'.
        rv_type = gc_ref_d.
      WHEN OTHERS.
* todo?
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.                    "get_type

  METHOD decode_commit.

    DATA: lv_string TYPE string,
          lv_char40 TYPE c LENGTH 40,
          lv_mode   TYPE string,
          lv_len    TYPE i,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> TYPE string.


    lv_string = lcl_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

    lv_mode = 'tree'.
    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_len = strlen( lv_mode ).

      IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
        CASE lv_mode.
          WHEN 'tree'.
            lv_char40 = <lv_string>+5.
            TRANSLATE lv_char40 TO UPPER CASE.
            rs_commit-tree = lv_char40.
            lv_mode = 'parent'.
          WHEN 'parent'.
            lv_char40 = <lv_string>+7.
            TRANSLATE lv_char40 TO UPPER CASE.
            rs_commit-parent = lv_char40.
            lv_mode = 'author'.
          WHEN 'author'.
            rs_commit-author = <lv_string>+7.
            lv_mode = 'committer'.
          WHEN 'committer'.
            rs_commit-committer = <lv_string>+10.
            CLEAR lv_mode.
        ENDCASE.
      ELSEIF lv_mode = 'parent' AND <lv_string>(6) = 'author'.
* first commit doesnt have parent
        rs_commit-author = <lv_string>+7.
        lv_mode = 'committer'.
      ELSE.
* body
        CONCATENATE rs_commit-body <lv_string> INTO rs_commit-body
          SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.
    ENDLOOP.

* strip first newline
    IF strlen( rs_commit-body ) >= 2.
      rs_commit-body = rs_commit-body+2.
    ENDIF.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
* multiple parents? not supported
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.                    "decode_commit

  METHOD delta_header.

    DATA: lv_bitbyte TYPE t_bitbyte,
          lv_header1 TYPE i,                                "#EC NEEDED
          lv_header2 TYPE i,                                "#EC NEEDED
          lv_bits    TYPE string,
          lv_x       TYPE x.

* todo, use headers for verification

* Header 1
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
    lv_header1 = lcl_convert=>bitbyte_to_int( lv_bits ).

* Header 2
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
    lv_header2 = lcl_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.                    "delta_header

  METHOD delta.

    DATA: lv_delta   TYPE xstring,
          lv_base    TYPE xstring,
          lv_result  TYPE xstring,
          lv_bitbyte TYPE t_bitbyte,
          lv_offset  TYPE i,
          ls_object  LIKE LINE OF ct_objects,
          lv_len     TYPE i,
          lv_x       TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.



    lv_delta = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object> WITH KEY sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      BREAK-POINT.
      WRITE: / 'Base not found'.                            "#EC NOTEXT
      RETURN.
    ELSE.
      lv_base = <ls_object>-data.
    ENDIF.


    delta_header( CHANGING cv_delta = lv_delta ).


    WHILE xstrlen( lv_delta ) > 0.

      lv_x = lv_delta(1).
      lv_delta = lv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
*    WRITE: / 'Opcode', lv_x, lv_bitbyte.

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

        CONCATENATE lv_result lv_base+lv_offset(lv_len) INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_x.
        CONCATENATE lv_result lv_delta(lv_len) INTO lv_result IN BYTE MODE.
        lv_delta = lv_delta+lv_len.
      ENDIF.

    ENDWHILE.

    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = <ls_object>-type iv_data = lv_result ).
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    APPEND ls_object TO ct_objects.

  ENDMETHOD.                    "delta

  METHOD decode_deltas.

    DATA: ls_object LIKE LINE OF ct_objects,
          lt_deltas LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object WHERE type = gc_ref_d.
      DELETE ct_objects INDEX sy-tabix.
      APPEND ls_object TO lt_deltas.
    ENDLOOP.

    LOOP AT lt_deltas INTO ls_object.
      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.                    "decode_deltas

  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE string,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          ls_node    TYPE st_node,
          lv_start   TYPE i.

    DO.
      IF lv_cursor >= xstrlen( iv_data ).
        EXIT. " current loop
      ENDIF.

      IF iv_data+lv_cursor(1) = '00'.
        lv_len = lv_cursor - lv_start.
        lv_xstring = iv_data+lv_start(lv_len).

        lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
        SPLIT lv_string AT space INTO lv_chmod lv_name.

        lv_offset = lv_cursor + 1.

        CLEAR ls_node.
        ls_node-chmod = lv_chmod.
        IF ls_node-chmod(1) = '4'.
          ls_node-directory = abap_true.
        ELSE.
          ls_node-directory = abap_false.
        ENDIF.
        ls_node-name = lv_name.
        ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
        APPEND ls_node TO rt_nodes.

        lv_start = lv_cursor + 1 + lc_sha_length.
        lv_cursor = lv_start.
      ELSE.
        lv_cursor = lv_cursor + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "decode_tree

  METHOD decode.

    DATA: lv_x           TYPE x,
          lv_data        TYPE xstring,
          lv_bitbyte     TYPE t_bitbyte,
          lv_type        TYPE c LENGTH 6,
          lv_zlib        TYPE x LENGTH 2,
          lv_objects     TYPE i,
          lv_ref_delta   TYPE t_sha1,
          lv_adler32     TYPE t_adler32,
          lv_compressed     TYPE xstring,
          lv_compressed_len TYPE i,
          lv_decompress_len TYPE i,
          lv_decompressed   TYPE xstring,
          lv_objects_count  TYPE i,
          lv_xstring     TYPE xstring,
          lv_expected    TYPE i,
          lv_length_bits TYPE string,
          ls_object      LIKE LINE OF rt_objects.


    lv_data = iv_data.

    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> '5041434B'.
      BREAK-POINT.
      RETURN.
    ENDIF.

    IF c_debug_pack = abap_true.
      WRITE: / '-------------------Start PACK-------------------'. "#EC NOTEXT
    ENDIF.

    lv_data = lv_data+4.

    IF c_debug_pack = abap_true.
      WRITE: / 'Version:', lv_data(4).                      "#EC NOTEXT
    ENDIF.
    lv_data = lv_data+4.

    lv_xstring = lv_data(4).
    lv_objects = lcl_convert=>xstring_to_int( lv_xstring ).
    IF c_debug_pack = abap_true.
      WRITE: / 'Objects:', lv_data(4), '(hex)', lv_objects. "#EC NOTEXT
    ENDIF.
    lv_data = lv_data+4.

    IF c_debug_pack = abap_true.
      WRITE: /.
    ENDIF.

    DO lv_objects TIMES.
      lv_objects_count = sy-index.
      IF c_debug_pack = abap_true.
        WRITE: / 'Object', lv_objects_count.                "#EC NOTEXT
      ENDIF.

      lv_x = lv_data(1).
      IF c_debug_pack = abap_true.
        WRITE: / 'A:', lv_x, '(hex)'.
      ENDIF.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      IF c_debug_pack = abap_true.
        WRITE: lv_bitbyte.
      ENDIF.

      lv_type = get_type( lv_bitbyte ).

      IF c_debug_pack = abap_true.
        WRITE: lv_type.
      ENDIF.
      lv_data = lv_data+1.
      lv_length_bits = lv_bitbyte+4.

      WHILE lv_bitbyte(1) <> '0'.
        lv_x = lv_data(1).
        IF c_debug_pack = abap_true.
          WRITE: / 'x:', lv_x, '(hex)'.
        ENDIF.
        lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
        IF c_debug_pack = abap_true.
          WRITE: lv_bitbyte.
        ENDIF.
        lv_data = lv_data+1.
        CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
      ENDWHILE.

      lv_expected = lcl_convert=>bitbyte_to_int( lv_length_bits ).
      IF c_debug_pack = abap_true.
        WRITE: / 'Length', lv_length_bits, '=', lv_expected. "#EC NOTEXT
      ENDIF.

      IF lv_type = gc_ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> '789C' AND lv_zlib <> '7801'.
        BREAK-POINT.
        WRITE: / 'Unexpected zlib header'.                  "#EC NOTEXT
        RETURN.
      ENDIF.
      lv_data = lv_data+2.

*******************************

      IF lv_zlib = '789C'.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_data
          IMPORTING
            raw_out     = lv_decompressed
            raw_out_len = lv_decompress_len ).

        IF lv_expected <> lv_decompress_len.
          BREAK-POINT.
          RETURN.
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          BREAK-POINT.
          WRITE: / 'Compressed data doesnt match'.          "#EC NOTEXT
          RETURN.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.
        lv_data = lv_data+4. " skip adler checksum

      ELSEIF lv_zlib = '7801'.
* this takes some processing, when time permits, implement DEFLATE algorithm
* cl_abap_gzip copmression works for '789C', but does not produce the same
* result when '7801'
* compressed data might be larger than origial so add 10, adding 10 is safe
* as package always ends with sha1 checksum
        DO lv_expected + 10 TIMES.
          lv_compressed_len = sy-index.

          cl_abap_gzip=>decompress_binary(
            EXPORTING
              gzip_in     = lv_data
              gzip_in_len = lv_compressed_len
            IMPORTING
              raw_out     = lv_decompressed
              raw_out_len = lv_decompress_len ).

          IF lv_decompress_len = lv_expected.
            EXIT.
          ELSE.
            CLEAR lv_compressed_len.
          ENDIF.
        ENDDO.

        IF lv_compressed_len IS INITIAL.
          BREAK-POINT.
          WRITE: / 'Decompression falied :o/'.              "#EC NOTEXT
          RETURN.
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
          WRITE: / 'Wrong checksum'.                        "#EC NOTEXT
          BREAK-POINT.
          RETURN.
        ENDIF.

        lv_data = lv_data+4. " skip adler checksum

      ENDIF.

*************************

      CLEAR ls_object.
      IF lv_type = gc_ref_d.
        ls_object-sha1 = lv_ref_delta.
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

*  WRITE: / 'Done,', lv_objects_count, 'objects, expected', lv_objects. "#EC NOTEXT
* todo, SHA1 at end

  ENDMETHOD.                    "decode

  METHOD encode.
* todo
    RETURN.
  ENDMETHOD.                    "encode

ENDCLASS.                    "lcl_pack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_transport DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_transport DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS upload_pack IMPORTING iv_repo TYPE string
                              RETURNING value(rv_pack) TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: c_debug_http TYPE abap_bool VALUE abap_false.

    CLASS-METHODS pkt IMPORTING iv_string TYPE string CHANGING cv_pkt TYPE string.
    CLASS-METHODS parse EXPORTING ev_pack TYPE xstring CHANGING cv_data TYPE xstring.
    CLASS-METHODS length_utf8_hex IMPORTING iv_data TYPE xstring RETURNING value(rv_len) TYPE i.

ENDCLASS.                    "lcl_transport DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_transport IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_transport IMPLEMENTATION.

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

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = length_utf8_hex( cv_data ).
      IF c_debug_http = abap_true.
        WRITE: / '---------------------------------------------'.
        WRITE: / 'PKT, length:', lv_len.                    "#EC NOTEXT
      ENDIF.
      IF lv_len = 0.
        EXIT. " current loop
      ENDIF.

      lv_contents = cv_data(lv_len).
      IF c_debug_http = abap_true.
        WRITE: '(hex length:', lv_contents(4), ')'.
      ENDIF.
      lv_contents = lv_contents+4.
      IF c_debug_http = abap_true.
        WRITE: / lv_contents.
      ENDIF.
      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = '01'. " band 1
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
        IF c_debug_http = abap_true.
          WRITE: / 'Pack data'.                             "#EC NOTEXT
        ENDIF.
      ELSE.
        IF c_debug_http = abap_true.
          PERFORM output_string USING lv_contents.
        ENDIF.
      ENDIF.

      IF c_debug_http = abap_true.
        WRITE: /.
      ENDIF.

      cv_data = cv_data+lv_len.
    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.                    "parse

  METHOD upload_pack.

    DATA: lv_code        TYPE i,
          lv_status_text TYPE string,
          li_client      TYPE REF TO if_http_client,
          lt_result      TYPE TABLE OF string,
          lv_buffer      TYPE string,
          lv_hash        TYPE c LENGTH 40,
          lv_xstring     TYPE xstring,
          lv_line        TYPE string,
          lv_repo        TYPE string,
          lv_pkt         TYPE string,
          lv_data        TYPE string.


    CONCATENATE iv_repo '.git' INTO lv_repo.

    cl_http_client=>create_by_url(
      EXPORTING
        url    = 'https://github.com'
      IMPORTING
        client = li_client ).

    lv_buffer = ''.
    li_client->request->set_cdata( lv_buffer ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_repo && '/info/refs?service=git-upload-pack' ).
    li_client->send( ).
    li_client->receive( ).
    li_client->response->get_status(
      IMPORTING
        code   = lv_code
        reason = lv_status_text ).
    IF c_debug_http = abap_true.
      WRITE: / 'HTTP_STATUS_CODE = ', lv_code, lv_status_text.
    ENDIF.
    lv_data = li_client->response->get_cdata( ).

    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_result.
    LOOP AT lt_result INTO lv_data.
      IF lv_data CP '*refs/heads/master*'.
        lv_hash = lv_data+4.
      ENDIF.
    ENDLOOP.

    IF strlen( lv_hash ) <> 40.
      WRITE: / 'Branch not found'.                          "#EC NOTEXT
      BREAK-POINT.
      RETURN.
    ENDIF.

*  lv_data = lo_client->response->get_cdata( ).
*  SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_result.
*  LOOP AT lt_result INTO lv_data.
*    WRITE: / lv_data.
*  ENDLOOP.

*--------------------------------------------------------------------

    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_repo && '/git-upload-pack' ).
    li_client->request->set_header_field(
        name  = 'Content-Type'
        value = 'Content-Type: application/x-git-upload-pack-request' ). "#EC NOTEXT

    lv_line = 'want' && ` ` && lv_hash && ` ` && 'side-band-64k no-progress' && cl_abap_char_utilities=>newline.
    pkt( EXPORTING iv_string = lv_line
         CHANGING cv_pkt = lv_pkt ).

    lv_buffer = lv_pkt
             && '0000'
             && '0009done' && cl_abap_char_utilities=>newline.

    li_client->request->set_cdata( lv_buffer ).
    li_client->send( ).
    li_client->receive( ).
    li_client->response->get_status(
      IMPORTING
        code   = lv_code
        reason = lv_status_text ).
    IF c_debug_http = abap_true.
      WRITE: / 'HTTP_STATUS_CODE = ', lv_code, lv_status_text.
    ENDIF.

    lv_xstring = li_client->response->get_data( ).

    parse( IMPORTING ev_pack = rv_pack
           CHANGING cv_data = lv_xstring ).

  ENDMETHOD.                    "uplaod_pack

  METHOD pkt.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.

    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
* todo
      BREAK-POINT.
    ENDIF.

    lv_x = lv_len + 4.

    cv_pkt = cv_pkt && '00' && lv_x && iv_string.

  ENDMETHOD.                    "pkt

ENDCLASS.                    "lcl_transport IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

*  DATA: lv_repo TYPE string VALUE '/larshp/Foobar'.                " 100%
*DATA: lv_repo TYPE string VALUE '/larshp/MouseChase'.            " 100%
  DATA: lv_repo TYPE string VALUE '/larshp/Dicing'.                " 100%
*DATA: lv_repo TYPE string VALUE '/snowplow/snowplow'.            " base not found, 10000 ref deltas, multiple parents
*DATA: lv_repo TYPE string VALUE '/rvanmil/ABAP-Regex-Training'.  " 100%
*DATA: lv_repo TYPE string VALUE '/sciruela/ABAP-Exercises'.      " 100%
*DATA: lv_repo TYPE string VALUE '/adsworth/ABAP-Utils'.          " 100%
*DATA: lv_repo TYPE string VALUE '/rvanmil/Run-ABAP-Code'.        " 100%
*DATA: lv_repo TYPE string VALUE '/rvanmil/ABAP-OOP-Library'.     " 100%
*DATA: lv_repo TYPE string VALUE '/ivanfemia/abap2xlsx'.          " base not found, 2000 ref deltas, multiple parents
*DATA: lv_repo TYPE string VALUE '/InfoSize/abapsourcesearch'.    " 100%
*DATA: lv_repo TYPE string VALUE '/google/flatbuffers'.           " 100%
*DATA: lv_repo TYPE string VALUE '/antiboredom/videogrep'.        " 100%
*DATA: lv_repo TYPE string VALUE '/idank/explainshell'.           " 100%
*DATA: lv_repo TYPE string VALUE '/education/teachers_pet'.       " base not found, 694 ref deltas, multiple parents
*DATA: lv_repo TYPE string VALUE '/gmarik/Vundle.vim'.            " base not found, 829 ref deltas, multiple parents
*DATA: lv_repo TYPE string VALUE '/mephux/komanda'.               " base not found, 685 ref deltas, multiple parents
*DATA: lv_repo TYPE string VALUE '/mrmrs/colors'.                 " 100%
*DATA: lv_repo TYPE string VALUE '/montagejs/collections'.        " 100%

  DATA: lv_pack    TYPE xstring,
        lt_latest  TYPE tt_latest,
        lt_objects TYPE tt_objects.

  FIELD-SYMBOLS: <ls_latest> LIKE LINE OF lt_latest.


  lv_pack = lcl_transport=>upload_pack( lv_repo ).

  lt_objects = lcl_pack=>decode( lv_pack ).

  PERFORM output_summary USING lt_objects.

  lcl_pack=>decode_deltas( CHANGING ct_objects = lt_objects ).

  lcl_pack=>sanity_checks( lt_objects ).

  lt_latest = lcl_pack=>latest( lt_objects ).

  LOOP AT lt_latest ASSIGNING <ls_latest>.
    WRITE: / <ls_latest>-path, 40 <ls_latest>-filename.
  ENDLOOP.

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  show_utf8
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_DATA    text
*----------------------------------------------------------------------*
FORM output_string USING pv_data TYPE xstring.

  DATA: lv_string TYPE string.


  lv_string = lcl_convert=>xstring_to_string_utf8( pv_data ).
  WRITE: / lv_string.

ENDFORM.                    "show_utf8

*&---------------------------------------------------------------------*
*&      Form  output_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_summary USING pt_objects TYPE tt_objects.

  DATA: lv_lines TYPE i.

  FIELD-SYMBOLS: <ls_object> LIKE LINE OF pt_objects.

  DEFINE _count.
    lv_lines = 0.
    loop at pt_objects assigning <ls_object> where type = &1.
      lv_lines = lv_lines + 1.
    endloop.
  END-OF-DEFINITION.


  _count gc_commit.
  WRITE: / lv_lines, 'commits'.

  _count gc_tree.
  WRITE: / lv_lines, 'trees'.

  _count gc_blob.
  WRITE: / lv_lines, 'blobs'.

  _count gc_ref_d.
  WRITE: / lv_lines, 'ref deltas'.

  WRITE: /.

ENDFORM.                    "output_summary

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_XSTRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_xstring USING pv_data TYPE xstring.

  CONSTANTS: lc_len TYPE i VALUE 100.

  DATA: lv_offset TYPE i,
        lv_len    TYPE i.


  lv_len = xstrlen( pv_data ).
  DO.
    IF lv_offset + lc_len > lv_len.
      WRITE: / pv_data+lv_offset.
      EXIT. " current loop
    ELSE.
      WRITE: / pv_data+lv_offset(lc_len).
    ENDIF.
    lv_offset = lv_offset + lc_len.
  ENDDO.

ENDFORM.                    " OUTPUT_XSTRING

*----------------------------------------------------------------------*
*       CLASS test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS repository_larshp_foobar FOR TESTING.
    METHODS repository_larshp_mousechase FOR TESTING.
    METHODS repository_larshp_dicing FOR TESTING.

    METHODS encode_decode_tree FOR TESTING.
    METHODS encode_decode_commit FOR TESTING.

    CLASS-METHODS latest IMPORTING iv_repo TYPE string
                         RETURNING value(rt_latest) TYPE tt_latest.
    CLASS-METHODS compare IMPORTING iv_repo TYPE string.
    CLASS-METHODS http_fetch IMPORTING iv_url TYPE string
                             RETURNING value(rv_data) TYPE xstring.

ENDCLASS.                    "test DEFINITION
*----------------------------------------------------------------------*
*       CLASS test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_unit IMPLEMENTATION.

  METHOD encode_decode_tree.

    DATA: lt_nodes  TYPE tt_nodes,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE tt_nodes.

    CLEAR ls_node.
    ls_node-chmod = '100644'.
    ls_node-directory = abap_false.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = '5F46CB3C4B7F0B3600B64F744CDE614A283A88DC'.
    APPEND ls_node TO lt_nodes.

    lv_data = lcl_pack=>encode_tree( lt_nodes ).
    lt_result = lcl_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.                    "tree_encode_decode

  METHOD encode_decode_commit.

    DATA: ls_commit TYPE st_commit,
          ls_result TYPE st_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = '44CDE614A283A88DC5F46CB3C4B7F0B3600B64F7'.
    ls_commit-parent    = '83A88DC5F46CB3C4B7F0B3600B64F744CDE614A2'.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.

    lv_data = lcl_pack=>encode_commit( ls_commit ).
    ls_result = lcl_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.                    "commit_encode_decode

  METHOD http_fetch.

    DATA: li_client TYPE REF TO if_http_client,
          lv_code   TYPE i.

    cl_http_client=>create_by_url(
      EXPORTING
        url    = iv_url
      IMPORTING
        client = li_client ).

    li_client->send( ).
    li_client->receive( ).
    li_client->response->get_status(
      IMPORTING
        code   = lv_code ).

    cl_abap_unit_assert=>assert_equals(
        exp = 200
        act = lv_code ).

    rv_data = li_client->response->get_data( ).

  ENDMETHOD.                    "http_fetch

  METHOD compare.

    DATA: lv_url    TYPE string,
          lv_data   TYPE xstring,
          lt_latest TYPE tt_latest.

    FIELD-SYMBOLS: <ls_latest> LIKE LINE OF lt_latest.


    lt_latest = latest( iv_repo ).

    LOOP AT lt_latest ASSIGNING <ls_latest>.
      CONCATENATE
        'https://raw.githubusercontent.com'
        iv_repo
        '/master'
        <ls_latest>-path
        <ls_latest>-filename INTO lv_url.                   "#EC NOTEXT

      lv_data = http_fetch( lv_url ).

      cl_abap_unit_assert=>assert_equals(
          exp = <ls_latest>-data
          act = lv_data ).
    ENDLOOP.

  ENDMETHOD.                    "compare

  METHOD latest.

    DATA: lv_pack    TYPE xstring,
          lt_objects TYPE tt_objects.

    lv_pack = lcl_transport=>upload_pack( iv_repo ).
    lt_objects = lcl_pack=>decode( lv_pack ).
    lcl_pack=>decode_deltas( CHANGING ct_objects = lt_objects ).
    rt_latest = lcl_pack=>latest( lt_objects ).

  ENDMETHOD.                    "latest

  METHOD repository_larshp_foobar.
    compare( '/larshp/Foobar' ).                            "#EC NOTEXT
  ENDMETHOD.                    "test_minus_ten_percent

  METHOD repository_larshp_mousechase.
    compare( '/larshp/MouseChase' ).                        "#EC NOTEXT
  ENDMETHOD.                    "larshp_mousechase

  METHOD repository_larshp_dicing.
    compare( '/larshp/Dicing' ).                            "#EC NOTEXT
  ENDMETHOD.                    "larshp_dicing

ENDCLASS.                    "test IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_COMMITS  text
*----------------------------------------------------------------------*
FORM output_objects USING pt_objects TYPE tt_objects.

  FIELD-SYMBOLS: <ls_object> LIKE LINE OF pt_objects.


  LOOP AT pt_objects ASSIGNING <ls_object>.
    CASE <ls_object>-type.
      WHEN gc_commit.
        PERFORM output_commit USING <ls_object>.
      WHEN OTHERS.
        BREAK-POINT.
        WRITE: / 'todo'.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " OUTPUT_OBJECTS

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_COMMIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LS_COMMIT>  text
*----------------------------------------------------------------------*
FORM output_commit USING ps_object TYPE st_object.

  DATA: ls_commit TYPE st_commit.


  WRITE: / 'SHA1:', ps_object-sha1.

  ls_commit = lcl_pack=>decode_commit( ps_object-data ).

  WRITE: / 'tree', ls_commit-tree.
  WRITE: / 'parent', ls_commit-parent.
  WRITE: / 'author', ls_commit-author.
  WRITE: / 'committer', ls_commit-committer.
  WRITE: / ls_commit-body.

ENDFORM.                    " OUTPUT_COMMIT
