CLASS zcl_abapgit_git_pack DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_node,
        chmod TYPE zif_abapgit_definitions=>ty_chmod,
        name  TYPE string,
        sha1  TYPE zif_abapgit_definitions=>ty_sha1,
      END OF ty_node .
    TYPES:
      ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_commit,
        tree      TYPE zif_abapgit_definitions=>ty_sha1,
        parent    TYPE zif_abapgit_definitions=>ty_sha1,
        parent2   TYPE zif_abapgit_definitions=>ty_sha1,
        author    TYPE string,
        committer TYPE string,
        body      TYPE string,
      END OF ty_commit .
    TYPES:
      BEGIN OF ty_adler32,
        sha1 TYPE zif_abapgit_definitions=>ty_sha1,
        type TYPE zif_abapgit_definitions=>ty_type,
      END OF ty_adler32 .

    CLASS-METHODS decode
      IMPORTING
        !iv_data          TYPE xstring
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS decode_tree
      IMPORTING
        !iv_data        TYPE xstring
      RETURNING
        VALUE(rt_nodes) TYPE ty_nodes_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS decode_commit
      IMPORTING
        !iv_data         TYPE xstring
      RETURNING
        VALUE(rs_commit) TYPE ty_commit
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS encode
      IMPORTING
        !it_objects    TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS encode_tree
      IMPORTING
        !it_nodes      TYPE ty_nodes_tt
      RETURNING
        VALUE(rv_data) TYPE xstring .
    CLASS-METHODS encode_commit
      IMPORTING
        !is_commit     TYPE ty_commit
      RETURNING
        VALUE(rv_data) TYPE xstring .
  PRIVATE SECTION.

    CONSTANTS:
      c_pack_start TYPE x LENGTH 4 VALUE '5041434B' ##NO_TEXT.
    CONSTANTS:
      c_zlib       TYPE x LENGTH 2 VALUE '789C' ##NO_TEXT.
    CONSTANTS:
      c_zlib_hmm   TYPE x LENGTH 2 VALUE '7801' ##NO_TEXT.
    CONSTANTS:                                                  " PACK
      c_version    TYPE x LENGTH 4 VALUE '00000002' ##NO_TEXT.

    CLASS-METHODS decode_deltas
      CHANGING
        !ct_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delta
      IMPORTING
        !is_object  TYPE zif_abapgit_definitions=>ty_object
      CHANGING
        !ct_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delta_header
      EXPORTING
        !ev_header TYPE i
      CHANGING
        !cv_delta  TYPE xstring .
    CLASS-METHODS sort_tree
      IMPORTING
        !it_nodes       TYPE ty_nodes_tt
      RETURNING
        VALUE(rt_nodes) TYPE ty_nodes_tt .
    CLASS-METHODS get_type
      IMPORTING
        !iv_x          TYPE x
      RETURNING
        VALUE(rv_type) TYPE zif_abapgit_definitions=>ty_type
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_length
      EXPORTING
        !ev_length TYPE i
      CHANGING
        !cv_data   TYPE xstring .
    CLASS-METHODS type_and_length
      IMPORTING
        !iv_type          TYPE zif_abapgit_definitions=>ty_type
        !iv_length        TYPE i
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS zlib_decompress
      CHANGING
        !cv_data         TYPE xstring
        !cv_decompressed TYPE xstring
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GIT_PACK IMPLEMENTATION.


  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE zif_abapgit_definitions=>ty_sha1,
          lv_ref_delta      TYPE zif_abapgit_definitions=>ty_sha1,
          lv_compressed_len TYPE i,
          lv_compressed     TYPE xstring,
          lv_decompressed   TYPE xstring,
          lv_decompress_len TYPE i,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects.


    lv_data = iv_data.

* header
    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> c_pack_start.
      zcx_abapgit_exception=>raise( 'Unexpected pack header' ).
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      zcx_abapgit_exception=>raise( 'Version not supported' ).
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = zcl_abapgit_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = zif_abapgit_definitions=>gc_type-ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        zcx_abapgit_exception=>raise( 'Unexpected zlib header' ).
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
          zcx_abapgit_exception=>raise( 'Decompression falied' ).
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          "Lets try with zlib before error in out for good
          "This fixes issues with TFS 2017 and visualstudio.com Git repos
          zlib_decompress( CHANGING cv_data = lv_data
                                    cv_decompressed = lv_decompressed ).
        ELSE.
          lv_data = lv_data+lv_compressed_len.
        ENDIF.

      ELSEIF lv_zlib = c_zlib_hmm.
* cl_abap_gzip copmression works for header '789C', but does not work for
* '7801', call custom implementation of DEFLATE algorithm.
* The custom implementation could handle both, but most likely the kernel
* implementation runs faster than the custom ABAP.
        zlib_decompress( CHANGING cv_data = lv_data
                                  cv_decompressed = lv_decompressed ).
      ENDIF.

      CLEAR ls_object.
      ls_object-adler32 = lv_data(4).
      lv_data = lv_data+4. " skip adler checksum

      IF lv_type = zif_abapgit_definitions=>gc_type-ref_d.
        ls_object-sha1 = lv_ref_delta.
        TRANSLATE ls_object-sha1 TO LOWER CASE.
      ELSE.
        ls_object-sha1 = zcl_abapgit_hash=>sha1(
          iv_type = lv_type
          iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      APPEND ls_object TO rt_objects.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = zcl_abapgit_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      zcx_abapgit_exception=>raise( 'SHA1 at end of pack doesnt match' ).
    ENDIF.

    decode_deltas( CHANGING ct_objects = rt_objects ).

  ENDMETHOD.                    "decode


  METHOD decode_commit.

    DATA: lv_string TYPE string,
          lv_word   TYPE string,
          lv_length TYPE i,
          lv_trash  TYPE string ##NEEDED,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT zif_abapgit_definitions=>gc_newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_length = strlen( <lv_string> ) + 1.
      lv_string = lv_string+lv_length.

      SPLIT <lv_string> AT space INTO lv_word lv_trash.
      CASE lv_word.
        WHEN 'tree'.
          rs_commit-tree = <lv_string>+5.
        WHEN 'parent'.
          IF rs_commit-parent IS INITIAL.
            rs_commit-parent = <lv_string>+7.
          ELSE.
            rs_commit-parent2 = <lv_string>+7.
          ENDIF.
        WHEN 'author'.
          rs_commit-author = <lv_string>+7.
        WHEN 'committer'.
          rs_commit-committer = <lv_string>+10.
          EXIT. " current loop
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

    rs_commit-body = lv_string+1.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      zcx_abapgit_exception=>raise( 'multiple parents? not supported' ).
    ENDIF.

  ENDMETHOD.                    "decode_commit


  METHOD decode_deltas.

    DATA: ls_object   LIKE LINE OF ct_objects,
          lo_progress TYPE REF TO zcl_abapgit_progress,
          lt_deltas   LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object WHERE type = zif_abapgit_definitions=>gc_type-ref_d.
      DELETE ct_objects INDEX sy-tabix.
      APPEND ls_object TO lt_deltas.
    ENDLOOP.

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( lt_deltas ).

    LOOP AT lt_deltas INTO ls_object.
      lo_progress->show( iv_current = sy-tabix
                         iv_text    = 'Decode deltas' ) ##NO_TEXT.

      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.                    "decode_deltas


  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE zif_abapgit_definitions=>ty_chmod,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          lv_match   TYPE i,
          ls_node    TYPE ty_node.


    DO.
      FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor OF iv_data
        IN BYTE MODE MATCH OFFSET lv_match.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      lv_len = lv_match - lv_cursor.
      lv_xstring = iv_data+lv_cursor(lv_len).

      lv_string = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).
      SPLIT lv_string AT space INTO lv_chmod lv_name.

      CLEAR ls_node.
      ls_node-chmod = lv_chmod.
      IF ls_node-chmod <> zif_abapgit_definitions=>gc_chmod-dir
          AND ls_node-chmod <> zif_abapgit_definitions=>gc_chmod-file
          AND ls_node-chmod <> zif_abapgit_definitions=>gc_chmod-executable.
        zcx_abapgit_exception=>raise( 'Unknown chmod' ).
      ENDIF.

      lv_offset = lv_match + 1.
      ls_node-name = lv_name.
      ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
      TRANSLATE ls_node-sha1 TO LOWER CASE.
      APPEND ls_node TO rt_nodes.

      lv_cursor = lv_match + 1 + lc_sha_length.
    ENDDO.

  ENDMETHOD.                    "decode_tree


  METHOD delta.

    CONSTANTS: lc_1   TYPE x VALUE '01',
               lc_2   TYPE x VALUE '02',
               lc_4   TYPE x VALUE '04',
               lc_8   TYPE x VALUE '08',
               lc_16  TYPE x VALUE '10',
               lc_32  TYPE x VALUE '20',
               lc_64  TYPE x VALUE '40',
               lc_128 TYPE x VALUE '80'.

    DEFINE _eat_byte.
      lv_x = lv_delta(1).
      lv_delta = lv_delta+1.
    END-OF-DEFINITION.

    DATA: lv_delta  TYPE xstring,
          lv_base   TYPE xstring,
          lv_result TYPE xstring,
          lv_offset TYPE i,
          lv_sha1   TYPE zif_abapgit_definitions=>ty_sha1,
          ls_object LIKE LINE OF ct_objects,
          lv_len    TYPE i,
          lv_org    TYPE x,
          lv_x      TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    lv_delta = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object> WITH KEY sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Base not found, { is_object-sha1 }| ).
    ELSEIF <ls_object>-type = zif_abapgit_definitions=>gc_type-ref_d.
* sanity check
      zcx_abapgit_exception=>raise( 'Delta, base eq delta' ).
    ENDIF.

    lv_base = <ls_object>-data.

* skip the 2 headers
    delta_header( CHANGING cv_delta = lv_delta ).
    delta_header( CHANGING cv_delta = lv_delta ).

    WHILE xstrlen( lv_delta ) > 0.

      _eat_byte.
      lv_org = lv_x.

      IF lv_x BIT-AND lc_128 = lc_128. " MSB = 1

        lv_offset = 0.
        IF lv_org BIT-AND lc_1 = lc_1.
          _eat_byte.
          lv_offset = lv_x.
        ENDIF.
        IF lv_org BIT-AND lc_2 = lc_2.
          _eat_byte.
          lv_offset = lv_offset + lv_x * 256.
        ENDIF.
        IF lv_org BIT-AND lc_4 = lc_4.
          _eat_byte.
          lv_offset = lv_offset + lv_x * 65536.
        ENDIF.
        IF lv_org BIT-AND lc_8 = lc_8.
          _eat_byte.
          lv_offset = lv_offset + lv_x * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_org BIT-AND lc_16 = lc_16.
          _eat_byte.
          lv_len = lv_x.
        ENDIF.
        IF lv_org BIT-AND lc_32 = lc_32.
          _eat_byte.
          lv_len = lv_len + lv_x * 256.
        ENDIF.
        IF lv_org BIT-AND lc_64 = lc_64.
          _eat_byte.
          lv_len = lv_len + lv_x * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len)
          INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_x.
        CONCATENATE lv_result lv_delta(lv_len) INTO lv_result IN BYTE MODE.
        lv_delta = lv_delta+lv_len.
      ENDIF.

    ENDWHILE.

    lv_sha1 = zcl_abapgit_hash=>sha1( iv_type = <ls_object>-type iv_data = lv_result ).

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    APPEND ls_object TO ct_objects.

  ENDMETHOD.                    "delta


  METHOD delta_header.

    DATA: lv_bitbyte TYPE zif_abapgit_definitions=>ty_bitbyte,
          lv_bits    TYPE string,
          lv_x       TYPE x.


    lv_bits = ''.
    DO.
      lv_x = cv_delta(1).
      cv_delta = cv_delta+1.
      lv_bitbyte = zcl_abapgit_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    ev_header = zcl_abapgit_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.                    "delta_header


  METHOD encode.

    DATA: lv_sha1          TYPE x LENGTH 20,
          lv_adler32       TYPE zif_abapgit_definitions=>ty_adler32,
          lv_compressed    TYPE xstring,
          lv_xstring       TYPE xstring,
          lo_progress      TYPE REF TO zcl_abapgit_progress,
          lv_objects_total TYPE i.

    FIELD-SYMBOLS: <ls_object>  LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_xstring = zcl_abapgit_convert=>int_to_xstring4( lines( it_objects ) ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    lv_objects_total = lines( it_objects ).

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lv_objects_total.

    LOOP AT it_objects ASSIGNING <ls_object>.
      IF sy-tabix MOD 200 = 0.
        lo_progress->show(
          iv_current = sy-tabix
          iv_text    = |Encoding objects ( { sy-tabix } of { lv_objects_total } )| ).
      ENDIF.

      lv_xstring = type_and_length(
        iv_type   = <ls_object>-type
        iv_length = xstrlen( <ls_object>-data ) ).
      CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in   = <ls_object>-data
        IMPORTING
          gzip_out = lv_compressed ).

      CONCATENATE rv_data c_zlib lv_compressed INTO rv_data IN BYTE MODE.

      IF NOT <ls_object>-adler32 IS INITIAL.
        lv_adler32 = <ls_object>-adler32.
      ELSE.
        lv_adler32 = zcl_abapgit_hash=>adler32( <ls_object>-data ).
      ENDIF.
      CONCATENATE rv_data lv_adler32 INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = to_upper( zcl_abapgit_hash=>sha1_raw( rv_data ) ).
    CONCATENATE rv_data lv_sha1 INTO rv_data IN BYTE MODE.

  ENDMETHOD.                    "encode


  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space. "#EC NOTEXT
    CONCATENATE lv_string lv_tmp zif_abapgit_definitions=>gc_newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      lv_parent_lower = is_commit-parent.
      TRANSLATE lv_parent_lower TO LOWER CASE.

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.                     "#EC NOTEXT
      CONCATENATE lv_string lv_tmp zif_abapgit_definitions=>gc_newline INTO lv_string.
    ENDIF.

    IF NOT is_commit-parent2 IS INITIAL.
      lv_parent_lower = is_commit-parent2.
      TRANSLATE lv_parent_lower TO LOWER CASE.

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.                     "#EC NOTEXT
      CONCATENATE lv_string lv_tmp zif_abapgit_definitions=>gc_newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp zif_abapgit_definitions=>gc_newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp zif_abapgit_definitions=>gc_newline INTO lv_string.

    CONCATENATE lv_string zif_abapgit_definitions=>gc_newline is_commit-body INTO lv_string.

    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.                    "encode_commit


  METHOD encode_tree.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lt_nodes   LIKE it_nodes,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lt_nodes = sort_tree( it_nodes ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      ASSERT NOT <ls_node>-chmod IS INITIAL.
      ASSERT NOT <ls_node>-name IS INITIAL.
      ASSERT NOT <ls_node>-sha1 IS INITIAL.

      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_node>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.                    "encode_tree


  METHOD get_length.

    DATA: lv_x           TYPE x,
          lv_length_bits TYPE string,
          lv_bitbyte     TYPE zif_abapgit_definitions=>ty_bitbyte.


    lv_x = cv_data(1).
    lv_bitbyte = zcl_abapgit_convert=>x_to_bitbyte( lv_x ).

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      lv_bitbyte = zcl_abapgit_convert=>x_to_bitbyte( lv_x ).
      cv_data = cv_data+1.
      CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
    ENDWHILE.

    ev_length = zcl_abapgit_convert=>bitbyte_to_int( lv_length_bits ).

  ENDMETHOD.                    "get_length


  METHOD get_type.

    DATA: lv_char3   TYPE c LENGTH 3,
          lv_bitbyte TYPE zif_abapgit_definitions=>ty_bitbyte.


    lv_bitbyte = zcl_abapgit_convert=>x_to_bitbyte( iv_x ).
    lv_char3 = lv_bitbyte+1.

    CASE lv_char3.
      WHEN '001'.
        rv_type = zif_abapgit_definitions=>gc_type-commit.
      WHEN '010'.
        rv_type = zif_abapgit_definitions=>gc_type-tree.
      WHEN '011'.
        rv_type = zif_abapgit_definitions=>gc_type-blob.
      WHEN '100'.
        rv_type = zif_abapgit_definitions=>gc_type-tag.
      WHEN '111'.
        rv_type = zif_abapgit_definitions=>gc_type-ref_d.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'Todo, unknown type' ).
    ENDCASE.

  ENDMETHOD.                    "get_type


  METHOD sort_tree.

    TYPES: BEGIN OF ty_sort,
             sort TYPE string,
             node TYPE ty_node,
           END OF ty_sort.

    DATA: lt_sort TYPE STANDARD TABLE OF ty_sort WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_sort> LIKE LINE OF lt_sort,
                   <ls_node> LIKE LINE OF it_nodes.


    LOOP AT it_nodes ASSIGNING <ls_node>.
      APPEND INITIAL LINE TO lt_sort ASSIGNING <ls_sort>.
      IF <ls_node>-chmod = zif_abapgit_definitions=>gc_chmod-dir.
        CONCATENATE <ls_node>-name '/' INTO <ls_sort>-sort.
      ELSE.
        <ls_sort>-sort = <ls_node>-name.
      ENDIF.
      <ls_sort>-node = <ls_node>.
    ENDLOOP.

* following has to be done, or unpack will fail on server side
    SORT lt_sort BY sort ASCENDING.

    LOOP AT lt_sort ASSIGNING <ls_sort>.
      APPEND <ls_sort>-node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.


  METHOD type_and_length.

* see http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/#pack_file_objects

    DATA: lv_type   TYPE i,
          lv_length TYPE i,
          lv_hex    TYPE x LENGTH 1.


    CASE iv_type.
      WHEN zif_abapgit_definitions=>gc_type-commit.
        lv_type = 16.
      WHEN zif_abapgit_definitions=>gc_type-tree.
        lv_type = 32.
      WHEN zif_abapgit_definitions=>gc_type-blob.
        lv_type = 48.
      WHEN zif_abapgit_definitions=>gc_type-ref_d.
        lv_type = 112.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'Unexpected object type while encoding pack' ).
    ENDCASE.

    lv_length = iv_length.

    IF lv_length <= 15.
      lv_hex = 0 + lv_type + lv_length MOD 16.
      rv_xstring = lv_hex.
      lv_length = lv_length DIV 16.

    ELSEIF lv_length <= 2047.
      lv_hex = 128 + lv_type + lv_length MOD 16.
      rv_xstring = lv_hex.
      lv_length = lv_length DIV 16.

      lv_hex = lv_length.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
    ELSEIF lv_length <= 262143.
      lv_hex = 128 + lv_type + lv_length MOD 16.
      rv_xstring = lv_hex.
      lv_length = lv_length DIV 16.

      lv_hex = 128 + lv_length MOD 128.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
      lv_length = lv_length DIV 128.

      lv_hex = lv_length.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
    ELSEIF lv_length <= 33554431.
      lv_hex = 128 + lv_type + lv_length MOD 16.
      rv_xstring = lv_hex.
      lv_length = lv_length DIV 16.

      lv_hex = 128 + lv_length MOD 128.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
      lv_length = lv_length DIV 128.

      lv_hex = 128 + lv_length MOD 128.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
      lv_length = lv_length DIV 128.

      lv_hex = lv_length.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
    ELSE.
* this IF can be refactored, use shifting?
      zcx_abapgit_exception=>raise( 'Todo, encoding length' ).
    ENDIF.

  ENDMETHOD.                    "type_and_length


  METHOD zlib_decompress.

    DATA: ls_data           TYPE zcl_abapgit_zlib=>ty_decompress,
          lv_compressed_len TYPE i,
          lv_adler32        TYPE zif_abapgit_definitions=>ty_adler32.


    ls_data = zcl_abapgit_zlib=>decompress( cv_data ).
    lv_compressed_len = ls_data-compressed_len.
    cv_decompressed = ls_data-raw.

    IF lv_compressed_len IS INITIAL.
      zcx_abapgit_exception=>raise( 'Decompression falied :o/' ).
    ENDIF.

    cv_data = cv_data+lv_compressed_len.

    lv_adler32 = zcl_abapgit_hash=>adler32( cv_decompressed ).
    IF cv_data(4) <> lv_adler32.
      cv_data = cv_data+1.
    ENDIF.
    IF cv_data(4) <> lv_adler32.
      cv_data = cv_data+1.
    ENDIF.
    IF cv_data(4) <> lv_adler32.
      zcx_abapgit_exception=>raise( 'Wrong Adler checksum' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
