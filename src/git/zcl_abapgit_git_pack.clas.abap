CLASS zcl_abapgit_git_pack DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_node,
        chmod TYPE zif_abapgit_git_definitions=>ty_chmod,
        name  TYPE string,
        sha1  TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_node .
    TYPES:
      ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_commit,
        tree      TYPE zif_abapgit_git_definitions=>ty_sha1,
        parent    TYPE zif_abapgit_git_definitions=>ty_sha1,
        parent2   TYPE zif_abapgit_git_definitions=>ty_sha1,
        author    TYPE string,
        committer TYPE string,
        gpgsig    TYPE string,
        body      TYPE string,
      END OF ty_commit .
    TYPES:
      BEGIN OF ty_tag,
        object       TYPE string,
        type         TYPE string,
        tag          TYPE string,
        tagger_name  TYPE string,
        tagger_email TYPE string,
        message      TYPE string,
        body         TYPE string,
      END OF ty_tag .

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
    CLASS-METHODS decode_tag
      IMPORTING
        !iv_data      TYPE xstring
      RETURNING
        VALUE(rs_tag) TYPE ty_tag
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
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS encode_commit
      IMPORTING
        !is_commit     TYPE ty_commit
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS encode_tag
      IMPORTING
        !is_tag        TYPE ty_tag
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_int_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY .

    CONSTANTS:
      c_pack_start TYPE x LENGTH 4 VALUE '5041434B' ##NO_TEXT.
    CONSTANTS:
      c_zlib       TYPE x LENGTH 2 VALUE '789C' ##NO_TEXT.
    CONSTANTS:
      c_zlib_hmm   TYPE x LENGTH 2 VALUE '7801' ##NO_TEXT.
    CONSTANTS:                                                    " PACK
      c_version    TYPE x LENGTH 4 VALUE '00000002' ##NO_TEXT.
    CONSTANTS:
      c_tail_zeros_long TYPE x LENGTH 16 VALUE '00000000000000000000000000000000' ##NO_TEXT.
    CONSTANTS:
      c_tail_ones_long  TYPE x LENGTH 16 VALUE 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' ##NO_TEXT.

    CLASS-METHODS sort_tree
      IMPORTING
        !it_nodes       TYPE ty_nodes_tt
      RETURNING
        VALUE(rt_nodes) TYPE ty_nodes_tt .
    CLASS-METHODS get_type
      IMPORTING
        !iv_x          TYPE x
      RETURNING
        VALUE(rv_type) TYPE zif_abapgit_git_definitions=>ty_type
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_length
      EXPORTING
        !ev_length TYPE i
      CHANGING
        !cv_data   TYPE xstring .
    CLASS-METHODS type_and_length
      IMPORTING
        !iv_type          TYPE zif_abapgit_git_definitions=>ty_type
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
    CLASS-METHODS decompress_object
      IMPORTING
        !iv_expected     TYPE i
        !iv_last         TYPE abap_bool DEFAULT abap_false
      CHANGING
        !cv_data         TYPE xstring
        !cv_decompressed TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS decompress_by_header
      IMPORTING
        !iv_expected     TYPE i
        !iv_last         TYPE abap_bool
      EXPORTING
        !ev_ok           TYPE abap_bool
      CHANGING
        !cv_data         TYPE xstring
        !cv_decompressed TYPE xstring .
    CLASS-METHODS find_next_zlib_header
      IMPORTING
        !iv_data      TYPE xstring
        !iv_offset    TYPE i
      RETURNING
        VALUE(rv_pos) TYPE i .
    CLASS-METHODS collect_header_candidates
      IMPORTING
        !iv_data             TYPE xstring
        !iv_zpos             TYPE i
      RETURNING
        VALUE(rt_candidates) TYPE ty_int_tt .
    CLASS-METHODS try_stream_end
      IMPORTING
        !iv_data         TYPE xstring
        !iv_length       TYPE i
        !iv_expected     TYPE i
      EXPORTING
        !ev_decompressed TYPE xstring
        !ev_ended        TYPE abap_bool
        !ev_exact        TYPE abap_bool .
    CLASS-METHODS find_zlib_end
      IMPORTING
        !iv_data         TYPE xstring
        !iv_decompressed TYPE xstring
        !iv_guess        TYPE i OPTIONAL
      RETURNING
        VALUE(rv_end)    TYPE i .
    CLASS-METHODS inflates_to
      IMPORTING
        !iv_data         TYPE xstring
        !iv_length       TYPE i
        !iv_tail         TYPE xstring
        !iv_decompressed TYPE xstring
      RETURNING
        VALUE(rv_equal)  TYPE abap_bool .
    CLASS-METHODS stream_ended_by
      IMPORTING
        !iv_data         TYPE xstring
        !iv_length       TYPE i
        !iv_decompressed TYPE xstring
      RETURNING
        VALUE(rv_ended)  TYPE abap_bool .
    CLASS-METHODS next_object_fits
      IMPORTING
        !iv_data       TYPE xstring
        !iv_offset     TYPE i
      RETURNING
        VALUE(rv_fits) TYPE abap_bool .
ENDCLASS.



CLASS zcl_abapgit_git_pack IMPLEMENTATION.


  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_ref_delta      TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_decompressed   TYPE xstring,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects,
          lv_uindex         TYPE sy-index.


    lv_data = iv_data.

* header
    IF xstrlen( lv_data ) < 4.
      zcx_abapgit_exception=>raise( |Unexpected pack header, short reply| ).
    ENDIF.

    IF lv_data(4) <> c_pack_start.
      zcx_abapgit_exception=>raise( |Unexpected pack header, { lv_data(4) }| ).
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      zcx_abapgit_exception=>raise( |Version not supported, { lv_data(4) }| ).
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = zcl_abapgit_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_uindex = sy-index.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = zif_abapgit_git_definitions=>c_type-ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        zcx_abapgit_exception=>raise( |Unexpected zlib header| ).
      ENDIF.
      lv_data = lv_data+2.

*******************************

      decompress_object(
        EXPORTING
          iv_expected     = lv_expected
          iv_last         = boolc( lv_uindex = lv_objects )
        CHANGING
          cv_data         = lv_data
          cv_decompressed = lv_decompressed ).

      CLEAR ls_object.
      " Skip the Adler-32 checksum. It is not always checked, so it is not kept either:
      " encode computes it from the data.
      lv_data = lv_data+4.

      IF lv_type = zif_abapgit_git_definitions=>c_type-ref_d.
        ls_object-sha1 = lv_ref_delta.
        ls_object-sha1 = to_lower( ls_object-sha1 ).
      ELSE.
        ls_object-sha1 = zcl_abapgit_hash=>sha1(
          iv_type = lv_type
          iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      ls_object-index = lv_uindex.
      APPEND ls_object TO rt_objects.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = zcl_abapgit_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      zcx_abapgit_exception=>raise( |SHA1 at end of pack doesn't match| ).
    ENDIF.

    zcl_abapgit_git_delta=>decode_deltas( CHANGING ct_objects = rt_objects ).

  ENDMETHOD.


  METHOD decode_commit.

    DATA: lv_string        TYPE string,
          lv_word          TYPE string,
          lv_offset        TYPE i,
          lv_length        TYPE i,
          lv_length_gpgsig TYPE i,
          lv_trash         TYPE string ##NEEDED,
          lt_string        TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8_raw( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

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
          ASSERT 1 = 0.
      ENDCASE.

    ENDLOOP.

    lv_length = strlen( lv_string ).
    IF lv_length >= 6 AND lv_string+0(6) = 'gpgsig'.
      FIND REGEX |-----END PGP SIGNATURE-----[[:space:]]+|
        IN lv_string
        MATCH OFFSET lv_offset
        MATCH LENGTH lv_length ##REGEX_POSIX.
      lv_length = lv_length - 1.
      lv_length_gpgsig = lv_offset + lv_length - 7.
      lv_length = lv_offset + lv_length.
      rs_commit-gpgsig = lv_string+7(lv_length_gpgsig).
      lv_string = lv_string+lv_length.
    ENDIF.

    rs_commit-body = lv_string+1.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      zcx_abapgit_exception=>raise( |multiple parents? not supported| ).
    ENDIF.

  ENDMETHOD.


  METHOD decode_tag.

    DATA: lv_string TYPE string,
          lv_word   TYPE string,
          lv_trash  TYPE string ##NEEDED,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8_raw( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.

      SPLIT <lv_string> AT space INTO lv_word lv_trash.

      CASE lv_word.
        WHEN 'object'.
          rs_tag-object = lv_trash.
        WHEN 'type'.
          rs_tag-type = lv_trash.
        WHEN 'tag'.
          rs_tag-tag = lv_trash.
        WHEN 'tagger'.

          FIND FIRST OCCURRENCE OF REGEX `(.*)<(.*)>`
                     IN lv_trash
                     SUBMATCHES rs_tag-tagger_name
                                rs_tag-tagger_email ##REGEX_POSIX.

          rs_tag-tagger_name = condense( rs_tag-tagger_name ).

        WHEN ''.
          " ignore blank lines
          CONTINUE.
        WHEN OTHERS.

          " these are the non empty line which don't start with a key word
          " the first one is the message, the rest are cumulated to the body

          IF rs_tag-message IS INITIAL.
            rs_tag-message = <lv_string>.
          ELSE.

            IF rs_tag-body IS NOT INITIAL.
              rs_tag-body = rs_tag-body && cl_abap_char_utilities=>newline.
            ENDIF.

            rs_tag-body = rs_tag-body && <lv_string>.

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE zif_abapgit_git_definitions=>ty_chmod,
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

      lv_string = zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_xstring ).
      SPLIT lv_string AT space INTO lv_chmod lv_name.

      ls_node-chmod = lv_chmod.
      IF ls_node-chmod <> zif_abapgit_git_definitions=>c_chmod-dir
          AND ls_node-chmod <> zif_abapgit_git_definitions=>c_chmod-file
          AND ls_node-chmod <> zif_abapgit_git_definitions=>c_chmod-symbolic_link
          AND ls_node-chmod <> zif_abapgit_git_definitions=>c_chmod-executable
          AND ls_node-chmod <> zif_abapgit_git_definitions=>c_chmod-submodule.
        zcx_abapgit_exception=>raise( |Unknown chmod { ls_node-chmod }| ).
      ENDIF.

      lv_offset = lv_match + 1.
      ls_node-name = lv_name.
      ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
      ls_node-sha1 = to_lower( ls_node-sha1 ).
      APPEND ls_node TO rt_nodes.

      lv_cursor = lv_match + 1 + lc_sha_length.
    ENDDO.

  ENDMETHOD.

  METHOD decompress_by_header.

* Fast path: locate the next object's zlib header (789C/7801) or the 20-byte pack trailer,
* derive candidate stream ends behind the 4-byte Adler-32 checksum, and inflate + verify the
* candidate directly. This avoids both re-compressing every object and binary-searching
* cl_abap_gzip=>decompress_binary when the server's zlib differs from the kernel's.

    DATA: lv_len      TYPE i,
          lv_min_end  TYPE i,
          lv_search   TYPE i,
          lv_upper    TYPE i,
          lv_zpos     TYPE i,
          lv_cand     TYPE i,
          lv_ended    TYPE abap_bool,
          lv_exact    TYPE abap_bool,
          lt_cands    TYPE ty_int_tt,
          lv_raw      TYPE xstring.

    ev_ok = abap_false.
    lv_len = xstrlen( cv_data ).

    IF iv_last = abap_true.
      " Last object: <deflate stream><4-byte Adler-32><20-byte pack SHA-1>
      lv_cand = lv_len - 24.
      IF lv_cand >= 2.
        try_stream_end(
          EXPORTING
            iv_data         = cv_data
            iv_length       = lv_cand
            iv_expected     = iv_expected
          IMPORTING
            ev_decompressed = lv_raw
            ev_ended        = lv_ended
            ev_exact        = lv_exact ).
        IF lv_exact = abap_true.
          cv_decompressed = lv_raw.
          cv_data = cv_data+lv_cand.
          ev_ok = abap_true.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    " RFC 1951 maximum DEFLATE compression ratio is 1032:1; use 1033 conservatively.
    " Earliest possible next zlib header is lv_min_end + 4 (Adler-32) + 1 (1-byte header).
    lv_min_end = iv_expected DIV 1033.
    IF lv_min_end < 2.
      lv_min_end = 2.
    ENDIF.
    lv_search = lv_min_end + 5.
    lv_upper = -1.

    WHILE lv_search < lv_len - 20.
      " A REF_DELTA can place its zlib header up to 4 + 10 + 20 = 34 bytes past the stream end
      IF lv_upper > 0 AND lv_search > lv_upper + 34.
        RETURN.
      ENDIF.

      lv_zpos = find_next_zlib_header(
        iv_data   = cv_data
        iv_offset = lv_search ).
      IF lv_zpos < 0.
        RETURN.
      ENDIF.

      lt_cands = collect_header_candidates(
        iv_data = cv_data
        iv_zpos = lv_zpos ).

      LOOP AT lt_cands INTO lv_cand.
        IF lv_cand < lv_min_end OR ( lv_upper > 0 AND lv_cand > lv_upper ).
          CONTINUE.
        ENDIF.

        try_stream_end(
          EXPORTING
            iv_data         = cv_data
            iv_length       = lv_cand
            iv_expected     = iv_expected
          IMPORTING
            ev_decompressed = lv_raw
            ev_ended        = lv_ended
            ev_exact        = lv_exact ).
        IF lv_exact = abap_true.
          cv_decompressed = lv_raw.
          cv_data = cv_data+lv_cand.
          ev_ok = abap_true.
          RETURN.
        ENDIF.

        IF lv_ended = abap_true AND ( lv_upper < 0 OR lv_cand - 1 < lv_upper ).
          lv_upper = lv_cand - 1.
        ENDIF.
      ENDLOOP.

      lv_search = lv_zpos + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD find_next_zlib_header.

    DATA: lv_off1 TYPE i,
          lv_off2 TYPE i,
          lv_span TYPE i.

    rv_pos = -1.

    " Search for the common 789C header first; only look for 7801 before that hit
    " so an absent 7801 does not scan the entire remaining pack.
    FIND FIRST OCCURRENCE OF c_zlib IN SECTION OFFSET iv_offset OF iv_data
      IN BYTE MODE MATCH OFFSET lv_off1.
    IF sy-subrc = 0.
      lv_span = lv_off1 - iv_offset.
      IF lv_span >= 2.
        FIND FIRST OCCURRENCE OF c_zlib_hmm IN SECTION OFFSET iv_offset LENGTH lv_span OF iv_data
          IN BYTE MODE MATCH OFFSET lv_off2.
        IF sy-subrc = 0.
          rv_pos = lv_off2.
          RETURN.
        ENDIF.
      ENDIF.
      rv_pos = lv_off1.
      RETURN.
    ENDIF.

    FIND FIRST OCCURRENCE OF c_zlib_hmm IN SECTION OFFSET iv_offset OF iv_data
      IN BYTE MODE MATCH OFFSET lv_off2.
    IF sy-subrc = 0.
      rv_pos = lv_off2.
    ENDIF.

  ENDMETHOD.


  METHOD collect_header_candidates.

    CONSTANTS: lc_msb       TYPE x LENGTH 1 VALUE '80',
               lc_type_mask TYPE x LENGTH 1 VALUE '70',
               lc_zero      TYPE x LENGTH 1 VALUE '00'.

    DATA: lv_pos  TYPE i,
          lv_byte TYPE x LENGTH 1,
          lv_type TYPE x LENGTH 1,
          lv_cand TYPE i.

    " Non-REF_DELTA object: varint header ends at iv_zpos - 1 (type in {16, 32, 48, 64})
    lv_pos = iv_zpos - 1.
    IF lv_pos >= 6.
      lv_byte = iv_data+lv_pos(1).
      IF lv_byte BIT-AND lc_msb = lc_zero.
        lv_type = lv_byte BIT-AND lc_type_mask.
        IF lv_type = 16 OR lv_type = 32 OR lv_type = 48 OR lv_type = 64.
          lv_cand = lv_pos - 4.
          APPEND lv_cand TO rt_candidates.
        ENDIF.
        lv_pos = lv_pos - 1.
        WHILE lv_pos >= 6.
          lv_byte = iv_data+lv_pos(1).
          IF lv_byte BIT-AND lc_msb <> lc_msb.
            EXIT.
          ENDIF.
          lv_type = lv_byte BIT-AND lc_type_mask.
          IF lv_type = 16 OR lv_type = 32 OR lv_type = 48 OR lv_type = 64.
            lv_cand = lv_pos - 4.
            APPEND lv_cand TO rt_candidates.
          ENDIF.
          lv_pos = lv_pos - 1.
        ENDWHILE.
      ENDIF.
    ENDIF.

    " REF_DELTA object: 20-byte base SHA-1 precedes iv_zpos; varint ends at iv_zpos - 21 (type 112)
    lv_pos = iv_zpos - 21.
    IF lv_pos >= 6.
      lv_byte = iv_data+lv_pos(1).
      IF lv_byte BIT-AND lc_msb = lc_zero.
        lv_type = lv_byte BIT-AND lc_type_mask.
        IF lv_type = 112.
          lv_cand = lv_pos - 4.
          APPEND lv_cand TO rt_candidates.
        ENDIF.
        lv_pos = lv_pos - 1.
        WHILE lv_pos >= 6.
          lv_byte = iv_data+lv_pos(1).
          IF lv_byte BIT-AND lc_msb <> lc_msb.
            EXIT.
          ENDIF.
          lv_type = lv_byte BIT-AND lc_type_mask.
          IF lv_type = 112.
            lv_cand = lv_pos - 4.
            APPEND lv_cand TO rt_candidates.
          ENDIF.
          lv_pos = lv_pos - 1.
        ENDWHILE.
      ENDIF.
    ENDIF.

    " Test smallest candidate end first so multi-byte headers are hit before longer offsets
    SORT rt_candidates ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_candidates.

  ENDMETHOD.


  METHOD try_stream_end.

    DATA: lv_prefix  TYPE xstring,
          lv_dec_len TYPE i,
          lv_tail    TYPE xstring,
          lv_prev    TYPE i.

    CLEAR ev_decompressed.
    ev_ended = abap_false.
    ev_exact = abap_false.

    " First inflate with the 16-byte zero tail; produces ev_decompressed and checks expected length
    CONCATENATE iv_data(iv_length) c_tail_zeros_long INTO lv_prefix IN BYTE MODE.
    TRY.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_prefix
          IMPORTING
            raw_out     = ev_decompressed
            raw_out_len = lv_dec_len ).
      CATCH cx_sy_compression_error cx_sy_buffer_overflow.
        RETURN.
    ENDTRY.
    IF lv_dec_len <> iv_expected.
      RETURN.
    ENDIF.

    " Confirm with the 16-byte one tail
    lv_tail = c_tail_ones_long.
    IF inflates_to( iv_data         = iv_data
                    iv_length       = iv_length
                    iv_tail         = lv_tail
                    iv_decompressed = ev_decompressed ) = abap_false.
      RETURN.
    ENDIF.

    ev_ended = abap_true.

    " Verify the stream had not already ended one byte earlier (one-bits tail fails fastest)
    lv_prev = iv_length - 1.
    IF lv_prev >= 2 AND inflates_to( iv_data         = iv_data
                                     iv_length       = lv_prev
                                     iv_tail         = lv_tail
                                     iv_decompressed = ev_decompressed ) = abap_true.
      lv_tail = c_tail_zeros_long.
      IF inflates_to( iv_data         = iv_data
                      iv_length       = lv_prev
                      iv_tail         = lv_tail
                      iv_decompressed = ev_decompressed ) = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    ev_exact = abap_true.

  ENDMETHOD.


  METHOD decompress_object.

    DATA: lv_decompress_len TYPE i,
          lv_compressed     TYPE xstring,
          lv_compressed_len TYPE i,
          lv_end            TYPE i,
          lv_limit          TYPE i,
          lv_slice          TYPE xstring,
          lv_ok             TYPE abap_bool.

    decompress_by_header(
      EXPORTING
        iv_expected     = iv_expected
        iv_last         = iv_last
      IMPORTING
        ev_ok           = lv_ok
      CHANGING
        cv_data         = cv_data
        cv_decompressed = cv_decompressed ).
    IF lv_ok = abap_true.
      RETURN.
    ENDIF.

    " Fallback: bound the input slice so the kernel does not process the whole remaining pack.
    " Data the kernel cannot decompress takes the ABAP inflate fallback below.
    lv_limit = iv_expected + iv_expected DIV 1000 + 65536.
    IF xstrlen( cv_data ) > lv_limit.
      lv_slice = cv_data(lv_limit).
    ELSE.
      lv_slice = cv_data.
    ENDIF.

    TRY.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_slice
          IMPORTING
            raw_out     = cv_decompressed
            raw_out_len = lv_decompress_len ).
      CATCH cx_sy_compression_error cx_sy_buffer_overflow.
        lv_decompress_len = -1.
    ENDTRY.

    IF lv_decompress_len <> iv_expected AND xstrlen( cv_data ) > lv_limit.
      TRY.
          cl_abap_gzip=>decompress_binary(
            EXPORTING
              gzip_in     = cv_data
            IMPORTING
              raw_out     = cv_decompressed
              raw_out_len = lv_decompress_len ).
        CATCH cx_sy_compression_error cx_sy_buffer_overflow.
          lv_decompress_len = -1.
      ENDTRY.
    ENDIF.

    IF lv_decompress_len = iv_expected.
      " The kernel does not tell where the stream ends. If the git server compressed the
      " same way as the kernel, compressing again gives the length.
      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in       = cv_decompressed
        IMPORTING
          gzip_out     = lv_compressed
          gzip_out_len = lv_compressed_len ).

      IF xstrlen( cv_data ) > lv_compressed_len AND lv_compressed(lv_compressed_len) = cv_data(lv_compressed_len).
        cv_data = cv_data+lv_compressed_len.
        RETURN.
      ENDIF.

      " Otherwise search for the end of the stream, starting at that length
      lv_end = find_zlib_end(
        iv_data         = cv_data
        iv_decompressed = cv_decompressed
        iv_guess        = lv_compressed_len ).
      " Behind the stream and its Adler-32 bytes, the next object or the pack trailer must follow
      IF lv_end > 0 AND next_object_fits( iv_data   = cv_data
                                          iv_offset = lv_end + 4 ) = abap_true.
        cv_data = cv_data+lv_end.
        RETURN.
      ENDIF.
    ENDIF.

    " Last resort, slow: DEFLATE implemented in ABAP
    " This also fixes issues with TFS 2017 and visualstudio.com Git repos
    zlib_decompress( CHANGING cv_data         = cv_data
                              cv_decompressed = cv_decompressed ).

    IF xstrlen( cv_decompressed ) <> iv_expected.
      zcx_abapgit_exception=>raise( |Decompression failed| ).
    ENDIF.

  ENDMETHOD.


  METHOD encode.

    DATA: lv_sha1          TYPE x LENGTH 20,
          lv_adler32       TYPE zif_abapgit_git_definitions=>ty_adler32,
          lv_compressed    TYPE xstring,
          lv_xstring       TYPE xstring,
          li_progress      TYPE REF TO zif_abapgit_progress,
          lv_objects_total TYPE i.

    FIELD-SYMBOLS: <ls_object>  LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_xstring = zcl_abapgit_convert=>int_to_xstring4( lines( it_objects ) ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    lv_objects_total = lines( it_objects ).

    li_progress = zcl_abapgit_progress=>get_instance( lv_objects_total ).

    LOOP AT it_objects ASSIGNING <ls_object>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Encoding objects ( { sy-tabix } of { lv_objects_total } )| ).

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

  ENDMETHOD.


  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    lv_tree_lower = to_lower( lv_tree_lower ).

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      lv_parent_lower = is_commit-parent.
      lv_parent_lower = to_lower( lv_parent_lower ).

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    IF NOT is_commit-parent2 IS INITIAL.
      lv_parent_lower = is_commit-parent2.
      lv_parent_lower = to_lower( lv_parent_lower ).

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-gpgsig IS INITIAL.
      CONCATENATE 'gpgsig' is_commit-gpgsig
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp INTO lv_string.
    ENDIF.

    CONCATENATE lv_string cl_abap_char_utilities=>newline is_commit-body INTO lv_string.

    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.


  METHOD encode_tag.

    DATA: lv_string TYPE string,
          lv_time   TYPE zcl_abapgit_git_time=>ty_unixtime.

    lv_time = zcl_abapgit_git_time=>get_unix( ).

    lv_string = |object { is_tag-object }{ cl_abap_char_utilities=>newline }|
             && |type { is_tag-type }{ cl_abap_char_utilities=>newline }|
             && |tag { zcl_abapgit_git_tag=>remove_tag_prefix( is_tag-tag ) }{ cl_abap_char_utilities=>newline }|
             && |tagger { is_tag-tagger_name } <{ is_tag-tagger_email }> { lv_time }|
             && |{ cl_abap_char_utilities=>newline }|
             && |{ cl_abap_char_utilities=>newline }|
             && |{ is_tag-message }|.

    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD find_zlib_end.

* stream_ended_by is true from the end of the DEFLATE stream on, and false below it for
* any stream a compressor writes (see there). So the end is the smallest length for which
* it is true. A crafted stream could still make it true too early; then the next object
* or the SHA-1 at the end of the pack does not fit (see next_object_fits), and
* decompress_object falls back to the ABAP inflate. Starting at
* iv_guess, the search steps down or up with growing steps until the end lies between a
* length that is false and one that is true, then halves that range.
*
* This needs no Adler-32 checksum, which is slow in ABAP. The checksum bytes behind the
* stream are not checked here; the SHA-1 at the end of the pack covers them.

    DATA: lv_max  TYPE i,
          lv_low  TYPE i,
          lv_high TYPE i,
          lv_mid  TYPE i,
          lv_step TYPE i.

    rv_end = -1.
    lv_max = xstrlen( iv_data ).

    " The shortest DEFLATE stream has two bytes, so one byte never ends it
    lv_low = 1.
    lv_high = iv_guess.
    IF lv_high < 2.
      lv_high = 2.
    ENDIF.
    IF lv_high > lv_max.
      lv_high = lv_max.
    ENDIF.
    IF lv_high <= lv_low.
      RETURN.
    ENDIF.

    lv_step = 4.
    IF stream_ended_by( iv_data         = iv_data
                        iv_length       = lv_high
                        iv_decompressed = iv_decompressed ) = abap_true.
      WHILE lv_high - lv_step > lv_low.
        lv_mid = lv_high - lv_step.
        IF stream_ended_by( iv_data         = iv_data
                            iv_length       = lv_mid
                            iv_decompressed = iv_decompressed ) = abap_false.
          lv_low = lv_mid.
          EXIT.
        ENDIF.
        lv_high = lv_mid.
        IF lv_step < lv_max.
          lv_step = lv_step * 4.
        ENDIF.
      ENDWHILE.
    ELSE.
      lv_low = lv_high.
      DO.
        IF lv_low >= lv_max.
          " The data ends before the stream does
          RETURN.
        ENDIF.
        lv_high = lv_low + lv_step.
        IF lv_high > lv_max.
          lv_high = lv_max.
        ENDIF.
        IF stream_ended_by( iv_data         = iv_data
                            iv_length       = lv_high
                            iv_decompressed = iv_decompressed ) = abap_true.
          EXIT.
        ENDIF.
        lv_low = lv_high.
        IF lv_step < lv_max.
          lv_step = lv_step * 4.
        ENDIF.
      ENDDO.
    ENDIF.

    WHILE lv_high - lv_low > 1.
      lv_mid = lv_low + ( lv_high - lv_low ) DIV 2.
      IF stream_ended_by( iv_data         = iv_data
                          iv_length       = lv_mid
                          iv_decompressed = iv_decompressed ) = abap_true.
        lv_high = lv_mid.
      ELSE.
        lv_low = lv_mid.
      ENDIF.
    ENDWHILE.

    rv_end = lv_high.

  ENDMETHOD.


  METHOD get_length.

* https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt

* n-byte sizeN (as long as MSB is set, each 7-bit)
*    size0..sizeN form 4+7+7+..+7 bit integer, size0
*    is the least significant part, and sizeN is the
*    most significant part.

    CONSTANTS: lc_msb  TYPE x LENGTH 1 VALUE '80',  " 10000000 - MSB continuation flag
               lc_low4 TYPE x LENGTH 1 VALUE '0F',  " 00001111 - lower 4 bits mask
               lc_low7 TYPE x LENGTH 1 VALUE '7F',  " 01111111 - lower 7 bits mask
               lc_zero TYPE x LENGTH 1 VALUE '00'.

    DATA: lv_byte     TYPE x LENGTH 1,
          lv_bits     TYPE x LENGTH 1,
          lv_bits_int TYPE i,
          lv_factor   TYPE i.

    lv_byte = cv_data(1).
    cv_data = cv_data+1.

* First byte: lower 4 bits contain the initial length value
    lv_bits = lv_byte BIT-AND lc_low4.
    ev_length = lv_bits.
    lv_factor = 16. " 2^4, first continuation byte shifts by 4

* While MSB is set, continue reading bytes
    WHILE lv_byte BIT-AND lc_msb <> lc_zero.
      IF sy-index > 1.
        lv_factor = lv_factor * 128. " 2^7, each subsequent byte shifts by 7 more bits
      ENDIF.
      IF xstrlen( cv_data ) = 0.
        EXIT.
      ENDIF.
      lv_byte = cv_data(1).
      cv_data = cv_data+1.
* Add the lower 7 bits multiplied by the current factor
      lv_bits = lv_byte BIT-AND lc_low7.
      lv_bits_int = lv_bits.
      ev_length = ev_length + lv_bits_int * lv_factor.
    ENDWHILE.

  ENDMETHOD.


  METHOD get_type.

    CONSTANTS: lc_mask TYPE x VALUE 112.
    DATA: lv_xtype TYPE x.

    lv_xtype = iv_x BIT-AND lc_mask.

    CASE lv_xtype.
      WHEN 16.
        rv_type = zif_abapgit_git_definitions=>c_type-commit.
      WHEN 32.
        rv_type = zif_abapgit_git_definitions=>c_type-tree.
      WHEN 48.
        rv_type = zif_abapgit_git_definitions=>c_type-blob.
      WHEN 64.
        rv_type = zif_abapgit_git_definitions=>c_type-tag.
      WHEN 112.
        rv_type = zif_abapgit_git_definitions=>c_type-ref_d.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Todo, unknown git pack type| ).
    ENDCASE.

  ENDMETHOD.


  METHOD inflates_to.

    DATA: lv_prefix TYPE xstring,
          lv_raw    TYPE xstring.

    CONCATENATE iv_data(iv_length) iv_tail INTO lv_prefix IN BYTE MODE.
    TRY.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in = lv_prefix
          IMPORTING
            raw_out = lv_raw ).
        rv_equal = boolc( lv_raw = iv_decompressed ).
      CATCH cx_sy_compression_error cx_sy_buffer_overflow.
        rv_equal = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD stream_ended_by.

* The kernel does not report a truncated stream: a stream cut after its last data byte,
* but before the end-of-block code, still decompresses to all of the data. So the first
* iv_length bytes are decompressed with tails appended: eight zero bytes, eight one bytes
* and sixteen zero bytes. If the stream ended within iv_length bytes, every tail is
* ignored. Otherwise the next bit differs between the tails, so usually one of them
* continues the stream and yields more data or an error (another block, invalid block
* type or code, stored block length).
*
* Two tails are not always enough: if the rest of the data is a run that both eight-byte
* tails happen to encode, with literals for zero bits and copies for one bits, both give
* all of the data. A tail of twice the length encodes twice the run and gives too much.
* The same holds with the roles swapped (if the all-zero code ends the block, the zero
* tails always agree), so there is a long tail of one bits as well.

    CONSTANTS: lc_zeros TYPE x LENGTH 8 VALUE '0000000000000000',
               lc_ones  TYPE x LENGTH 8 VALUE 'FFFFFFFFFFFFFFFF'.

    DATA lv_tail TYPE xstring.

    rv_ended = abap_false.

    lv_tail = lc_zeros.
    IF inflates_to( iv_data         = iv_data
                    iv_length       = iv_length
                    iv_tail         = lv_tail
                    iv_decompressed = iv_decompressed ) = abap_false.
      RETURN.
    ENDIF.

    lv_tail = lc_ones.
    IF inflates_to( iv_data         = iv_data
                    iv_length       = iv_length
                    iv_tail         = lv_tail
                    iv_decompressed = iv_decompressed ) = abap_false.
      RETURN.
    ENDIF.

    lv_tail = c_tail_zeros_long.
    IF inflates_to( iv_data         = iv_data
                    iv_length       = iv_length
                    iv_tail         = lv_tail
                    iv_decompressed = iv_decompressed ) = abap_false.
      RETURN.
    ENDIF.

    lv_tail = c_tail_ones_long.
    rv_ended = inflates_to( iv_data         = iv_data
                            iv_length       = iv_length
                            iv_tail         = lv_tail
                            iv_decompressed = iv_decompressed ).

  ENDMETHOD.


  METHOD next_object_fits.

* In a pack, a zlib stream and its four Adler-32 bytes are followed by the next object or
* by the 20-byte SHA-1 that ends the pack. An object starts with its type and size (the
* size continues while the high bit is set), a ref delta then has the 20-byte base SHA-1,
* and then comes the zlib header. A stream end found off by some bytes lands in other
* data, where this almost never matches. Offsets only, as the pack can be large.

    CONSTANTS lc_msb TYPE x LENGTH 1 VALUE '80'.

    DATA: lv_len  TYPE i,
          lv_pos  TYPE i,
          lv_byte TYPE x LENGTH 1,
          lv_type TYPE zif_abapgit_git_definitions=>ty_type,
          lv_zlib TYPE x LENGTH 2.

    rv_fits = abap_false.
    lv_len = xstrlen( iv_data ).

    IF lv_len - iv_offset = 20.
      rv_fits = abap_true.
      RETURN.
    ENDIF.

    lv_pos = iv_offset.
    IF lv_pos < 0 OR lv_pos >= lv_len.
      RETURN.
    ENDIF.

    lv_byte = iv_data+lv_pos(1).
    TRY.
        lv_type = get_type( lv_byte ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    WHILE lv_byte BIT-AND lc_msb = lc_msb.
      lv_pos = lv_pos + 1.
      IF lv_pos >= lv_len.
        RETURN.
      ENDIF.
      lv_byte = iv_data+lv_pos(1).
    ENDWHILE.
    lv_pos = lv_pos + 1.

    IF lv_type = zif_abapgit_git_definitions=>c_type-ref_d.
      lv_pos = lv_pos + 20.
    ENDIF.

    IF lv_pos + 2 > lv_len.
      RETURN.
    ENDIF.

    lv_zlib = iv_data+lv_pos(2).
    rv_fits = boolc( lv_zlib = c_zlib OR lv_zlib = c_zlib_hmm ).

  ENDMETHOD.


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
      IF <ls_node>-chmod = zif_abapgit_git_definitions=>c_chmod-dir.
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
      WHEN zif_abapgit_git_definitions=>c_type-commit.
        lv_type = 16.
      WHEN zif_abapgit_git_definitions=>c_type-tree.
        lv_type = 32.
      WHEN zif_abapgit_git_definitions=>c_type-blob.
        lv_type = 48.
      WHEN zif_abapgit_git_definitions=>c_type-tag.
        lv_type = 64.
      WHEN zif_abapgit_git_definitions=>c_type-ref_d.
        lv_type = 112.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Unexpected object type while encoding pack| ).
    ENDCASE.

    lv_length = iv_length.

* first byte
    IF lv_length > 15.
      lv_hex = 128.
    ENDIF.
    lv_hex = lv_hex + lv_type + lv_length MOD 16.
    rv_xstring = lv_hex.
    lv_length = lv_length DIV 16.

* subsequent bytes
    WHILE lv_length >= 128.
      lv_hex = 128 + lv_length MOD 128.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
      lv_length = lv_length DIV 128.
    ENDWHILE.

* last byte
    IF lv_length > 0.
      lv_hex = lv_length.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
    ENDIF.

  ENDMETHOD.


  METHOD zlib_decompress.

    DATA: ls_data           TYPE zcl_abapgit_zlib=>ty_decompress,
          lv_compressed_len TYPE i,
          lv_adler32        TYPE zif_abapgit_git_definitions=>ty_adler32.


    ls_data = zcl_abapgit_zlib=>decompress( cv_data ).
    lv_compressed_len = ls_data-compressed_len.
    cv_decompressed = ls_data-raw.

    IF lv_compressed_len IS INITIAL.
      zcx_abapgit_exception=>raise( |Decompression failed :o/| ).
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
      zcx_abapgit_exception=>raise( |Wrong Adler checksum| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
