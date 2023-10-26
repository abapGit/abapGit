CLASS zcl_abapgit_po_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_i18n_file.

    METHODS constructor
      IMPORTING
        iv_lang TYPE laiso.

    METHODS parse
      IMPORTING
        iv_xdata TYPE xstring
      RAISING
        zcx_abapgit_exception.

    METHODS push_text_pairs
      IMPORTING
        iv_objtype    TYPE trobjtype
        iv_objname    TYPE lxeobjname
        it_text_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_comment,
        translator TYPE i VALUE 1,
        extracted  TYPE i VALUE 2,
        reference  TYPE i VALUE 3,
        flag       TYPE i VALUE 4,
        previous   TYPE i VALUE 5,
      END OF c_comment.
    TYPES:
      BEGIN OF ty_comment,
        kind TYPE i,
        text TYPE string,
      END OF ty_comment.
    TYPES:
      BEGIN OF ty_msg_pair,
        source   TYPE string,
        target   TYPE string,
        comments TYPE STANDARD TABLE OF ty_comment WITH KEY kind text,
      END OF ty_msg_pair.


    DATA mv_lang TYPE laiso.
    DATA mt_pairs TYPE SORTED TABLE OF ty_msg_pair WITH UNIQUE KEY source.

    METHODS build_po_body
      RETURNING
        VALUE(ro_buf) TYPE REF TO zcl_abapgit_string_buffer.
    METHODS build_po_head
      RETURNING
        VALUE(ro_buf) TYPE REF TO zcl_abapgit_string_buffer.
    METHODS parse_po
      IMPORTING
        iv_data TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_comment_marker
      IMPORTING
        iv_comment_kind  TYPE i
      RETURNING
        VALUE(rv_marker) TYPE string.

    CLASS-METHODS quote
      IMPORTING
        iv_text        TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
    CLASS-METHODS unquote
      IMPORTING
        iv_text        TYPE string
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_PO_FILE IMPLEMENTATION.


  METHOD build_po_body.

    FIELD-SYMBOLS <ls_pair> LIKE LINE OF mt_pairs.
    FIELD-SYMBOLS <ls_comment> LIKE LINE OF <ls_pair>-comments.

    CREATE OBJECT ro_buf.

    LOOP AT mt_pairs ASSIGNING <ls_pair>.
      IF sy-tabix <> 1.
        ro_buf->add( '' ).
      ENDIF.

      " TODO integrate translator comments ?

      SORT <ls_pair>-comments BY kind.
      LOOP AT <ls_pair>-comments ASSIGNING <ls_comment>.
        ro_buf->add( |#{ get_comment_marker( <ls_comment>-kind ) } { <ls_comment>-text }| ).
      ENDLOOP.

      ro_buf->add( |msgid { quote( <ls_pair>-source ) }| ).
      ro_buf->add( |msgstr { quote( <ls_pair>-target ) }| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD build_po_head.

    CREATE OBJECT ro_buf.

    " TODO, more headers ? sample: https://www.gnu.org/software/trans-coord/manual/gnun/html_node/PO-Header.html
    " TODO, does \n really necessary ? check editors support for non-\n
    " TODO, should be unfuzzy for final version, and potentially should have more fields

    ro_buf->add( '#, fuzzy' ).
    ro_buf->add( 'msgid ""' ).
    ro_buf->add( 'msgstr ""' ).
    ro_buf->add( '"MIME-Version: 1.0\n"' ).
    ro_buf->add( '"Content-Type: text/plain; charset=UTF-8\n"' ).
    ro_buf->add( '"Content-Transfer-Encoding: 8bit\n"' ).
    ro_buf->add( '' ).

  ENDMETHOD.


  METHOD constructor.
    mv_lang = to_lower( iv_lang ).
  ENDMETHOD.


  METHOD get_comment_marker.
    CASE iv_comment_kind.
      WHEN c_comment-translator.
        rv_marker = ''.
      WHEN c_comment-extracted.
        rv_marker = '.'.
      WHEN c_comment-reference.
        rv_marker = ':'.
      WHEN c_comment-flag.
        rv_marker = ','.
      WHEN c_comment-previous.
        rv_marker = '|'.
    ENDCASE.
  ENDMETHOD.


  METHOD parse.

    DATA lv_xdata TYPE xstring.
    DATA lv_data TYPE string.

    IF xstrlen( iv_xdata ) > 3 AND iv_xdata(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
      lv_xdata = iv_xdata+3.
    ELSE.
      lv_xdata = iv_xdata.
    ENDIF.

    lv_data = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xdata ).

    parse_po( lv_data ).

  ENDMETHOD.


  METHOD parse_po.

    CONSTANTS:
      BEGIN OF c_state,
        wait_id  TYPE i VALUE 0,
        wait_str TYPE i VALUE 1,
        wait_eos TYPE i VALUE 2,
        " TODO msgctx
      END OF c_state.

    DATA lv_state TYPE i VALUE c_state-wait_id.
    DATA lt_lines TYPE string_table.
    DATA ls_pair LIKE LINE OF mt_pairs.
    DATA lv_whitespace TYPE c LENGTH 2.
    FIELD-SYMBOLS <lv_i> TYPE string.

    lv_whitespace = ` ` && cl_abap_char_utilities=>horizontal_tab.

    SPLIT iv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
    APPEND '' TO lt_lines. " terminator

    LOOP AT lt_lines ASSIGNING <lv_i>.
      IF lv_state = c_state-wait_eos.
        IF strlen( <lv_i> ) >= 1 AND <lv_i>+0(1) = '"'.
          ls_pair-target = ls_pair-target && unquote( <lv_i> ).
          CONTINUE.
        ELSE.
          lv_state = c_state-wait_id.
          IF ls_pair-source IS NOT INITIAL. " skip header entry for now
            INSERT ls_pair INTO TABLE mt_pairs. " Sorted, duplicates will not be inserted
          ENDIF.
          CLEAR ls_pair.
        ENDIF.
      ENDIF.

      CASE lv_state.
        WHEN c_state-wait_id.
          IF <lv_i> IS INITIAL
            OR <lv_i>+0(1) = '#' " TODO, potentially parse comments in future, to re-integrate
            OR <lv_i> CO lv_whitespace.
            CONTINUE.
          ENDIF.
          IF strlen( <lv_i> ) >= 6 AND <lv_i>+0(6) = `msgid `. " w/trailing space
            ls_pair-source = unquote( substring(
              val = <lv_i>
              off = 6 ) ).
            lv_state = c_state-wait_str.
          ELSE.
            zcx_abapgit_exception=>raise( 'PO file format error: expected msgid' ).
          ENDIF.

        WHEN c_state-wait_str.
          IF strlen( <lv_i> ) >= 7 AND <lv_i>+0(7) = `msgstr `. " w/trailing space
            ls_pair-target = unquote( substring(
              val = <lv_i>
              off = 7 ) ).
            lv_state = c_state-wait_eos.
          ELSE.
            zcx_abapgit_exception=>raise( 'PO file format error: expected msgstr' ).
          ENDIF.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD push_text_pairs.

    DATA ls_out LIKE LINE OF mt_pairs.
    FIELD-SYMBOLS <ls_in> LIKE LINE OF it_text_pairs.
    FIELD-SYMBOLS <ls_out> LIKE LINE OF mt_pairs.
    DATA ls_comment LIKE LINE OF <ls_out>-comments.

    LOOP AT it_text_pairs ASSIGNING <ls_in>.
      CHECK <ls_in>-s_text IS NOT INITIAL.

      READ TABLE mt_pairs ASSIGNING <ls_out> WITH KEY source = <ls_in>-s_text.
      IF sy-subrc <> 0.
        ls_out-source = <ls_in>-s_text.
        INSERT ls_out INTO TABLE mt_pairs ASSIGNING <ls_out>.
        ASSERT sy-subrc = 0.
      ENDIF.

      IF <ls_out>-target IS INITIAL. " For a case of orig text duplication
        <ls_out>-target = <ls_in>-t_text.
      ENDIF.

      ls_comment-kind = c_comment-reference.
      ls_comment-text = condense( |{ iv_objtype }/{ iv_objname }/{ <ls_in>-textkey }| )
        && |, maxlen={ <ls_in>-unitmlt }|.
      APPEND ls_comment TO <ls_out>-comments.
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.


  METHOD quote.
    rv_text = '"' && replace(
      val  = iv_text
      sub  = '"'
      with = '\"'
      occ  = 0 ) && '"'.
  ENDMETHOD.


  METHOD unquote.

    DATA lv_len TYPE i.
    DATA lv_prev_char TYPE i.

    rv_text = iv_text.
    SHIFT rv_text RIGHT DELETING TRAILING space. " Measure perf ? Could be slowish, maybe use find
    SHIFT rv_text LEFT DELETING LEADING space.
    lv_len = strlen( rv_text ).

    IF lv_len < 2.
      zcx_abapgit_exception=>raise( 'PO file format error: bad quoting' ).
    ENDIF.

    lv_prev_char = lv_len - 1.
    IF rv_text+0(1) <> '"' OR rv_text+lv_prev_char(1) <> '"'.
      zcx_abapgit_exception=>raise( 'PO file format error: bad quoting' ).
    ENDIF.

    lv_prev_char = lv_prev_char - 1.
    IF lv_len >= 3 AND rv_text+lv_prev_char(1) = '\'. " escaped quote
      zcx_abapgit_exception=>raise( 'PO file format error: bad quoting' ).
    ENDIF.

    rv_text = substring(
      val = rv_text
      off = 1
      len = lv_len - 2 ).

    rv_text = replace(
      val  = rv_text
      sub  = '\"'
      with = '"'
      occ  = 0 ).

    rv_text = replace(
      val  = rv_text
      sub  = '\n'
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).

    " TODO: theoretically there can be unescaped " - is it a problem ? check standard

  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~ext.
    rv_ext = 'po'.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~lang.
    rv_lang = mv_lang.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~render.

    DATA lv_str TYPE string.

    lv_str = build_po_body( )->join_w_newline_and_flush( ).

    IF lv_str IS NOT INITIAL.
      lv_str = build_po_head( )->join_w_newline_and_flush( )
        && cl_abap_char_utilities=>newline
        && lv_str
        && cl_abap_char_utilities=>newline. " Trailing LF
      rv_data = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_str ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~translate.

    FIELD-SYMBOLS <ls_lxe> LIKE LINE OF ct_text_pairs.
    FIELD-SYMBOLS <ls_tr> LIKE LINE OF mt_pairs.
    DATA lv_idx TYPE i.

    LOOP AT ct_text_pairs ASSIGNING <ls_lxe>.
      CHECK <ls_lxe>-s_text IS NOT INITIAL.
      lv_idx = sy-tabix.

      READ TABLE mt_pairs ASSIGNING <ls_tr> WITH KEY source = <ls_lxe>-s_text.
      IF sy-subrc = 0 AND <ls_tr>-target IS NOT INITIAL.
        <ls_lxe>-t_text = <ls_tr>-target.
      ELSE.
        DELETE ct_text_pairs INDEX lv_idx. " Otherwise error in LXE FMs for empty translation
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
