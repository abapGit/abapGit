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
*          WITH NON-UNIQUE SORTED KEY by_kind COMPONENTS kind,
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

    CLASS-METHODS escape_text
      IMPORTING
        iv_text        TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.

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

      ro_buf->add( |msgid "{ escape_text( <ls_pair>-source ) }"| ).
      ro_buf->add( |msgstr "{ escape_text( <ls_pair>-target ) }"| ).
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


  METHOD escape_text.
    rv_text = iv_text. " TODO
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

    DATA lv_data TYPE string.

    lv_data = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xdata ).

    parse_po( lv_data ).

  ENDMETHOD.


  METHOD parse_po.

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
        INSERT ls_out INTO  TABLE mt_pairs ASSIGNING <ls_out>.
        ASSERT sy-subrc = 0.
      ENDIF.

      <ls_out>-target = <ls_in>-t_text.
      ls_comment-kind = c_comment-reference.
      ls_comment-text = condense( |{ iv_objtype }/{ iv_objname }/{ <ls_in>-textkey }| )
        && |, maxlen={ <ls_in>-unitmlt }|.
      APPEND ls_comment TO <ls_out>-comments.
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~ext.
    rv_ext = 'po'.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~lang.
    rv_lang = mv_lang.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~render.

    DATA lv_str TYPE string.

    " TODO header, check if not empty, then return empty string

    lv_str = build_po_body( )->join_w_newline_and_flush( ).

    IF lv_str IS NOT INITIAL.
      lv_str = build_po_head( )->join_w_newline_and_flush( )
        && cl_abap_char_utilities=>newline
        && lv_str
        && cl_abap_char_utilities=>newline. " Trailing LF
      rv_data = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_str ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
