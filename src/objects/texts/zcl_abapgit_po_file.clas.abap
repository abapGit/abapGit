CLASS zcl_abapgit_po_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_i18n_file.

    METHODS constructor
      IMPORTING
        iv_lang TYPE laiso.

    METHODS push_text_pairs
      IMPORTING
        it_text_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_lang TYPE laiso.
    DATA mt_text_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs.

ENDCLASS.



CLASS ZCL_ABAPGIT_PO_FILE IMPLEMENTATION.


  METHOD constructor.
    mv_lang = iv_lang.
  ENDMETHOD.


  METHOD push_text_pairs.
    APPEND LINES OF it_text_pairs TO mt_text_pairs.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~ext.
    rv_ext = 'po'.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~lang.
    rv_lang = mv_lang.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~render.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.
    DATA lv_str TYPE string.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF mt_text_pairs.

    CREATE OBJECT lo_buf.

    " TODO header, check if not empty, then return empty string

    LOOP AT mt_text_pairs ASSIGNING <ls_p>.
      IF sy-tabix <> 1.
        lo_buf->add( `` ).
      ENDIF.
      " TODO integrate translator comments
      lo_buf->add( `#. max_len=` && |{ <ls_p>-unitmlt }| ).
      lo_buf->add( `#: ` && <ls_p>-textkey ).
      lo_buf->add( `msgid "` && <ls_p>-s_text && `"` ). " TODO escape, validate non empty
      lo_buf->add( `msgstr "` && <ls_p>-t_text && `"` ). " TODO escape
    ENDLOOP.

    lv_str = lo_buf->join_w_newline_and_flush( ).

    IF lv_str IS NOT INITIAL.
      lv_str = lv_str && cl_abap_char_utilities=>newline. " Trailing LF
      rv_data = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_str ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
