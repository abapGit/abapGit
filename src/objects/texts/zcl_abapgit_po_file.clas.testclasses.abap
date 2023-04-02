CLASS ltcl_po_file DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS po_body FOR TESTING RAISING zcx_abapgit_exception.
    METHODS simple_parse FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_po_file DEFINITION LOCAL FRIENDS ltcl_po_file.

CLASS ltcl_po_file IMPLEMENTATION.

  METHOD po_body.

    DATA lo_po TYPE REF TO zcl_abapgit_po_file.
    DATA lt_lxe_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs.
    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    FIELD-SYMBOLS <ls_p> LIKE LINE OF lt_lxe_pairs.

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.

    APPEND INITIAL LINE TO lt_lxe_pairs ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K1'.
    <ls_p>-unitmlt = 10.
    <ls_p>-s_text  = 'Hello'.
    <ls_p>-t_text  = 'Hello DE'.
    APPEND INITIAL LINE TO lt_lxe_pairs ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K2    X'.
    <ls_p>-unitmlt = 11.
    <ls_p>-s_text  = 'Hello'. " Intentional duplicate !
    <ls_p>-t_text  = 'Hello DE 2'.
    APPEND INITIAL LINE TO lt_lxe_pairs ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K3'.
    <ls_p>-unitmlt = 12.
    <ls_p>-s_text  = 'World'.
    <ls_p>-t_text  = 'World DE'.

    lo_po->push_text_pairs(
      iv_objtype = 'T1'
      iv_objname = 'OBJ1'
      it_text_pairs = lt_lxe_pairs ).

    lv_act = lo_po->build_po_body( )->join_w_newline_and_flush( ).

    CREATE OBJECT lo_buf.

    lo_buf->add( '#: T1/OBJ1/K1, maxlen=10' ).
    lo_buf->add( '#: T1/OBJ1/K2 X, maxlen=11' ).
    lo_buf->add( 'msgid "Hello"' ).
    lo_buf->add( 'msgstr "Hello DE 2"' ).
    lo_buf->add( '' ).
    lo_buf->add( '#: T1/OBJ1/K3, maxlen=12' ).
    lo_buf->add( 'msgid "World"' ).
    lo_buf->add( 'msgstr "World DE"' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.

  METHOD simple_parse.

    DATA lo_po TYPE REF TO zcl_abapgit_po_file.
    DATA lt_lxe_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs.
    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    FIELD-SYMBOLS <ls_p> LIKE LINE OF lt_lxe_pairs.

    CREATE OBJECT lo_buf.

    lo_buf->add( 'msgid ""' ).
    lo_buf->add( 'msgstr ""' ).
    lo_buf->add( '"some header stuff"' ).
    lo_buf->add( '' ).
    lo_buf->add( '#: T1/OBJ1/K1, maxlen=10' ).
    lo_buf->add( '#: T1/OBJ1/K2 X, maxlen=11' ).
    lo_buf->add( 'msgid "Hello"' ).
    lo_buf->add( 'msgstr "Hello DE 2"' ).
    lo_buf->add( '' ).
    lo_buf->add( '#: T1/OBJ1/K3, maxlen=12' ).
    lo_buf->add( 'msgid "World"' ).
    lo_buf->add( 'msgstr "World DE"' ).

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.
    lo_po->parse_po( lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.

ENDCLASS.
