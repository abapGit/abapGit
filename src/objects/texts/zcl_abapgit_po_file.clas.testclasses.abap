CLASS ltcl_po_file DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS po_body FOR TESTING RAISING zcx_abapgit_exception.
    METHODS parse_happy_path FOR TESTING RAISING zcx_abapgit_exception.
    METHODS parse_negative FOR TESTING RAISING zcx_abapgit_exception.
    METHODS unquote FOR TESTING RAISING zcx_abapgit_exception.
    METHODS multiline_parsing FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_po_file DEFINITION LOCAL FRIENDS ltcl_po_file.

CLASS ltcl_po_file IMPLEMENTATION.

  METHOD po_body.

    DATA lo_po TYPE REF TO zcl_abapgit_po_file.
    DATA lt_lxe_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs.
    DATA lv_act TYPE string.
    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    FIELD-SYMBOLS <ls_p> LIKE LINE OF lt_lxe_pairs.

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.

    APPEND INITIAL LINE TO lt_lxe_pairs ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K1'.
    <ls_p>-unitmlt = 10.
    <ls_p>-s_text  = 'Hello'.
    <ls_p>-t_text  = 'Hello DE'.
    APPEND INITIAL LINE TO lt_lxe_pairs ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K2    X'. " To condense
    <ls_p>-unitmlt = 11.
    <ls_p>-s_text  = 'Hello'. " Intentional duplicate !
    <ls_p>-t_text  = 'Hello DE 2'.
    APPEND INITIAL LINE TO lt_lxe_pairs ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K3'.
    <ls_p>-unitmlt = 12.
    <ls_p>-s_text  = 'World'.
    <ls_p>-t_text  = 'World "DE"'.

    lo_po->push_text_pairs(
      iv_objtype = 'T1'
      iv_objname = 'OBJ1'
      it_text_pairs = lt_lxe_pairs ).

    lv_act = lo_po->build_po_body( )->join_w_newline_and_flush( ).

    CREATE OBJECT lo_buf.

    lo_buf->add( '#: T1/OBJ1/K1, maxlen=10'
      )->add( '#: T1/OBJ1/K2 X, maxlen=11'
      )->add( 'msgid "Hello"'
      )->add( 'msgstr "Hello DE"'
      )->add( ''
      )->add( '#: T1/OBJ1/K3, maxlen=12'
      )->add( 'msgid "World"'
      )->add( 'msgstr "World \"DE\""' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.

  METHOD parse_happy_path.

    DATA lo_po TYPE REF TO zcl_abapgit_po_file.
    DATA lt_lxe_pairs_act TYPE zif_abapgit_lxe_texts=>ty_text_pairs.
    DATA lt_lxe_pairs_exp TYPE zif_abapgit_lxe_texts=>ty_text_pairs.
    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    FIELD-SYMBOLS <ls_p> LIKE LINE OF lt_lxe_pairs_act.

    APPEND INITIAL LINE TO lt_lxe_pairs_exp ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K1'.
    <ls_p>-unitmlt = 10.
    <ls_p>-s_text  = 'Hello'. " Intentional duplicate ! same translation is applied
    <ls_p>-t_text  = 'Hello DE 2'.
    APPEND INITIAL LINE TO lt_lxe_pairs_exp ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K2    X'.
    <ls_p>-unitmlt = 11.
    <ls_p>-s_text  = 'Hello'. " Intentional duplicate !
    <ls_p>-t_text  = 'Hello DE 2'.
    APPEND INITIAL LINE TO lt_lxe_pairs_exp ASSIGNING <ls_p>.
    <ls_p>-textkey = 'K3'.
    <ls_p>-unitmlt = 12.
    <ls_p>-s_text  = 'World'.
    <ls_p>-t_text  = 'World DE'.

    lt_lxe_pairs_act = lt_lxe_pairs_exp.
    LOOP AT lt_lxe_pairs_act ASSIGNING <ls_p>.
      CLEAR <ls_p>-t_text.
    ENDLOOP.

    CREATE OBJECT lo_buf.

    lo_buf->add( 'msgid ""'
      )->add( 'msgstr ""'
      )->add( '"some header stuff"'
      )->add( ''
      )->add( '#: T1/OBJ1/K1, maxlen=10'
      )->add( '#: T1/OBJ1/K2 X, maxlen=11'
      )->add( 'msgid "Hello"'
      )->add( 'msgstr "Hello DE 2"'
      )->add( ''
      )->add( '#: T1/OBJ1/K3, maxlen=12'
      )->add( 'msgid "World"'
      )->add( 'msgstr "World DE"' ).

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.
    lo_po->parse_po( lo_buf->join_w_newline_and_flush( ) ).
    lo_po->zif_abapgit_i18n_file~translate( CHANGING ct_text_pairs = lt_lxe_pairs_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_lxe_pairs_act
      exp = lt_lxe_pairs_exp ).

  ENDMETHOD.

  METHOD parse_negative.

    DATA lo_po TYPE REF TO zcl_abapgit_po_file.

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.

    TRY.
        lo_po->parse_po( zcl_abapgit_string_buffer=>new(
          )->add( 'wrong'
          )->add( 'format'
          )->join_w_newline_and_flush( ) ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_po->parse_po( zcl_abapgit_string_buffer=>new(
          )->add( 'msgid "a"'
          )->add( 'wrong tag'
          )->join_w_newline_and_flush( ) ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_po->parse_po( zcl_abapgit_string_buffer=>new(
          )->add( '""'
          )->add( ''
          )->join_w_newline_and_flush( ) ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_po->parse_po( zcl_abapgit_string_buffer=>new(
          )->add( 'msgid "a"'
          )->add( '# comment'
          )->add( 'msgstr "b"'
          )->join_w_newline_and_flush( ) ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_po->parse_po( zcl_abapgit_string_buffer=>new(
          )->add( 'msgid "a"'
          )->add( 'msgstr "'
          )->join_w_newline_and_flush( ) ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_po->parse_po( zcl_abapgit_string_buffer=>new(
          )->add( 'msgid"a"'
          )->add( 'msgstr"b"'
          )->join_w_newline_and_flush( ) ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD unquote.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_po_file=>unquote( ` "abc \"123" ` )
      exp = 'abc "123' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_po_file=>unquote( ` "" ` )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_po_file=>unquote( ` "\n" ` )
      exp = |{ cl_abap_char_utilities=>newline }| ).

    TRY.
        zcl_abapgit_po_file=>unquote( `abc \"123"` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_po_file=>unquote( `"abc \"123` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_po_file=>unquote( `"abc \"` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_po_file=>unquote( `"` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD multiline_parsing.

    DATA lo_po TYPE REF TO zcl_abapgit_po_file.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF lo_po->mt_pairs.

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.
    lo_po->parse_po( zcl_abapgit_string_buffer=>new(
      )->add( 'msgid "a"'
      )->add( 'msgstr "1\n"'
      )->add( '"2"'
      )->join_w_newline_and_flush( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_po->mt_pairs )
      exp = 1 ).
    READ TABLE lo_po->mt_pairs INDEX 1 ASSIGNING <ls_p>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_p>-target
      exp = |1{ cl_abap_char_utilities=>newline }2| ).

    CREATE OBJECT lo_po EXPORTING iv_lang = 'xx'.
    lo_po->parse_po( zcl_abapgit_string_buffer=>new(
      )->add( 'msgid "a"'
      )->add( 'msgstr ""'
      )->add( '"2\n"'
      )->add( '"3"'
      )->join_w_newline_and_flush( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_po->mt_pairs )
      exp = 1 ).
    READ TABLE lo_po->mt_pairs INDEX 1 ASSIGNING <ls_p>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_p>-target
      exp = |2{ cl_abap_char_utilities=>newline }3| ).

  ENDMETHOD.

ENDCLASS.
