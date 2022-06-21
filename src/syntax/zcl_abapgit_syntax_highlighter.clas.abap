CLASS zcl_abapgit_syntax_highlighter DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS process_line
      IMPORTING
        !iv_line       TYPE string
      RETURNING
        VALUE(rv_line) TYPE string .
    METHODS set_hidden_chars
      IMPORTING
        !iv_hidden_chars TYPE abap_bool .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_match,
        token    TYPE c LENGTH 1,  " Type of matches
        offset   TYPE i,      " Beginning position of the string that should be formatted
        length   TYPE i,      " Length of the string that should be formatted
        text_tag TYPE string, " Type of text tag
      END OF ty_match .
    TYPES:
      ty_match_tt  TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_rule,
        regex             TYPE REF TO cl_abap_regex,
        token             TYPE c LENGTH 1,
        style             TYPE string,
        relevant_submatch TYPE i,
      END OF ty_rule .

    CONSTANTS c_token_none TYPE c VALUE '.' ##NO_TEXT.
    DATA:
      mt_rules TYPE STANDARD TABLE OF ty_rule .
    DATA mv_hidden_chars TYPE abap_bool .

    METHODS add_rule
      IMPORTING
        !iv_regex    TYPE string
        !iv_token    TYPE c
        !iv_style    TYPE string
        !iv_submatch TYPE i OPTIONAL .
    METHODS parse_line
      IMPORTING
        !iv_line          TYPE string
      RETURNING
        VALUE(rt_matches) TYPE ty_match_tt .
    METHODS order_matches
      IMPORTING
        !iv_line    TYPE string
      CHANGING
        !ct_matches TYPE ty_match_tt .
    METHODS extend_matches
      IMPORTING
        !iv_line    TYPE string
      CHANGING
        !ct_matches TYPE ty_match_tt .
    METHODS format_line
      IMPORTING
        !iv_line       TYPE string
        !it_matches    TYPE ty_match_tt
      RETURNING
        VALUE(rv_line) TYPE string .
    METHODS apply_style
      IMPORTING
        !iv_line       TYPE string
        !iv_class      TYPE string
      RETURNING
        VALUE(rv_line) TYPE string .
    METHODS is_whitespace
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    METHODS show_hidden_chars
      IMPORTING
        !iv_line       TYPE string
      RETURNING
        VALUE(rv_line) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_syntax_highlighter IMPLEMENTATION.


  METHOD add_rule.

    DATA ls_rule LIKE LINE OF mt_rules.

    IF NOT iv_regex IS INITIAL.
      CREATE OBJECT ls_rule-regex
        EXPORTING
          pattern     = iv_regex
          ignore_case = abap_true.
    ENDIF.

    ls_rule-token         = iv_token.
    ls_rule-style         = iv_style.
    ls_rule-relevant_submatch = iv_submatch.
    APPEND ls_rule TO mt_rules.

  ENDMETHOD.


  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val    = iv_line
                         format = cl_abap_format=>e_html_text ).

    lv_escaped = show_hidden_chars( lv_escaped ).

    IF iv_class IS NOT INITIAL.
      rv_line = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      rv_line = lv_escaped.
    ENDIF.

  ENDMETHOD.


  METHOD extend_matches.

    DATA: lv_line_len TYPE i,
          lv_last_pos TYPE i VALUE 0,
          lv_length   TYPE i,
          ls_match    TYPE ty_match.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    lv_line_len = strlen( iv_line ).

    SORT ct_matches BY offset.

    " Add entries refering to parts of text that should not be formatted
    LOOP AT ct_matches ASSIGNING <ls_match>.
      IF <ls_match>-offset > lv_last_pos.
        lv_length = <ls_match>-offset - lv_last_pos.
        ls_match-token  = c_token_none.
        ls_match-offset = lv_last_pos.
        ls_match-length = lv_length.
        INSERT ls_match INTO ct_matches INDEX sy-tabix.
      ENDIF.
      lv_last_pos = <ls_match>-offset + <ls_match>-length.
    ENDLOOP.

    " Add remainder of the string
    IF lv_line_len > lv_last_pos.
      lv_length = lv_line_len - lv_last_pos.
      ls_match-token  = c_token_none.
      ls_match-offset = lv_last_pos.
      ls_match-length = lv_length.
      APPEND ls_match TO ct_matches.
    ENDIF.

  ENDMETHOD.


  METHOD format_line.

    DATA:
      lv_chunk TYPE string,
      ls_rule  LIKE LINE OF mt_rules.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    LOOP AT it_matches ASSIGNING <ls_match>.
      lv_chunk = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CLEAR ls_rule. " Failed read equals no style
      READ TABLE mt_rules INTO ls_rule WITH KEY token = <ls_match>-token.

      lv_chunk = apply_style( iv_line  = lv_chunk
                              iv_class = ls_rule-style ).

      rv_line = rv_line && lv_chunk.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_whitespace.

    DATA: lv_whitespace TYPE string.

    "/^\s+$/
    lv_whitespace = ` ` && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.

    rv_result = boolc( iv_string CO lv_whitespace ).

  ENDMETHOD.


  METHOD order_matches.
  ENDMETHOD.


  METHOD parse_line.

    DATA:
      lo_regex   TYPE REF TO cl_abap_regex,
      lo_matcher TYPE REF TO cl_abap_matcher,
      lt_result  TYPE match_result_tab,
      ls_match   TYPE ty_match.

    FIELD-SYMBOLS:
      <ls_regex>    LIKE LINE OF mt_rules,
      <ls_result>   TYPE match_result,
      <ls_submatch> LIKE LINE OF <ls_result>-submatches.


    " Process syntax-dependent regex table and find all matches
    LOOP AT mt_rules ASSIGNING <ls_regex> WHERE regex IS BOUND.
      lo_regex   = <ls_regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT lt_result ASSIGNING <ls_result>.
        CLEAR: ls_match.
        IF <ls_regex>-relevant_submatch = 0.
          ls_match-token  = <ls_regex>-token.
          ls_match-offset = <ls_result>-offset.
          ls_match-length = <ls_result>-length.
          APPEND ls_match TO rt_matches.
        ELSE.
          READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX <ls_regex>-relevant_submatch.
          "submatch might be empty if only discarted parts matched
          IF sy-subrc = 0 AND <ls_submatch>-offset >= 0 AND <ls_submatch>-length > 0.
            ls_match-token  = <ls_regex>-token.
            ls_match-offset = <ls_submatch>-offset.
            ls_match-length = <ls_submatch>-length.
            APPEND ls_match TO rt_matches.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF iv_line IS INITIAL OR is_whitespace( iv_line ) = abap_true.
      rv_line = show_hidden_chars( iv_line ).
      RETURN.
    ENDIF.

    lt_matches = parse_line( iv_line ).

    order_matches( EXPORTING iv_line    = iv_line
                   CHANGING  ct_matches = lt_matches ).

    extend_matches( EXPORTING iv_line    = iv_line
                    CHANGING  ct_matches = lt_matches ).

    rv_line = format_line( iv_line    = iv_line
                           it_matches = lt_matches ).

  ENDMETHOD.


  METHOD set_hidden_chars.
    mv_hidden_chars = iv_hidden_chars.
  ENDMETHOD.


  METHOD show_hidden_chars.

    DATA lv_bom TYPE x LENGTH 3.

    rv_line = iv_line.

    IF mv_hidden_chars = abap_true.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_line WITH '&nbsp;&rarr;&nbsp;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN rv_line WITH '&para;'.
      REPLACE ALL OCCURRENCES OF ` `                                    IN rv_line WITH '&middot;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN rv_line
        WITH '<span class="red">&odash;</span>'.

      IF strlen( rv_line ) BETWEEN 1 AND 2.
        TRY.
            lv_bom = zcl_abapgit_convert=>string_to_xstring( rv_line ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
        IF lv_bom(2) = cl_abap_char_utilities=>byte_order_mark_big.
          rv_line = '<span class="red">&squf;</span>'. " UTF-16 big-endian (FE FF)
        ENDIF.
        IF lv_bom(2) = cl_abap_char_utilities=>byte_order_mark_little.
          rv_line = '<span class="red">&compfn;</span>'. " UTF-16 little-endian (FF FE)
        ENDIF.
        IF lv_bom(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
          rv_line = '<span class="red">&curren;</span>'. " UTF-8 (EF BB BF)
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
