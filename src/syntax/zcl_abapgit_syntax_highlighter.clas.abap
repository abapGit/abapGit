CLASS zcl_abapgit_syntax_highlighter DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_filename       TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_syntax_highlighter .
    METHODS process_line
      IMPORTING
        !iv_line       TYPE string
      RETURNING
        VALUE(rv_line) TYPE string .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_match,
        token    TYPE char1,  " Type of matches
        offset   TYPE i,      " Beginning position of the string that should be formatted
        length   TYPE i,      " Length of the string that should be formatted
        text_tag TYPE string, " Type of text tag
      END OF ty_match.

    TYPES:
      ty_match_tt  TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_rule,
        regex TYPE REF TO cl_abap_regex,
        token TYPE char1,
        style TYPE string,
      END OF ty_rule.

    CONSTANTS c_token_none TYPE c VALUE '.'.

    DATA mt_rules TYPE STANDARD TABLE OF ty_rule.

    METHODS add_rule
      IMPORTING
        iv_regex TYPE string
        iv_token TYPE c
        iv_style TYPE string.

    METHODS parse_line
      IMPORTING iv_line    TYPE string
      EXPORTING et_matches TYPE ty_match_tt.

    METHODS order_matches ABSTRACT
      IMPORTING iv_line    TYPE string
      CHANGING  ct_matches TYPE ty_match_tt.

    METHODS extend_matches
      IMPORTING iv_line    TYPE string
      CHANGING  ct_matches TYPE ty_match_tt.

    METHODS format_line
      IMPORTING iv_line        TYPE string
                it_matches     TYPE ty_match_tt
      RETURNING VALUE(rv_line) TYPE string.

    METHODS apply_style
      IMPORTING iv_line        TYPE string
                iv_class       TYPE string
      RETURNING VALUE(rv_line) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_SYNTAX_HIGHLIGHTER IMPLEMENTATION.


  METHOD add_rule.

    DATA ls_rule LIKE LINE OF mt_rules.

    CREATE OBJECT ls_rule-regex
      EXPORTING
        pattern     = iv_regex
        ignore_case = abap_true.

    ls_rule-token = iv_token.
    ls_rule-style = iv_style.
    APPEND ls_rule TO mt_rules.

  ENDMETHOD.


  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val = iv_line  format = cl_abap_format=>e_html_attr ).
    IF iv_class IS NOT INITIAL.
      rv_line = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      rv_line = lv_escaped.
    ENDIF.

  ENDMETHOD.                    " apply_style


  METHOD create.

    " Create instance of highighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      CREATE OBJECT ro_instance TYPE zcl_abapgit_syntax_abap.
    ELSEIF iv_filename CP '*.xml'.
      CREATE OBJECT ro_instance TYPE zcl_abapgit_syntax_xml.
    ELSE.
      CLEAR ro_instance.
    ENDIF.

  ENDMETHOD.                    " create.


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

  ENDMETHOD.                    " extend_matches


  METHOD format_line.

    DATA:
      lv_chunk TYPE string,
      ls_rule  LIKE LINE OF mt_rules.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    LOOP AT it_matches ASSIGNING <ls_match>.
      lv_chunk = substring( val = iv_line off = <ls_match>-offset len = <ls_match>-length ).

      CLEAR ls_rule. " Failed read equals no style
      READ TABLE mt_rules INTO ls_rule WITH KEY token = <ls_match>-token.

      lv_chunk = me->apply_style( iv_line  = lv_chunk
                                  iv_class = ls_rule-style ).

      rv_line = rv_line && lv_chunk.
    ENDLOOP.

  ENDMETHOD.                    " format_line


  METHOD parse_line.

    DATA:
      lo_regex   TYPE REF TO cl_abap_regex,
      lo_matcher TYPE REF TO cl_abap_matcher,
      lt_result  TYPE match_result_tab,
      ls_match   TYPE ty_match.

    FIELD-SYMBOLS:
      <ls_regex>  LIKE LINE OF mt_rules,
      <ls_result> TYPE match_result.


    CLEAR et_matches.

    " Process syntax-dependent regex table and find all matches
    LOOP AT mt_rules ASSIGNING <ls_regex>.
      lo_regex   = <ls_regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT lt_result ASSIGNING <ls_result>.
        CLEAR: ls_match.
        ls_match-token  = <ls_regex>-token.
        ls_match-offset = <ls_result>-offset.
        ls_match-length = <ls_result>-length.
        APPEND ls_match TO et_matches.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    " parse_line


  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF strlen( iv_line ) = 0.
      RETURN.
    ENDIF.

    me->parse_line( EXPORTING iv_line    = iv_line
                    IMPORTING et_matches = lt_matches ).

    me->order_matches( EXPORTING iv_line    = iv_line
                       CHANGING  ct_matches = lt_matches ).

    me->extend_matches( EXPORTING iv_line    = iv_line
                        CHANGING  ct_matches = lt_matches ).

    rv_line = me->format_line( iv_line    = iv_line
                               it_matches = lt_matches ).

  ENDMETHOD.                    " process_line
ENDCLASS.
