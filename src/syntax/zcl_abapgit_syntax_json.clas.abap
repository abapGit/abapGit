CLASS zcl_abapgit_syntax_json DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      " JSON... This was easy :-)
      " JSONC... With comments
      BEGIN OF c_css,
        keyword TYPE string VALUE 'selectors',              "#EC NOTEXT
        text    TYPE string VALUE 'text',                   "#EC NOTEXT
        values  TYPE string VALUE 'properties',             "#EC NOTEXT
        comment TYPE string VALUE 'comment',                "#EC NOTEXT
      END OF c_css.
    CONSTANTS:
      BEGIN OF c_token,
        keyword TYPE c VALUE 'K',                           "#EC NOTEXT
        text    TYPE c VALUE 'T',                           "#EC NOTEXT
        values  TYPE c VALUE 'V',                           "#EC NOTEXT
        comment TYPE c VALUE 'C',                           "#EC NOTEXT
      END OF c_token.
    CONSTANTS:
      BEGIN OF c_regex,
        " comments /* ... */ or //
        comment TYPE string VALUE '\/\*.*\*\/|\/\*|\*\/|\/\/', "#EC NOTEXT
        " not much here
        keyword TYPE string VALUE 'true|false|null',        "#EC NOTEXT
        " double quoted strings
        text    TYPE string VALUE '"',                      "#EC NOTEXT
      END OF c_regex.

    METHODS constructor.
  PROTECTED SECTION.

    METHODS order_matches REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_syntax_json IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression

    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    " Style for keys
    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    " Style for values
    add_rule( iv_regex = ''
              iv_token = c_token-values
              iv_style = c_css-values ).

    " JSONC comments
    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.


  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_count      TYPE i,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      IF <ls_match>-token = c_token-text.
        <ls_match>-text_tag = lv_match.
        IF lv_prev_token = c_token-text.
          IF <ls_match>-text_tag = <ls_prev>-text_tag.
            <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
            CLEAR lv_prev_token.
          ENDIF.
          CLEAR <ls_match>-token.
          CONTINUE.
        ENDIF.
      ENDIF.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

    " Switch style of second text match to values
    LOOP AT ct_matches ASSIGNING <ls_match> WHERE token = c_token-text.
      lv_count = lv_count + 1.
      IF lv_count >= 2.
        <ls_match>-token = c_token-values.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
