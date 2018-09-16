CLASS zcl_abapgit_syntax_xml DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_css,
        xml_tag  TYPE string VALUE 'xml_tag',               "#EC NOTEXT
        attr     TYPE string VALUE 'attr',                  "#EC NOTEXT
        attr_val TYPE string VALUE 'attr_val',              "#EC NOTEXT
      END OF c_css .
    CONSTANTS:
      BEGIN OF c_token,
        xml_tag  TYPE c VALUE 'X',                          "#EC NOTEXT
        attr     TYPE c VALUE 'A',                          "#EC NOTEXT
        attr_val TYPE c VALUE 'V',                          "#EC NOTEXT
      END OF c_token .
    CONSTANTS:
      BEGIN OF c_regex,
        "for XML tags, we will use a submatch
        " main pattern includes quoted strings so we can ignore < and > in attr values
        xml_tag  TYPE string VALUE '(?:"[^"]*")|(?:''[^'']*'')|([<>])',    "#EC NOTEXT
        attr     TYPE string VALUE '(?:^|\s)[-a-z:_0-9]+\s*(?==)', "#EC NOTEXT
        attr_val TYPE string VALUE '("[^"]*")|(''[^'']*'')',     "#EC NOTEXT
      END OF c_regex .

    METHODS constructor .
  PROTECTED SECTION.

    METHODS order_matches REDEFINITION.

ENDCLASS.



CLASS zcl_abapgit_syntax_xml IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expressions

    add_rule( iv_regex    = c_regex-xml_tag
              iv_token    = c_token-xml_tag
              iv_style    = c_css-xml_tag
              iv_submatch = 1 ).

    add_rule( iv_regex = c_regex-attr
              iv_token = c_token-attr
              iv_style = c_css-attr ).

    add_rule( iv_regex = c_regex-attr_val
              iv_token = c_token-attr_val
              iv_style = c_css-attr_val ).

  ENDMETHOD.


  METHOD order_matches.

    DATA:
      lv_index      TYPE sy-tabix,
      lv_prev_token TYPE c,
      lv_state      TYPE c VALUE 'O'. " O - for open tag; C - for closed tag;

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.


    SORT ct_matches BY offset.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      lv_index = sy-tabix.

      CASE <ls_match>-token.
        WHEN c_token-xml_tag.
          <ls_match>-text_tag = substring( val = iv_line
                                        off = <ls_match>-offset
                                        len = <ls_match>-length ).

          " No other matches between two tags
          IF <ls_match>-text_tag = '>' AND lv_prev_token = c_token-xml_tag.
            lv_state = 'C'.
            <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset + <ls_match>-length.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.

            " Adjust length and offset of closing tag
          ELSEIF <ls_match>-text_tag = '>' AND lv_prev_token <> c_token-xml_tag.
            lv_state = 'C'.
            IF <ls_prev> IS ASSIGNED.
              <ls_match>-length = <ls_match>-offset - <ls_prev>-offset - <ls_prev>-length + <ls_match>-length.
              <ls_match>-offset = <ls_prev>-offset + <ls_prev>-length.
            ENDIF.
          ELSE.
            lv_state = 'O'.
          ENDIF.

        WHEN OTHERS.
          IF lv_prev_token = c_token-xml_tag.
            <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset. " Extend length of the opening tag
          ENDIF.

          IF lv_state = 'C'.  " Delete all matches between tags
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    "if the last XML tag is not closed, extend it to the end of the tag
    IF    lv_prev_token = c_token-xml_tag
      AND <ls_prev> IS ASSIGNED
      AND <ls_prev>-length  = 1
      AND <ls_prev>-text_tag = '<'.

      FIND REGEX '<\s*[^\s]*' IN iv_line+<ls_prev>-offset MATCH LENGTH <ls_prev>-length.
      IF sy-subrc <> 0.
        <ls_prev>-length = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
