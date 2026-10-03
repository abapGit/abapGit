CLASS zcl_abapgit_syntax_xml DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_css,
        xml_tag  TYPE string VALUE 'xml_tag',
        attr     TYPE string VALUE 'attr',
        attr_val TYPE string VALUE 'attr_val',
        comment  TYPE string VALUE 'comment',
      END OF c_css .
    CONSTANTS:
      BEGIN OF c_token,
        xml_tag  TYPE c VALUE 'X',
        attr     TYPE c VALUE 'A',
        attr_val TYPE c VALUE 'V',
        comment  TYPE c VALUE 'C',
      END OF c_token .
    CONSTANTS:
      BEGIN OF c_regex,
        "for XML tags, we will use a submatch
        " main pattern includes quoted strings so we can ignore < and > in attr values
        xml_tag  TYPE string VALUE '(?:"[^"]*")|(?:''[^'']*'')|(?:`[^`]*`)|([<>])',
        attr     TYPE string VALUE '(?:^|\s)[-a-z:_.0-9]+\s*(?==\s*["''`])',
        attr_val TYPE string VALUE '("[^"]*")|(''[^'']*'')|(`[^`]*`)',
        " comments <!-- ... -->
        comment  TYPE string VALUE '<!--(?:(?!-->).)*-->|<!--|-->',
      END OF c_regex .

    METHODS constructor .
  PROTECTED SECTION.
    DATA mv_comment TYPE abap_bool.

    METHODS order_matches REDEFINITION.
    METHODS parse_line REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_syntax_xml IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR mv_comment.

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

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.


  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_comment_end TYPE i,
      lv_prev_end   TYPE i,
      ls_comment    TYPE ty_match,
      lv_index      TYPE sy-tabix,
      lv_prev_token TYPE c,
      lv_state      TYPE c VALUE 'O'. " O - for open tag; C - for closed tag;

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    lv_line_len = strlen( iv_line ).

    " A continued comment ends at the first delimiter, regardless of its content.
    IF mv_comment = abap_true.
      FIND FIRST OCCURRENCE OF '-->' IN iv_line MATCH OFFSET lv_comment_end.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
      lv_comment_end = lv_comment_end + 3.
      DELETE ct_matches WHERE offset < lv_comment_end.
      ls_comment-token = c_token-comment.
      ls_comment-length = lv_comment_end.
      APPEND ls_comment TO ct_matches.
      mv_comment = abap_false.
    ENDIF.

    " Longest matches, including any continued comment prefix.
    SORT ct_matches BY offset length DESCENDING.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      lv_index = sy-tabix.

      " Ignore comment delimiters and nested quotes inside an accepted match.
      IF <ls_match>-offset < lv_prev_end.
        DELETE ct_matches INDEX lv_index.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-xml_tag.
          <ls_match>-text_tag = lv_match.

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

        WHEN c_token-comment.
          lv_state = 'C'.
          IF lv_match = '<!--'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            DELETE ct_matches WHERE offset = <ls_match>-offset AND token = c_token-xml_tag.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            mv_comment = abap_true.
          ELSEIF lv_match = '-->'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 3.
            <ls_match>-offset = 0.
            mv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset < lv_cmmt_end.
            DELETE ct_matches WHERE offset = <ls_match>-offset AND token = c_token-xml_tag.
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

      lv_prev_end = <ls_match>-offset + <ls_match>-length.
      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    "if the last XML tag is not closed, extend it to the end of the tag
    IF lv_prev_token = c_token-xml_tag
        AND <ls_prev> IS ASSIGNED
        AND <ls_prev>-length  = 1
        AND <ls_prev>-text_tag = '<'.

      FIND REGEX '<\s*[^\s]*' IN iv_line+<ls_prev>-offset MATCH LENGTH <ls_prev>-length ##REGEX_POSIX.
      IF sy-subrc <> 0.
        <ls_prev>-length = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD parse_line.

    DATA:
      lv_line_len       TYPE i,
      lv_segment_start  TYPE i,
      lv_scan_offset    TYPE i,
      lv_pattern        TYPE string,
      lv_comment_end    TYPE i,
      lv_found_offset   TYPE i,
      lv_found_length   TYPE i,
      lv_segment        TYPE string,
      lv_comment_length TYPE i,
      lt_segment_matches TYPE ty_match_tt,
      ls_comment        TYPE ty_match.

    FIELD-SYMBOLS <ls_segment_match> TYPE ty_match.

    lv_line_len = strlen( iv_line ).
    lv_pattern = c_regex-attr_val && '|<!--'.

    " Comments are parsed separately so their quotes cannot hide subsequent tags.
    IF mv_comment = abap_true.
      FIND FIRST OCCURRENCE OF '-->' IN iv_line MATCH OFFSET lv_comment_end.
      IF sy-subrc <> 0.
        ls_comment-token = c_token-comment.
        ls_comment-length = lv_line_len.
        APPEND ls_comment TO rt_matches.
        RETURN.
      ENDIF.
      lv_segment_start = lv_comment_end + 3.
      lv_scan_offset = lv_segment_start.
      ls_comment-token = c_token-comment.
      ls_comment-length = lv_segment_start.
      APPEND ls_comment TO rt_matches.
    ENDIF.

    WHILE lv_scan_offset < lv_line_len.
      FIND FIRST OCCURRENCE OF REGEX lv_pattern IN iv_line+lv_scan_offset
        MATCH OFFSET lv_found_offset MATCH LENGTH lv_found_length ##REGEX_POSIX.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      lv_found_offset = lv_found_offset + lv_scan_offset.
      lv_scan_offset = lv_found_offset + lv_found_length.
      IF substring( val = iv_line
                    off = lv_found_offset
                    len = lv_found_length ) <> '<!--'.
        CONTINUE.
      ENDIF.

      lv_segment = substring( val = iv_line
                              off = lv_segment_start
                              len = lv_found_offset - lv_segment_start ).
      lt_segment_matches = super->parse_line( lv_segment ).
      LOOP AT lt_segment_matches ASSIGNING <ls_segment_match>.
        <ls_segment_match>-offset = <ls_segment_match>-offset + lv_segment_start.
      ENDLOOP.
      APPEND LINES OF lt_segment_matches TO rt_matches.

      lv_comment_length = 4.
      FIND FIRST OCCURRENCE OF '-->' IN iv_line+lv_scan_offset MATCH OFFSET lv_comment_end.
      IF sy-subrc = 0.
        lv_scan_offset = lv_scan_offset + lv_comment_end + 3.
        lv_comment_length = lv_scan_offset - lv_found_offset.
      ELSE.
        lv_scan_offset = lv_line_len.
      ENDIF.
      CLEAR ls_comment.
      ls_comment-token = c_token-comment.
      ls_comment-offset = lv_found_offset.
      ls_comment-length = lv_comment_length.
      APPEND ls_comment TO rt_matches.
      lv_segment_start = lv_scan_offset.
    ENDWHILE.

    lv_segment = substring( val = iv_line
                            off = lv_segment_start ).
    lt_segment_matches = super->parse_line( lv_segment ).
    LOOP AT lt_segment_matches ASSIGNING <ls_segment_match>.
      <ls_segment_match>-offset = <ls_segment_match>-offset + lv_segment_start.
    ENDLOOP.
    APPEND LINES OF lt_segment_matches TO rt_matches.

  ENDMETHOD.
ENDCLASS.
