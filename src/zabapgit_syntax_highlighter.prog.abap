*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SYNTAX_HIGHLIGHTER
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class lcl_code_highligher
*&---------------------------------------------------------------------*

CLASS ltcl_code_highlighter DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_code_highlighter DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_code_highlighter DEFINITION FRIENDS ltcl_code_highlighter.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_token,
        keyword   TYPE c VALUE 'K',
        text      TYPE c VALUE 'T',
        comment   TYPE c VALUE 'C',
        none      TYPE c VALUE 'N',
      END OF c_token.

    CONSTANTS:
      BEGIN OF c_css,
        keyword TYPE string VALUE 'keyword',
        text    TYPE string VALUE 'text',
        comment TYPE string VALUE 'comment',
        none    TYPE string VALUE 'none',
      END OF c_css.

    TYPES:
      BEGIN OF ty_match,
        token        TYPE char1,  " Type of matches
        offset       TYPE i,      " Beginning position of the string that should be formatted
        length       TYPE i,      " Length of the string that should be formatted
        text_tag     TYPE string, " Type of text tag
      END OF ty_match.

    TYPES:
      ty_match_tt  TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_regex,
        regex TYPE REF TO cl_abap_regex,
        token TYPE char1,
      END OF ty_regex.

    CLASS-DATA:
      BEGIN OF c_regex,
        comment   TYPE string,
        text      TYPE string,
        keyword   TYPE string,
      END OF c_regex.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      process_line
        IMPORTING iv_line        TYPE string
        RETURNING value(rv_line) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: mo_regex_table  TYPE TABLE OF ty_regex.

    METHODS:
      parse_line
        IMPORTING iv_line           TYPE string
        RETURNING value(rt_matches) TYPE ty_match_tt.

    METHODS:
      order_matches
        IMPORTING iv_line     TYPE string
        CHANGING  ct_matches  TYPE ty_match_tt.

    METHODS:
      format_line
        IMPORTING iv_line        TYPE string
                  it_matches     TYPE ty_match_tt
        RETURNING value(rv_line) TYPE string.

    METHODS:
      apply_style
        IMPORTING iv_line        TYPE string
                  iv_class       TYPE string
        RETURNING value(rv_line) TYPE string.

ENDCLASS.                      "lcl_code_highlighter DEFINITION

*----------------------------------------------------------------------*
*       Macros
*----------------------------------------------------------------------*

DEFINE _add_regex.

  CREATE OBJECT ls_regex_table-regex
    EXPORTING
      pattern     = c_regex-&1
      ignore_case = abap_true.

  ls_regex_table-token = c_token-&1.
  APPEND ls_regex_table TO mo_regex_table.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_code_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_code_highlighter IMPLEMENTATION.

  METHOD class_constructor.

    DATA: ls_regex_table TYPE ty_regex.

    c_regex-comment = '##|"|\*'.
    c_regex-text    = '`|''|\||\{|\}'.
    c_regex-keyword = '&&|\b(' &&
      '\*-INPUT|\?TO|ABAP-SOURCE|ABBREVIATED|ABS|ABSTRACT|ACCEPT|ACCEPTING|ACCESSPOLICY' &&
      '|ACCORDING|ACOS|ACTIVATION|ACTUAL|ADD|ADD-CORRESPONDING|ADJACENT|AFTER|ALIAS' &&
      '|ALIASES|ALIGN|ALL|ALLOCATE|ALPHA|ANALYSIS|ANALYZER|AND|ANY|APPEND|APPENDAGE' &&
      '|APPENDING|APPLICATION|ARCHIVE|AREA|ARITHMETIC|AS|ASCENDING|ASIN|ASPECT|ASSERT' &&
      '|ASSIGN|ASSIGNED|ASSIGNING|ASSOCIATION|ASYNCHRONOUS|AT|ATAN|ATTRIBUTES|AUTHORITY' &&
      '|AUTHORITY-CHECK|AVG|BACK|BACKGROUND|BACKUP|BACKWARD|BADI|BASE|BEFORE|BEGIN' &&
      '|BETWEEN|BIG|BINARY|BINDING|BIT|BIT-AND|BIT-NOT|BIT-OR|BIT-XOR|BLACK|BLANK' &&
      '|BLANKS|BLOB|BLOCK|BLOCKS|BLUE|BOUND|BOUNDARIES|BOUNDS|BOXED|BREAK-POINT|BT' &&
      '|BUFFER|BY|BYPASSING|BYTE|BYTE-CA|BYTE-CN|BYTE-CO|BYTE-CS|BYTE-NA|BYTE-NS' &&
      '|BYTE-ORDER|C|CA|CALL|CALLING|CASE|CAST|CASTING|CATCH|CEIL|CENTER|CENTERED' &&
      '|CHAIN|CHAIN-INPUT|CHAIN-REQUEST|CHANGE|CHANGING|CHANNELS|CHARACTER|CHARLEN' &&
      '|CHAR-TO-HEX|CHECK|CHECKBOX|CI_|CIRCULAR|CLASS|CLASS-CODING|CLASS-DATA' &&
      '|CLASS-EVENTS|CLASS-METHODS|CLASS-POOL|CLEANUP|CLEAR|CLIENT|CLOB|CLOCK|CLOSE' &&
      '|CN|CNT|CO|COALESCE|CODE|CODING|COL_BACKGROUND|COL_GROUP|COL_HEADING|COL_KEY' &&
      '|COL_NEGATIVE|COL_NORMAL|COL_POSITIVE|COL_TOTAL|COLLECT|COLOR|COLUMN|COLUMNS' &&
      '|COMMENT|COMMENTS|COMMIT|COMMON|COMMUNICATION|COMPARING|COMPONENT|COMPONENTS' &&
      '|COMPRESSION|COMPUTE|CONCAT|CONCATENATE|COND|CONDENSE|CONDITION|CONNECT' &&
      '|CONNECTION|CONSTANTS|CONTEXT|CONTEXTS|CONTINUE|CONTROL|CONTROLS|CONV|CONVERSION' &&
      '|CONVERT|COPIES|COPY|CORRESPONDING|COS|COSH|COUNT|COUNTRY|COVER|CP|CPI|CREATE' &&
      '|CREATING|CRITICAL|CS|CURRENCY|CURRENCY_CONVERSION|CURRENT|CURSOR|CURSOR-SELECTION' &&
      '|CUSTOMER|CUSTOMER-FUNCTION|DANGEROUS|DATA|DATABASE|DATAINFO|DATASET|DATE' &&
      '|DAYLIGHT|DBMAXLEN|DD/MM/YY|DD/MM/YYYY|DDMMYY|DEALLOCATE|DECIMAL_SHIFT|DECIMALS' &&
      '|DECLARATIONS|DEEP|DEFAULT|DEFERRED|DEFINE|DEFINING|DEFINITION|DELETE|DELETING' &&
      '|DEMAND|DEPARTMENT|DESCENDING|DESCRIBE|DESTINATION|DETAIL|DIALOG|DIRECTORY' &&
      '|DISCONNECT|DISPLAY|DISPLAY-MODE|DISTANCE|DISTINCT|DIV|DIVIDE|DIVIDE-CORRESPONDING' &&
      '|DIVISION|DO|DUMMY|DUPLICATE|DUPLICATES|DURATION|DURING|DYNAMIC|DYNPRO|E|EACH' &&
      '|EDIT|EDITOR-CALL|ELSE|ELSEIF|EMPTY|ENABLED|ENABLING|ENCODING|END|ENDAT|ENDCASE' &&
      '|ENDCATCH|ENDCHAIN|ENDCLASS|ENDDO|ENDENHANCEMENT|END-ENHANCEMENT-SECTION' &&
      '|ENDEXEC|ENDFOR|ENDFORM|ENDFUNCTION|ENDIAN|ENDIF|ENDING|ENDINTERFACE' &&
      '|END-LINES|ENDLOOP|ENDMETHOD|ENDMODULE|END-OF-DEFINITION|END-OF-FILE' &&
      '|END-OF-PAGE|END-OF-SELECTION|ENDON|ENDPROVIDE|ENDSELECT|ENDTRY|ENDWHILE' &&
      '|ENGINEERING|ENHANCEMENT|ENHANCEMENT-POINT|ENHANCEMENTS|ENHANCEMENT-SECTION' &&
      '|ENTRIES|ENTRY|ENVIRONMENT|EQ|EQUAL|EQUIV|ERRORMESSAGE|ERRORS|ESCAPE|ESCAPING' &&
      '|EVENT|EVENTS|EXACT|EXCEPT|EXCEPTION|EXCEPTIONS|EXCEPTION-TABLE|EXCLUDE|EXCLUDING' &&
      '|EXEC|EXECUTE|EXISTS|EXIT|EXIT-COMMAND|EXP|EXPAND|EXPANDING|EXPIRATION|EXPLICIT' &&
      '|EXPONENT|EXPORT|EXPORTING|EXTEND|EXTENDED|EXTENSION|EXTRACT|FAIL|FETCH|FIELD' &&
      '|FIELD-GROUPS|FIELDS|FIELD-SYMBOL|FIELD-SYMBOLS|FILE|FILTER|FILTERS|FILTER-TABLE' &&
      '|FINAL|FIND|FIRST|FIRST-LINE|FIXED-POINT|FKEQ|FKGE|FLOOR|FLUSH|FONT|FOR|FORM' &&
      '|FORMAT|FORWARD|FOUND|FRAC|FRAME|FRAMES|FREE|FRIENDS|FROM|FUNCTION|FUNCTIONALITY' &&
      '|FUNCTION-POOL|FURTHER|GAPS|GE|GENERATE|GET|GIVING|GKEQ|GKGE|GLOBAL|GRANT|GREATER' &&
      '|GREEN|GROUP|GROUPS|GT|HANDLE|HANDLER|HARMLESS|HASHED|HAVING|HDB|HEADER|HEADERS' &&
      '|HEADING|HEAD-LINES|HELP-ID|HELP-REQUEST|HIDE|HIGH|HINT|HOLD|HOTSPOT|I|ICON|ID' &&
      '|IDENTIFICATION|IDENTIFIER|IDS|IF|IGNORE|IGNORING|IMMEDIATELY|IMPLEMENTATION' &&
      '|IMPLEMENTATIONS|IMPLEMENTED|IMPLICIT|IMPORT|IMPORTING|IN|INACTIVE|INCL|INCLUDE' &&
      '|INCLUDES|INCLUDING|INCREMENT|INDEX|INDEX-LINE|INFOTYPES|INHERITING|INIT|INITIAL' &&
      '|INITIALIZATION|INNER|INOUT|INPUT|INSERT|INSTANCES|INTENSIFIED|INTERFACE' &&
      '|INTERFACE-POOL|INTERFACES|INTERNAL|INTERVALS|INTO|INVERSE|INVERTED-DATE|IS' &&
      '|ISO|ITERATOR|ITNO|JOB|JOIN|KEEP|KEEPING|KERNEL|KEY|KEYS|KEYWORDS|KIND' &&
      '|LANGUAGE|LAST|LATE|LAYOUT|LE|LEADING|LEAVE|LEFT|LEFT-JUSTIFIED|LEFTPLUS' &&
      '|LEFTSPACE|LEGACY|LENGTH|LESS|LET|LEVEL|LEVELS|LIKE|LINE|LINE-COUNT|LINEFEED' &&
      '|LINES|LINE-SELECTION|LINE-SIZE|LIST|LISTBOX|LIST-PROCESSING|LITTLE|LLANG' &&
      '|LOAD|LOAD-OF-PROGRAM|LOB|LOCAL|LOCALE|LOCATOR|LOG|LOG10|LOGFILE|LOGICAL' &&
      '|LOG-POINT|LONG|LOOP|LOW|LOWER|LPAD|LPI|LT|M|MAIL|MAIN|MAJOR-ID|MAPPING|MARGIN' &&
      '|MARK|MASK|MATCH|MATCHCODE|MAX|MAXIMUM|MEDIUM|MEMBERS|MEMORY|MESH|MESSAGE' &&
      '|MESSAGE-ID|MESSAGES|MESSAGING|METHOD|METHODS|MIN|MINIMUM|MINOR-ID|MM/DD/YY' &&
      '|MM/DD/YYYY|MMDDYY|MOD|MODE|MODIF|MODIFIER|MODIFY|MODULE|MOVE|MOVE-CORRESPONDING' &&
      '|MULTIPLY|MULTIPLY-CORRESPONDING|NA|NAME|NAMETAB|NATIVE|NB|NE|NESTED|NESTING' &&
      '|NEW|NEW-LINE|NEW-PAGE|NEW-SECTION|NEXT|NO|NODE|NODES|NO-DISPLAY' &&
      '|NO-EXTENSION|NO-GAP|NO-GAPS|NO-GROUPING|NO-HEADING|NON-UNICODE|NON-UNIQUE' &&
      '|NO-SCROLLING|NO-SIGN|NOT|NO-TITLE|NO-TOPOFPAGE|NO-ZERO|NP|NS|NULL|NUMBER' &&
      '|NUMOFCHAR|O|OBJECT|OBJECTS|OBLIGATORY|OCCURRENCE|OCCURRENCES|OCCURS|OF|OFF' &&
      '|OFFSET|OLE|ON|ONLY|OPEN|OPTION|OPTIONAL|OPTIONS|OR|ORDER|OTHER|OTHERS|OUT' &&
      '|OUTER|OUTPUT|OUTPUT-LENGTH|OVERFLOW|OVERLAY|PACK|PACKAGE|PAD|PADDING|PAGE' &&
      '|PAGES|PARAMETER|PARAMETERS|PARAMETER-TABLE|PART|PARTIALLY|PATTERN|PERCENTAGE' &&
      '|PERFORM|PERFORMING|PERSON|PF|PF-STATUS|PINK|PLACES|POOL|POS_HIGH|POS_LOW' &&
      '|POSITION|PRAGMAS|PRECOMPILED|PREFERRED|PRESERVING|PRIMARY|PRINT|PRINT-CONTROL' &&
      '|PRIORITY|PRIVATE|PROCEDURE|PROCESS|PROGRAM|PROPERTY|PROTECTED|PROVIDE|PUBLIC' &&
      '|PUSHBUTTON|PUT|QUEUE-ONLY|QUICKINFO|RADIOBUTTON|RAISE|RAISING|RANGE|RANGES' &&
      '|RAW|READ|READER|READ-ONLY|RECEIVE|RECEIVED|RECEIVER|RECEIVING|RED|REDEFINITION' &&
      '|REDUCE|REDUCED|REF|REFERENCE|REFRESH|REGEX|REJECT|REMOTE|RENAMING|REPLACE' &&
      '|REPLACEMENT|REPLACING|REPORT|REQUEST|REQUESTED|RESERVE|RESET|RESOLUTION' &&
      '|RESPECTING|RESPONSIBLE|RESULT|RESULTS|RESUMABLE|RESUME|RETRY|RETURN|RETURNCODE' &&
      '|RETURNING|RIGHT|RIGHT-JUSTIFIED|RIGHTPLUS|RIGHTSPACE|RISK|RMC_COMMUNICATION_FAILURE' &&
      '|RMC_INVALID_STATUS|RMC_SYSTEM_FAILURE|ROLE|ROLLBACK|ROUND|ROWS|RTTI|RUN|SAP|SAP-SPOOL' &&
      '|SAVING|SCALE_PRESERVING|SCALE_PRESERVING_SCIENTIFIC|SCAN|SCIENTIFIC|SCIENTIFIC_WITH_LEADING_ZERO' &&
      '|SCREEN|SCROLL|SCROLL-BOUNDARY|SCROLLING|SEARCH|SECONDARY|SECONDS|SECTION|SELECT|SELECTION' &&
      '|SELECTIONS|SELECTION-SCREEN|SELECTION-SET|SELECTION-SETS|SELECTION-TABLE|SELECT-OPTIONS' &&
      '|SELECTOR|SELECTOR|SEND|SEPARATE|SEPARATED|SET|SHARED|SHIFT|SHORT|SHORTDUMP-ID|SIGN' &&
      '|SIGN_AS_POSTFIX|SIMPLE|SIN|SINGLE|SINH|SIZE|SKIP|SKIPPING|SMART|SOME|SORT|SORTABLE' &&
      '|SORTED|SOURCE|SPACE|SPECIFIED|SPLIT|SPOOL|SPOTS|SQL|SQLSCRIPT|SQRT|STABLE|STAMP' &&
      '|STANDARD|STARTING|START-OF-SELECTION|STATE|STATEMENT|STATEMENTS|STATIC|STATICS|STATUSINFO' &&
      '|STEP-LOOP|STOP|STRLEN|STRUCTURE|STRUCTURES|STYLE|SUBKEY|SUBMATCHES|SUBMIT|SUBROUTINE' &&
      '|SUBSCREEN|SUBSTRING|SUBTRACT|SUBTRACT-CORRESPONDING|SUFFIX|SUM|SUMMARY|SUMMING|SUPPLIED' &&
      '|SUPPLY|SUPPRESS|SWITCH|SWITCHSTATES|SYMBOL|SYNCPOINTS|SYNTAX|SYNTAX-CHECK|SYNTAX-TRACE' &&
      '|SYSTEM-CALL|SYSTEM-EXCEPTIONS|SYSTEM-EXIT|TAB|TABBED|TABLE|TABLES|TABLEVIEW|TABSTRIP' &&
      '|TAN|TANH|TARGET|TASK|TASKS|TEST|TESTING|TEXT|TEXTPOOL|THEN|THROW|TIME|TIMES|TIMESTAMP' &&
      '|TIMEZONE|TITLE|TITLEBAR|TITLE-LINES|TO|TOKENIZATION|TOKENS|TOP-LINES|TOP-OF-PAGE' &&
      '|TRACE-FILE|TRACE-TABLE|TRAILING|TRANSACTION|TRANSFER|TRANSFORMATION|TRANSLATE' &&
      '|TRANSPORTING|TRMAC|TRUNC|TRUNCATE|TRUNCATION|TRY|TYPE|TYPE-POOL|TYPE-POOLS|TYPES' &&
      '|ULINE|UNASSIGN|UNDER|UNICODE|UNION|UNIQUE|UNIT|UNIT_CONVERSION|UNIX|UNPACK|UNTIL' &&
      '|UNWIND|UP|UPDATE|UPPER|USER|USER-COMMAND|USING|UTF-8|VALID|VALUE|VALUE-REQUEST|VALUES' &&
      '|VARY|VARYING|VERIFICATION-MESSAGE|VERSION|VIA|VIEW|VISIBLE|WAIT|WARNING|WHEN|WHENEVER' &&
      '|WHERE|WHILE|WIDTH|WINDOW|WINDOWS|WITH|WITH-HEADING|WITHOUT|WITH-TITLE|WORD|WORK' &&
      '|WRITE|WRITER|X|XML|XOR|XSD|XSTRLEN|YELLOW|YES|YYMMDD|Z|ZERO|ZONE' &&
      ')\b'.

    " Initialize instances of regular expressions
    _add_regex keyword.
    _add_regex comment.
    _add_regex text.

  ENDMETHOD.                    "class_constructor

  METHOD parse_line.

    DATA:
          lo_regex    TYPE REF TO cl_abap_regex,
          lo_matcher  TYPE REF TO cl_abap_matcher,
          lt_result   TYPE match_result_tab,
          ls_match    TYPE ty_match.

    FIELD-SYMBOLS:
          <regex>  TYPE ty_regex,
          <result> TYPE match_result,
          <match>  TYPE ty_match.

    LOOP AT mo_regex_table ASSIGNING <regex>.
      lo_regex   = <regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      LOOP AT lt_result ASSIGNING <result>.
        ls_match-token  = <regex>-token.
        ls_match-offset = <result>-offset.
        ls_match-length = <result>-length.

        IF ls_match-token = c_token-text.
          ls_match-text_tag = iv_line+ls_match-offset(ls_match-length).
        ENDIF.

        APPEND ls_match TO rt_matches.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    " parse_line

  METHOD order_matches.

    DATA:
          lv_index      TYPE sy-tabix,
          lv_line_len   TYPE i,
          lv_prev_token TYPE c,
          lv_last_pos   TYPE i VALUE 0,
          lv_length     TYPE i,
          ls_match      TYPE ty_match.

    FIELD-SYMBOLS:
         <prev>  TYPE ty_match,
         <match> TYPE ty_match.

    SORT ct_matches BY offset.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <match>.
      lv_index = sy-tabix.

      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <match>-token <> c_token-text.
        DELETE ct_matches INDEX lv_index.
        CONTINUE.
      ENDIF.

      CASE <match>-token.
        WHEN c_token-comment.
          <match>-length = lv_line_len - <match>-offset.
          DELETE ct_matches FROM lv_index + 1.
          CONTINUE.
        WHEN c_token-text.
          IF lv_prev_token = c_token-text.
            IF <match>-text_tag = <prev>-text_tag.
              <prev>-length = <match>-offset + <match>-length - <prev>-offset.
              CLEAR lv_prev_token.
            ELSEIF <prev>-text_tag = '}' AND <match>-text_tag = '{'.
              <prev>-length = <match>-offset - <prev>-offset - 1.   " Shifted } out of highlight
              <prev>-offset = <prev>-offset + 1.                    " Shifted { out of highlight
              CLEAR lv_prev_token.
            ELSEIF <match>-text_tag = '{'.
              <prev>-length = <match>-offset - <prev>-offset.
              CLEAR lv_prev_token.
            ELSEIF <prev>-text_tag = '}'.
              <prev>-length = <match>-offset - <prev>-offset.
              <prev>-offset = <prev>-offset + 1.                    " Shifted } out of highlight
              CLEAR lv_prev_token.
            ENDIF.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.
      ENDCASE.

      lv_prev_token = <match>-token.
      ASSIGN <match> TO <prev>.
    ENDLOOP.

    " Add entries refering to parts of text that should not be formatted
    LOOP AT ct_matches ASSIGNING <match>.
      IF <match>-offset > lv_last_pos.
        lv_length = <match>-offset - lv_last_pos.
        ls_match-token  = c_token-none.
        ls_match-offset = lv_last_pos.
        ls_match-length = lv_length.
        INSERT ls_match INTO ct_matches INDEX sy-tabix.
      ENDIF.
      lv_last_pos = <match>-offset + <match>-length.
    ENDLOOP.

    " Add remainder of the string
    IF lv_line_len > lv_last_pos.
      lv_length = lv_line_len - lv_last_pos.
      ls_match-token  = c_token-none.
      ls_match-offset = lv_last_pos.
      ls_match-length = lv_length.
      APPEND ls_match TO ct_matches.
    ENDIF.

  ENDMETHOD.  " order_matches.

  METHOD format_line.

    DATA:
          lv_chunk     TYPE string,
          lv_css_class TYPE string.

    FIELD-SYMBOLS:
          <match> TYPE ty_match.

    LOOP AT it_matches ASSIGNING <match>.
      lv_chunk = iv_line+<match>-offset(<match>-length).

      CASE <match>-token.
        WHEN c_token-keyword.
          lv_css_class = c_css-keyword.
        WHEN c_token-comment.
          lv_css_class = c_css-comment.
        WHEN c_token-text.
          lv_css_class = c_css-text.
        WHEN c_token-none.
          CLEAR: lv_css_class.
      ENDCASE.

      lv_chunk = me->apply_style( iv_line  = lv_chunk
                                  iv_class = lv_css_class ).

      rv_line = rv_line && lv_chunk.
    ENDLOOP.

  ENDMETHOD.                    "format_line

  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val = iv_line  format = cl_abap_format=>e_html_attr ).
    IF iv_class IS NOT INITIAL.
      rv_line = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      rv_line = lv_escaped.
    ENDIF.

  ENDMETHOD.                    "apply_style

  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF strlen( iv_line ) = 0.
      RETURN.
    ENDIF.

    lt_matches = me->parse_line( iv_line ).

    me->order_matches( EXPORTING iv_line    = iv_line
                       CHANGING  ct_matches = lt_matches ).

    rv_line = me->format_line( iv_line    = iv_line
                               it_matches = lt_matches ).

  ENDMETHOD.                    " process_line

ENDCLASS.                       " lcl_code_highlighter IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS ltcl_code_highlighter definition
*----------------------------------------------------------------------*
CLASS ltcl_code_highlighter DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
  INHERITING FROM cl_aunit_assert.

  PRIVATE SECTION.

    DATA:
          mo  TYPE REF TO lcl_code_highlighter.

    METHODS: setup.
    METHODS: process_line     FOR TESTING.
    METHODS: parse_and_order  FOR TESTING.
    METHODS: format_line      FOR TESTING.
    METHODS: apply_style      FOR TESTING.

ENDCLASS.                       " ltcl_code_highlighter
*----------------------------------------------------------------------*
*       CLASS ltcl_code_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_code_highlighter IMPLEMENTATION.

  DEFINE _generate_matches.
    ls_match-token    = &1.
    ls_match-offset   = &2.
    ls_match-length   = &3.
    ls_match-text_tag = &4.
    IF &5 = 'PARSE'.
      APPEND ls_match to lt_after_parse.
    ELSEIF &5 = 'ORDER'.
      APPEND ls_match to lt_after_order.
    ENDIF.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo.
  ENDMETHOD.                    " setup

  METHOD parse_and_order.
    DATA:
          lt_matches_act TYPE lcl_code_highlighter=>ty_match_tt,
          lt_after_parse TYPE lcl_code_highlighter=>ty_match_tt,
          lt_after_order TYPE lcl_code_highlighter=>ty_match_tt,
          ls_match       TYPE lcl_code_highlighter=>ty_match,
          lv_line        TYPE string.

******************************************************
* Test parsing and ordering of comments              *
******************************************************

    lv_line = '* commented out line with key word data'.

    " Generate table with expected values after parsing
    _generate_matches 'C' 0  1  '' 'PARSE'.
    _generate_matches 'K' 12 3  '' 'PARSE'.
    _generate_matches 'K' 16 4  '' 'PARSE'.
    _generate_matches 'K' 21 4  '' 'PARSE'.
    _generate_matches 'K' 26 3  '' 'PARSE'.
    _generate_matches 'K' 30 4  '' 'PARSE'.
    _generate_matches 'K' 35 4  '' 'PARSE'.

    " Generate table with expected values after ordering
    _generate_matches 'C' 0  39 '' 'ORDER'.

    lt_matches_act = mo->parse_line( lv_line ).

    SORT: lt_matches_act BY offset.

    assert_equals( exp = lt_after_parse
                   act = lt_matches_act
                   msg = | Error during parsing: { lv_line }| ).

    mo->order_matches( EXPORTING iv_line    = lv_line
                       CHANGING  ct_matches = lt_matches_act ).

    assert_equals( exp = lt_after_order
                   act = lt_matches_act
                   msg = | Error during ordering: { lv_line }| ).

******************************************************
* Test parsing and ordering of remainder of string   *
******************************************************

    CLEAR: lt_after_parse, lt_after_order, lt_matches_act.

    lv_line = 'data: lv_var_name type string.'.

    " Generate table with expected values after parsing
    _generate_matches 'K' 0  4  ''   'PARSE'.
    _generate_matches 'K' 18 4  ''   'PARSE'.

    " Generate table with expected values after ordering
    _generate_matches 'K' 0  4  ''   'ORDER'.
    _generate_matches 'N' 4  14 ''   'ORDER'.
    _generate_matches 'K' 18 4  ''   'ORDER'.
    _generate_matches 'N' 22 8  ''   'ORDER'.

    lt_matches_act = mo->parse_line( lv_line ).

    SORT: lt_matches_act BY offset.

    assert_equals( exp = lt_after_parse
                   act = lt_matches_act
                   msg = | Error during parsing: { lv_line }| ).

    mo->order_matches( EXPORTING iv_line    = lv_line
                       CHANGING  ct_matches = lt_matches_act ).

    assert_equals( exp = lt_after_order
                   act = lt_matches_act
                   msg = | Error during ordering: { lv_line }| ).

******************************************************
* Test parsing and ordering of key words & texts     *
******************************************************

    CLEAR: lt_after_parse, lt_after_order, lt_matches_act.

    lv_line = 'call function ''FM_NAME''. " Commented'.

    " Generate table with expected values after parsing
    _generate_matches 'K' 0  4  ''   'PARSE'.
    _generate_matches 'K' 5  8  ''   'PARSE'.
    _generate_matches 'T' 14 1  '''' 'PARSE'.
    _generate_matches 'T' 22 1  '''' 'PARSE'.
    _generate_matches 'C' 25 1  ''   'PARSE'.

    " Generate table with expected values after ordering
    _generate_matches 'K' 0  4  ''   'ORDER'.
    _generate_matches 'N' 4  1  ''   'ORDER'.
    _generate_matches 'K' 5  8  ''   'ORDER'.
    _generate_matches 'N' 13 1  ''   'ORDER'.
    _generate_matches 'T' 14 9  '''' 'ORDER'.
    _generate_matches 'N' 23 2  ''   'ORDER'.
    _generate_matches 'C' 25 11 ''   'ORDER'.

    lt_matches_act = mo->parse_line( lv_line ).

    SORT: lt_matches_act BY offset.

    assert_equals( exp = lt_after_parse
                   act = lt_matches_act
                   msg = | Error during parsing: { lv_line }| ).

    mo->order_matches( EXPORTING iv_line    = lv_line
                       CHANGING  ct_matches = lt_matches_act ).

    assert_equals( exp = lt_after_order
                   act = lt_matches_act
                   msg = | Error during ordering: { lv_line }| ).

******************************************************
* Test parsing and ordering of key words in texts    *
******************************************************

    CLEAR: lt_after_parse, lt_after_order, lt_matches_act.

    lv_line = 'constants: lc_var type string value ''simpletext data simpletext''.'.

    " Generate table with expected values after parsing
    _generate_matches 'K' 0  9  ''   'PARSE'.
    _generate_matches 'K' 18 4  ''   'PARSE'.
    _generate_matches 'K' 30 5  ''   'PARSE'.
    _generate_matches 'T' 36 1  '''' 'PARSE'.
    _generate_matches 'K' 48 4  ''   'PARSE'.
    _generate_matches 'T' 63 1  '''' 'PARSE'.

    " Generate table with expected values after ordering
    _generate_matches 'K' 0  9  ''   'ORDER'.
    _generate_matches 'N' 9  9  ''   'ORDER'.
    _generate_matches 'K' 18 4  ''   'ORDER'.
    _generate_matches 'N' 22 8  ''   'ORDER'.
    _generate_matches 'K' 30 5  ''   'ORDER'.
    _generate_matches 'N' 35 1  ''   'ORDER'.
    _generate_matches 'T' 36 28 '''' 'ORDER'.
    _generate_matches 'N' 64 1  ''   'ORDER'.

    lt_matches_act = mo->parse_line( lv_line ).

    SORT: lt_matches_act BY offset.

    assert_equals( exp = lt_after_parse
                   act = lt_matches_act
                   msg = | Error during parsing: { lv_line }| ).

    mo->order_matches( EXPORTING iv_line    = lv_line
                       CHANGING  ct_matches = lt_matches_act ).

    assert_equals( exp = lt_after_order
                   act = lt_matches_act
                   msg = | Error during ordering: { lv_line }| ).

******************************************************
* Test parsing and ordering texts in curly brackets  *
******************************************************

    CLEAR: lt_after_parse, lt_after_order, lt_matches_act.

    lv_line = 'a = |{ b }={ c }|.'.

    " Generate table with expected values after parsing
    _generate_matches 'T' 4  1  '|'  'PARSE'.
    _generate_matches 'T' 5  1  '{'  'PARSE'.
    _generate_matches 'T' 9  1  '}'  'PARSE'.
    _generate_matches 'T' 11 1  '{'  'PARSE'.
    _generate_matches 'K' 13 1  ''   'PARSE'.
    _generate_matches 'T' 15 1  '}'  'PARSE'.
    _generate_matches 'T' 16 1  '|'  'PARSE'.

    " Generate table with expected values after ordering
    _generate_matches 'N' 0  4  ''   'ORDER'.
    _generate_matches 'T' 4  1  '|'  'ORDER'.
    _generate_matches 'N' 5  5  ''   'ORDER'.
    _generate_matches 'T' 10 1  '}'  'ORDER'.
    _generate_matches 'N' 11 2  ''   'ORDER'.
    _generate_matches 'K' 13 1  ''   'ORDER'.
    _generate_matches 'N' 14 2  ''   'ORDER'.
    _generate_matches 'T' 16 1  '}'  'ORDER'.
    _generate_matches 'N' 17 1  ''   'ORDER'.

    lt_matches_act = mo->parse_line( lv_line ).

    SORT: lt_matches_act BY offset.

    assert_equals( exp = lt_after_parse
                   act = lt_matches_act
                   msg = | Error during parsing: { lv_line }| ).

    mo->order_matches( EXPORTING iv_line    = lv_line
                       CHANGING  ct_matches = lt_matches_act ).

    assert_equals( exp = lt_after_order
                   act = lt_matches_act
                   msg = | Error during ordering: { lv_line }| ).

******************************************************
* Test parsing and ordering of texts                 *
******************************************************

    CLEAR: lt_after_parse, lt_after_order, lt_matches_act.

    lv_line = 'lv_line = lc_constant && |XYZ { ''ab'' && |ac{ ''UU'' }| }|'.

    " Generate table with expected values after parsing
    _generate_matches 'K' 22 2 ''    'PARSE'.
    _generate_matches 'T' 25 1 '|'   'PARSE'.
    _generate_matches 'T' 30 1 '{'   'PARSE'.
    _generate_matches 'T' 32 1 ''''  'PARSE'.
    _generate_matches 'T' 35 1 ''''  'PARSE'.
    _generate_matches 'K' 37 2 ''    'PARSE'.
    _generate_matches 'T' 40 1 '|'   'PARSE'.
    _generate_matches 'T' 43 1 '{'   'PARSE'.
    _generate_matches 'T' 45 1 ''''  'PARSE'.
    _generate_matches 'T' 48 1 ''''  'PARSE'.
    _generate_matches 'T' 50 1 '}'   'PARSE'.
    _generate_matches 'T' 51 1 '|'   'PARSE'.
    _generate_matches 'T' 53 1 '}'   'PARSE'.
    _generate_matches 'T' 54 1 '|'   'PARSE'.

    " Generate table with expected values after ordering
    _generate_matches 'N' 00 22 ''   'ORDER'.
    _generate_matches 'K' 22 2  ''   'ORDER'.
    _generate_matches 'N' 24 1  ''   'ORDER'.
    _generate_matches 'T' 25 5  '|'  'ORDER'.
    _generate_matches 'N' 30 2  ''   'ORDER'.
    _generate_matches 'T' 32 4  '''' 'ORDER'.
    _generate_matches 'N' 36 1  ''   'ORDER'.
    _generate_matches 'K' 37 2  ''   'ORDER'.
    _generate_matches 'N' 39 1  ''   'ORDER'.
    _generate_matches 'T' 40 3  '|'  'ORDER'.
    _generate_matches 'N' 43 2  ''   'ORDER'.
    _generate_matches 'T' 45 4  '''' 'ORDER'.
    _generate_matches 'N' 49 2  ''   'ORDER'.
    _generate_matches 'T' 51 1  '}'  'ORDER'.
    _generate_matches 'N' 52 2  ''   'ORDER'.
    _generate_matches 'T' 54 1  '}'  'ORDER'.

    lt_matches_act = mo->parse_line( lv_line ).

    SORT: lt_matches_act BY offset.

    assert_equals( exp = lt_after_parse
                   act = lt_matches_act
                   msg = | Error during parsing: { lv_line }| ).

    mo->order_matches( EXPORTING iv_line    = lv_line
                       CHANGING  ct_matches = lt_matches_act ).

    assert_equals( exp = lt_after_order
                   act = lt_matches_act
                   msg = | Error during ordering: { lv_line }| ).

  ENDMETHOD.            " parse_and_order

  METHOD format_line.
    DATA:
          lt_matches_act TYPE lcl_code_highlighter=>ty_match_tt,
          lt_after_parse TYPE lcl_code_highlighter=>ty_match_tt,
          lt_after_order TYPE lcl_code_highlighter=>ty_match_tt,
          ls_match       TYPE lcl_code_highlighter=>ty_match,
          lv_line        TYPE string,
          lv_line_act    TYPE string,
          lv_line_exp    TYPE string.

    lv_line = 'call function ''FM_NAME''. " Commented'.

    " Generate expected value of formated line
    lv_line_exp = '<span class="keyword">call function</span>'  &&
                  ' <span class="text">&#39;FM_NAME&#39;</span>.' &&
                  ' <span class="comment">&quot; Commented</span>'.

    " Generate table with expected values after ordering
    _generate_matches 'K' 0  13 ''   'ORDER'.
    _generate_matches 'N' 13 1  ''   'ORDER'.
    _generate_matches 'T' 14 9  '''' 'ORDER'.
    _generate_matches 'N' 23 2  ''   'ORDER'.
    _generate_matches 'C' 25 11 ''   'ORDER'.

    lv_line_act = mo->format_line( iv_line    = lv_line
                                   it_matches = lt_after_order ).

    assert_equals( exp = lv_line_exp
                   act = lv_line_act
                   msg = | Error during formating: { lv_line }| ).

  ENDMETHOD.                    " format_line

  METHOD apply_style.
    DATA:
          lv_line_act TYPE string.

    " Call the method and compare results
    lv_line_act = mo->apply_style( iv_line  = 'CALL FUNCTION'
                                   iv_class = lcl_code_highlighter=>c_css-keyword ).

    assert_equals( act = lv_line_act
                   exp = '<span class="keyword">CALL FUNCTION</span>'
                   msg = 'Failure during applying of style.' ).
  ENDMETHOD.                    " apply_style

  METHOD process_line.
    DATA:
          lv_line_act TYPE string.

    " Call the method with empty parameter and compare results
    lv_line_act = mo->process_line( iv_line  = '' ).

    assert_equals( act = lv_line_act
                   exp = ''
                   msg = 'Failure in method process_line.' ).

    " Call the method with non-empty line and compare results
    lv_line_act = mo->process_line( iv_line  = '* CALL FUNCTION' ).

    assert_equals( act = lv_line_act
                   exp = '<span class="comment">* CALL FUNCTION</span>'
                   msg = 'Failure in method process_line.' ).
  ENDMETHOD.                    " process_line

ENDCLASS.                       " ltcl_code_highlighter