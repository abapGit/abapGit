*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SYNTAX_HIGHLIGHTER
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class lcl_syntax_highligher
*&---------------------------------------------------------------------*

CLASS ltcl_syntax_highlighter1 DEFINITION DEFERRED.
CLASS ltcl_syntax_highlighter2 DEFINITION DEFERRED.

CLASS lcl_syntax_abap DEFINITION DEFERRED.
CLASS lcl_syntax_xml DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_syntax_highlighter DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_syntax_highlighter DEFINITION ABSTRACT
  FRIENDS ltcl_syntax_highlighter1 ltcl_syntax_highlighter2.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING iv_filename TYPE string
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_syntax_highlighter.

    METHODS process_line
        IMPORTING iv_line        TYPE string
        RETURNING VALUE(rv_line) TYPE string.

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
      BEGIN OF ty_regex,
        regex     TYPE REF TO cl_abap_regex,
        token     TYPE char1,
      END OF ty_regex.

    TYPES:
      BEGIN OF ty_style_map,
        token TYPE char1,
        style TYPE string,
      END OF ty_style_map.

    CONSTANTS:
      BEGIN OF c_css,
        keyword  TYPE string VALUE 'keyword',                "#EC NOTEXT
        text     TYPE string VALUE 'text',                   "#EC NOTEXT
        comment  TYPE string VALUE 'comment',                "#EC NOTEXT
        xml_tag  TYPE string VALUE 'xml_tag',                "#EC NOTEXT
        attr     TYPE string VALUE 'attr',                   "#EC NOTEXT
        attr_val TYPE string VALUE 'attr_val',               "#EC NOTEXT
        none     TYPE string VALUE 'none',                   "#EC NOTEXT
      END OF c_css.

    CONSTANTS:
      BEGIN OF c_token,
        keyword  TYPE c VALUE 'K',                           "#EC NOTEXT
        text     TYPE c VALUE 'T',                           "#EC NOTEXT
        comment  TYPE c VALUE 'C',                           "#EC NOTEXT
        xml_tag  TYPE c VALUE 'X',                           "#EC NOTEXT
        attr     TYPE c VALUE 'A',                           "#EC NOTEXT
        attr_val TYPE c VALUE 'V',                           "#EC NOTEXT
        none     TYPE c VALUE 'N',                           "#EC NOTEXT
      END OF c_token.

    CLASS-DATA:
      BEGIN OF c_regex,
        comment  TYPE string,
        text     TYPE string,
        keyword  TYPE string,
        xml_tag  TYPE string,
        attr     TYPE string,
        attr_val TYPE string,
      END OF c_regex.

    CLASS-DATA:
      mo_regex_table  TYPE TABLE OF ty_regex,
      mo_style_map    TYPE TABLE OF ty_style_map.

    METHODS parse_line
        IMPORTING iv_line           TYPE string
        RETURNING VALUE(rt_matches) TYPE ty_match_tt.

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

ENDCLASS.                       " lcl_syntax_highlighter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_syntax_abap DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_syntax_abap DEFINITION INHERITING FROM lcl_syntax_highlighter.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
    CLASS-METHODS get_keywords RETURNING VALUE(rv_string) TYPE string.
    METHODS order_matches REDEFINITION.

ENDCLASS.                       " lcl_syntax_abap DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_syntax_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_syntax_xml DEFINITION INHERITING FROM lcl_syntax_highlighter.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
    METHODS order_matches REDEFINITION.

ENDCLASS.                       " lcl_syntax_xml DEFINITION

*----------------------------------------------------------------------*
* Macros to fill table with a regular expressions to be parsed
*----------------------------------------------------------------------*

DEFINE _add_regex.

  CREATE OBJECT ls_regex_table-regex
    EXPORTING
      pattern     = c_regex-&1
      ignore_case = abap_true.

  ls_regex_table-token     = c_token-&1.
  APPEND ls_regex_table TO mo_regex_table.

END-OF-DEFINITION.           " _add_regex

*----------------------------------------------------------------------*
* Macros to fill table with mapping of tokens against CSS-styles
*----------------------------------------------------------------------*

DEFINE _add_style_mapping.

  ls_style_map-token = c_token-&1.
  ls_style_map-style = c_css-&1.
  APPEND ls_style_map TO mo_style_map.

END-OF-DEFINITION.           " _add_style_mapping
*----------------------------------------------------------------------*
*       CLASS lcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of syntax highligther for ABAP source code
*----------------------------------------------------------------------*
CLASS lcl_syntax_highlighter IMPLEMENTATION.

  METHOD create.

    " Create instance of highighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      CREATE OBJECT ro_instance TYPE lcl_syntax_abap.
    ELSEIF iv_filename CP '*.xml'.
      CREATE OBJECT ro_instance TYPE lcl_syntax_xml.
    ELSE.
      CLEAR ro_instance.
    ENDIF.

  ENDMETHOD.                    " create.

  METHOD parse_line.

    DATA:
      lo_regex   TYPE REF TO cl_abap_regex,
      lo_matcher TYPE REF TO cl_abap_matcher,
      lt_result  TYPE match_result_tab,
      ls_match   TYPE ty_match.

    FIELD-SYMBOLS:
      <regex>  TYPE ty_regex,
      <result> TYPE match_result,
      <match>  TYPE ty_match.

    " Process syntax-dependent regex table and find all matches
    LOOP AT mo_regex_table ASSIGNING <regex>.
      lo_regex   = <regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT lt_result ASSIGNING <result>.
        CLEAR: ls_match.
        ls_match-token  = <regex>-token.
        ls_match-offset = <result>-offset.
        ls_match-length = <result>-length.
        APPEND ls_match TO rt_matches.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    " parse_line

  METHOD extend_matches.
    DATA:
      lv_line_len   TYPE i,
      lv_last_pos   TYPE i VALUE 0,
      lv_length     TYPE i,
      ls_match      TYPE ty_match.

    FIELD-SYMBOLS <match> TYPE ty_match.

    lv_line_len = strlen( iv_line ).

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

  ENDMETHOD.                    " extend_matches

  METHOD format_line.

    DATA:
      lv_chunk     TYPE string,
      lv_css_class TYPE string,
      ls_style_map TYPE ty_style_map.

    FIELD-SYMBOLS <match> TYPE ty_match.

    LOOP AT it_matches ASSIGNING <match>.
      lv_chunk = substring( val = iv_line off = <match>-offset len = <match>-length ).

      READ TABLE mo_style_map INTO ls_style_map WITH KEY token = <match>-token.

      IF sy-subrc IS INITIAL.
        lv_css_class = ls_style_map-style.
      ELSE.
        CLEAR: lv_css_class.
      ENDIF.

      lv_chunk = me->apply_style( iv_line  = lv_chunk
                                  iv_class = lv_css_class ).

      rv_line = rv_line && lv_chunk.
    ENDLOOP.

  ENDMETHOD.                    " format_line

  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val = iv_line  format = cl_abap_format=>e_html_attr ).
    IF iv_class IS NOT INITIAL.
      rv_line = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      rv_line = lv_escaped.
    ENDIF.

  ENDMETHOD.                    " apply_style

  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF strlen( iv_line ) = 0.
      RETURN.
    ENDIF.

    lt_matches = me->parse_line( iv_line = iv_line ).

    me->order_matches( EXPORTING iv_line    = iv_line
                       CHANGING  ct_matches = lt_matches ).

    me->extend_matches( EXPORTING iv_line    = iv_line
                        CHANGING  ct_matches = lt_matches ).

    rv_line = me->format_line( iv_line    = iv_line
                               it_matches = lt_matches ).

  ENDMETHOD.                    " process_line

ENDCLASS.                       " lcl_syntax_highlighter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_syntax_abap IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of syntax highligther for XML source code
*----------------------------------------------------------------------*
CLASS lcl_syntax_abap IMPLEMENTATION.

  METHOD class_constructor.

    DATA:
      ls_regex_table TYPE ty_regex,
      ls_style_map   TYPE ty_style_map.

    " Declare regular expressions' constants
    c_regex-comment = '##|"|^\*'.                           "#EC NOTEXT
    c_regex-text    = '`|''|\||\{|\}'.                      "#EC NOTEXT
    c_regex-keyword = '&&|\b(' && get_keywords( ) && ')\b'. "#EC NOTEXT

    " Initialize instances of regular expressions
    _add_regex keyword.
    _add_regex comment.
    _add_regex text.

    " Initialize mapping of tokens and CSS-styles
    _add_style_mapping keyword.
    _add_style_mapping comment.
    _add_style_mapping text.

  ENDMETHOD.                    " class_constructor

  METHOD get_keywords.

    rv_string =
      '\?TO|ABAP-SOURCE|ABBREVIATED|ABS|ABSTRACT|ACCEPT|ACCEPTING|ACCESSPOLICY' &&
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
      '|WRITE|WRITER|X|XML|XOR|XSD|XSTRLEN|YELLOW|YES|YYMMDD|Z|ZERO|ZONE'.
  ENDMETHOD.                    " get_keywords.

  METHOD order_matches.

    DATA:
      lv_index      TYPE sy-tabix,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c.

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
        WHEN c_token-keyword.
          IF <match>-offset > 0.
            " Delete match if keyword is part of structure or field symbol
            IF substring( val = iv_line off = ( <match>-offset - 1 ) len = 1 ) CA '-<'.
              DELETE ct_matches INDEX lv_index.
              CONTINUE.
            ENDIF.
          ENDIF.
        WHEN c_token-comment.
          <match>-length = lv_line_len - <match>-offset.
          DELETE ct_matches FROM lv_index + 1.
          CONTINUE.
        WHEN c_token-text.
          <match>-text_tag = substring( val = iv_line off = <match>-offset len = <match>-length ).
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
  ENDMETHOD.                    " order_matches.
ENDCLASS.                       " lcl_syntax_abap IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_syntax_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_syntax_xml IMPLEMENTATION.
  METHOD class_constructor.

    DATA:
      ls_regex_table TYPE ty_regex,
      ls_style_map   TYPE ty_style_map.

    c_regex-xml_tag  = '[<>]'.              "#EC NOTEXT
    c_regex-attr     = '\s[a-z:]+\s*(?==)'. "#EC NOTEXT
    c_regex-attr_val = '["''][^''"]+[''"]'. "#EC NOTEXT

    " Initialize instances of regular expressions
    _add_regex xml_tag.
    _add_regex attr.
    _add_regex attr_val.

    " Initialize mapping of tokens and CSS-styles
    _add_style_mapping xml_tag.
    _add_style_mapping attr.
    _add_style_mapping attr_val.

  ENDMETHOD.

  METHOD order_matches.
    DATA:
      lv_index      TYPE sy-tabix,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c,
      lv_state      TYPE c VALUE 'O'. " O - for open tag; C - for closed tag;

    FIELD-SYMBOLS:
      <prev>  TYPE ty_match,
      <match> TYPE ty_match.

    SORT ct_matches BY offset.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <match>.
      lv_index = sy-tabix.

      CASE <match>-token.
        WHEN c_token-xml_tag.
          <match>-text_tag = substring( val = iv_line off = <match>-offset len = <match>-length ).
          IF <match>-text_tag = '>' AND lv_prev_token = c_token-xml_tag. " No other matches between two tags
            lv_state = 'C'.
            <prev>-length = <match>-offset - <prev>-offset + <match>-length.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ELSEIF <match>-text_tag = '>' AND lv_prev_token <> c_token-xml_tag. " Adjust length and offset of closing tag
            lv_state = 'C'.
            <match>-length = <match>-offset - <prev>-offset - <prev>-length + <match>-length.
            <match>-offset = <prev>-offset + <prev>-length.
          ELSE.
            lv_state = 'O'.
          ENDIF.

        WHEN OTHERS.
          IF lv_prev_token = c_token-xml_tag.
            <prev>-length = <match>-offset - <prev>-offset. " Extend length of the opening tag
          ENDIF.

          IF lv_state = 'C'.  " Delete all matches between tags
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.
      ENDCASE.

      lv_prev_token = <match>-token.
      ASSIGN <match> TO <prev>.
    ENDLOOP.
  ENDMETHOD.                    " order_matches
ENDCLASS.                       " lcl_syntax_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter1 definition
*----------------------------------------------------------------------*
CLASS ltcl_syntax_highlighter1 DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    DATA:
      mo              TYPE REF TO lcl_syntax_highlighter,
      mt_after_parse  TYPE lcl_syntax_highlighter=>ty_match_tt,
      mt_after_order  TYPE lcl_syntax_highlighter=>ty_match_tt,
      mt_after_extend TYPE lcl_syntax_highlighter=>ty_match_tt,
      ms_match        TYPE lcl_syntax_highlighter=>ty_match.

    METHODS:
      setup,
      test IMPORTING iv_line     TYPE string
                     iv_filename TYPE string
           RETURNING VALUE(ro_instance) TYPE REF TO lcl_syntax_highlighter,
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING,
      test04 FOR TESTING,
      test05 FOR TESTING,
      test06 FOR TESTING,
      test07 FOR TESTING,
      test08 FOR TESTING.

ENDCLASS.                       " ltcl_syntax_highlighter1
*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter1 IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_highlighter1 IMPLEMENTATION.

  DEFINE _generate_parse.
    ms_match-token    = &1.
    ms_match-offset   = &2.
    ms_match-length   = &3.
    append ms_match to mt_after_parse.
  END-OF-DEFINITION.           " _generate_parse

  DEFINE _generate_order.
    ms_match-token    = &1.
    ms_match-offset   = &2.
    ms_match-length   = &3.
    ms_match-text_tag = &4.
    append ms_match to mt_after_order.
  END-OF-DEFINITION.           " _generate_order

  DEFINE _generate_extend.
    ms_match-token    = &1.
    ms_match-offset   = &2.
    ms_match-length   = &3.
    ms_match-text_tag = &4.
    append ms_match to mt_after_extend.
  END-OF-DEFINITION.           " _generate_extend

  METHOD setup.
    CLEAR mt_after_parse.
    CLEAR mt_after_order.
    CLEAR mt_after_extend.
  ENDMETHOD.                    " setup

  METHOD test.

    DATA:
          lt_matches_act TYPE lcl_syntax_highlighter=>ty_match_tt.

    mo =  lcl_syntax_highlighter=>create( iv_filename ).

    lt_matches_act = mo->parse_line( iv_line = iv_line ).

    SORT lt_matches_act BY offset.

    cl_abap_unit_assert=>assert_equals( exp = mt_after_parse
                                        act = lt_matches_act
                                        msg = | Error during parsing: { iv_line }| ).

    mo->order_matches( EXPORTING iv_line    = iv_line
                       CHANGING  ct_matches = lt_matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = mt_after_order
                                        act = lt_matches_act
                                        msg = | Error during ordering: { iv_line }| ).

    mo->extend_matches( EXPORTING iv_line   = iv_line
                       CHANGING  ct_matches = lt_matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = mt_after_extend
                                        act = lt_matches_act
                                        msg = | Error during extending: { iv_line }| ).

  ENDMETHOD.                    "test

******************************************************
* Test parsing and ordering of comments              *
******************************************************
  METHOD test01.

    DATA:
      lv_line     TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = '* commented out line with key word data'.    "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'C' 0  1.
    _generate_parse 'K' 12 3.
    _generate_parse 'K' 16 4.
    _generate_parse 'K' 21 4.
    _generate_parse 'K' 26 3.
    _generate_parse 'K' 30 4.
    _generate_parse 'K' 35 4.

    " Generate table with expected values after ordering
    _generate_order 'C' 0  39 ''.

    " Generate table with expected values after ordering
    _generate_extend 'C' 0  39 ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test01

******************************************************
* Test parsing and ordering of remainder of string   *
******************************************************
  METHOD test02.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = 'data: lv_var_name type string.'.             "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  4.
    _generate_parse 'K' 18 4.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  4  ''.
    _generate_order 'K' 18 4  ''.

    " Generate table with expected values after extending
    _generate_extend 'K' 0  4  ''.
    _generate_extend 'N' 4  14 ''.
    _generate_extend 'K' 18 4  ''.
    _generate_extend 'N' 22 8  ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test02

******************************************************
* Test parsing and ordering of key words & texts     *
******************************************************
  METHOD test03.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT


    lv_line = 'call function ''FM_NAME''. " Commented'.     "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  4.
    _generate_parse 'K' 5  8.
    _generate_parse 'T' 14 1.
    _generate_parse 'T' 22 1.
    _generate_parse 'C' 25 1.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  4  ''.
    _generate_order 'K' 5  8  ''.
    _generate_order 'T' 14 9  ''''.
    _generate_order 'C' 25 11 ''.

    " Generate table with expected values after extending
    _generate_extend 'K' 0  4  ''.
    _generate_extend 'N' 4  1  ''.
    _generate_extend 'K' 5  8  ''.
    _generate_extend 'N' 13 1  ''.
    _generate_extend 'T' 14 9  ''''.
    _generate_extend 'N' 23 2  ''.
    _generate_extend 'C' 25 11 ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test03

******************************************************
* Test parsing and ordering of key words in texts    *
******************************************************
  METHOD test04.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = 'constants: lc_var type string value ''simpletext data simpletext''.'. "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  9.
    _generate_parse 'K' 18 4.
    _generate_parse 'K' 30 5.
    _generate_parse 'T' 36 1.
    _generate_parse 'K' 48 4.
    _generate_parse 'T' 63 1.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  9  ''.
    _generate_order 'K' 18 4  ''.
    _generate_order 'K' 30 5  ''.
    _generate_order 'T' 36 28 ''''.

    " Generate table with expected values after ordering
    _generate_extend 'K' 0  9  ''.
    _generate_extend 'N' 9  9  ''.
    _generate_extend 'K' 18 4  ''.
    _generate_extend 'N' 22 8  ''.
    _generate_extend 'K' 30 5  ''.
    _generate_extend 'N' 35 1  ''.
    _generate_extend 'T' 36 28 ''''.
    _generate_extend 'N' 64 1  ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test04

******************************************************
* Test parsing and ordering texts in curly brackets  *
******************************************************
  METHOD test05.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = 'a = |{ b }={ c }|.'.                         "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'T' 4  1.
    _generate_parse 'T' 5  1.
    _generate_parse 'T' 9  1.
    _generate_parse 'T' 11 1.
    _generate_parse 'K' 13 1.
    _generate_parse 'T' 15 1.
    _generate_parse 'T' 16 1.

    " Generate table with expected values after ordering
    _generate_order 'T' 4  1  '|'.
    _generate_order 'T' 10 1  '}'.
    _generate_order 'K' 13 1  ''.
    _generate_order 'T' 16 1  '}'.

    " Generate table with expected values after extending
    _generate_extend 'N' 0  4  ''.
    _generate_extend 'T' 4  1  '|'.
    _generate_extend 'N' 5  5  ''.
    _generate_extend 'T' 10 1  '}'.
    _generate_extend 'N' 11 2  ''.
    _generate_extend 'K' 13 1  ''.
    _generate_extend 'N' 14 2  ''.
    _generate_extend 'T' 16 1  '}'.
    _generate_extend 'N' 17 1  ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test05

******************************************************
* Test parsing and ordering of texts                 *
******************************************************
  METHOD test06.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = 'lv_line = lc_constant && |XYZ { ''ab'' && |ac{ ''UU'' }| }|'. "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 22 2.
    _generate_parse 'T' 25 1.
    _generate_parse 'T' 30 1.
    _generate_parse 'T' 32 1.
    _generate_parse 'T' 35 1.
    _generate_parse 'K' 37 2.
    _generate_parse 'T' 40 1.
    _generate_parse 'T' 43 1.
    _generate_parse 'T' 45 1.
    _generate_parse 'T' 48 1.
    _generate_parse 'T' 50 1.
    _generate_parse 'T' 51 1.
    _generate_parse 'T' 53 1.
    _generate_parse 'T' 54 1.

    " Generate table with expected values after ordering
    _generate_order 'K' 22 2  ''.
    _generate_order 'T' 25 5  '|'.
    _generate_order 'T' 32 4  ''''.
    _generate_order 'K' 37 2  ''.
    _generate_order 'T' 40 3  '|'.
    _generate_order 'T' 45 4  ''''.
    _generate_order 'T' 51 1  '}'.
    _generate_order 'T' 54 1  '}'.

    " Generate table with expected values after extending
    _generate_extend 'N' 00 22 ''.
    _generate_extend 'K' 22 2  ''.
    _generate_extend 'N' 24 1  ''.
    _generate_extend 'T' 25 5  '|'.
    _generate_extend 'N' 30 2  ''.
    _generate_extend 'T' 32 4  ''''.
    _generate_extend 'N' 36 1  ''.
    _generate_extend 'K' 37 2  ''.
    _generate_extend 'N' 39 1  ''.
    _generate_extend 'T' 40 3  '|'.
    _generate_extend 'N' 43 2  ''.
    _generate_extend 'T' 45 4  ''''.
    _generate_extend 'N' 49 2  ''.
    _generate_extend 'T' 51 1  '}'.
    _generate_extend 'N' 52 2  ''.
    _generate_extend 'T' 54 1  '}'.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test06

********************************************************
* Check that '*' in select statement is not a match    *
********************************************************
  METHOD test07.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = 'SELECT * FROM foo'.                          "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  6.
    _generate_parse 'K' 9  4.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  6 ''.
    _generate_order 'K' 9  4 ''.

    " Generate table with expected values after ordering
    _generate_extend 'K' 0  6 ''.
    _generate_extend 'N' 6  3 ''.
    _generate_extend 'K' 9  4 ''.
    _generate_extend 'N' 13 4 ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test07

********************************************************
* Test parsing and ordering of key words in structures *
********************************************************
  METHOD test08.

    DATA:
      lv_line TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT

    lv_line = 'lv_length = <match>-length.'.                "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 13 5.
    _generate_parse 'K' 20 6.

    " Generate table with expected values after ordering
    _generate_extend 'N' 0  27 ''.

    test( iv_line = lv_line iv_filename = lv_filename ).

  ENDMETHOD.                    "test08
ENDCLASS.                    " ltcl_syntax_highlighter1 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter2 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_syntax_highlighter2 DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    DATA mo TYPE REF TO lcl_syntax_highlighter.

    METHODS: process_line  FOR TESTING.
    METHODS: format_line   FOR TESTING.
    METHODS: apply_style   FOR TESTING.

ENDCLASS.                       " ltcl_syntax_highlighter2

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_highlighter2 IMPLEMENTATION.
  METHOD format_line.

    DATA:
      lv_line     TYPE string,
      lv_line_act TYPE string,
      lv_line_exp TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT.

    " Create syntax highlighter
    mo =  lcl_syntax_highlighter=>create( lv_filename ).

    lv_line = 'call function ''FM_NAME''. " Commented'.     "#EC NOTEXT

    lv_line_exp =
      '<span class="keyword">call</span>' &&                "#EC NOTEXT
      ' <span class="keyword">function</span>' &&           "#EC NOTEXT
      ' <span class="text">&#39;FM_NAME&#39;</span>.' &&    "#EC NOTEXT
      ' <span class="comment">&quot; Commented</span>'.     "#EC NOTEXT

    lv_line_act = mo->process_line( iv_line = lv_line ).

    cl_abap_unit_assert=>assert_equals( exp = lv_line_exp
                                        act = lv_line_act
                                        msg = | Error during formating: { lv_line }| ).

  ENDMETHOD.                    " format_line

  METHOD apply_style.
    DATA:
      lv_line_act TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT.

    " Create syntax highlighter
    mo =  lcl_syntax_highlighter=>create( lv_filename ).

    " Call the method and compare results
    lv_line_act = mo->apply_style( iv_line  = 'CALL FUNCTION' "#EC NOTEXT
                                   iv_class = lcl_syntax_highlighter=>c_css-keyword ).

    cl_abap_unit_assert=>assert_equals( act = lv_line_act
                                        exp = '<span class="keyword">CALL FUNCTION</span>' "#EC NOTEXT
                                        msg = 'Failure during applying of style.' ). "#EC NOTEXT
  ENDMETHOD.                    " apply_style

  METHOD process_line.
    DATA:
      lv_line_act TYPE string,
      lv_filename TYPE string VALUE 'file_name.abap'.       "#EC NOTEXT.

    " Create syntax highlighter
    mo =  lcl_syntax_highlighter=>create( lv_filename ).

    " Call the method with empty parameter and compare results
    lv_line_act = mo->process_line( iv_line  = '' ).

    cl_abap_unit_assert=>assert_equals( act = lv_line_act
                                        exp = ''
                                        msg = 'Failure in method process_line.' ). "#EC NOTEXT

    " Call the method with non-empty line and compare results
    lv_line_act = mo->process_line( iv_line  = '* CALL FUNCTION' ). "#EC NOTEXT

    cl_abap_unit_assert=>assert_equals( act = lv_line_act
                                        exp = '<span class="comment">* CALL FUNCTION</span>' "#EC NOTEXT
                                        msg = 'Failure in method process_line.' ). "#EC NOTEXT
  ENDMETHOD.                    " process_line

ENDCLASS.                       " ltcl_syntax_highlighter