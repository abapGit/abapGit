CLASS zcl_abapgit_syntax_js DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      " JavaScript
      " Language keywords / built-in members and objects
      BEGIN OF c_css,
        keyword   TYPE string VALUE 'keyword',
        text      TYPE string VALUE 'text',
        comment   TYPE string VALUE 'comment',
        variables TYPE string VALUE 'variables',
      END OF c_css .
    CONSTANTS:
      BEGIN OF c_token,
        keyword   TYPE c VALUE 'K',
        text      TYPE c VALUE 'T',
        comment   TYPE c VALUE 'C',
        variables TYPE c VALUE 'V',
      END OF c_token .
    CONSTANTS:
      BEGIN OF c_regex,
        " comments /* ... */ or //
        comment TYPE string VALUE '\/\*|\*\/|\/\/',
        " Single / double quoted strings and template literals
        text    TYPE string VALUE '"|''|`',
        " Consume whole identifiers, including digits, underscores and dollar signs
        keyword TYPE string VALUE '[a-zA-Z0-9_$]+',
      END OF c_regex .

    CLASS-METHODS class_constructor .
    METHODS constructor .
  PROTECTED SECTION.
    TYPES: ty_token TYPE c LENGTH 1.

    TYPES: BEGIN OF ty_keyword,
             keyword TYPE string,
             token   TYPE ty_token,
           END OF ty_keyword.

    CLASS-DATA gt_keywords TYPE HASHED TABLE OF ty_keyword WITH UNIQUE KEY keyword.
    DATA mv_comment TYPE abap_bool.
    DATA mv_text_tag TYPE string.

    CLASS-METHODS init_keywords.
    CLASS-METHODS insert_keywords
      IMPORTING
        iv_keywords TYPE string
        iv_token    TYPE ty_token.
    CLASS-METHODS is_keyword
      IMPORTING iv_chunk      TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    METHODS order_matches REDEFINITION.
    METHODS parse_line REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_syntax_js IMPLEMENTATION.


  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR: mv_comment, mv_text_tag.

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    " Styles for keywords
    add_rule( iv_regex = ''
              iv_token = c_token-variables
              iv_style = c_css-variables ).

  ENDMETHOD.


  METHOD init_keywords.

    DATA: lv_keywords TYPE string.

    CLEAR gt_keywords.

    " Language keywords, reserved words and contextual keywords (ECMAScript)
    lv_keywords =
    'async|await|break|case|catch|class|const|continue|debugger|default|delete|do|else|enum|export|extends|' &&
    'false|finally|for|from|function|get|if|implements|import|in|instanceof|interface|let|new|null|of|package|' &&
    'private|protected|public|return|set|static|super|switch|this|throw|true|try|typeof|using|var|void|while|with|' &&
    'yield'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-keyword ).

    " Global values / functions and commonly used built-in members (case-sensitive)
    lv_keywords =
    'Infinity|NaN|undefined|decodeURI|decodeURIComponent|encodeURI|encodeURIComponent|escape|eval|isFinite|' &&
    'isNaN|parseFloat|parseInt|unescape|arguments|constructor|prototype|length|name|valueOf|toString|' &&
    'toLocaleString|apply|bind|call|assign|create|defineProperties|defineProperty|entries|freeze|fromEntries|' &&
    'getOwnPropertyDescriptor|getOwnPropertyDescriptors|getOwnPropertyNames|getOwnPropertySymbols|' &&
    'getPrototypeOf|hasOwn|hasOwnProperty|is|isExtensible|isFrozen|isSealed|keys|preventExtensions|' &&
    'propertyIsEnumerable|seal|setPrototypeOf|values|at|concat|copyWithin|every|fill|filter|find|findIndex|' &&
    'findLast|findLastIndex|flat|flatMap|forEach|includes|indexOf|isArray|join|lastIndexOf|map|pop|push|' &&
    'reduce|reduceRight|reverse|shift|slice|some|sort|splice|toReversed|toSorted|toSpliced|unshift|' &&
    'charAt|charCodeAt|codePointAt|endsWith|fromCharCode|fromCodePoint|localeCompare|match|matchAll|' &&
    'normalize|padEnd|padStart|repeat|replace|replaceAll|search|split|startsWith|substring|substr|' &&
    'toLowerCase|toUpperCase|toLocaleLowerCase|toLocaleUpperCase|trim|trimEnd|trimStart|' &&
    'MAX_VALUE|MIN_VALUE|MAX_SAFE_INTEGER|MIN_SAFE_INTEGER|NEGATIVE_INFINITY|POSITIVE_INFINITY|EPSILON|' &&
    'isInteger|isSafeInteger|toExponential|toFixed|toPrecision|E|LN10|LN2|LOG10E|LOG2E|PI|SQRT1_2|SQRT2|' &&
    'abs|acos|acosh|asin|asinh|atan|atan2|atanh|cbrt|ceil|clz32|cos|cosh|exp|expm1|floor|fround|hypot|' &&
    'imul|log|log10|log1p|log2|max|min|pow|random|round|sign|sin|sinh|sqrt|tan|tanh|trunc|' &&
    'now|parse|UTC|getDate|getDay|getFullYear|getHours|getMilliseconds|getMinutes|getMonth|getSeconds|' &&
    'getTime|getTimezoneOffset|getUTCDate|getUTCDay|getUTCFullYear|getUTCHours|getUTCMilliseconds|' &&
    'getUTCMinutes|getUTCMonth|getUTCSeconds|setDate|setFullYear|setHours|setMilliseconds|setMinutes|' &&
    'setMonth|setSeconds|setTime|setUTCDate|setUTCFullYear|setUTCHours|setUTCMilliseconds|setUTCMinutes|' &&
    'setUTCMonth|setUTCSeconds|toDateString|toISOString|toJSON|toTimeString|toUTCString|' &&
    'all|allSettled|any|race|reject|resolve|then|withResolvers|add|clear|has|size|stringify|exec|test'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-keyword ).

    " Browser globals / DOM members; HTML tag names are not JavaScript keywords
    lv_keywords =
    'alert|confirm|prompt|console|debug|error|info|warn|window|document|navigator|screen|history|location|' &&
    'self|parent|top|opener|frames|localStorage|sessionStorage|fetch|setTimeout|clearTimeout|setInterval|' &&
    'clearInterval|requestAnimationFrame|cancelAnimationFrame|queueMicrotask|structuredClone|atob|btoa|' &&
    'addEventListener|removeEventListener|dispatchEvent|getElementById|getElementsByClassName|' &&
    'getElementsByTagName|querySelector|querySelectorAll|createElement|createTextNode|appendChild|' &&
    'removeChild|replaceChild|insertBefore|setAttribute|getAttribute|removeAttribute|classList|' &&
    'innerHTML|outerHTML|textContent|style|value|checked|disabled|selected|selectedIndex|children|' &&
    'parentNode|parentElement|nextSibling|previousSibling|firstChild|lastChild|body|head|title|cookie|' &&
    'forms|images|links|href|host|hostname|pathname|port|protocol|hash|origin|reload|replaceState|' &&
    'pushState|back|forward|go|open|close|focus|blur|click|submit|reset|scroll|scrollBy|scrollTo|' &&
    'clientHeight|clientWidth|offsetHeight|offsetWidth|offsetLeft|offsetTop|offsetParent|scrollTop|' &&
    'scrollLeft|innerHeight|innerWidth|outerHeight|outerWidth|pageXOffset|pageYOffset|userAgent|' &&
    'onabort|onblur|onchange|onclick|ondblclick|onerror|onfocus|oninput|onkeydown|onkeypress|onkeyup|' &&
    'onload|onmousedown|onmousemove|onmouseout|onmouseover|onmouseup|onreset|onresize|onselect|onsubmit|onunload'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-keyword ).

    " Built-in objects / constructors (Function is distinct from the function keyword)
    lv_keywords =
    'Array|ArrayBuffer|AsyncDisposableStack|Atomics|BigInt|BigInt64Array|BigUint64Array|Boolean|DataView|' &&
    'Date|DisposableStack|Error|EvalError|FinalizationRegistry|Float16Array|Float32Array|Float64Array|' &&
    'Function|Int8Array|Int16Array|Int32Array|Intl|Iterator|JSON|Map|Math|Number|Object|Promise|Proxy|' &&
    'RangeError|ReferenceError|Reflect|RegExp|Set|SharedArrayBuffer|String|SuppressedError|Symbol|' &&
    'SyntaxError|TypeError|Uint8Array|Uint8ClampedArray|Uint16Array|Uint32Array|URIError|WeakMap|WeakRef|' &&
    'WeakSet|globalThis|AggregateError|AbortController|AbortSignal|Blob|CustomEvent|Event|File|FileReader|' &&
    'FormData|Headers|Image|Node|Option|Request|Response|TextDecoder|TextEncoder|URL|URLSearchParams|WebSocket'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-variables ).

  ENDMETHOD.


  METHOD insert_keywords.

    DATA: lt_keywords TYPE STANDARD TABLE OF string,
          ls_keyword  TYPE ty_keyword.

    FIELD-SYMBOLS: <lv_keyword> TYPE any.

    SPLIT iv_keywords AT '|' INTO TABLE lt_keywords.

    LOOP AT lt_keywords ASSIGNING <lv_keyword>.
      CLEAR ls_keyword.
      ls_keyword-keyword = <lv_keyword>.
      ls_keyword-token = iv_token.
      INSERT ls_keyword INTO TABLE gt_keywords.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_keyword.

    READ TABLE gt_keywords WITH TABLE KEY keyword = iv_chunk TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD order_matches.

    DATA:
      lt_matches       TYPE ty_match_tt,
      ls_match         TYPE ty_match,
      lv_match         TYPE string,
      lv_line_len      TYPE i,
      lv_comment_start TYPE i,
      lv_next          TYPE i,
      lv_pos           TYPE i,
      lv_tag           TYPE string,
      lv_closed        TYPE abap_bool.

    FIELD-SYMBOLS:
      <ls_match>   TYPE ty_match,
      <ls_keyword> TYPE ty_keyword.

    SORT ct_matches BY offset length DESCENDING.
    lv_line_len = strlen( iv_line ).

    " A template literal may continue from the preceding line
    IF mv_text_tag IS NOT INITIAL.
      CLEAR ls_match.
      ls_match-token = c_token-text.
      INSERT ls_match INTO ct_matches INDEX 1.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      IF <ls_match>-offset < lv_next.
        CONTINUE.
      ENDIF.
      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      " Only */ can end an open block comment; quotes and // are ordinary text
      IF mv_comment = abap_true.
        IF <ls_match>-token = c_token-comment AND lv_match = '*/'.
          CLEAR ls_match.
          ls_match-token = c_token-comment.
          ls_match-offset = lv_comment_start.
          lv_next = <ls_match>-offset + 2.
          ls_match-length = lv_next - lv_comment_start.
          APPEND ls_match TO lt_matches.
          CLEAR mv_comment.
        ENDIF.
        CONTINUE.
      ENDIF.

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          READ TABLE gt_keywords ASSIGNING <ls_keyword> WITH TABLE KEY keyword = lv_match.
          IF sy-subrc = 0.
            ls_match = <ls_match>.
            ls_match-token = <ls_keyword>-token.
            APPEND ls_match TO lt_matches.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '/*'.
            lv_comment_start = <ls_match>-offset.
            mv_comment = abap_true.
          ELSEIF lv_match = '//'.
            ls_match = <ls_match>.
            ls_match-length = lv_line_len - ls_match-offset.
            APPEND ls_match TO lt_matches.
            EXIT.
          ENDIF.

        WHEN c_token-text.
          lv_tag = lv_match.
          lv_pos = <ls_match>-offset + 1.
          IF mv_text_tag IS NOT INITIAL.
            lv_tag = mv_text_tag.
            lv_pos = 0.
            CLEAR mv_text_tag.
          ENDIF.
          CLEAR lv_closed.
          WHILE lv_pos < lv_line_len.
            IF iv_line+lv_pos(1) = '\'.
              " Escaped delimiters and escaped backslashes do not close the string
              lv_pos = lv_pos + 2.
            ELSEIF iv_line+lv_pos(1) = lv_tag.
              lv_pos = lv_pos + 1.
              lv_closed = abap_true.
              EXIT.
            ELSE.
              lv_pos = lv_pos + 1.
            ENDIF.
          ENDWHILE.
          lv_next = nmin( val1 = lv_pos
                          val2 = lv_line_len ).
          ls_match = <ls_match>.
          ls_match-length = lv_next - ls_match-offset.
          APPEND ls_match TO lt_matches.
          " Treat the entire template as text, including interpolation
          IF lv_closed = abap_false AND lv_tag = '`'.
            mv_text_tag = lv_tag.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF mv_comment = abap_true.
      CLEAR ls_match.
      ls_match-token = c_token-comment.
      ls_match-offset = lv_comment_start.
      ls_match-length = lv_line_len - lv_comment_start.
      APPEND ls_match TO lt_matches.
    ENDIF.
    ct_matches = lt_matches.

  ENDMETHOD.


  METHOD parse_line. "REDEFINITION

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        CLEAR <ls_match>-token.
      ENDIF.
    ENDLOOP.

    DELETE rt_matches WHERE token IS INITIAL.

  ENDMETHOD.
ENDCLASS.
