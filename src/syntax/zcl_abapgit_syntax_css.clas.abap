CLASS zcl_abapgit_syntax_css DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC .

  PUBLIC SECTION.

    " CSS Standard            https://www.w3.org/TR/css-2025/
    " CSS Reference           https://www.w3schools.com/cssref/default.asp

    " We used a mixture of above as reference for the keyword list
    " 1) CSS Properties       https://www.w3schools.com/cssref/default.asp
    " 2) CSS Values           https://www.w3.org/TR/css-values-4/
    " 3) CSS Selectors        https://www.w3.org/TR/css-2018/#selectors
    " 4) CSS Functions        https://www.w3schools.com/cssref/css_functions.asp
    " 5) CSS Colors           https://www.w3schools.com/colors/colors_names.asp
    " 6) CSS Extensions
    " 7) CSS At-Rules         https://www.w3.org/TR/css-2018/#at-rules
    " 8) HTML Tags

    CONSTANTS:
      BEGIN OF c_css,
        keyword    TYPE string VALUE 'keyword',
        text       TYPE string VALUE 'text',
        comment    TYPE string VALUE 'comment',
        selectors  TYPE string VALUE 'selectors',
        units      TYPE string VALUE 'units',
        properties TYPE string VALUE 'properties',
        values     TYPE string VALUE 'values',
        functions  TYPE string VALUE 'functions',
        colors     TYPE string VALUE 'colors',
        extensions TYPE string VALUE 'extensions',
        at_rules   TYPE string VALUE 'at_rules',
        html       TYPE string VALUE 'html',
      END OF c_css .
    CONSTANTS:
      BEGIN OF c_token,
        keyword    TYPE c VALUE 'K',
        text       TYPE c VALUE 'T',
        comment    TYPE c VALUE 'C',
        selectors  TYPE c VALUE 'S',
        units      TYPE c VALUE 'U',
        properties TYPE c VALUE 'P',
        values     TYPE c VALUE 'V',
        functions  TYPE c VALUE 'F',
        colors     TYPE c VALUE 'Z',
        extensions TYPE c VALUE 'E',
        at_rules   TYPE c VALUE 'A',
        html       TYPE c VALUE 'H',
      END OF c_token .
    CONSTANTS:
      BEGIN OF c_regex,
        " comments /* ... */
        comment   TYPE string VALUE '\/\*.*\*\/|\/\*|\*\/',
        " single or double quoted strings
        text      TYPE string VALUE '("[^"]*")|(''[^'']*'')|(`[^`]*`)',
        " Digits occur in HTML tags and CSS functions (h1, matrix3d)
        keyword   TYPE string VALUE '@-?[a-z][a-z0-9\-]*\b|\b[a-z][a-z0-9\-]*\b',
        " selectors begin with :
        selectors TYPE string VALUE '::?[a-z][a-z0-9\-]*\b',
        " units
        units     TYPE string
        VALUE '\b[0-9\. ]+(ch|cm|em|ex|in|mm|pc|pt|px|rem|vh|vmax|vmin|vw)\b|\b[0-9\. ]+%',
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
    CLASS-DATA gv_comment TYPE abap_bool.

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



CLASS zcl_abapgit_syntax_css IMPLEMENTATION.


  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

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

    add_rule( iv_regex = c_regex-selectors
              iv_token = c_token-selectors
              iv_style = c_css-selectors ).

    add_rule( iv_regex = c_regex-units
              iv_token = c_token-units
              iv_style = c_css-units ).

    " Styles for keywords
    add_rule( iv_regex = ''
              iv_token = c_token-html
              iv_style = c_css-html ).

    add_rule( iv_regex = ''
              iv_token = c_token-properties
              iv_style = c_css-properties ).

    add_rule( iv_regex = ''
              iv_token = c_token-values
              iv_style = c_css-values ).

    add_rule( iv_regex = ''
              iv_token = c_token-functions
              iv_style = c_css-functions ).

    add_rule( iv_regex = ''
              iv_token = c_token-colors
              iv_style = c_css-colors ).

    add_rule( iv_regex = ''
              iv_token = c_token-extensions
              iv_style = c_css-extensions ).

    add_rule( iv_regex = ''
              iv_token = c_token-at_rules
              iv_style = c_css-at_rules ).

  ENDMETHOD.


  METHOD init_keywords.

    DATA: lv_keywords TYPE string.

    CLEAR gt_keywords.

    " Keywords shared by categories keep the first inserted token (properties take precedence).
    " 1) CSS Properties
    lv_keywords =
    'align-content|align-items|align-self|animation|animation-delay|animation-direction|animation-duration|' &&
    'animation-fill-mode|animation-iteration-count|animation-name|animation-play-state|animation-timing-function|' &&
    'backface-visibility|background|background-attachment|background-blend-mode|background-clip|background-color|' &&
    'background-image|background-origin|background-position|background-repeat|background-size|border|' &&
    'border-bottom|border-bottom-color|border-bottom-left-radius|border-bottom-right-radius|border-bottom-style|' &&
    'border-bottom-width|border-collapse|border-color|border-image|border-image-outset|border-image-repeat|' &&
    'border-image-slice|border-image-source|border-image-width|border-left|border-left-color|border-left-style|' &&
    'border-left-width|border-radius|border-right|border-right-color|border-right-style|border-right-width|' &&
    'border-spacing|border-style|border-top|border-top-color|border-top-left-radius|border-top-right-radius|' &&
    'border-top-style|border-top-width|border-width|box-decoration-break|box-shadow|box-sizing|caption-side|' &&
    'caret-color|clear|clip|color|column-count|column-fill|column-gap|column-rule|column-rule-color|' &&
    'column-rule-style|column-rule-width|column-span|column-width|columns|content|counter-increment|' &&
    'counter-reset|cursor|direction|display|empty-cells|filter|flex|flex-basis|flex-direction|flex-flow|' &&
    'flex-grow|flex-shrink|flex-wrap|float|font|font-family|font-kerning|font-size|font-size-adjust|' &&
    'font-stretch|font-style|font-variant|font-weight|grid|grid-area|grid-auto-columns|grid-auto-flow|' &&
    'grid-auto-rows|grid-column|grid-column-end|grid-column-gap|grid-column-start|grid-gap|grid-row|' &&
    'grid-row-end|grid-row-gap|grid-row-start|grid-template|grid-template-areas|grid-template-columns|' &&
    'grid-template-rows|hanging-punctuation|height|hyphens|isolation|justify-content|' &&
    'letter-spacing|line-height|list-style|list-style-image|list-style-position|list-style-type|margin|' &&
    'margin-bottom|margin-left|margin-right|margin-top|max-height|max-width|min-height|min-width|' &&
    'mix-blend-mode|object-fit|object-position|opacity|order|outline|outline-color|outline-offset|' &&
    'outline-style|outline-width|overflow|overflow-x|overflow-y|padding|padding-bottom|padding-left|' &&
    'padding-right|padding-top|page-break-after|page-break-before|page-break-inside|perspective|' &&
    'perspective-origin|pointer-events|position|quotes|resize|scroll-behavior|tab-size|table-layout|' &&
    'text-align|text-align-last|text-decoration|text-decoration-color|text-decoration-line|' &&
    'text-decoration-style|text-indent|text-justify|text-overflow|text-rendering|text-shadow|text-transform|' &&
    'transform|transform-origin|transform-style|transition|transition-delay|transition-duration|' &&
    'transition-property|transition-timing-function|unicode-bidi|user-select|vertical-align|visibility|' &&
    'white-space|width|word-break|word-spacing|word-wrap|writing-mode|z-index|' &&
    'accent-color|all|appearance|aspect-ratio|backdrop-filter|background-position-x|background-position-y|' &&
    'block-size|border-block|border-block-color|border-block-end|border-block-end-color|' &&
    'border-block-end-style|border-block-end-width|border-block-start|border-block-start-color|' &&
    'border-block-start-style|border-block-start-width|border-block-style|border-block-width|' &&
    'border-end-end-radius|border-end-start-radius|border-inline|border-inline-color|border-inline-end|' &&
    'border-inline-end-color|border-inline-end-style|border-inline-end-width|border-inline-start|' &&
    'border-inline-start-color|border-inline-start-style|border-inline-start-width|border-inline-style|' &&
    'border-inline-width|border-start-end-radius|border-start-start-radius|bottom|break-after|break-before|' &&
    'break-inside|clip-path|contain|container|container-name|container-type|content-visibility|counter-set|' &&
    'font-feature-settings|font-optical-sizing|font-variant-caps|font-variant-east-asian|' &&
    'font-variant-ligatures|font-variant-numeric|font-variant-position|font-variation-settings|gap|' &&
    'image-rendering|inline-size|inset|inset-block|inset-block-end|inset-block-start|inset-inline|' &&
    'inset-inline-end|inset-inline-start|justify-items|justify-self|left|line-break|margin-block|' &&
    'margin-block-end|margin-block-start|margin-inline|margin-inline-end|margin-inline-start|mask|' &&
    'mask-clip|mask-composite|mask-image|mask-mode|mask-origin|mask-position|mask-repeat|mask-size|mask-type|' &&
    'max-block-size|max-inline-size|min-block-size|min-inline-size|offset|offset-anchor|offset-distance|' &&
    'offset-path|offset-position|offset-rotate|orphans|overflow-anchor|overflow-clip-margin|overflow-wrap|' &&
    'overscroll-behavior|overscroll-behavior-block|overscroll-behavior-inline|overscroll-behavior-x|' &&
    'overscroll-behavior-y|padding-block|padding-block-end|padding-block-start|padding-inline|' &&
    'padding-inline-end|padding-inline-start|place-content|place-items|place-self|print-color-adjust|right|' &&
    'rotate|row-gap|scale|scroll-margin|scroll-margin-block|scroll-margin-block-end|scroll-margin-block-start|' &&
    'scroll-margin-bottom|scroll-margin-inline|scroll-margin-inline-end|scroll-margin-inline-start|' &&
    'scroll-margin-left|scroll-margin-right|scroll-margin-top|scroll-padding|scroll-padding-block|' &&
    'scroll-padding-block-end|scroll-padding-block-start|scroll-padding-bottom|scroll-padding-inline|' &&
    'scroll-padding-inline-end|scroll-padding-inline-start|scroll-padding-left|scroll-padding-right|' &&
    'scroll-padding-top|scroll-snap-align|scroll-snap-stop|scroll-snap-type|scrollbar-color|scrollbar-gutter|' &&
    'scrollbar-width|shape-image-threshold|shape-margin|shape-outside|text-combine-upright|' &&
    'text-decoration-skip-ink|text-decoration-thickness|text-orientation|text-size-adjust|' &&
    'text-underline-offset|text-underline-position|text-wrap|top|touch-action|translate|widows|will-change|zoom'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-properties ).

    " 2) CSS Values
    lv_keywords =
    'absolute|auto|block|bold|border-box|both|center|cover|dashed|fixed|hidden|important|' &&
    'inherit|initial|inline-block|italic|max-content|middle|min-content|no-repeat|none|normal|pointer|' &&
    'relative|solid|table-cell|text|underline|unset|revert|revert-layer|' &&
    'alternate|alternate-reverse|always|avoid|avoid-column|avoid-page|backwards|baseline|bolder|capitalize|' &&
    'collapse|column-reverse|contain|content-box|contents|crosshair|default|dense|disc|dotted|double|' &&
    'ease|ease-in|ease-in-out|ease-out|ellipsis|end|fill|flex-end|flex-start|flow-root|forwards|grab|grabbing|' &&
    'groove|help|infinite|inline|inline-flex|inline-grid|inline-table|inside|keep-all|lighter|linear|' &&
    'list-item|lowercase|ltr|move|no-drop|nowrap|not-allowed|outside|overline|paused|progress|repeat|' &&
    'repeat-x|repeat-y|reverse|ridge|round|row|row-reverse|rtl|running|scroll|separate|space|' &&
    'space-around|space-between|space-evenly|start|static|step-end|step-start|sticky|stretch|subgrid|' &&
    'table-caption|table-column|table-column-group|table-footer-group|table-header-group|table-row|' &&
    'table-row-group|uppercase|visible|wait|wrap|wrap-reverse|zoom-in|zoom-out|' &&
    'balance|break-all|break-word|column|cursive|ew-resize|fantasy|horizontal-tb|justify|large|larger|' &&
    'line-through|medium|monospace|n-resize|ne-resize|nesw-resize|ns-resize|nw-resize|nwse-resize|' &&
    'oblique|pre-line|pre-wrap|print|s-resize|sans-serif|screen|se-resize|serif|small|small-caps|' &&
    'smaller|smooth|sw-resize|thick|thin|vertical-lr|vertical-rl|w-resize|x-large|x-small|xx-large|xx-small'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-values ).

    " 3) CSS Selectors
    lv_keywords =
    ':active|::after|::before|:checked|:disabled|:empty|:enabled|:first-child|::first-letter|::first-line|' &&
    ':first-of-type|:focus|:hover|:lang|:last-child|:last-of-type|:link|:not|:nth-child|:nth-last-child|' &&
    ':nth-last-of-type|:nth-of-type|:only-child|:only-of-type|:root|:target|:visited|' &&
    ':any-link|:default|:defined|:dir|:focus-visible|:focus-within|:has|:indeterminate|:in-range|' &&
    ':invalid|:is|:optional|:out-of-range|:placeholder-shown|:read-only|:read-write|:required|' &&
    ':user-invalid|:user-valid|:valid|:where|::backdrop|::file-selector-button|::marker|::part|' &&
    '::placeholder|::selection|::slotted'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-selectors ).

    " 4) CSS Functions
    lv_keywords =
    'attr|blur|brightness|calc|circle|clamp|color|color-mix|conic-gradient|contrast|counter|counters|' &&
    'cubic-bezier|drop-shadow|ellipse|env|fit-content|grayscale|hsl|hsla|hue-rotate|hwb|inset|invert|' &&
    'lab|lch|linear-gradient|matrix|matrix3d|max|min|minmax|oklab|oklch|opacity|perspective|polygon|' &&
    'radial-gradient|repeat|repeating-conic-gradient|repeating-linear-gradient|repeating-radial-gradient|' &&
    'rgb|rgba|rotate|rotate3d|rotatex|rotatey|rotatez|saturate|scale|scale3d|scalex|scaley|scalez|sepia|' &&
    'skew|skewx|skewy|steps|translate|translate3d|translatex|translatey|translatez|url|var'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-functions ).

    " 5) CSS Colors
    lv_keywords =
    'currentcolor|transparent|aliceblue|antiquewhite|aqua|aquamarine|azure|beige|bisque|black|' &&
    'blanchedalmond|blue|blueviolet|brown|' &&
    'burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|cyan|darkblue|darkcyan|' &&
    'darkgoldenrod|darkgray|darkgreen|darkgrey|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|' &&
    'darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|' &&
    'deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|fuchsia|gainsboro|' &&
    'ghostwhite|gold|goldenrod|gray|green|greenyellow|grey|honeydew|hotpink|indianred|indigo|ivory|khaki|' &&
    'lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|' &&
    'lightgray|lightgreen|lightgrey|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|' &&
    'lightslategrey|lightsteelblue|lightyellow|lime|limegreen|linen|magenta|maroon|mediumaquamarine|' &&
    'mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|' &&
    'mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|navy|oldlace|olive|olivedrab|' &&
    'orange|orangered|orchid|palegoldenrod|palegreen|paleturquoise|palevioletred|papayawhip|peachpuff|' &&
    'peru|pink|plum|powderblue|purple|rebeccapurple|red|rosybrown|royalblue|saddlebrown|salmon|sandybrown|' &&
    'seagreen|seashell|sienna|silver|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|' &&
    'tan|teal|thistle|tomato|turquoise|violet|wheat|white|whitesmoke|yellow|yellowgreen'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-colors ).

    " 6) CSS Extensions
    lv_keywords =
    'moz|moz-binding|moz-border-bottom-colors|moz-border-left-colors|moz-border-right-colors|' &&
    'moz-border-top-colors|moz-box-align|moz-box-direction|moz-box-flex|moz-box-ordinal-group|' &&
    'moz-box-orient|moz-box-pack|moz-box-shadow|moz-context-properties|moz-float-edge|' &&
    'moz-force-broken-image-icon|moz-image-region|moz-orient|moz-osx-font-smoothing|' &&
    'moz-outline-radius|moz-outline-radius-bottomleft|moz-outline-radius-bottomright|' &&
    'moz-outline-radius-topleft|moz-outline-radius-topright|moz-stack-sizing|moz-system-metric|' &&
    'moz-transform|moz-transform-origin|moz-transition|moz-transition-delay|moz-user-focus|' &&
    'moz-user-input|moz-user-modify|moz-window-dragging|moz-window-shadow|ms|ms-accelerator|' &&
    'ms-block-progression|ms-content-zoom-chaining|ms-content-zoom-limit|' &&
    'ms-content-zoom-limit-max|ms-content-zoom-limit-min|ms-content-zoom-snap|' &&
    'ms-content-zoom-snap-points|ms-content-zoom-snap-type|ms-content-zooming|ms-filter|' &&
    'ms-flow-from|ms-flow-into|ms-high-contrast-adjust|ms-hyphenate-limit-chars|' &&
    'ms-hyphenate-limit-lines|ms-hyphenate-limit-zone|ms-ime-align|ms-overflow-style|' &&
    'ms-scroll-chaining|ms-scroll-limit|ms-scroll-limit-x-max|ms-scroll-limit-x-min|' &&
    'ms-scroll-limit-y-max|ms-scroll-limit-y-min|ms-scroll-rails|ms-scroll-snap-points-x|' &&
    'ms-scroll-snap-points-y|ms-scroll-snap-x|ms-scroll-snap-y|ms-scroll-translation|' &&
    'ms-scrollbar-3dlight-color|ms-scrollbar-arrow-color|ms-scrollbar-base-color|' &&
    'ms-scrollbar-darkshadow-color|ms-scrollbar-face-color|ms-scrollbar-highlight-color|' &&
    'ms-scrollbar-shadow-color|ms-scrollbar-track-color|ms-transform|ms-text-autospace|' &&
    'ms-touch-select|ms-wrap-flow|ms-wrap-margin|ms-wrap-through|o|o-transform|webkit|' &&
    'webkit-animation-trigger|webkit-app-region|webkit-appearance|webkit-aspect-ratio|' &&
    'webkit-backdrop-filter|webkit-background-composite|webkit-border-after|' &&
    'webkit-border-after-color|webkit-border-after-style|webkit-border-after-width|' &&
    'webkit-border-before|webkit-border-before-color|webkit-border-before-style|' &&
    'webkit-border-before-width|webkit-border-end|webkit-border-end-color|' &&
    'webkit-border-end-style|webkit-border-end-width|webkit-border-fit|' &&
    'webkit-border-horizontal-spacing|webkit-border-radius|webkit-border-start|' &&
    'webkit-border-start-color|webkit-border-start-style|webkit-border-start-width|' &&
    'webkit-border-vertical-spacing|webkit-box-align|webkit-box-direction|webkit-box-flex|' &&
    'webkit-box-flex-group|webkit-box-lines|webkit-box-ordinal-group|webkit-box-orient|' &&
    'webkit-box-pack|webkit-box-reflect|webkit-box-shadow|webkit-column-axis|' &&
    'webkit-column-break-after|webkit-column-break-before|webkit-column-break-inside|' &&
    'webkit-column-progression|webkit-cursor-visibility|webkit-dashboard-region|' &&
    'webkit-font-size-delta|webkit-font-smoothing|webkit-highlight|webkit-hyphenate-character|' &&
    'webkit-hyphenate-limit-after|webkit-hyphenate-limit-before|webkit-hyphenate-limit-lines|' &&
    'webkit-initial-letter|webkit-line-align|webkit-line-box-contain|webkit-line-clamp|' &&
    'webkit-line-grid|webkit-line-snap|webkit-locale|webkit-logical-height|' &&
    'webkit-logical-width|webkit-margin-after|webkit-margin-after-collapse|' &&
    'webkit-margin-before|webkit-margin-before-collapse|webkit-margin-bottom-collapse|' &&
    'webkit-margin-collapse|webkit-margin-end|webkit-margin-start|webkit-margin-top-collapse|' &&
    'webkit-marquee|webkit-marquee-direction|webkit-marquee-increment|' &&
    'webkit-marquee-repetition|webkit-marquee-speed|webkit-marquee-style|webkit-mask-box-image|' &&
    'webkit-mask-box-image-outset|webkit-mask-box-image-repeat|webkit-mask-box-image-slice|' &&
    'webkit-mask-box-image-source|webkit-mask-box-image-width|webkit-mask-repeat-x|' &&
    'webkit-mask-repeat-y|webkit-mask-source-type|webkit-max-logical-height|' &&
    'webkit-max-logical-width|webkit-min-logical-height|webkit-min-logical-width|' &&
    'webkit-nbsp-mode|webkit-padding-after|webkit-padding-before|webkit-padding-end|' &&
    'webkit-padding-start|webkit-perspective-origin-x|webkit-perspective-origin-y|' &&
    'webkit-print-color-adjust|webkit-rtl-ordering|webkit-svg-shadow|' &&
    'webkit-tap-highlight-color|webkit-text-combine|webkit-text-decoration-skip|' &&
    'webkit-text-decorations-in-effect|webkit-text-fill-color|webkit-text-security|' &&
    'webkit-text-stroke|webkit-text-stroke-color|webkit-text-stroke-width|webkit-text-zoom|' &&
    'webkit-transform|webkit-transform-origin|webkit-transform-origin-x|' &&
    'webkit-transform-origin-y|webkit-transform-origin-z|webkit-transition|' &&
    'webkit-transition-delay|webkit-user-drag|webkit-user-modify|overflow-clip-box|' &&
    'overflow-clip-box-block|overflow-clip-box-inline|moz-appearance|moz-user-select|ms-user-select|' &&
    'webkit-animation|webkit-animation-delay|webkit-animation-direction|webkit-animation-duration|' &&
    'webkit-animation-fill-mode|webkit-animation-iteration-count|webkit-animation-name|' &&
    'webkit-animation-play-state|webkit-animation-timing-function|webkit-user-select'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-extensions ).

    " 7) CSS At-Rules
    lv_keywords =
    '@charset|@container|@counter-style|@font-face|@font-feature-values|@font-palette-values|@import|' &&
    '@keyframes|@layer|@media|@namespace|@page|@property|@scope|@starting-style|@supports|@viewport|' &&
    '@-moz-keyframes|@-o-keyframes|@-webkit-keyframes'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-at_rules ).

    " 8) HTML Tags (including legacy elements)
    lv_keywords =
    'a|abbr|acronym|address|applet|area|article|aside|audio|b|base|basefont|bdi|bdo|bgsound|big|blink|' &&
    'blockquote|body|br|button|canvas|caption|center|cite|code|col|colgroup|data|datalist|dd|del|details|' &&
    'dfn|dialog|dir|div|dl|dt|em|embed|fieldset|figcaption|figure|font|footer|form|frame|frameset|' &&
    'h1|h2|h3|h4|h5|h6|head|header|hgroup|hr|html|i|iframe|ilayer|img|input|ins|isindex|kbd|keygen|' &&
    'label|layer|legend|li|link|listing|main|map|mark|menu|meta|meter|multicol|nav|nobr|noembed|noframes|' &&
    'nolayer|noscript|object|ol|optgroup|option|output|p|param|picture|plaintext|pre|progress|q|rp|rt|' &&
    'ruby|s|samp|script|search|section|select|server|slot|small|sound|source|spacer|span|strike|strong|' &&
    'style|sub|summary|sup|table|tbody|td|template|textarea|tfoot|th|thead|time|title|tr|track|tt|u|ul|' &&
    'var|video|wbr|xmp|xml|xsl'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-html ).

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

    DATA lv_str TYPE string.

    lv_str = to_lower( iv_chunk ).
    READ TABLE gt_keywords WITH TABLE KEY keyword = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_prev_end   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>    TYPE ty_match,
      <ls_match>   TYPE ty_match,
      <ls_keyword> TYPE ty_keyword.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          " Skip keyword that's part of previous (longer) keyword
          IF <ls_match>-offset < lv_prev_end.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

          " Map generic keyword to specific CSS token
          lv_match = to_lower( lv_match ).
          READ TABLE gt_keywords ASSIGNING <ls_keyword> WITH TABLE KEY keyword = lv_match.
          IF sy-subrc = 0.
            <ls_match>-token = <ls_keyword>-token.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '/*'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '*/'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 2.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
          ENDIF.

        WHEN c_token-text.
          <ls_match>-text_tag = lv_match.
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ENDIF.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      lv_prev_end   = <ls_match>-offset + <ls_match>-length.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

  ENDMETHOD.


  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        CLEAR <ls_match>-token.
      ENDIF.
    ENDLOOP.

    DELETE rt_matches WHERE token IS INITIAL.

  ENDMETHOD.
ENDCLASS.
