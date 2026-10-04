CLASS ltcl_syntax_js DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_syntax_js.
    METHODS:
      setup,
      language_keywords FOR TESTING,
      built_in_objects FOR TESTING,
      built_in_members FOR TESTING,
      case_sensitive FOR TESTING,
      identifier_boundaries FOR TESTING,
      quoted_strings FOR TESTING,
      escaped_quotes FOR TESTING,
      unterminated_string FOR TESTING,
      inline_comments FOR TESTING,
      multiline_comments FOR TESTING,
      instance_state FOR TESTING,
      template_literals FOR TESTING.
ENDCLASS.


CLASS ltcl_syntax_js IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD language_keywords.
    DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_keyword TYPE string.
    DATA lv_keywords TYPE string.

    lv_keywords = 'async|await|catch|class|const|debugger|enum|extends|finally|from|instanceof|' &&
                  'let|of|static|super|throw|try|typeof|using|yield'.
    SPLIT lv_keywords AT '|' INTO TABLE lt_keywords.
    LOOP AT lt_keywords INTO lv_keyword.
      cl_abap_unit_assert=>assert_equals(
        act = mo_cut->process_line( lv_keyword )
        exp = |<span class="keyword">{ lv_keyword }</span>| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD built_in_objects.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'Array Promise Map Set Symbol BigInt Uint8Array JSON Function function' )
      exp = |<span class="variables">Array</span> <span class="variables">Promise</span> |
            && |<span class="variables">Map</span> <span class="variables">Set</span> |
            && |<span class="variables">Symbol</span> <span class="variables">BigInt</span> |
            && |<span class="variables">Uint8Array</span> <span class="variables">JSON</span> |
            && |<span class="variables">Function</span> <span class="keyword">function</span>| ).
  ENDMETHOD.

  METHOD built_in_members.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'document.querySelector(item).innerHTML = parseInt(value);' )
      exp = |<span class="keyword">document</span>.<span class="keyword">querySelector</span>(item).|
            && |<span class="keyword">innerHTML</span> = <span class="keyword">parseInt</span>(|
            && |<span class="keyword">value</span>);| ).
  ENDMETHOD.

  METHOD case_sensitive.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'CONST array promise ParseInt innerhtml math nan True' )
      exp = 'CONST array promise ParseInt innerhtml math nan True' ).
  ENDMETHOD.

  METHOD identifier_boundaries.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '$const const$ _return return_ const1 1const myPromise' )
      exp = '$const const$ _return return_ const1 1const myPromise' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'value-return' )
      exp = |<span class="keyword">value</span>-<span class="keyword">return</span>| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'anchor applet bytetostring fileupload layer unit' )
      exp = 'anchor applet bytetostring fileupload layer unit' ).
  ENDMETHOD.

  METHOD quoted_strings.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '"const // /*"; return' )
      exp = |<span class="text">"const // /*"</span>; <span class="keyword">return</span>| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '''Promise " //''; const' )
      exp = |<span class="text">'Promise " //'</span>; <span class="keyword">const</span>| ).
  ENDMETHOD.

  METHOD escaped_quotes.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '"a\" const"; return' )
      exp = '<span class="text">"a\" const"</span>; <span class="keyword">return</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '"a\\"; return' )
      exp = '<span class="text">"a\\"</span>; <span class="keyword">return</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '''a\'' const''; return' )
      exp = '<span class="text">''a\'' const''</span>; <span class="keyword">return</span>' ).
  ENDMETHOD.

  METHOD unterminated_string.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '"const // rest' )
      exp = '<span class="text">"const // rest</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'return' )
      exp = '<span class="keyword">return</span>' ).
  ENDMETHOD.

  METHOD inline_comments.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '/* first */ const /* second */ return // tail' )
      exp = |<span class="comment">/* first */</span> <span class="keyword">const</span> |
            && |<span class="comment">/* second */</span> <span class="keyword">return</span> |
            && |<span class="comment">// tail</span>| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '*/ const' )
      exp = '*/ <span class="keyword">const</span>' ).
  ENDMETHOD.

  METHOD multiline_comments.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'const /* start' )
      exp = '<span class="keyword">const</span> <span class="comment">/* start</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '"return" // still /* comment' )
      exp = '<span class="comment">"return" // still /* comment</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'end */ let' )
      exp = '<span class="comment">end */</span> <span class="keyword">let</span>' ).
  ENDMETHOD.

  METHOD instance_state.
    DATA lo_other TYPE REF TO zcl_abapgit_syntax_js.
    DATA lv_line TYPE string.

    lv_line = mo_cut->process_line( '/* start' ).
    CREATE OBJECT lo_other.
    cl_abap_unit_assert=>assert_equals(
      act = lo_other->process_line( 'const' )
      exp = '<span class="keyword">const</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'const' )
      exp = '<span class="comment">const</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_line
      exp = '<span class="comment">/* start</span>' ).
  ENDMETHOD.

  METHOD template_literals.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '`const // start' )
      exp = '<span class="text">`const // start</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'Promise /* text */' )
      exp = '<span class="text">Promise /* text */</span>' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'end\` text`; return' )
      exp = '<span class="text">end\` text`</span>; <span class="keyword">return</span>' ).
  ENDMETHOD.
ENDCLASS.
