CLASS ltcl_syntax_css DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_syntax_css.

    METHODS:
      setup,
      properties_and_values FOR TESTING,
      functions FOR TESTING,
      function_calls FOR TESTING,
      custom_properties FOR TESTING,
      selectors FOR TESTING,
      at_rules FOR TESTING,
      html_tags FOR TESTING,
      colors_and_units FOR TESTING,
      unit_categories FOR TESTING,
      unit_numbers FOR TESTING,
      unit_boundaries FOR TESTING,
      extensions FOR TESTING,
      unknown_keywords FOR TESTING.
ENDCLASS.


CLASS ltcl_syntax_css IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD properties_and_values.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'gap: inherit; position: sticky; display: inline-flex;' )
      exp = |<span class="properties">gap</span>: <span class="values">inherit</span>; |
         && |<span class="properties">position</span>: <span class="values">sticky</span>; |
         && |<span class="properties">display</span>: <span class="values">inline-flex</span>;| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'margin-inline: revert-layer;' )
      exp = |<span class="properties">margin-inline</span>: <span class="values">revert-layer</span>;| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'white-space: pre-wrap; word-break: break-all;' )
      exp = |<span class="properties">white-space</span>: <span class="values">pre-wrap</span>; |
         && |<span class="properties">word-break</span>: <span class="values">break-all</span>;| ).
  ENDMETHOD.

  METHOD functions.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'translateX(1px) matrix3d(1) counter(item) url(image)' )
      exp = |<span class="functions">translateX</span>(<span class="units">1px</span>) |
         && |<span class="functions">matrix3d</span>(1) <span class="functions">counter</span>(item) |
         && |<span class="functions">url</span>(image)| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'TRANSLATEY(0) clamp(0, 1, 2)' )
      exp = |<span class="functions">TRANSLATEY</span>(0) <span class="functions">clamp</span>(0, 1, 2)| ).
  ENDMETHOD.

  METHOD function_calls.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'transform: rotate(45deg) scale(2);' )
      exp = |<span class="properties">transform</span>: <span class="functions">rotate</span>(|
         && |<span class="units">45deg</span>) |
         && |<span class="functions">scale</span>(2);| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'grid-template-columns: repeat(2, 1fr);' )
      exp = |<span class="properties">grid-template-columns</span>: <span class="functions">repeat</span>(2, |
         && |<span class="units">1fr</span>);| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'translate(0) inset(0) opacity(0) perspective(0)' )
      exp = |<span class="functions">translate</span>(0) <span class="functions">inset</span>(0) |
         && |<span class="functions">opacity</span>(0) <span class="functions">perspective</span>(0)| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'rotate: 45deg; @media(min-width: 1px)' )
      exp = |<span class="properties">rotate</span>: <span class="units">45deg</span>; |
         && |<span class="at_rules">@media</span>(|
         && |<span class="properties">min-width</span>: <span class="units">1px</span>)| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'rotate scale repeat' )
      exp = |<span class="properties">rotate</span> <span class="properties">scale</span> |
         && |<span class="values">repeat</span>| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '/* rotate(0) */ "scale(2)"' )
      exp = |<span class="comment">/* rotate(0) */</span> <span class="text">"scale(2)"</span>| ).
  ENDMETHOD.

  METHOD custom_properties.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '--gap: 1rem; width: var(--gap);' )
      exp = |--gap: <span class="units">1rem</span>; <span class="properties">width</span>: |
         && |<span class="functions">var</span>(--gap);| ).
  ENDMETHOD.

  METHOD selectors.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( ':first-child:focus-visible::placeholder' )
      exp = |<span class="selectors">:first-child</span><span class="selectors">:focus-visible</span>|
         && |<span class="selectors">::placeholder</span>| ).
  ENDMETHOD.

  METHOD at_rules.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '@media @supports @container @layer @-webkit-keyframes' )
      exp = |<span class="at_rules">@media</span> <span class="at_rules">@supports</span> |
         && |<span class="at_rules">@container</span> <span class="at_rules">@layer</span> |
         && |<span class="at_rules">@-webkit-keyframes</span>| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'container: none;' )
      exp = |<span class="properties">container</span>: <span class="values">none</span>;| ).
  ENDMETHOD.

  METHOD html_tags.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'h1 article dialog td' )
      exp = |<span class="html">h1</span> <span class="html">article</span> |
         && |<span class="html">dialog</span> <span class="html">td</span>| ).
  ENDMETHOD.

  METHOD colors_and_units.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'color: currentColor; margin: 1rem; background: transparent;' )
      exp = |<span class="properties">color</span>: <span class="colors">currentColor</span>; |
         && |<span class="properties">margin</span>: <span class="units">1rem</span>; |
         && |<span class="properties">background</span>: <span class="colors">transparent</span>;| ).
  ENDMETHOD.

  METHOD unit_categories.
    DATA lv_units TYPE string.
    DATA lt_units TYPE string_table.
    DATA lv_unit TYPE string.

    lv_units = 'cm|mm|Q|in|pt|pc|px|em|rem|ex|rex|cap|rcap|ch|rch|ic|ric|lh|rlh|'
            && 'vw|vh|vi|vb|vmin|vmax|svw|svh|svi|svb|svmin|svmax|'
            && 'lvw|lvh|lvi|lvb|lvmin|lvmax|dvw|dvh|dvi|dvb|dvmin|dvmax|'
            && 'cqw|cqh|cqi|cqb|cqmin|cqmax|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx|x|fr|%'.
    SPLIT lv_units AT '|' INTO TABLE lt_units.
    LOOP AT lt_units INTO lv_unit.
      cl_abap_unit_assert=>assert_equals(
        act = mo_cut->process_line( |1{ lv_unit }| )
        exp = |<span class="units">1{ lv_unit }</span>|
        msg = lv_unit ).
    ENDLOOP.
  ENDMETHOD.

  METHOD unit_numbers.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '-.5rem +0.25s 1e3ms 2E-1fr -10% .5%' )
      exp = |<span class="units">-.5rem</span> <span class="units">+0.25s</span> |
         && |<span class="units">1e3ms</span> <span class="units">2E-1fr</span> |
         && |<span class="units">-10%</span> <span class="units">.5%</span>| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'calc(100% - 2px)' )
      exp = |<span class="functions">calc</span>(<span class="units">100%</span> - |
         && |<span class="units">2px</span>)| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '1PX 2DEG 3MS 4FR' )
      exp = |<span class="units">1PX</span> <span class="units">2DEG</span> |
         && |<span class="units">3MS</span> <span class="units">4FR</span>| ).
  ENDMETHOD.

  METHOD unit_boundaries.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '10 px 1 2px foo1px foo-1px --gap1px 1pxfoo 0 1qu' )
      exp = |10 px 1 <span class="units">2px</span> foo1px foo-1px --gap1px 1pxfoo 0 1qu| ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '/* 45deg */ "1fr"' )
      exp = |<span class="comment">/* 45deg */</span> <span class="text">"1fr"</span>| ).
  ENDMETHOD.

  METHOD extensions.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '-webkit-user-select: none;' )
      exp = |-<span class="extensions">webkit-user-select</span>: <span class="values">none</span>;| ).
  ENDMETHOD.

  METHOD unknown_keywords.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( 'doctyype href cellpadding media unknown-property' )
      exp = 'doctyype href cellpadding media unknown-property' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->process_line( '/* gap: sticky */' )
      exp = |<span class="comment">/* gap: sticky */</span>| ).
  ENDMETHOD.
ENDCLASS.
