CLASS ltcl_syntax_css DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_syntax_css.

    METHODS:
      setup,
      properties_and_values FOR TESTING,
      functions FOR TESTING,
      selectors FOR TESTING,
      at_rules FOR TESTING,
      html_tags FOR TESTING,
      colors_and_units FOR TESTING,
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
