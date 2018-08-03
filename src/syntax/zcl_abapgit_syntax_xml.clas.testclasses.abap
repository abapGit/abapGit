CLASS abapgit_syntax_xml DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
    mo_cut TYPE REF TO zcl_abapgit_syntax_xml.

    METHODS:
      setup,
      sole_closing_xml_tag FOR TESTING RAISING cx_static_check,
      complete_xml_tag FOR TESTING RAISING cx_static_check,
      complete_xml_tag_with_closing FOR TESTING RAISING cx_static_check,
      empty_attributes FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS abapgit_syntax_xml IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD sole_closing_xml_tag.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&gt;</span>|
      act = mo_cut->process_line( |>| ) ).

  ENDMETHOD.

  METHOD complete_xml_tag.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;tag&gt;</span>|
      act = mo_cut->process_line( |<tag>| ) ).

  ENDMETHOD.

  METHOD complete_xml_tag_with_closing.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;tag/&gt;</span>|
      act = mo_cut->process_line( |<tag/>| ) ).

  ENDMETHOD.

  METHOD empty_attributes.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;ECTD</span>|
         && |<span class="attr"> SAPRL</span>=|
         && |<span class="attr_val">&quot;751&quot;</span>|
         && |<span class="attr"> VERSION</span>=|
         && |<span class="attr_val">&quot;1.5&quot;</span>|
         && |<span class="attr"> DOWNLOADDATE</span>=<span class="attr_val">&quot;&quot;</span>|
         && |<span class="attr"> DOWNLOADTIME</span>=<span class="attr_val">&quot;&quot;</span>|
         && |<span class="xml_tag">&gt;</span>|
      act = mo_cut->process_line( |<ECTD SAPRL="751" VERSION="1.5" DOWNLOADDATE="" DOWNLOADTIME="">| ) ).

  ENDMETHOD.

ENDCLASS.
