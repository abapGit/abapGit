CLASS ltcl_aff_type_mapping DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mi_cut TYPE REF TO zif_abapgit_aff_type_mapping.

    METHODS:
      setup,
      output_characteristics FOR TESTING,
      output_style_enums FOR TESTING,
      fixed_value_appends FOR TESTING.
ENDCLASS.

CLASS ltcl_aff_type_mapping IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mi_cut TYPE lcl_aff_type_mapping.
  ENDMETHOD.

  METHOD output_characteristics.
    DATA lo_source TYPE REF TO lcl_doma_data.
    DATA lo_actual TYPE REF TO lcl_doma_data.
    DATA ls_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.

    CREATE OBJECT lo_source.
    lo_source->mv_output_style = zif_abapgit_aff_doma_v1=>co_output_style-scientific.
    lo_source->mv_am_pm_time_format = abap_true.

    mi_cut->to_aff(
      EXPORTING
        iv_data = lo_source
      IMPORTING
        es_data = ls_aff ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-output_characteristics-style
      exp = zif_abapgit_aff_doma_v1=>co_output_style-scientific ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-output_characteristics-am_pm_time_format
      exp = abap_true ).

    mi_cut->to_abapgit(
      EXPORTING
        iv_data        = ls_aff
        iv_object_name = 'ZDOMAIN'
      IMPORTING
        es_data        = lo_actual ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_actual->mv_output_style
      exp = zif_abapgit_aff_doma_v1=>co_output_style-scientific ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_actual->mv_am_pm_time_format
      exp = abap_true ).
  ENDMETHOD.

  METHOD output_style_enums.
    DATA lt_enum_mappings TYPE zcl_abapgit_json_handler=>ty_enum_mappings.
    DATA ls_enum_mapping TYPE zcl_abapgit_json_handler=>ty_enum_mapping.
    DATA ls_mapping TYPE zcl_abapgit_json_handler=>ty_json_abap_mapping.

    lt_enum_mappings = lcl_aff_metadata_handler=>get_enum_mappings( ).

    READ TABLE lt_enum_mappings INTO ls_enum_mapping
      WITH KEY path = '/outputCharacteristics/style'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_enum_mapping-mappings )
      exp = 7 ).

    READ TABLE ls_enum_mapping-mappings INTO ls_mapping
      WITH KEY abap = zif_abapgit_aff_doma_v1=>co_output_style-scientific_with_leading_zero.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mapping-json
      exp = 'scientificWithLeadingZero' ).
  ENDMETHOD.

  METHOD fixed_value_appends.
    DATA lo_source TYPE REF TO lcl_doma_data.
    DATA lo_actual TYPE REF TO lcl_doma_data.
    DATA ls_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA ls_fixed_value_append TYPE zif_abapgit_aff_doma_v1=>ty_fixed_value_append.

    CREATE OBJECT lo_source.
    APPEND 'ZAPPEND1' TO lo_source->mt_fixed_value_append_names.
    APPEND 'ZAPPEND2' TO lo_source->mt_fixed_value_append_names.

    mi_cut->to_aff(
      EXPORTING
        iv_data = lo_source
      IMPORTING
        es_data = ls_aff ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_aff-fixed_value_appends )
      exp = 2 ).
    READ TABLE ls_aff-fixed_value_appends INDEX 2 INTO ls_fixed_value_append.
    cl_abap_unit_assert=>assert_equals(
      act = ls_fixed_value_append-name
      exp = 'ZAPPEND2' ).

    mi_cut->to_abapgit(
      EXPORTING
        iv_data        = ls_aff
        iv_object_name = 'ZDOMAIN'
      IMPORTING
        es_data        = lo_actual ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_actual->mt_fixed_value_append_names )
      exp = 2 ).
    READ TABLE lo_actual->mt_fixed_value_append_names INDEX 1
      INTO ls_fixed_value_append-name.
    cl_abap_unit_assert=>assert_equals(
      act = ls_fixed_value_append-name
      exp = 'ZAPPEND1' ).
  ENDMETHOD.

ENDCLASS.
