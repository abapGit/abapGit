CLASS ltcl_i18n_params_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

    METHODS iso_langs_to_lang_filter FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_i18n_params DEFINITION LOCAL FRIENDS ltcl_i18n_params_test.

CLASS ltcl_i18n_params_test IMPLEMENTATION.

  METHOD iso_langs_to_lang_filter.

    DATA lt_filter TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_act TYPE zif_abapgit_environment=>ty_system_language_filter.
    DATA lt_exp TYPE zif_abapgit_environment=>ty_system_language_filter.
    DATA ls_range LIKE LINE OF lt_act.

    APPEND 'EN' TO lt_filter.
    APPEND 'DE' TO lt_filter.

    ls_range-sign   = 'I'.
    ls_range-option = 'EQ'.
    ls_range-low    = 'E'.
    APPEND ls_range TO lt_exp.
    ls_range-low    = 'D'.
    APPEND ls_range TO lt_exp.

    lt_act = zcl_abapgit_i18n_params=>iso_langs_to_lang_filter( lt_filter ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

ENDCLASS.
