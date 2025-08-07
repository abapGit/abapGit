CLASS ltcl_i18n_params_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

    METHODS iso_langs_to_lang_filter FOR TESTING.
    METHODS filter_sap_langs FOR TESTING RAISING zcx_abapgit_exception.
    METHODS filter_sap_langs_tab FOR TESTING RAISING zcx_abapgit_exception.
    METHODS normalize_obj_patterns FOR TESTING RAISING zcx_abapgit_exception.
    METHODS match_obj_patterns FOR TESTING RAISING zcx_abapgit_exception.

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

  METHOD filter_sap_langs.

    DATA lt_act TYPE zif_abapgit_definitions=>ty_sap_langu_tab.
    DATA lt_exp TYPE zif_abapgit_definitions=>ty_sap_langu_tab.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_languages.
    DATA lo_p TYPE REF TO zcl_abapgit_i18n_params.

    APPEND 'DE' TO lt_filter.
    APPEND 'EN' TO lt_filter.

    APPEND 'E' TO lt_act.
    APPEND 'D' TO lt_act.
    APPEND 'S' TO lt_act.

    APPEND 'E' TO lt_exp.
    APPEND 'D' TO lt_exp.

    lo_p = zcl_abapgit_i18n_params=>new(
      iv_main_language = '?'
      it_translation_langs = lt_filter ).
    lo_p->trim_saplang_list( CHANGING ct_sap_langs = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " Empty filter
    CLEAR: lt_act, lt_exp, lt_filter.
    APPEND 'E' TO lt_act.
    APPEND 'D' TO lt_act.

    APPEND 'E' TO lt_exp.
    APPEND 'D' TO lt_exp.

    lo_p = zcl_abapgit_i18n_params=>new(
      iv_main_language = '?'
      it_translation_langs = lt_filter ).
    lo_p->trim_saplang_list( CHANGING ct_sap_langs = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD filter_sap_langs_tab.

    DATA:
      BEGIN OF ls_i,
        spras TYPE sy-langu,
        stuff TYPE string,
      END OF ls_i.

    DATA lt_act LIKE TABLE OF ls_i.
    DATA lt_exp LIKE TABLE OF ls_i.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_languages.
    DATA lo_p TYPE REF TO zcl_abapgit_i18n_params.

    APPEND 'DE' TO lt_filter.
    APPEND 'EN' TO lt_filter.

    ls_i-spras = 'E'.
    APPEND ls_i TO lt_act.
    ls_i-spras = 'D'.
    APPEND ls_i TO lt_act.
    ls_i-spras = 'S'.
    APPEND ls_i TO lt_act.

    ls_i-spras = 'E'.
    APPEND ls_i TO lt_exp.
    ls_i-spras = 'D'.
    APPEND ls_i TO lt_exp.

    lo_p = zcl_abapgit_i18n_params=>new(
      iv_main_language = '?'
      it_translation_langs = lt_filter ).
    lo_p->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " Keep master lang
    CLEAR lt_filter.
    APPEND 'DE' TO lt_filter.

    lo_p = zcl_abapgit_i18n_params=>new(
      iv_main_language = 'E'
      it_translation_langs = lt_filter ).
    lo_p->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name  = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " Empty filter
    CLEAR: lt_act, lt_exp, lt_filter.
    ls_i-spras = 'E'.
    APPEND ls_i TO lt_act.
    ls_i-spras = 'D'.
    APPEND ls_i TO lt_act.

    ls_i-spras = 'E'.
    APPEND ls_i TO lt_exp.
    ls_i-spras = 'D'.
    APPEND ls_i TO lt_exp.

    lo_p = zcl_abapgit_i18n_params=>new(
      iv_main_language = '?'
      it_translation_langs = lt_filter ).
    lo_p->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD normalize_obj_patterns.

    DATA lt_patterns TYPE string_table.
    DATA lv_pattern TYPE string.

    APPEND '  zcl_XY.clas' TO lt_patterns.
    APPEND 'pkg/*' TO lt_patterns.
    lt_patterns = zcl_abapgit_i18n_params=>normalize_obj_patterns( lt_patterns ).
    READ TABLE lt_patterns INDEX 1 INTO lv_pattern.
    cl_abap_unit_assert=>assert_equals(
      act = lv_pattern
      exp = 'zcl_xy.clas' ).

  ENDMETHOD.

  METHOD match_obj_patterns.

    DATA lt_patterns TYPE string_table.
    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.

    APPEND '*/no_tran/*' TO lt_patterns.
    APPEND 'no_tran2/*' TO lt_patterns. " Arguable feature
    APPEND '*/c1.clas' TO lt_patterns.
    APPEND 'c2.clas' TO lt_patterns.
    APPEND '*.devc' TO lt_patterns.

    ls_tadir-path     = '/src/'.
    ls_tadir-object   = 'TABL'.
    ls_tadir-obj_name = 'T1'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_i18n_params=>match_obj_patterns(
        it_wo_translation_patterns = lt_patterns
        is_tadir = ls_tadir ) ).

    ls_tadir-path     = '/src/'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'C1'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_i18n_params=>match_obj_patterns(
        it_wo_translation_patterns = lt_patterns
        is_tadir = ls_tadir ) ).

    ls_tadir-path     = '/src/'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'C2'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_i18n_params=>match_obj_patterns(
        it_wo_translation_patterns = lt_patterns
        is_tadir = ls_tadir ) ).

    ls_tadir-path     = '/src/'.
    ls_tadir-object   = 'DEVC'.
    ls_tadir-obj_name = 'PACKAGE'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_i18n_params=>match_obj_patterns(
        it_wo_translation_patterns = lt_patterns
        is_tadir = ls_tadir ) ).

    ls_tadir-path     = '/src/no_tran/'.
    ls_tadir-object   = 'TABL'.
    ls_tadir-obj_name = 'T1'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_i18n_params=>match_obj_patterns(
        it_wo_translation_patterns = lt_patterns
        is_tadir = ls_tadir ) ).

    ls_tadir-path     = '/src/no_tran2/'.
    ls_tadir-object   = 'TABL'.
    ls_tadir-obj_name = 'T2'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_i18n_params=>match_obj_patterns(
        it_wo_translation_patterns = lt_patterns
        is_tadir = ls_tadir ) ).

  ENDMETHOD.

ENDCLASS.
