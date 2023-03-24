CLASS ltcl_lxe_texts DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      filter_sap_langs FOR TESTING RAISING zcx_abapgit_exception,
      filter_sap_langs_tab FOR TESTING RAISING zcx_abapgit_exception,
      apply_iso_langs_to_lang_filter FOR TESTING RAISING zcx_abapgit_exception,
      check_langs_versus_installed FOR TESTING RAISING zcx_abapgit_exception,
      lang_string_to_table FOR TESTING,
      table_to_lang_string FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_lxe_texts DEFINITION LOCAL FRIENDS ltcl_lxe_texts.

CLASS ltcl_lxe_texts IMPLEMENTATION.

  METHOD check_langs_versus_installed.

    DATA lt_testsuite TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_installed TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_intersections_act TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_intersections_exp TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_missfits_act TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_missfits_exp TYPE zif_abapgit_definitions=>ty_languages.

    lt_installed         = zcl_abapgit_lxe_texts=>convert_lang_string_to_table( 'EN,DE,ES,IT,SK,FR' ).
    lt_testsuite         = zcl_abapgit_lxe_texts=>convert_lang_string_to_table( 'ZA,EN,FR,DE,JP' ).
    lt_intersections_exp = zcl_abapgit_lxe_texts=>convert_lang_string_to_table( 'EN,FR,DE' ).
    lt_missfits_exp      = zcl_abapgit_lxe_texts=>convert_lang_string_to_table( 'ZA,JP' ).

    zcl_abapgit_lxe_texts=>check_langs_versus_installed(
      EXPORTING
        it_languages = lt_testsuite
        it_installed = lt_installed
      IMPORTING
        et_intersection = lt_intersections_act
        et_missfits     = lt_missfits_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_intersections_act
      exp = lt_intersections_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_missfits_act
      exp = lt_missfits_exp ).

  ENDMETHOD.

  METHOD lang_string_to_table.

    DATA lt_act TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_exp TYPE zif_abapgit_definitions=>ty_languages.

    TRY.
        lt_act = zcl_abapgit_lxe_texts=>convert_lang_string_to_table(
                   iv_langs              = 'en , de, es'
                   iv_skip_main_language = 'E' ).

        APPEND 'DE' TO lt_exp.
        APPEND 'ES' TO lt_exp.

        cl_abap_unit_assert=>assert_equals(
          act = lt_act
          exp = lt_exp ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    TRY.
        lt_act = zcl_abapgit_lxe_texts=>convert_lang_string_to_table(
                   iv_langs              = 'en , DE, es'
                   iv_skip_main_language = 'E' ).

        cl_abap_unit_assert=>assert_equals(
          act = lt_act
          exp = lt_exp ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    TRY.
        lt_act = zcl_abapgit_lxe_texts=>convert_lang_string_to_table(
                   iv_langs              = '*'
                   iv_skip_main_language = 'E' ).

        CLEAR lt_exp.
        APPEND '*' TO lt_exp.

        cl_abap_unit_assert=>assert_equals(
          act = lt_act
          exp = lt_exp ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD table_to_lang_string.

    DATA lt_langu TYPE zif_abapgit_definitions=>ty_languages.
    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.

    TRY.
        APPEND 'DE' TO lt_langu.
        APPEND 'ES' TO lt_langu.

        lv_act = zcl_abapgit_lxe_texts=>convert_table_to_lang_string( lt_langu ).
        lv_exp = 'DE,ES'.

        cl_abap_unit_assert=>assert_equals(
          act = lv_act
          exp = lv_exp ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    TRY.
        CLEAR lt_langu.
        APPEND '*' TO lt_langu.

        lv_act = zcl_abapgit_lxe_texts=>convert_table_to_lang_string( lt_langu ).
        lv_exp = '*'.

        cl_abap_unit_assert=>assert_equals(
          act = lv_act
          exp = lv_exp ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD filter_sap_langs.

    DATA lt_act TYPE zif_abapgit_definitions=>ty_sap_langu_tab.
    DATA lt_exp TYPE zif_abapgit_definitions=>ty_sap_langu_tab.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_languages.

    APPEND 'DE' TO lt_filter.
    APPEND 'EN' TO lt_filter.

    APPEND 'E' TO lt_act.
    APPEND 'D' TO lt_act.
    APPEND 'S' TO lt_act.

    APPEND 'E' TO lt_exp.
    APPEND 'D' TO lt_exp.

    zcl_abapgit_lxe_texts=>trim_saplangu_by_iso(
      EXPORTING
        it_iso_filter = lt_filter
      CHANGING
        ct_sap_langs  = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " Empty filter
    CLEAR: lt_act, lt_exp, lt_filter.
    APPEND 'E' TO lt_act.
    APPEND 'D' TO lt_act.

    APPEND 'E' TO lt_exp.
    APPEND 'D' TO lt_exp.

    zcl_abapgit_lxe_texts=>trim_saplangu_by_iso(
      EXPORTING
        it_iso_filter = lt_filter
      CHANGING
        ct_sap_langs  = lt_act ).

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

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = lt_filter
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " Keep master lang
    CLEAR lt_filter.
    APPEND 'DE' TO lt_filter.

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = lt_filter
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = 'E'
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

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = lt_filter
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD apply_iso_langs_to_lang_filter.

    DATA lt_filter TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_act TYPE zif_abapgit_environment=>ty_system_language_filter.
    DATA lt_exp TYPE zif_abapgit_environment=>ty_system_language_filter.
    DATA ls_range LIKE LINE OF lt_act.

    APPEND 'EN' TO lt_filter.
    APPEND 'DE' TO lt_filter.

    ls_range-sign   = 'E'.
    ls_range-option = 'EQ'.
    ls_range-low    = '!'.
    APPEND ls_range TO lt_act.
    APPEND ls_range TO lt_exp.

    ls_range-sign   = 'I'.
    ls_range-option = 'EQ'.
    ls_range-low    = 'E'.
    APPEND ls_range TO lt_exp.
    ls_range-low    = 'D'.
    APPEND ls_range TO lt_exp.

    zcl_abapgit_lxe_texts=>add_iso_langs_to_lang_filter(
      EXPORTING
        it_iso_filter = lt_filter
      CHANGING
        ct_language_filter = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

ENDCLASS.
