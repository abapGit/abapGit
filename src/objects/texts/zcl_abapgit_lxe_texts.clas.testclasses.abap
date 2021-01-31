CLASS ltcl_lxe_texts DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      lang_string_to_table FOR TESTING,
      table_to_lang_string FOR TESTING.

ENDCLASS.

CLASS ltcl_lxe_texts IMPLEMENTATION.

  METHOD lang_string_to_table.

    DATA lv_langu TYPE string.
    DATA lt_act TYPE zif_abapgit_definitions=>ty_languages.
    DATA lt_exp TYPE zif_abapgit_definitions=>ty_languages.

    TRY.
        lt_act = zcl_abapgit_lxe_texts=>convert_lang_string_to_table(
                   iv_langs              = 'en , de, es'
                   iv_skip_main_language = 'E' ).

        APPEND 'D' TO lt_exp.
        APPEND 'S' TO lt_exp.

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
        APPEND 'D' TO lt_langu.
        APPEND 'S' TO lt_langu.

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

ENDCLASS.
