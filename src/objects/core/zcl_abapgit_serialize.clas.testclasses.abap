CLASS ltcl_determine_max_threads DEFINITION DEFERRED.
CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS ltcl_determine_max_threads.

CLASS ltcl_determine_max_threads DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      determine_max_threads FOR TESTING RAISING zcx_abapgit_exception,
      force FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_determine_max_threads IMPLEMENTATION.

  METHOD setup.
    TRY.
        CREATE OBJECT mo_cut.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
  ENDMETHOD.

  METHOD determine_max_threads.

    DATA: lv_threads TYPE i.

    lv_threads = mo_cut->determine_max_threads( ).

    cl_abap_unit_assert=>assert_differs(
      act = lv_threads
      exp = 0 ).

  ENDMETHOD.

  METHOD force.

    DATA: lv_threads TYPE i.

    lv_threads = mo_cut->determine_max_threads( abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_threads
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_serialize DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_dot TYPE REF TO zcl_abapgit_dot_abapgit,
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      test FOR TESTING RAISING zcx_abapgit_exception,
      unsupported FOR TESTING RAISING zcx_abapgit_exception,
      ignored FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_serialize IMPLEMENTATION.

  METHOD setup.

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    TRY.
        CREATE OBJECT mo_cut
          EXPORTING
            io_dot_abapgit = mo_dot.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_sequential TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_parallel   TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'PROG'.
    <ls_tadir>-obj_name = 'RSABAPPROGRAM'.
    <ls_tadir>-devclass = 'PACKAGE'.
    <ls_tadir>-path     = 'foobar'.
    <ls_tadir>-masterlang = sy-langu.

    lt_sequential = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_true ).

    lt_parallel = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_sequential
      exp = lt_parallel ).

  ENDMETHOD.

  METHOD unsupported.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_msg   TYPE zif_abapgit_log=>ty_log_out,
          lt_msg   TYPE zif_abapgit_log=>ty_log_outs,
          li_log1  TYPE REF TO zif_abapgit_log,
          li_log2  TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'ABCD'.
    <ls_tadir>-obj_name = 'OBJECT'.

    CREATE OBJECT li_log1 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log1
      iv_force_sequential = abap_true ).

    CREATE OBJECT li_log2 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log2
      iv_force_sequential = abap_false ).

    lt_msg = li_log1->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object type ABCD not supported*' ).

    lt_msg = li_log2->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object type ABCD not supported*' ).

  ENDMETHOD.

  METHOD ignored.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_msg   TYPE zif_abapgit_log=>ty_log_out,
          lt_msg   TYPE zif_abapgit_log=>ty_log_outs,
          li_log1  TYPE REF TO zif_abapgit_log,
          li_log2  TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    mo_dot->add_ignore(
      iv_path     = '/src/'
      iv_filename = 'zcl_test_ignore.clas.*' ).

    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'CLAS'.
    <ls_tadir>-obj_name = 'ZCL_TEST'.
    <ls_tadir>-devclass = '$ZTEST'.

    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'CLAS'.
    <ls_tadir>-obj_name = 'ZCL_TEST_IGNORE'.
    <ls_tadir>-devclass = '$ZTEST'.

    CREATE OBJECT li_log1 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      iv_package          = '$ZTEST'
      it_tadir            = lt_tadir
      ii_log              = li_log1
      iv_force_sequential = abap_true ).

    CREATE OBJECT li_log2 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      iv_package          = '$ZTEST'
      it_tadir            = lt_tadir
      ii_log              = li_log2
      iv_force_sequential = abap_false ).

    lt_msg = li_log1->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object CLAS ZCL_TEST_IGNORE ignored*' ).

    lt_msg = li_log2->get_messages( ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_msg-text
      exp = '*Object CLAS ZCL_TEST_IGNORE ignored*' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_i18n DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_english TYPE sy-langu VALUE 'E',
      c_german  TYPE sy-langu VALUE 'D'.

    DATA:
      mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
      mo_cut         TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      test FOR TESTING RAISING zcx_abapgit_exception.


ENDCLASS.


CLASS ltcl_i18n IMPLEMENTATION.

  METHOD setup.
    DATA ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

    " Main language: English, Translations: German
    ls_data-master_language = c_english.
    " ls_data-i18n_languages needs to be initial to get classic I18N data

    TRY.
        CREATE OBJECT mo_dot_abapgit
          EXPORTING
            is_data = ls_data.

        CREATE OBJECT mo_cut
          EXPORTING
            io_dot_abapgit = mo_dot_abapgit.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error creating serializer' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_result     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lv_xml        TYPE string,
          lo_input      TYPE REF TO zcl_abapgit_xml_input,
          ls_dd02v      TYPE dd02v,
          lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS: <ls_tadir>      LIKE LINE OF lt_tadir,
                   <ls_result>     LIKE LINE OF lt_result,
                   <ls_i18n_langs> LIKE LINE OF lt_i18n_langs.

    " Assumption: Table T100 has at least English and German description
    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'TABL'.
    <ls_tadir>-obj_name = 'T100'.
    <ls_tadir>-devclass = 'PACKAGE'.
    <ls_tadir>-path     = 'foobar'.

    lt_result = mo_cut->serialize( lt_tadir ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1 ).

    READ TABLE lt_result ASSIGNING <ls_result> INDEX 1.
    ASSERT sy-subrc = 0.

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_result>-file-data ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.

    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'DD02V'
                                          CHANGING  cg_data = ls_dd02v ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd02v-ddlanguage
      exp = c_english ).

    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'I18N_LANGS'
                                          CHANGING  cg_data = lt_i18n_langs ).

    cl_abap_unit_assert=>assert_not_initial( lt_i18n_langs ).

    READ TABLE lt_i18n_langs ASSIGNING <ls_i18n_langs> WITH KEY table_line = c_german.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.
ENDCLASS.
