CLASS ltcl_test_base DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT ABSTRACT.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      add_file IMPORTING iv_url     TYPE string
                         iv_content TYPE string OPTIONAL
               RAISING   zcx_abapgit_exception.
    DATA:
      mi_asset_manager TYPE REF TO zif_abapgit_gui_asset_manager,
      mo_cut           TYPE REF TO zcl_abapgit_gui_css_processor.
  PRIVATE SECTION.
    METHODS:
      setup.
ENDCLASS.

CLASS ltcl_test_base IMPLEMENTATION.
  METHOD setup.
    mi_asset_manager = zcl_abapgit_gui_asset_manager=>create( ).
    CREATE OBJECT mo_cut
      EXPORTING
        ii_asset_manager = mi_asset_manager.
  ENDMETHOD.

  METHOD add_file.
    mi_asset_manager->register_asset(
      iv_url = iv_url
      iv_type = 'text/css'
      iv_inline = iv_content ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_single_file DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT INHERITING FROM ltcl_test_base.
  PUBLIC SECTION.
    METHODS:
      test_file_exists FOR TESTING RAISING zcx_abapgit_exception,
      test_file_does_not_exist FOR TESTING,
      test_empty_file FOR TESTING RAISING zcx_abapgit_exception,
      test_no_variables FOR TESTING RAISING zcx_abapgit_exception,
      test_simple_variables FOR TESTING RAISING zcx_abapgit_exception,
      test_complex_variables FOR TESTING RAISING zcx_abapgit_exception,
      test_overwrite FOR TESTING RAISING zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_single_file IMPLEMENTATION.
  METHOD test_file_exists.
    add_file( iv_url = 'does/exist.css'
              iv_content = |body \{\}\n| ).
    mo_cut->add_file( 'does/exist.css' ).
    TRY.
        mo_cut->process( ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_file_does_not_exist.
    mo_cut->add_file( 'not/existing.css' ).
    TRY.
        mo_cut->process( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_empty_file.
    add_file( 'does/exist.css' ).
    mo_cut->add_file( 'does/exist.css' ).
    TRY.
        mo_cut->process( ).
        cl_abap_unit_assert=>fail( ). " Assetman fails on empty content
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_no_variables.
    DATA: lv_content TYPE string.

    lv_content =
      `body {\n` &&
      `  font-family: 'Arial', serif;\n` &&
      `  background: #000000;\n` &&
      `  color: #ffffff;\n` &&
      `}\n`.
    add_file( iv_url = 'novars.css'
              iv_content = lv_content ).

    mo_cut->add_file( 'novars.css' ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->process( )
                                        exp = lv_content ).
  ENDMETHOD.

  METHOD test_simple_variables.
    DATA: lv_content  TYPE string,
          lv_expected TYPE string.

    lv_content =
      `:root {\n` &&
      `  --my-bg-color: #123456;\n` &&
      `}\n` &&
      `body {\n` &&
      `  font-family: 'Arial', serif;\n` &&
      `  background: var(--my-bg-color);\n` &&
      `  color: #ffffff;\n` &&
      `}\n`.
    add_file( iv_url = 'simple.css'
              iv_content = lv_content ).

    lv_expected =
      `:root {\n` &&
      `  --my-bg-color: #123456;\n` &&
      `}\n` &&
      `body {\n` &&
      `  font-family: 'Arial', serif;\n` &&
      `  background: #123456;\n` &&
      `  color: #ffffff;\n` &&
      `}\n`.

    mo_cut->add_file( 'simple.css' ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->process( )
                                        exp = lv_expected ).
  ENDMETHOD.

  METHOD test_complex_variables.
    DATA: lv_content  TYPE string,
          lv_expected TYPE string.

    lv_content =
      `:root {\n` &&
      `  --primary-color: #0099FF;\n` &&
      `  --dark-percentage: 15%;\n` &&
      `  --primary-color-dark: calc(var(--primary-color) + var(--dark-percentage));\n` &&
      `  --my-bg-color: var(--primary-color-dark);\n` &&
      `}\n` &&
      `body {\n` &&
      `  font-family: 'Arial', serif;\n` &&
      `  background: var(--my-bg-color);\n` &&
      `  color: #ffffff;\n` &&
      `}\n`.
    add_file( iv_url = 'complex.css'
              iv_content = lv_content ).

    lv_expected =
      `:root {\n` &&
      `  --primary-color: #0099FF;\n` &&
      `  --dark-percentage: 15%;\n` &&
      `  --primary-color-dark: calc(#0099FF + 15%);\n` &&
      `  --my-bg-color: calc(#0099FF + 15%);\n` &&
      `}\n` &&
      `body {\n` &&
      `  font-family: 'Arial', serif;\n` &&
      `  background: calc(#0099FF + 15%);\n` &&
      `  color: #ffffff;\n` &&
      `}\n`.

    mo_cut->add_file( 'complex.css' ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->process( )
                                        exp = lv_expected ).
  ENDMETHOD.

  METHOD test_overwrite.
    DATA: lv_content  TYPE string,
          lv_expected TYPE string.

    lv_content =
      `:root {\n` &&
      `  --var3: 5;\n` &&
      `  --var1: var(--var2);\n` &&
      `  --var2: var(--var3);\n` &&
      `  --var3: 1;\n` &&
      `}\n` &&
      `body {\n` &&
      `  width: var(--var1);\n` &&
      `}\n`.
    add_file( iv_url = 'overwrite.css'
              iv_content = lv_content ).

    lv_expected =
      `:root {\n` &&
      `  --var3: 5;\n` &&
      `  --var1: 1;\n` &&
      `  --var2: 1;\n` &&
      `  --var3: 1;\n` &&
      `}\n` &&
      `body {\n` &&
      `  width: 1;\n` &&
      `}\n`.

    mo_cut->add_file( 'overwrite.css' ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->process( )
                                        exp = lv_expected ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_multiple_files DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT INHERITING FROM ltcl_test_base.
  PUBLIC SECTION.
    METHODS:
      test_simple FOR TESTING RAISING zcx_abapgit_exception,
      test_overwrite FOR TESTING RAISING zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_multiple_files IMPLEMENTATION.
  METHOD test_simple.
    DATA: lv_file1    TYPE string,
          lv_file2    TYPE string,
          lv_expected TYPE string.

    lv_file1 =
      `:root {\n` &&
      `  --var2: var(--var3);\n` &&
      `  --var3: 6;\n` &&
      `}`.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_file1 WITH cl_abap_char_utilities=>newline.
    add_file( iv_url = 'file1.css'
              iv_content = lv_file1 ).
    mo_cut->add_file( 'file1.css' ).

    lv_file2 =
      `:root {\n` &&
      `  --var3: 19;\n` &&
      `}`.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_file2 WITH cl_abap_char_utilities=>newline.
    add_file( iv_url = 'file2.css'
              iv_content = lv_file2 ).
    mo_cut->add_file( 'file2.css' ).

    lv_expected =
      `:root {\n` &&
      `  --var2: 19;\n` &&
      `  --var3: 6;\n` &&
      `}\n` &&
      `:root {\n` &&
      `  --var3: 19;\n` &&
      `}`.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_expected WITH cl_abap_char_utilities=>newline.

    cl_abap_unit_assert=>assert_equals( act = mo_cut->process( )
                                        exp = lv_expected ).
  ENDMETHOD.

  METHOD test_overwrite.
    DATA: lv_file1    TYPE string,
          lv_file2    TYPE string,
          lv_expected TYPE string.

    lv_file1 =
      `:root {\n` &&
      `  --var3: 5;\n` &&
      `  --var1: var(--var2);\n` &&
      `  --var2: var(--var3);\n` &&
      `  --var3: 6;\n` &&
      `}\n` &&
      `body {\n` &&
      `  width: var(--var1);\n` &&
      `}\n`.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_file1 WITH cl_abap_char_utilities=>newline.
    add_file( iv_url = 'file1.css'
              iv_content = lv_file1 ).
    mo_cut->add_file( 'file1.css' ).

    lv_file2 =
      `:root {\n` &&
      `  --var3: 19;\n` &&
      `}\n`.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_file2 WITH cl_abap_char_utilities=>newline.
    add_file( iv_url = 'file2.css'
              iv_content = lv_file2 ).
    mo_cut->add_file( 'file2.css' ).

    lv_expected =
      `:root {\n` &&
      `  --var3: 5;\n` &&
      `  --var1: 19;\n` &&
      `  --var2: 19;\n` &&
      `  --var3: 6;\n` &&
      `}\n` &&
      `body {\n` &&
      `  width: 19;\n` &&
      `}\n` &&
      `\n` &&
      `:root {\n` &&
      `  --var3: 19;\n` &&
      `}\n`.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_expected WITH cl_abap_char_utilities=>newline.

    cl_abap_unit_assert=>assert_equals( act = mo_cut->process( )
                                        exp = lv_expected ).
  ENDMETHOD.
ENDCLASS.
