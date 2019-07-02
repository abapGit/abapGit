CLASS lcl_gui_mock DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_cache_signature,
        url TYPE string,
        type TYPE string,
        data TYPE string,
      END OF ty_cache_signature.
    DATA ms_last_cache_signature TYPE ty_cache_signature.

    INTERFACES zif_abapgit_gui_services.
ENDCLASS.

CLASS lcl_gui_mock IMPLEMENTATION.
  METHOD zif_abapgit_gui_services~cache_asset.
    ms_last_cache_signature-url  = iv_url.
    ms_last_cache_signature-type = iv_type && '/' && iv_subtype.
    ms_last_cache_signature-data = iv_text.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_html_processor_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA mv_source TYPE string.
    DATA mo_cut TYPE REF TO zcl_abapgit_gui_html_processor.
    DATA mo_gui_mock TYPE REF TO lcl_gui_mock.

    METHODS render_html
      IMPORTING
        iv_src TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS setup.
    METHODS process_typical FOR TESTING RAISING zcx_abapgit_exception.
    METHODS process_with_preserve FOR TESTING RAISING zcx_abapgit_exception.
    METHODS process_no_css FOR TESTING RAISING zcx_abapgit_exception.
    METHODS process_fails FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_html_processor_test IMPLEMENTATION.

  METHOD render_html.
    rv_html = iv_src.
    REPLACE ALL OCCURRENCES OF '\n' IN rv_html WITH cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD setup.

    DATA lo_asset_man TYPE REF TO zcl_abapgit_gui_asset_manager.

    CREATE OBJECT lo_asset_man.
    lo_asset_man->register_asset( iv_url = 'css/style1.css' iv_type = 'text/css' iv_inline = 'dummy1' ).
    lo_asset_man->register_asset( iv_url = 'css/style2.css' iv_type = 'text/css' iv_inline = 'dummy2' ).
    lo_asset_man->register_asset( iv_url = 'css/style3.css' iv_type = 'text/css' iv_inline = 'dummy3' ).

    CREATE OBJECT mo_cut
      EXPORTING
        ii_asset_man = lo_asset_man.

    CREATE OBJECT mo_gui_mock.

    mv_source = render_html(
      `<html>\n` &&
      `  <head>\n` &&
      `    <title>abapGit</title>\n` &&
      `    <LINK  rel="stylesheet" type="text/css" href="css/style1.css">\n` && " +case & extra space
      `    <link rel="stylesheet" type="text/css" href="css/style2.css">\n` &&
      `    <link rel="stylesheet" type="text/css" href="css/style3.css">\n` &&
      `    <script type="text/javascript" src="js/common.js"></script>\n` &&
      `  </head>\n` &&
      `  <body>hello</body>\n` &&
      `</html>\n` ).

  ENDMETHOD.

  METHOD process_typical.

    DATA lv_act TYPE string.

    lv_act = mo_cut->zif_abapgit_gui_html_processor~process(
      iv_html = mv_source
      ii_gui_services = mo_gui_mock ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = render_html(
        `<html>\n` &&
        `  <head>\n` &&
        `    <title>abapGit</title>\n` &&
        `    <!-- by AG HTML preprocessor <LINK  rel="stylesheet" type="text/css" href="css/style1.css">-->\n` &&
        `    <!-- by AG HTML preprocessor <link rel="stylesheet" type="text/css" href="css/style2.css">-->\n` &&
        `    <!-- by AG HTML preprocessor <link rel="stylesheet" type="text/css" href="css/style3.css">-->\n` &&
        `    <script type="text/javascript" src="js/common.js"></script>\n` &&
        `    <!-- by AG HTML preprocessor --><link rel="stylesheet" type="text/css" href="css/bundle.css">\n` &&
        `  </head>\n` &&
        `  <body>hello</body>\n` &&
        `</html>\n`
      ) ).

      " GUI call checks
    cl_abap_unit_assert=>assert_equals(
      act = mo_gui_mock->ms_last_cache_signature-url
      exp = 'css/bundle.css' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_gui_mock->ms_last_cache_signature-type
      exp = 'text/css' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_gui_mock->ms_last_cache_signature-data
      exp = render_html( 'dummy1\ndummy2\ndummy3' ) ).

  ENDMETHOD.

  METHOD process_with_preserve.

    DATA lv_act TYPE string.

    mo_cut->preserve_css( 'css/style2.css' ).
    lv_act = mo_cut->zif_abapgit_gui_html_processor~process(
      iv_html = mv_source
      ii_gui_services = mo_gui_mock ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = render_html(
        `<html>\n` &&
        `  <head>\n` &&
        `    <title>abapGit</title>\n` &&
        `    <!-- by AG HTML preprocessor <LINK  rel="stylesheet" type="text/css" href="css/style1.css">-->\n` &&
        `    <link rel="stylesheet" type="text/css" href="css/style2.css">\n` && " Preserved
        `    <!-- by AG HTML preprocessor <link rel="stylesheet" type="text/css" href="css/style3.css">-->\n` &&
        `    <script type="text/javascript" src="js/common.js"></script>\n` &&
        `    <!-- by AG HTML preprocessor --><link rel="stylesheet" type="text/css" href="css/bundle.css">\n` &&
        `  </head>\n` &&
        `  <body>hello</body>\n` &&
        `</html>\n`
      ) ).

  ENDMETHOD.

  METHOD process_no_css.

    DATA lv_act TYPE string.

    lv_act = mo_cut->zif_abapgit_gui_html_processor~process(
      iv_html = '<html><head></head></html>'
      ii_gui_services = mo_gui_mock ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '<html><head></head></html>' ).

  ENDMETHOD.

  METHOD process_fails.

    TRY.
        " BTW this is valid HTML, maybe refactor the code ...
        mo_cut->zif_abapgit_gui_html_processor~process(
          iv_html = '<html><body></body></html>'
          ii_gui_services = mo_gui_mock ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
