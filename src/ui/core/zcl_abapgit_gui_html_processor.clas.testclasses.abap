CLASS ltcl_gui_mock DEFINITION FOR TESTING FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_cache_signature,
        url  TYPE string,
        type TYPE string,
        data TYPE string,
      END OF ty_cache_signature.

    INTERFACES zif_abapgit_gui_services.

    METHODS get_asset RETURNING VALUE(rs_asset) TYPE ty_cache_signature.

  PRIVATE SECTION.
    DATA ms_last_cache_signature TYPE ty_cache_signature.
ENDCLASS.

CLASS ltcl_gui_mock IMPLEMENTATION.
  METHOD zif_abapgit_gui_services~cache_asset.
    ms_last_cache_signature-url  = iv_url.
    ms_last_cache_signature-type = iv_type && '/' && iv_subtype.
    ms_last_cache_signature-data = iv_text.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~register_event_handler.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_current_page_name.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_hotkeys_ctl.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_html_parts.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_log.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~register_page_asset.
  ENDMETHOD.

  METHOD get_asset.
    rs_asset = ms_last_cache_signature.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_html_processor_test DEFINITION DEFERRED.
CLASS zcl_abapgit_gui_html_processor DEFINITION LOCAL FRIENDS ltcl_html_processor_test.

CLASS ltcl_html_processor_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA mv_source TYPE string.
    DATA mo_cut TYPE REF TO zcl_abapgit_gui_html_processor.
    DATA mo_gui_mock TYPE REF TO ltcl_gui_mock.

    METHODS render_html
      IMPORTING
        iv_src         TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS setup RAISING zcx_abapgit_exception.
    METHODS process_typical FOR TESTING RAISING zcx_abapgit_exception.
    METHODS process_with_preserve FOR TESTING RAISING zcx_abapgit_exception.
    METHODS process_no_css FOR TESTING RAISING zcx_abapgit_exception.
    METHODS process_fails FOR TESTING RAISING zcx_abapgit_exception.
    METHODS find_head_closing_tag FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_html_processor_test IMPLEMENTATION.

  METHOD render_html.
    rv_html = iv_src.
    REPLACE ALL OCCURRENCES OF '\n' IN rv_html WITH cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD setup.

    DATA li_asset_man TYPE REF TO zif_abapgit_gui_asset_manager.

    li_asset_man = zcl_abapgit_gui_asset_manager=>create( ).

    li_asset_man->register_asset( iv_url = 'css/style1.css'
                                  iv_type = 'text/css'
                                  iv_inline = 'dummy1' ).
    li_asset_man->register_asset( iv_url = 'css/style2.css'
                                  iv_type = 'text/css'
                                  iv_inline = 'dummy2' ).
    li_asset_man->register_asset( iv_url = 'css/style3.css'
                                  iv_type = 'text/css'
                                  iv_inline = 'dummy3' ).

    CREATE OBJECT mo_cut
      EXPORTING
        ii_asset_man = li_asset_man.

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
        `    <!--<LINK  rel="stylesheet" type="text/css" href="css/style1.css">-->\n` &&
        `    <!--<link rel="stylesheet" type="text/css" href="css/style2.css">-->\n` &&
        `    <!--<link rel="stylesheet" type="text/css" href="css/style3.css">-->\n` &&
        `    <script type="text/javascript" src="js/common.js"></script>\n` &&
        `    <!-- abapgit HTML preprocessor -->\n` &&
        `    <link rel="stylesheet" type="text/css" href="css/bundle.css">\n` &&
        `  </head>\n` &&
        `  <body>hello</body>\n` &&
        `</html>\n`
      ) ).

    " GUI call checks
    cl_abap_unit_assert=>assert_equals(
      act = mo_gui_mock->get_asset( )-url
      exp = 'css/bundle.css' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_gui_mock->get_asset( )-type
      exp = 'text/css' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_gui_mock->get_asset( )-data
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
        `    <!--<LINK  rel="stylesheet" type="text/css" href="css/style1.css">-->\n` &&
        `    <link rel="stylesheet" type="text/css" href="css/style2.css">\n` && " Preserved
        `    <!--<link rel="stylesheet" type="text/css" href="css/style3.css">-->\n` &&
        `    <script type="text/javascript" src="js/common.js"></script>\n` &&
        `    <!-- abapgit HTML preprocessor -->\n` &&
        `    <link rel="stylesheet" type="text/css" href="css/bundle.css">\n` &&
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


  METHOD find_head_closing_tag.
    "given
    DATA: lv_head_end TYPE i,
          lv_html     TYPE string.

    lv_html = '<!DOCTYPE html><html><head><title>abapGit</title><link rel="stylesheet" type="text/css" ' &&
              'href="css/common.css"><link rel="stylesheet" type="text/css" href="css/ag-icons.css">' &&
              '<link rel="stylesheet" type="text/css" href="css/theme-default.css"><script type="text/javascript"' &&
              ' src="js/common.js"></script></head>'.

    "when
    TRY.
        lv_head_end = mo_cut->find_head_offset( iv_html = lv_html ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( msg = 'HEAD closing tag could not be found' ).
    ENDTRY.

    "then
    cl_abap_unit_assert=>assert_equals(
          act = lv_head_end
          exp = 299
          msg = 'Head closing tag was not found' ).

  ENDMETHOD.

ENDCLASS.
