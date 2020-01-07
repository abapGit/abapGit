CLASS ltcl_abapgit_gui_asset_manager DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS get_inline_asset FOR TESTING RAISING zcx_abapgit_exception.
    METHODS get_text_asset FOR TESTING RAISING zcx_abapgit_exception.
    METHODS get_mime_asset FOR TESTING RAISING zcx_abapgit_exception.
    METHODS get_base64_asset FOR TESTING RAISING zcx_abapgit_exception.
    METHODS get_all FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_abapgit_gui_asset_manager IMPLEMENTATION.

  METHOD get_inline_asset.

    DATA lo_assetman TYPE REF TO zcl_abapgit_gui_asset_manager.
    DATA ls_asset TYPE zif_abapgit_gui_asset_manager=>ty_web_asset.

    CREATE OBJECT lo_assetman.

    lo_assetman->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_inline    = 'ABC' ).

    ls_asset = lo_assetman->zif_abapgit_gui_asset_manager~get_asset( 'css/common.css' ).

    cl_abap_unit_assert=>assert_equals( act = ls_asset-type    exp = 'text' ).
    cl_abap_unit_assert=>assert_equals( act = ls_asset-subtype exp = 'css' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_convert=>xstring_to_string_utf8( ls_asset-content )
      exp = 'ABC' ).

  ENDMETHOD.

  METHOD get_text_asset.

    DATA lo_assetman TYPE REF TO zcl_abapgit_gui_asset_manager.
    CREATE OBJECT lo_assetman.

    lo_assetman->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_inline    = 'ABC' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_assetman->zif_abapgit_gui_asset_manager~get_text_asset( 'css/common.css' )
      exp = 'ABC' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_assetman->zif_abapgit_gui_asset_manager~get_text_asset(
        iv_url = 'css/common.css'
        iv_assert_subtype = 'css' )
      exp = 'ABC' ).

    TRY.
        lo_assetman->zif_abapgit_gui_asset_manager~get_text_asset(
          iv_url = 'css/common.css'
          iv_assert_subtype = 'xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        " no futher check
    ENDTRY.

    lo_assetman->register_asset(
      iv_url       = 'css/common.xyz'
      iv_type      = 'nottext/bin'
      iv_inline    = 'XYZ' ).

    TRY.
        lo_assetman->zif_abapgit_gui_asset_manager~get_text_asset( 'css/common.xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        " no futher check
    ENDTRY.

  ENDMETHOD.

  METHOD get_mime_asset.

    DATA lo_assetman TYPE REF TO zcl_abapgit_gui_asset_manager.
    CREATE OBJECT lo_assetman.

    lo_assetman->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_COMMON' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lo_assetman->zif_abapgit_gui_asset_manager~get_text_asset( 'css/common.css' )
      exp = '*ABAPGIT COMMON CSS*' ).

  ENDMETHOD.

  METHOD get_base64_asset.

    DATA lo_assetman TYPE REF TO zcl_abapgit_gui_asset_manager.
    CREATE OBJECT lo_assetman.

    lo_assetman->register_asset(
      iv_url    = 'css/common.css'
      iv_type   = 'text/css'
      iv_base64 = 'QEE=' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_assetman->zif_abapgit_gui_asset_manager~get_text_asset( 'css/common.css' )
      exp = '@A' ).

  ENDMETHOD.

  METHOD get_all.

    DATA lo_assetman TYPE REF TO zcl_abapgit_gui_asset_manager.
    CREATE OBJECT lo_assetman.

    lo_assetman->register_asset(
      iv_url    = 'css/common.css'
      iv_type   = 'text/css'
      iv_base64 = 'QEE=' ).

    lo_assetman->register_asset(
      iv_url    = 'css/common.css'
      iv_type   = 'text/css'
      iv_inline = 'ABC' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_assetman->zif_abapgit_gui_asset_manager~get_all_assets( ) )
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.
