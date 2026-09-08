CLASS ltd_frontend_services DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_frontend_services.

    METHODS constructor
      IMPORTING
        !iv_is_webgui             TYPE abap_bool
        !iv_is_sapgui_for_windows TYPE abap_bool.

  PRIVATE SECTION.

    DATA mv_is_webgui TYPE abap_bool.
    DATA mv_is_sapgui_for_windows TYPE abap_bool.

ENDCLASS.


CLASS ltd_frontend_services IMPLEMENTATION.

  METHOD constructor.
    mv_is_webgui = iv_is_webgui.
    mv_is_sapgui_for_windows = iv_is_sapgui_for_windows.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~is_webgui.
    rv_is_webgui = mv_is_webgui.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~is_sapgui_for_windows.
    rv_result = mv_is_sapgui_for_windows.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~is_sapgui_for_java.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~clipboard_export.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~directory_browse.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~directory_create.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~directory_exist.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~execute.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~file_download.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~file_upload.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_file_separator.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_gui_type.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_gui_version.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~get_system_directory.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~gui_is_available.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~open_ie_devtools.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~show_file_open_dialog.
  ENDMETHOD.

  METHOD zif_abapgit_frontend_services~show_file_save_dialog.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_render_environment DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS teardown.

    METHODS sapgui_for_html FOR TESTING RAISING cx_static_check.
    METHODS sapgui_for_windows FOR TESTING RAISING cx_static_check.
    METHODS sapgui_for_java FOR TESTING RAISING cx_static_check.

    METHODS assert_seeded
      IMPORTING
        !iv_is_webgui             TYPE abap_bool
        !iv_is_sapgui_for_windows TYPE abap_bool
        !iv_exp                   TYPE string
      RAISING
        cx_static_check.

ENDCLASS.

CLASS zcl_abapgit_gui_page DEFINITION LOCAL FRIENDS ltcl_render_environment.


CLASS ltcl_render_environment IMPLEMENTATION.

  METHOD teardown.

    DATA li_frontend_services TYPE REF TO zif_abapgit_frontend_services.

    " Unbound, so the factory creates the real implementation again
    zcl_abapgit_ui_injector=>set_frontend_services( li_frontend_services ).

  ENDMETHOD.

  METHOD assert_seeded.

    DATA li_frontend_services TYPE REF TO zif_abapgit_frontend_services.
    DATA li_html TYPE REF TO zif_abapgit_html.
    DATA lv_act TYPE string.

    CREATE OBJECT li_frontend_services TYPE ltd_frontend_services
      EXPORTING
        iv_is_webgui             = iv_is_webgui
        iv_is_sapgui_for_windows = iv_is_sapgui_for_windows.

    zcl_abapgit_ui_injector=>set_frontend_services( li_frontend_services ).

    CREATE OBJECT li_html TYPE zcl_abapgit_html.

    zcl_abapgit_gui_page=>render_environment( li_html ).

    " Layout is formatting; the contract common.js relies on is the call, the
    " key names and the JS literals, so compare without any whitespace
    lv_act = li_html->render( iv_no_line_breaks = abap_true ).
    CONDENSE lv_act NO-GAPS.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = iv_exp ).

  ENDMETHOD.

  METHOD sapgui_for_html.

    assert_seeded(
      iv_is_webgui             = abap_true
      iv_is_sapgui_for_windows = abap_false
      iv_exp                   = `setEnvironment({isWebGui:true,isSapGuiForWindows:false});` ).

  ENDMETHOD.

  METHOD sapgui_for_windows.

    assert_seeded(
      iv_is_webgui             = abap_false
      iv_is_sapgui_for_windows = abap_true
      iv_exp                   = `setEnvironment({isWebGui:false,isSapGuiForWindows:true});` ).

  ENDMETHOD.

  METHOD sapgui_for_java.

    assert_seeded(
      iv_is_webgui             = abap_false
      iv_is_sapgui_for_windows = abap_false
      iv_exp                   = `setEnvironment({isWebGui:false,isSapGuiForWindows:false});` ).

  ENDMETHOD.

ENDCLASS.
