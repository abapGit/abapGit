CLASS zcl_abapgit_gui_page_debuginfo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.
    METHODS render_debug_info
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING   zcx_abapgit_exception.
    METHODS render_supported_object_types
      RETURNING VALUE(rv_html) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DEBUGINFO IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'DEBUG INFO'.
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="debug_info" class="debug_container">' ).
    ro_html->add( render_debug_info( ) ).
    ro_html->add( render_supported_object_types( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_debug_info.

    DATA: lt_ver_tab     TYPE filetable,
          lv_rc          TYPE i,
          lv_gui_version TYPE string,
          ls_version     LIKE LINE OF lt_ver_tab.

    cl_gui_frontend_services=>get_gui_version(
      CHANGING version_table = lt_ver_tab rc = lv_rc
      EXCEPTIONS OTHERS = 1 ).
    READ TABLE lt_ver_tab INTO ls_version INDEX 1.
    lv_gui_version = ls_version-filename.
    READ TABLE lt_ver_tab INTO ls_version INDEX 2.
    lv_gui_version = |{ lv_gui_version }.{ ls_version-filename }|.

    CREATE OBJECT ro_html.

    ro_html->add( |<p>abapGit version: { zif_abapgit_version=>gc_abap_version }</p>| ).
    ro_html->add( |<p>XML version:     { zif_abapgit_version=>gc_xml_version }</p>| ).
    ro_html->add( |<p>GUI version:     { lv_gui_version }</p>| ).
    ro_html->add( |<p>LCL_TIME:        { zcl_abapgit_time=>get_unix( ) }</p>| ).
    ro_html->add( |<p>SY time:         { sy-datum } { sy-uzeit } { sy-tzone }</p>| ).

  ENDMETHOD.


  METHOD render_supported_object_types.

    DATA: lv_list  TYPE string,
          lt_types TYPE zcl_abapgit_objects=>ty_types_tt,
          lv_type  LIKE LINE OF lt_types.


    lt_types = zcl_abapgit_objects=>supported_list( ).

    LOOP AT lt_types INTO lv_type.
      IF lv_list IS INITIAL.
        lv_list = lv_type.
      ELSE.
        lv_list = lv_list && `, ` && lv_type.
      ENDIF.
    ENDLOOP.

    rv_html = |<p>Supported objects: { lv_list }</p>|.

  ENDMETHOD.


  METHOD scripts.

    ro_html = super->scripts( ).

    ro_html->add( 'debugOutput("Browser: " + navigator.userAgent + ' &&
      '"<br>Frontend time: " + new Date(), "debug_info");' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
