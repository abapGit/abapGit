*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DEBUG
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_debuginfo DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

    METHODS styles
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS scripts
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

  PRIVATE SECTION.
    METHODS render_debug_info
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.                       "lcl_gui_page_debuginfo

CLASS lcl_gui_page_debuginfo IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DEBUG INFO' ) ).
    ro_html->add( render_debug_info( ) ).
    ro_html->add( footer( io_include_script = scripts( ) ) ).

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

    ro_html->add( '<div id="debug_info" class="debug_container">' ).
    ro_html->add( |abapGit version: { gc_abap_version }<br>| ).
    ro_html->add( |XML version:     { gc_xml_version }<br>| ).
    ro_html->add( |GUI version:     { lv_gui_version }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD. "render_debug_info

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DEBUG INFO STYLES */'.
    _add 'div.debug_container {'.
    _add '  padding: 0.5em;'.
    _add '  font-size: 10pt;'.
    _add '  color: #444;'.
    _add '  font-family: Consolas, Courier, monospace;'.
    _add '}'.

  ENDMETHOD.

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'debugOutput("Browser: " + navigator.userAgent, "debug_info");' ).

  ENDMETHOD.  "scripts

ENDCLASS.                       "lcl_gui_page_debuginfo