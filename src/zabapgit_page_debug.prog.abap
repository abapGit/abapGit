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
    METHODS render_supported_object_types
        RETURNING VALUE(rv_html) TYPE string.

ENDCLASS.                       "lcl_gui_page_debuginfo

CLASS lcl_gui_page_debuginfo IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DEBUG INFO' ) ).

    ro_html->add( '<div id="debug_info" class="debug_container">' ).
    ro_html->add( render_debug_info( ) ).
    ro_html->add( render_supported_object_types( ) ).
    ro_html->add( '</div>' ).

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

    ro_html->add( |<p>abapGit version: { gc_abap_version }</p>| ).
    ro_html->add( |<p>XML version:     { gc_xml_version }</p>| ).
    ro_html->add( |<p>GUI version:     { lv_gui_version }</p>| ).

  ENDMETHOD. "render_debug_info

  METHOD render_supported_object_types.

    DATA: lt_objects TYPE STANDARD TABLE OF ko100,
          lv_list    TYPE string,
          ls_item    TYPE ty_item.

    FIELD-SYMBOLS <object> LIKE LINE OF lt_objects.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1.

    LOOP AT lt_objects ASSIGNING <object> WHERE pgmid = 'R3TR'.
      ls_item-obj_type = <object>-object.
      IF lcl_objects=>is_supported( is_item = ls_item iv_native_only = abap_true ) = abap_true.
        IF lv_list IS INITIAL.
          lv_list = ls_item-obj_type.
        ELSE.
          lv_list = lv_list && `, ` && ls_item-obj_type.
        ENDIF.
      ENDIF.
    ENDLOOP.

    rv_html = |</p>Supported objects: { lv_list }</p>|.

  ENDMETHOD.  " render_supported_object_types

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DEBUG INFO STYLES */'.
    _add 'div.debug_container {'.
    _add '  padding: 0.5em;'.
    _add '  font-size: 10pt;'.
    _add '  color: #444;'.
    _add '  font-family: Consolas, Courier, monospace;'.
    _add '}'.
    _add 'div.debug_container p {'.
    _add '  margin: 0px;'.
    _add '}'.

  ENDMETHOD.

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'debugOutput("Browser: " + navigator.userAgent, "debug_info");' ).

  ENDMETHOD.  "scripts

ENDCLASS.                       "lcl_gui_page_debuginfo