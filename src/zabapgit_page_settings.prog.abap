*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_SETTINGS
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_settings DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.
  PRIVATE SECTION.
    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
ENDCLASS.

CLASS lcl_gui_page_settings IMPLEMENTATION.

  METHOD lif_gui_page~render.
    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'Settings' ) ).
  ENDMETHOD.

  METHOD styles.
    CREATE OBJECT ro_html.

    _add '/* settings STYLES */'.
    _add 'div.settings_container {'.
    _add '  padding: 0.5em;'.
    _add '  font-size: 10pt;'.
    _add '  color: #444;'.
    _add '}'.
  ENDMETHOD.

ENDCLASS.