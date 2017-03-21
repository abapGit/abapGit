*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_REPO_SETTINGS
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_repo_settings DEFINITION FINAL INHERITING FROM lcl_gui_page.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

ENDCLASS.                       "lcl_gui_page_debuginfo

CLASS lcl_gui_page_repo_settings IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'REPO SETTINGS'.
  ENDMETHOD.  " constructor.

  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="settings_container">stuff here</div>' ).

  ENDMETHOD.  "render_content

ENDCLASS.                       "lcl_gui_page_debuginfo
