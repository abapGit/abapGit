CLASS zcl_abapgit_ui_core_injector DEFINITION
  PUBLIC
  FOR TESTING
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS set_html_viewer
      IMPORTING
        !ii_html_viewer TYPE REF TO zif_abapgit_html_viewer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_ui_core_injector IMPLEMENTATION.

  METHOD set_html_viewer.

    zcl_abapgit_ui_core_factory=>gi_html_viewer = ii_html_viewer.

  ENDMETHOD.

ENDCLASS.
