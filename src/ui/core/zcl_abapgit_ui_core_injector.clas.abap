CLASS zcl_abapgit_ui_core_injector DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS set_html_viewer
      IMPORTING
        !ii_html_viewer TYPE REF TO zif_abapgit_html_viewer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_UI_CORE_INJECTOR IMPLEMENTATION.


  METHOD set_html_viewer.

    zcl_abapgit_ui_core_factory=>gi_html_viewer = ii_html_viewer.

  ENDMETHOD.
ENDCLASS.
