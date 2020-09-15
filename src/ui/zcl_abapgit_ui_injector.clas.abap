CLASS zcl_abapgit_ui_injector DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS set_popups
      IMPORTING
        !ii_popups TYPE REF TO zif_abapgit_popups .
    CLASS-METHODS set_tag_popups
      IMPORTING
        !ii_tag_popups TYPE REF TO zif_abapgit_tag_popups .
    CLASS-METHODS set_gui_functions
      IMPORTING
        !ii_gui_functions TYPE REF TO zif_abapgit_gui_functions .
    CLASS-METHODS set_gui_services
      IMPORTING
        !ii_gui_services TYPE REF TO zif_abapgit_gui_services .
    CLASS-METHODS get_dummy_gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO zif_abapgit_gui_services .
    CLASS-METHODS set_html_viewer
      IMPORTING
        !ii_html_viewer TYPE REF TO zif_abapgit_html_viewer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_UI_INJECTOR IMPLEMENTATION.


  METHOD get_dummy_gui_services.

    ri_gui_services = lcl_gui_services_dummy=>create( ).

  ENDMETHOD.


  METHOD set_gui_functions.

    zcl_abapgit_ui_factory=>gi_gui_functions = ii_gui_functions.

  ENDMETHOD.


  METHOD set_gui_services.

    zcl_abapgit_ui_factory=>gi_gui_services = ii_gui_services.

  ENDMETHOD.


  METHOD set_html_viewer.

    zcl_abapgit_ui_factory=>gi_html_viewer = ii_html_viewer.

  ENDMETHOD.


  METHOD set_popups.

    zcl_abapgit_ui_factory=>gi_popups = ii_popups.

  ENDMETHOD.


  METHOD set_tag_popups.

    zcl_abapgit_ui_factory=>gi_tag_popups = ii_tag_popups.

  ENDMETHOD.
ENDCLASS.
