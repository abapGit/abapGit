CLASS zcl_abapgit_ui_injector DEFINITION
  PUBLIC
  FOR TESTING
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS set_popups
      IMPORTING
        !ii_popups TYPE REF TO zif_abapgit_popups .
    CLASS-METHODS set_frontend_services
      IMPORTING
        !ii_fe_serv TYPE REF TO zif_abapgit_frontend_services .
    CLASS-METHODS set_gui_services
      IMPORTING
        !ii_gui_services TYPE REF TO zif_abapgit_gui_services .
    CLASS-METHODS get_dummy_gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO zif_abapgit_gui_services .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_ui_injector IMPLEMENTATION.


  METHOD get_dummy_gui_services.

    ri_gui_services = lcl_gui_services_dummy=>create( ).

  ENDMETHOD.


  METHOD set_frontend_services.

    zcl_abapgit_ui_factory=>gi_fe_services = ii_fe_serv.

  ENDMETHOD.


  METHOD set_gui_services.

    zcl_abapgit_ui_factory=>gi_gui_services = ii_gui_services.

  ENDMETHOD.


  METHOD set_popups.

    zcl_abapgit_ui_factory=>gi_popups = ii_popups.

  ENDMETHOD.
ENDCLASS.
