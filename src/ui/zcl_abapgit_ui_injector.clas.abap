CLASS zcl_abapgit_ui_injector DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      set_popups
        IMPORTING
          ii_popups TYPE REF TO zif_abapgit_popups,

      set_tag_popups
        IMPORTING
          ii_tag_popups TYPE REF TO zif_abapgit_tag_popups,

      set_gui_functions
        IMPORTING
          ii_gui_functions TYPE REF TO zif_abapgit_gui_functions.

ENDCLASS.



CLASS ZCL_ABAPGIT_UI_INJECTOR IMPLEMENTATION.


  METHOD set_gui_functions.

    zcl_abapgit_ui_factory=>gi_gui_functions = ii_gui_functions.

  ENDMETHOD.


  METHOD set_popups.

    zcl_abapgit_ui_factory=>gi_popups = ii_popups.

  ENDMETHOD.


  METHOD set_tag_popups.

    zcl_abapgit_ui_factory=>gi_tag_popups = ii_tag_popups.

  ENDMETHOD.
ENDCLASS.
