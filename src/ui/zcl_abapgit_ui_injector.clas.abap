CLASS zcl_abapgit_ui_injector DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      set_popups
        IMPORTING
          ii_popups TYPE REF TO zif_abapgit_popups.

ENDCLASS.



CLASS zcl_abapgit_ui_injector IMPLEMENTATION.

  METHOD set_popups.

    zcl_abapgit_ui_factory=>mi_popups = ii_popups.

  ENDMETHOD.

ENDCLASS.
