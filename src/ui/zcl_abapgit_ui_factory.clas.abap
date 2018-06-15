CLASS zcl_abapgit_ui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_injector.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_popups
        RETURNING
          VALUE(ri_popups) TYPE REF TO zif_abapgit_popups.

  PRIVATE SECTION.
    CLASS-DATA:
      mi_popups TYPE REF TO zif_abapgit_popups.

ENDCLASS.



CLASS zcl_abapgit_ui_factory IMPLEMENTATION.

  METHOD get_popups.

    IF mi_popups IS INITIAL.
      CREATE OBJECT mi_popups TYPE zcl_abapgit_popups.
    ENDIF.

    ri_popups = mi_popups.

  ENDMETHOD.

ENDCLASS.
