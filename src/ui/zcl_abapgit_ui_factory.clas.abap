CLASS zcl_abapgit_ui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_injector.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_popups
        RETURNING
          VALUE(ri_popups) TYPE REF TO zif_abapgit_popups,

      get_tag_popups
        RETURNING
          VALUE(ri_tag_popups) TYPE REF TO zif_abapgit_tag_popups.

  PRIVATE SECTION.
    CLASS-DATA:
      mi_popups     TYPE REF TO zif_abapgit_popups,
      mi_tag_popups TYPE REF TO zif_abapgit_tag_popups.

ENDCLASS.



CLASS zcl_abapgit_ui_factory IMPLEMENTATION.

  METHOD get_popups.

    IF mi_popups IS INITIAL.
      CREATE OBJECT mi_popups TYPE zcl_abapgit_popups.
    ENDIF.

    ri_popups = mi_popups.

  ENDMETHOD.

  METHOD get_tag_popups.

    IF mi_tag_popups IS INITIAL.
      CREATE OBJECT mi_tag_popups TYPE zcl_abapgit_tag_popups.
    ENDIF.

    ri_tag_popups = mi_tag_popups.

  ENDMETHOD.

ENDCLASS.
