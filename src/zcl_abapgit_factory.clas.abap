CLASS zcl_abapgit_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_injector.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_tadir
        RETURNING
          VALUE(ri_tadir) TYPE REF TO zif_abapgit_tadir.

  PRIVATE SECTION.
    CLASS-DATA:
      mi_tadir TYPE REF TO zif_abapgit_tadir.

ENDCLASS.



CLASS zcl_abapgit_factory IMPLEMENTATION.

  METHOD get_tadir.

    IF mi_tadir IS INITIAL.
      CREATE OBJECT mi_tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    ri_tadir = mi_tadir.

  ENDMETHOD.

ENDCLASS.
