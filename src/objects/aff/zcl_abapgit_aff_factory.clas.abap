CLASS zcl_abapgit_aff_factory DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get_registry
      RETURNING
        VALUE(ri_registry) TYPE REF TO zif_abapgit_aff_registry.
  PRIVATE SECTION.
    CLASS-DATA gi_registry TYPE REF TO zif_abapgit_aff_registry.
ENDCLASS.

CLASS zcl_abapgit_aff_factory IMPLEMENTATION.

  METHOD get_registry.
    IF gi_registry IS NOT BOUND.
      CREATE OBJECT gi_registry TYPE zcl_abapgit_aff_registry.
    ENDIF.
    ri_registry = gi_registry.
  ENDMETHOD.

ENDCLASS.
