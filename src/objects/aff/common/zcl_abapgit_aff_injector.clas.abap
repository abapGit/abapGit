CLASS zcl_abapgit_aff_injector DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS
      set_registry
        IMPORTING ii_registry TYPE REF TO zif_abapgit_aff_registry.

ENDCLASS.


CLASS zcl_abapgit_aff_injector IMPLEMENTATION.

  METHOD set_registry.
    zcl_abapgit_aff_factory=>gi_registry = ii_registry.
  ENDMETHOD.

ENDCLASS.
