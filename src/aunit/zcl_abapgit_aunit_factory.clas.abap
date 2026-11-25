CLASS zcl_abapgit_aunit_factory DEFINITION PUBLIC.
  PUBLIC SECTION.
    "! Classic runner if available, otherwise the modern one.
    "! Raises an exception if neither is supported in the release
    CLASS-METHODS get_runner
      IMPORTING
        !ii_repo         TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ri_runner) TYPE REF TO zif_abapgit_unit_test_runner
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_aunit_factory IMPLEMENTATION.

  METHOD get_runner.
    TRY.
        CREATE OBJECT ri_runner TYPE zcl_abapgit_unit_test_classic
          EXPORTING
            ii_repo = ii_repo.
      CATCH zcx_abapgit_exception.
        CREATE OBJECT ri_runner TYPE zcl_abapgit_unit_test_modern
          EXPORTING
            ii_repo = ii_repo.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
