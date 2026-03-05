CLASS zcl_abapgit_diff_factory DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(ri_diff) TYPE REF TO zif_abapgit_diff.
ENDCLASS.

CLASS zcl_abapgit_diff_factory IMPLEMENTATION.

  METHOD get.
    CREATE OBJECT ri_diff TYPE zcl_abapgit_diff.
  ENDMETHOD.

ENDCLASS.
