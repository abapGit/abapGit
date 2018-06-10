CLASS zcl_abapgit_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS set_tadir
      IMPORTING
        !ii_tadir TYPE REF TO zif_abapgit_tadir .

ENDCLASS.



CLASS zcl_abapgit_injector IMPLEMENTATION.

  METHOD set_tadir.

    zcl_abapgit_factory=>mi_tadir = ii_tadir.

  ENDMETHOD.

ENDCLASS.
