CLASS zcl_abapgit_persist_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS set_repo
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_persist_repo .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_INJECTOR IMPLEMENTATION.


  METHOD set_repo.

    zcl_abapgit_persist_factory=>gi_repo = ii_repo.

  ENDMETHOD.
ENDCLASS.
