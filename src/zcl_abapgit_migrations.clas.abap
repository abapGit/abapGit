CLASS zcl_abapgit_migrations DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_migrations IMPLEMENTATION.


  METHOD run.

    " Migrate STDTEXT to TABLE
    zcl_abapgit_persist_migrate=>run( ).

    " Create ZIF_APACK_MANIFEST interface
    zcl_abapgit_apack_migration=>run( ).

  ENDMETHOD.

ENDCLASS.
