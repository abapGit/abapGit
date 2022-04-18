CLASS zcl_abapgit_migrations DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_MIGRATIONS IMPLEMENTATION.


  METHOD run.

    " Migrate STDTEXT to TABLE
    zcl_abapgit_persist_migrate=>run( ).

    " Create ZIF_APACK_MANIFEST interface
    zcl_abapgit_apack_migration=>run( ).

    " Migrate checksums from repo metadata to separate DB object
    zcl_abapgit_repo_cs_migration=>run( ).

  ENDMETHOD.
ENDCLASS.
