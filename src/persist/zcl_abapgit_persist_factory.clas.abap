CLASS zcl_abapgit_persist_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_persist_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_repo
      RETURNING
        VALUE(ri_repo) TYPE REF TO zif_abapgit_persist_repo .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_repo TYPE REF TO zif_abapgit_persist_repo .
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_FACTORY IMPLEMENTATION.


  METHOD get_repo.

    IF gi_repo IS INITIAL.
      CREATE OBJECT gi_repo TYPE zcl_abapgit_persistence_repo.
    ENDIF.

    ri_repo = gi_repo.

  ENDMETHOD.
ENDCLASS.
