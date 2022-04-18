CLASS zcl_abapgit_persist_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_persist_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_repo
      RETURNING
        VALUE(ri_repo) TYPE REF TO zif_abapgit_persist_repo .
    CLASS-METHODS get_repo_cs
      RETURNING
        VALUE(ri_repo_cs) TYPE REF TO zif_abapgit_persist_repo_cs .
    CLASS-METHODS get_settings
      RETURNING
        VALUE(ri_settings) TYPE REF TO zif_abapgit_persist_settings .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_repo TYPE REF TO zif_abapgit_persist_repo .
    CLASS-DATA gi_repo_cs TYPE REF TO zif_abapgit_persist_repo_cs .
    CLASS-DATA gi_settings TYPE REF TO zif_abapgit_persist_settings .
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_FACTORY IMPLEMENTATION.


  METHOD get_repo.

    IF gi_repo IS INITIAL.
      CREATE OBJECT gi_repo TYPE zcl_abapgit_persistence_repo.
    ENDIF.

    ri_repo = gi_repo.

  ENDMETHOD.

  METHOD get_repo_cs.

    IF gi_repo_cs IS INITIAL.
      CREATE OBJECT gi_repo_cs TYPE zcl_abapgit_persistence_repo.
    ENDIF.

    ri_repo_cs = gi_repo_cs.

  ENDMETHOD.

  METHOD get_settings.

    IF gi_settings IS INITIAL.
      CREATE OBJECT gi_settings TYPE zcl_abapgit_persist_settings.
    ENDIF.

    ri_settings = gi_settings.

  ENDMETHOD.
ENDCLASS.
