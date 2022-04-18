CLASS zcl_abapgit_persist_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS set_repo
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_persist_repo .

    CLASS-METHODS set_repo_cs
      IMPORTING
        !ii_repo_cs TYPE REF TO zif_abapgit_persist_repo_cs .

    CLASS-METHODS set_settings
      IMPORTING
        !ii_settings TYPE REF TO zif_abapgit_persist_settings .

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_INJECTOR IMPLEMENTATION.


  METHOD set_repo.

    zcl_abapgit_persist_factory=>gi_repo = ii_repo.

  ENDMETHOD.

  METHOD set_repo_cs.

    zcl_abapgit_persist_factory=>gi_repo_cs = ii_repo_cs.

  ENDMETHOD.

  METHOD set_settings.

    zcl_abapgit_persist_factory=>gi_settings = ii_settings.

  ENDMETHOD.
ENDCLASS.
