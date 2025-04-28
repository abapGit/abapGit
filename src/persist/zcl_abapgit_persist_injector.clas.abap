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

    CLASS-METHODS set_background
      IMPORTING
        !ii_background TYPE REF TO zif_abapgit_persist_background.

    CLASS-METHODS set_packages
      IMPORTING
        !ii_packages TYPE REF TO zif_abapgit_persist_packages.

    CLASS-METHODS set_current_user
      IMPORTING
        !ii_current_user TYPE REF TO zif_abapgit_persist_user.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_abapgit_persist_injector IMPLEMENTATION.


  METHOD set_background.

    zcl_abapgit_persist_factory=>gi_background = ii_background.

  ENDMETHOD.


  METHOD set_current_user.

    zcl_abapgit_persist_factory=>gi_current_user = ii_current_user.

  ENDMETHOD.


  METHOD set_packages.

    zcl_abapgit_persist_factory=>gi_packages = ii_packages.

  ENDMETHOD.


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
