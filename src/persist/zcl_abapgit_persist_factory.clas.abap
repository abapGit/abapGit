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
    CLASS-METHODS get_background
      RETURNING
        VALUE(ri_background) TYPE REF TO zif_abapgit_persist_background.
    CLASS-METHODS get_packages
      RETURNING
        VALUE(ri_packages) TYPE REF TO zif_abapgit_persist_packages.
    CLASS-METHODS get_user
      IMPORTING
        !iv_user       TYPE sy-uname DEFAULT sy-uname
      RETURNING
        VALUE(ri_user) TYPE REF TO zif_abapgit_persist_user.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_repo TYPE REF TO zif_abapgit_persist_repo .
    CLASS-DATA gi_repo_cs TYPE REF TO zif_abapgit_persist_repo_cs .
    CLASS-DATA gi_settings TYPE REF TO zif_abapgit_persist_settings .
    CLASS-DATA gi_background TYPE REF TO zif_abapgit_persist_background.
    CLASS-DATA gi_packages TYPE REF TO zif_abapgit_persist_packages.
    CLASS-DATA gi_current_user TYPE REF TO zif_abapgit_persist_user.
ENDCLASS.



CLASS zcl_abapgit_persist_factory IMPLEMENTATION.


  METHOD get_background.

    IF gi_background IS INITIAL.
      CREATE OBJECT gi_background TYPE zcl_abapgit_persist_background.
    ENDIF.

    ri_background = gi_background.

  ENDMETHOD.


  METHOD get_packages.

    IF gi_packages IS INITIAL.
      CREATE OBJECT gi_packages TYPE zcl_abapgit_persist_packages.
    ENDIF.

    ri_packages = gi_packages.

  ENDMETHOD.


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


  METHOD get_user.

    IF iv_user = sy-uname ##USER_OK.
      IF gi_current_user IS NOT BOUND.
        CREATE OBJECT gi_current_user TYPE zcl_abapgit_persistence_user.
      ENDIF.
      ri_user = gi_current_user.
    ELSE.
      CREATE OBJECT ri_user TYPE zcl_abapgit_persistence_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
