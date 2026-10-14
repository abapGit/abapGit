CLASS zcl_abapgit_repo_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_repo_injector .

  PUBLIC SECTION.
    CLASS-METHODS get_repo_pull
      IMPORTING
        !ii_repo            TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ri_repo_pull) TYPE REF TO zif_abapgit_repo_pull.
    CLASS-METHODS get_repo_push
      IMPORTING
        !ii_repo_online     TYPE REF TO zif_abapgit_repo_online
      RETURNING
        VALUE(ri_repo_push) TYPE REF TO zif_abapgit_repo_push.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gi_repo_pull TYPE REF TO zif_abapgit_repo_pull.
    CLASS-DATA gi_repo_push TYPE REF TO zif_abapgit_repo_push.
ENDCLASS.



CLASS zcl_abapgit_repo_factory IMPLEMENTATION.

  METHOD get_repo_pull.

    " bound to one repository, so a new instance unless a test double is injected
    IF gi_repo_pull IS BOUND.
      ri_repo_pull = gi_repo_pull.
    ELSE.
      CREATE OBJECT ri_repo_pull TYPE zcl_abapgit_repo_pull
        EXPORTING
          ii_repo = ii_repo.
    ENDIF.

  ENDMETHOD.


  METHOD get_repo_push.

    " bound to one repository, so a new instance unless a test double is injected
    IF gi_repo_push IS BOUND.
      ri_repo_push = gi_repo_push.
    ELSE.
      CREATE OBJECT ri_repo_push TYPE zcl_abapgit_repo_push
        EXPORTING
          ii_repo_online = ii_repo_online.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
