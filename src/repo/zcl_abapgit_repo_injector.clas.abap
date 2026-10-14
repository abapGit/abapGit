CLASS zcl_abapgit_repo_injector DEFINITION
  PUBLIC
  FOR TESTING
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS set_repo_pull
      IMPORTING
        ii_repo_pull TYPE REF TO zif_abapgit_repo_pull.
    CLASS-METHODS set_repo_push
      IMPORTING
        ii_repo_push TYPE REF TO zif_abapgit_repo_push.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_repo_injector IMPLEMENTATION.

  METHOD set_repo_pull.
    zcl_abapgit_repo_factory=>gi_repo_pull = ii_repo_pull.
  ENDMETHOD.


  METHOD set_repo_push.
    zcl_abapgit_repo_factory=>gi_repo_push = ii_repo_push.
  ENDMETHOD.
ENDCLASS.
