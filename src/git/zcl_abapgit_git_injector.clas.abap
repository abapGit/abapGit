CLASS zcl_abapgit_git_injector DEFINITION
  PUBLIC
  FOR TESTING
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      set_git_transport
        IMPORTING
          ii_git_transport TYPE REF TO zif_abapgit_git_transport.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_git_injector IMPLEMENTATION.


  METHOD set_git_transport.
    zcl_abapgit_git_factory=>gi_git_transport = ii_git_transport.
  ENDMETHOD.
ENDCLASS.
