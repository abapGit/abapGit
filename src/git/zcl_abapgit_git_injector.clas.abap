CLASS zcl_abapgit_git_injector DEFINITION
  PUBLIC
  FOR TESTING
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      set_git_transport
        IMPORTING
          ii_git_transport TYPE REF TO zif_abapgit_git_transport OPTIONAL.
    CLASS-METHODS:
      set_v2_porcelain
        IMPORTING
          ii_v2 TYPE REF TO zif_abapgit_gitv2_porcelain OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_git_injector IMPLEMENTATION.
  METHOD set_v2_porcelain.
    zcl_abapgit_git_factory=>gi_v2 = ii_v2.
  ENDMETHOD.


  METHOD set_git_transport.
    zcl_abapgit_git_factory=>gi_git_transport = ii_git_transport.
  ENDMETHOD.
ENDCLASS.
