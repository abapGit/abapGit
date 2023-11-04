CLASS zcl_abapgit_git_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS
      get_v2_porcelain
        RETURNING VALUE(ri_v2) TYPE REF TO zif_abapgit_gitv2_porcelain.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GIT_FACTORY IMPLEMENTATION.


  METHOD get_v2_porcelain.
    CREATE OBJECT ri_v2 TYPE zcl_abapgit_gitv2_porcelain.
  ENDMETHOD.
ENDCLASS.
