CLASS zcl_abapgit_git_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_git_injector .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_v2_porcelain
        RETURNING VALUE(ri_v2) TYPE REF TO zif_abapgit_gitv2_porcelain,

      get_git_transport
        RETURNING
          VALUE(ri_git_transport) TYPE REF TO zif_abapgit_git_transport.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_git_transport TYPE REF TO zif_abapgit_git_transport.

ENDCLASS.



CLASS zcl_abapgit_git_factory IMPLEMENTATION.

  METHOD get_v2_porcelain.
    CREATE OBJECT ri_v2 TYPE zcl_abapgit_gitv2_porcelain.
  ENDMETHOD.


  METHOD get_git_transport.

    IF gi_git_transport IS INITIAL.
      CREATE OBJECT gi_git_transport TYPE zcl_abapgit_git_transport.
    ENDIF.

    ri_git_transport = gi_git_transport.

  ENDMETHOD.

ENDCLASS.
