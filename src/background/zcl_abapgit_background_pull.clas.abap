CLASS zcl_abapgit_background_pull DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_BACKGROUND_PULL IMPLEMENTATION.


  METHOD zif_abapgit_background~get_description.

    rv_description = 'Automatic pull' ##NO_TEXT.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_background~run.

    DATA: ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.


* todo, set defaults in ls_checks
    io_repo->deserialize( is_checks = ls_checks
                          ii_log    = ii_log ).

  ENDMETHOD.
ENDCLASS.
