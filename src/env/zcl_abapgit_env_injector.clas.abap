CLASS zcl_abapgit_env_injector DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS set_user_record
      IMPORTING
        !ii_user_record TYPE REF TO zif_abapgit_user_record.
ENDCLASS.

CLASS zcl_abapgit_env_injector IMPLEMENTATION.

  METHOD set_user_record.
    zcl_abapgit_env_factory=>gi_user_record = ii_user_record.
  ENDMETHOD.

ENDCLASS.
