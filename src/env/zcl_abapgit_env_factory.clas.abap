CLASS zcl_abapgit_env_factory DEFINITION PUBLIC GLOBAL FRIENDS zcl_abapgit_env_injector.
  PUBLIC SECTION.
    CLASS-METHODS get_user_record
      RETURNING
        VALUE(ri_user_record) TYPE REF TO zif_abapgit_user_record.

  PRIVATE SECTION.
    CLASS-DATA gi_user_record TYPE REF TO zif_abapgit_user_record.
ENDCLASS.

CLASS zcl_abapgit_env_factory IMPLEMENTATION.

  METHOD get_user_record.
    IF gi_user_record IS NOT BOUND.
      CREATE OBJECT gi_user_record TYPE zcl_abapgit_user_record.
    ENDIF.

    ri_user_record = gi_user_record.
  ENDMETHOD.

ENDCLASS.
