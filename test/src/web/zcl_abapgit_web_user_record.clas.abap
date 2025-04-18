CLASS zcl_abapgit_web_user_record DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_user_record.
ENDCLASS.

CLASS zcl_abapgit_web_user_record IMPLEMENTATION.

  METHOD zif_abapgit_user_record~get_name.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_user_record~get_email.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_user_record~get_title.
    RETURN. " todo, implement method
  ENDMETHOD.

ENDCLASS.
