CLASS zcl_abapgit_web_setup DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS setup
    RAISING
      zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_web_setup IMPLEMENTATION.
  METHOD setup.

    DATA lo_environment TYPE REF TO zcl_abapgit_web_environment.
    DATA lo_cts_api     TYPE REF TO zcl_abapgit_web_cts_api.
    DATA lo_user_record TYPE REF TO zcl_abapgit_web_user_record.

    zcl_abapgit_web_inject_fm=>inject( ).

    CREATE OBJECT lo_environment.
    zcl_abapgit_injector=>set_environment( lo_environment ).

    CREATE OBJECT lo_cts_api.
    zcl_abapgit_injector=>set_cts_api( lo_cts_api ).

    CREATE OBJECT lo_user_record.
    zcl_abapgit_env_injector=>set_user_record( lo_user_record ).

  ENDMETHOD.
ENDCLASS.
