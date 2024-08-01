CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS create FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD create.

    zcl_abapgit_gitea=>create_repo( 'repo-' && cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ) ).

  ENDMETHOD.

ENDCLASS.
