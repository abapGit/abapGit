CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS upsert FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD upsert.

    DATA li_repo_srv TYPE REF TO zif_abapgit_repo_srv.

    ASSERT sy-sysid = 'ABC'.

    zcl_abapgit_object_zag1=>upsert(
      iv_name    = 'ZFOOBAR'
      iv_value   = 'hello'
      iv_package = 'ZFOOBAR' ).

    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).

  ENDMETHOD.

ENDCLASS.
