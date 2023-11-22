CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS upsert FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD upsert.

    zcl_abapgit_object_zag1=>upsert(
      iv_name    = 'ZFOOBAR'
      iv_value   = 'hello'
      iv_package = 'ZFOOBAR' ).

  ENDMETHOD.

ENDCLASS.
