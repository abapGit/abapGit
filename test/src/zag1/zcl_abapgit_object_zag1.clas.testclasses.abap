CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS create FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD create.

    zcl_abapgit_object_zag1=>create(
      iv_name  = 'ZFOOBAR'
      iv_value = 'hello' ).

  ENDMETHOD.

ENDCLASS.
