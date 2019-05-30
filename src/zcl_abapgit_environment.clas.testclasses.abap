
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_environment.

    METHODS:
      setup,
      is_cloud FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD is_cloud.

    mo_cut->is_sap_cloud_platform( ).
* no assertions, just make sure the dynamic code runs

  ENDMETHOD.

ENDCLASS.
