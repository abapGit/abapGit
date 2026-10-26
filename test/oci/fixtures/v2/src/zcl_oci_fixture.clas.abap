CLASS zcl_oci_fixture DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS get_version
      RETURNING VALUE(rv_version) TYPE string.
ENDCLASS.

CLASS zcl_oci_fixture IMPLEMENTATION.
  METHOD get_version.
    rv_version = 'v2'.
  ENDMETHOD.
ENDCLASS.
