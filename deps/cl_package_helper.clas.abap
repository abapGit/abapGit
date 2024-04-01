CLASS cl_package_helper DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS check_package_name
      IMPORTING
        i_package_name TYPE any.

    CLASS-METHODS check_package_existence
      IMPORTING
        i_package_name   TYPE any
      EXPORTING
        e_package_exists TYPE abap_bool.
ENDCLASS.

CLASS cl_package_helper IMPLEMENTATION.

  METHOD check_package_name.
    ASSERT i_package_name IS NOT INITIAL.
  ENDMETHOD.

  METHOD check_package_existence.
    ASSERT i_package_name IS NOT INITIAL.
    e_package_exists = abap_true.
  ENDMETHOD.

ENDCLASS.
