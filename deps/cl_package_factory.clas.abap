CLASS cl_package_factory DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS load_package
      IMPORTING
        i_package_name TYPE devclass
        VALUE(i_force_reload) TYPE abap_bool OPTIONAL
      EXPORTING
        VALUE(e_package) TYPE REF TO if_package
      EXCEPTIONS
        object_not_existing
        unexpected_error
        intern_err
        no_access
        object_locked_and_modified.

    CLASS-METHODS create_new_package
      IMPORTING
        VALUE(i_reuse_deleted_object) TYPE abap_bool DEFAULT 'X'
        !i_suppress_dialog            TYPE abap_bool DEFAULT abap_false
        !i_suppress_access_permission TYPE abap_bool DEFAULT abap_false
      EXPORTING
        VALUE(e_package) TYPE REF TO if_package
      CHANGING
        c_package_data TYPE any.
ENDCLASS.

CLASS cl_package_factory IMPLEMENTATION.
  METHOD load_package.
    RAISE object_not_existing.
  ENDMETHOD.

  METHOD create_new_package.
* this should never be called in unit tests
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.
