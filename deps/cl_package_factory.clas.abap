CLASS cl_package_factory DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.

    INTERFACES if_package.

    CLASS-METHODS load_package
      IMPORTING
        i_package_name   TYPE devclass
        i_force_reload   TYPE abap_bool OPTIONAL
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
        i_reuse_deleted_object       TYPE abap_bool DEFAULT 'X'
        i_suppress_dialog            TYPE abap_bool DEFAULT abap_false
        i_suppress_access_permission TYPE abap_bool DEFAULT abap_false
      EXPORTING
        VALUE(e_package)             TYPE REF TO if_package
      CHANGING
        c_package_data               TYPE scompkdtln.

  PRIVATE SECTION.
    DATA ls_tdevc TYPE tdevc.
ENDCLASS.

CLASS cl_package_factory IMPLEMENTATION.
  METHOD if_package~save.
    INSERT INTO tdevc VALUES ls_tdevc.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD if_package~delete.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~set_changeable.
    RETURN.
  ENDMETHOD.

  METHOD if_package~get_changeable.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~get_all_attributes.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~save_generic.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~set_all_attributes.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~set_permissions_changeable.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~get_permissions_to_use.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD if_package~add_permission_to_use.
    ASSERT 1 = 2. " todo, implement method
  ENDMETHOD.

  METHOD load_package.
    DATA ls_tdevc   TYPE tdevc.
    DATA lo_package TYPE REF TO cl_package_factory.

    SELECT SINGLE * FROM tdevc INTO ls_tdevc WHERE devclass = i_package_name.
    IF sy-subrc <> 0.
      RAISE object_not_existing.
    ENDIF.

    CREATE OBJECT lo_package TYPE cl_package_factory.
    lo_package->ls_tdevc = ls_tdevc.
    e_package ?= lo_package.
  ENDMETHOD.

  METHOD create_new_package.

    DATA ls_tdevc TYPE tdevc.
    DATA lo_package TYPE REF TO cl_package_factory.

    MOVE-CORRESPONDING c_package_data TO ls_tdevc.

    CREATE OBJECT lo_package TYPE cl_package_factory.
    lo_package->ls_tdevc = ls_tdevc.
    e_package ?= lo_package.
  ENDMETHOD.
ENDCLASS.
