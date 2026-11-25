INTERFACE zif_abapgit_unit_test_runner
  PUBLIC.

  TYPES:
    BEGIN OF ty_method,
      name        TYPE string,
      failure     TYPE string,
      has_runtime TYPE abap_bool,
      runtime_ms  TYPE i,
    END OF ty_method,
    ty_methods_tt TYPE STANDARD TABLE OF ty_method WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_class,
      name    TYPE string,
      methods TYPE ty_methods_tt,
    END OF ty_class,
    ty_classes_tt TYPE STANDARD TABLE OF ty_class WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_program,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      name     TYPE string,
      classes  TYPE ty_classes_tt,
    END OF ty_program,
    ty_programs_tt TYPE STANDARD TABLE OF ty_program WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_result,
      errors   TYPE string_table,
      programs TYPE ty_programs_tt,
      summary  TYPE string,
    END OF ty_result.

  "! Runs the unit tests for all objects in the repository
  METHODS run
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
