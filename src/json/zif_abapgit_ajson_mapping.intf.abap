INTERFACE zif_abapgit_ajson_mapping
  PUBLIC.

  TYPES:
    BEGIN OF ty_mapping_field,
      abap TYPE string,
      json TYPE string,
    END OF ty_mapping_field,
    ty_mapping_fields TYPE STANDARD TABLE OF ty_mapping_field
      WITH UNIQUE SORTED KEY abap COMPONENTS abap
      WITH UNIQUE SORTED KEY json COMPONENTS json.

  METHODS to_abap
    IMPORTING
      !iv_path         TYPE string
      !iv_name         TYPE string
    RETURNING
      VALUE(rv_result) TYPE string.

  METHODS to_json
    IMPORTING
      !iv_path         TYPE string
      !iv_name         TYPE string
    RETURNING
      VALUE(rv_result) TYPE string.

ENDINTERFACE.
