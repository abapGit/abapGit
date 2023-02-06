INTERFACE zif_abapgit_ajson_mapping
  PUBLIC.

  TYPES:
    BEGIN OF ty_mapping_field, " deprecated, will be removed
      abap TYPE string,
      json TYPE string,
    END OF ty_mapping_field,
    ty_mapping_fields TYPE STANDARD TABLE OF ty_mapping_field
      WITH UNIQUE SORTED KEY abap COMPONENTS abap
      WITH UNIQUE SORTED KEY json COMPONENTS json.

  TYPES:
    BEGIN OF ty_rename,
      from TYPE string,
      to TYPE string,
    END OF ty_rename,
    tty_rename_map TYPE STANDARD TABLE OF ty_rename
      WITH UNIQUE SORTED KEY by_name COMPONENTS from.

  TYPES:
    ty_table_of TYPE STANDARD TABLE OF REF TO zif_abapgit_ajson_mapping.

  METHODS to_abap " deprecated, will be removed
    IMPORTING
      !iv_path         TYPE string
      !iv_name         TYPE string
    RETURNING
      VALUE(rv_result) TYPE string.

  METHODS to_json " deprecated, will be removed
    IMPORTING
      !iv_path         TYPE string
      !iv_name         TYPE string
    RETURNING
      VALUE(rv_result) TYPE string.

  METHODS rename_node
    IMPORTING
      !is_node TYPE zif_abapgit_ajson_types=>ty_node
    CHANGING
      !cv_name TYPE zif_abapgit_ajson_types=>ty_node-name.

ENDINTERFACE.
