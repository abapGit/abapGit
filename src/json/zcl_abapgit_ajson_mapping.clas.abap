CLASS zcl_abapgit_ajson_mapping DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF rename_by,
        attr_name TYPE i VALUE 0,
        full_path TYPE i VALUE 1,
        pattern TYPE i VALUE 2,
        " regex type i value 3, " TODO add if needed in future
      END OF rename_by.

    CLASS-METHODS create_camel_case " DEPRECATED
      IMPORTING
        it_mapping_fields   TYPE zif_abapgit_ajson_mapping=>ty_mapping_fields OPTIONAL
        iv_first_json_upper TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ri_mapping)   TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_upper_case
      IMPORTING
        it_mapping_fields TYPE zif_abapgit_ajson_mapping=>ty_mapping_fields OPTIONAL
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_lower_case
      IMPORTING
        it_mapping_fields TYPE zif_abapgit_ajson_mapping=>ty_mapping_fields OPTIONAL
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_field_mapping " DEPRECATED
      IMPORTING
        it_mapping_fields TYPE zif_abapgit_ajson_mapping=>ty_mapping_fields
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_rename
      IMPORTING
        it_rename_map TYPE zif_abapgit_ajson_mapping=>tty_rename_map
        iv_rename_by TYPE i DEFAULT rename_by-attr_name
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_compound_mapper
      IMPORTING
        ii_mapper1 TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
        ii_mapper2 TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
        ii_mapper3 TYPE REF TO zif_abapgit_ajson_mapping OPTIONAL
        it_more TYPE zif_abapgit_ajson_mapping=>ty_table_of OPTIONAL
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_to_snake_case
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

    CLASS-METHODS create_to_camel_case
      IMPORTING
        iv_first_json_upper TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_mapping) TYPE REF TO zif_abapgit_ajson_mapping.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_ajson_mapping IMPLEMENTATION.


  METHOD create_camel_case.

    CREATE OBJECT ri_mapping TYPE lcl_mapping_camel
      EXPORTING
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.


  METHOD create_compound_mapper.

    DATA lt_queue TYPE zif_abapgit_ajson_mapping=>ty_table_of.

    APPEND ii_mapper1 TO lt_queue.
    APPEND ii_mapper2 TO lt_queue.
    APPEND ii_mapper3 TO lt_queue.
    APPEND LINES OF it_more TO lt_queue.
    DELETE lt_queue WHERE table_line IS INITIAL.

    CREATE OBJECT ri_mapping TYPE lcl_compound_mapper
      EXPORTING
        it_queue = lt_queue.

  ENDMETHOD.


  METHOD create_field_mapping.

    CREATE OBJECT ri_mapping TYPE lcl_mapping_fields
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.


  METHOD create_lower_case.

    CREATE OBJECT ri_mapping TYPE lcl_mapping_to_lower
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.


  METHOD create_rename.

    CREATE OBJECT ri_mapping TYPE lcl_rename
      EXPORTING
        it_rename_map = it_rename_map
        iv_rename_by = iv_rename_by.

  ENDMETHOD.


  METHOD create_to_camel_case.

    CREATE OBJECT ri_mapping TYPE lcl_to_camel
      EXPORTING
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.


  METHOD create_to_snake_case.

    CREATE OBJECT ri_mapping TYPE lcl_to_snake.

  ENDMETHOD.


  METHOD create_upper_case.

    CREATE OBJECT ri_mapping TYPE lcl_mapping_to_upper
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
ENDCLASS.
