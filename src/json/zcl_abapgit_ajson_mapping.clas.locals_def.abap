CLASS lcl_mapping_fields DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields TYPE zif_abapgit_ajson_mapping~ty_mapping_fields OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_mapping_fields TYPE zif_abapgit_ajson_mapping~ty_mapping_fields.

ENDCLASS.

CLASS lcl_rename DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_rename_map TYPE zif_abapgit_ajson_mapping~tty_rename_map
        iv_rename_by TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_rename_map TYPE zif_abapgit_ajson_mapping~tty_rename_map.
    DATA mv_rename_by TYPE i.

ENDCLASS.

CLASS lcl_mapping_to_upper DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields TYPE zif_abapgit_ajson_mapping~ty_mapping_fields OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mi_mapping_fields TYPE REF TO zif_abapgit_ajson_mapping.

ENDCLASS.


CLASS lcl_mapping_to_lower DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields TYPE zif_abapgit_ajson_mapping~ty_mapping_fields OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mi_mapping_fields TYPE REF TO zif_abapgit_ajson_mapping.

ENDCLASS.


CLASS lcl_mapping_camel DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_mapping_fields   TYPE zif_abapgit_ajson_mapping~ty_mapping_fields OPTIONAL
        iv_first_json_upper TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_first_json_upper TYPE abap_bool.
    DATA mi_mapping_fields TYPE REF TO zif_abapgit_ajson_mapping.

ENDCLASS.

CLASS lcl_compound_mapper DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.

    METHODS constructor
      IMPORTING
        it_queue TYPE zif_abapgit_ajson_mapping=>ty_table_of.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_queue TYPE zif_abapgit_ajson_mapping=>ty_table_of.

ENDCLASS.

CLASS lcl_to_snake DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.
ENDCLASS.

CLASS lcl_to_camel DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.
    METHODS constructor
      IMPORTING
        iv_first_json_upper TYPE abap_bool.
  PRIVATE SECTION.
    DATA mv_first_json_upper TYPE abap_bool.
ENDCLASS.
